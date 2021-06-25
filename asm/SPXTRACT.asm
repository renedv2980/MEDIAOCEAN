*          DATA SET SPXTRACT   AT LEVEL 045 AS OF 02/24/21                      
*PHASE SXTRACTA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE SPXROUTS                 XTRACT RECORD CREATION MODULE                 
*INCLUDE SPXCNVX                  CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE BINSR31                  CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE GETPROF                                                                
*INCLUDE SPFMTINO                                                               
*                                                                               
         TITLE 'SPXTRACT - EXTRACT SPOT SYSTEM FILE SQL DATA'                   
*                                                                               
***********************************************************************         
*  SPOT SQL SUB SYSTEM EXTRACT CONTROL MODULE                         *         
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
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM, SEE RXUSERD              *         
***********************************************************************         
*                                                                               
* WHEN ADDING NEW TABLES:                                                       
* -----------------------                                                       
* 1. UPDATE TYPTAB                                                              
* 2. ADD INIT, FILTER, LOAD AND UPDATE ROUTINES (E.G. INITAGY, FILTAGY,         
*    LOADAGY, UPDTAGY)                                                          
* 3. UPDATE LOADTAB, UPDTTAB WITH ROUTINE ADDRESSES                             
* 4. UPDATE ADDRESS AREA AND ADDRESSD DSECT                                     
* 5. IF THE RECORD LIVES ON A FILE OTHER THAN SPOTDIR/SPOTFILE                  
*    SET THE CORRESPONDING FILE TYPE IN GETTYP, AND ALL THE                     
*    ACCOMPANYING VARIABLES IN PROCKEY, READHI, ETC.                            
*    SEARCH FOR 'STAFILEQ' AND 'XSPFILEQ' TO SEE WHAT IS BEING DONE             
* 6. IF GENERATING MULTIPLE OUTPUT LINES FROM ONE RECORD,                       
*    MAKE SURE YOU SKIP COPY/CHANGE COMPARE LOGIC AT UPDL01A                    
* 7. FOR RECORDS, CONTAINING PACKED CLIENT IN THE KEY,                          
*    UPDATE GETCPROF, TO AVOID AAA/AAN UNPACKING ERRORS                         
*                                                                               
***********************************************************************         
*                                                                               
SXTRACT  CSECT                                                                  
         ENTRY COMFACS                                                          
         ENTRY MASTC                                                            
         ENTRY SSB                                                              
         ENTRY ADDRESS                                                          
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 WORKL,*SPXTR**,CLEAR=YES                                         
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
DXU      USING RXUSERD,DXUSER                                                   
*                                                                               
         MVC   SYSCODE,DXSYSCOD                                                 
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         MVC   PLATFORM,SXDTPLFM                                                
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
LOW      SR    RC,RC                                                            
         CHI   RC,256                                                           
         J     EXIT                                                             
*                                                                               
HIGH     CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
*                                                                               
EQXIT    DS    0X                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NEQXIT   DS    0X                                                               
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
*                                                                               
NOMOREQX OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         BRAS  RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BRAS  RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BRAS  RE,PROCCLOS         CLOSE SYSTEM FILES                           
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
         BNE   MCLOX                                                            
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOX    CLI   DXMODE,DXCLOXDQ                                                  
         BNE   MERR                                                             
         BRAS  RE,PROCCLOX         CLOSE EXTRACT FILE MODEPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
*                                                                               
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                              *         
***********************************************************************         
*                                                                               
PROCLOAD NTR1  ,                                                                
         CLI   VERSION,50          ORGANIZER EXTRACT?                           
         BNE   *+8                                                              
         OI    FLAGS2,ORGANIZQ                                                  
*                                                                               
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
PROCUPDT NTR1  ,                                                                
         L     R1,=A(MAXIOSW)                                                   
         MVC   MAXIOS,0(R1)                                                     
*                                                                               
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
*                                                                               
         CLI   RFILTY,XSPFILQ      TEST XSPOT FILE RECORD TYPE                  
         JE    PROCUP20            ELSE IGNORE RECORD                           
         CLI   RFILTY,SPTFILQ      TEST FILE RECORD TYPE                        
         JE    PROCUP20            ELSE IGNORE RECORD                           
         CLI   RFILTY,STAFILQ      TEST FILE RECORD TYPE                        
         JE    PROCUP20            ELSE IGNORE RECORD                           
         CLI   RFILTY,STRFILQ      TEST SPT TRAFFIC FILE                        
         JE    PROCUP20            ELSE IGNORE RECORD                           
*                                                                               
         CLI   RFILTY,SPTDIRQ      TEST SPTDIR FILE                             
         JNE   YES                                                              
         CLC   =X'0D2AC3',RECDS+RHLENQ+4 COMSCORE DEMO? (SPGENNTDEM)            
         JNE   YES                                                              
*                                                                               
*                                                                               
PROCUP20 DS    0H                                                               
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   PROCUPNX            EXIT ERROR                                   
         J     PROCUPQX            EXIT OK                                      
*                                                                               
PROCUPQX DS    0H                                                               
         L     R1,=A(MAXIOSW)                                                   
         MVC   0(4,R1),MAXIOS                                                   
         J     YES                                                              
*                                                                               
PROCUPNX DS    0H                                                               
         L     R1,=A(MAXIOSW)                                                   
         MVC   0(4,R1),MAXIOS                                                   
         J     NO                                                               
         DROP  R5                                                               
*                                                                               
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
*                                                                               
         USING RECDS,R5                                                         
RX       USING AGYHDR,RECVHDR+L'RECVHDR                                         
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         L     R4,DXACPYB                                                       
         MVC   CTRL4,L'RECVHDR+4+15(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+15(R5)                                         
         MVI   BYTE,13             SPTDIR/FIL KEY LENGTH                        
*                                                                               
         TM    FLAGS,STAFILEQ                                                   
         BNO   PKEY01A                                                          
         MVC   CTRL4,L'RECVHDR+4+17(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+17(R5)                                         
         MVI   BYTE,15             STAFIL KEY LENGTH                            
         B     PKEY01X                                                          
*                                                                               
PKEY01A  DS    0H                                                               
         TM    FLAGS,XSPFILEQ                                                   
         BNO   PKEY01B                                                          
         MVC   CTRL4,L'RECVHDR+4+32(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+32(R5)                                         
         MVI   BYTE,32             XSPDIR/FIL KEY LENGTH                        
*                                                                               
PKEY01B  DS    0H                                                               
         CLI   RFILTY,SPTDIRQ                                                   
         BNE   PKEY01X                                                          
         MVC   CTRL4,L'RECVHDR+4+13+4(R4) RDW+HDR+KEY+DA                        
         MVC   CTRL5,L'RECVHDR+4+13+4(R5)                                       
*                                                                               
PKEY01X  DS    0H                                                               
         LLC   RF,BYTE             KEY LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)       SAME RECORD IN COPY/CHANGE BUFFERS?          
         BE    PKEY01Y             YES, SAME RECORD, ALL OK                     
*                                                                               
* COPY BUFFER IS STALE HERE                                                     
* CLEAR COPY BUFFER'S CONTROL BYTE                                              
         MVI   CTRL4,X'00'                                                      
*                                                                               
PKEY01Y  DS    0H                                                               
         TM    CTRL5,X'80'   IS THIS RECORD DELETED?                            
         BZ    PKEY02              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         TM    CTRL4,X'80'                                                      
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY02   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         TM    CTRL4,X'80'                                                      
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  RX,R5                                                            
*                                                                               
*                                                                               
*                                                                               
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
         DC    V(SPXCNVX)                                                       
         DC    V(PIDTAB)                                                        
         DC    V(PIDTABCA)                                                      
         DC    A(0)                CBLTAB                                       
*                                                                               
ADDRLQ1  EQU   *-ADDRESS                                                        
*                                                                               
* EXTRACT ROUTINES                                                              
*                                                                               
         DC    V(SPTMDC)          MEDIA/AGENCY                                  
         DC    V(SPTCNC)          CLIENT                                        
         DC    V(SPTPDC)          PRODUCT                                       
         DC    V(SPTPLC)          PRODUCT                                       
         DC    V(SPTESC)          ESTIMATE                                      
         DC    V(SPTMKC)          MARKET                                        
         DC    V(SPTSTC)          STATION                                       
         DC    V(SPTEDC)          ESTIMATE DEMO LIST                            
         DC    V(SPTSAC)          STATION ADDRESS RECORD                        
         DC    V(SPTEQV)          EQUIVALENCE FACTORS RECORD                    
         DC    V(SPTCML)          COMMERCIAL RECORDS                            
         DC    V(SPTSOW)          STATION OWNERSHIP INFO                        
         DC    V(SPTFOR)          STATION FORMAT                                
         DC    V(SPTOIN)          OWNER INFO                                    
         DC    V(SPTMGP)          MARKET GROUP DEF                              
         DC    V(SPTMGR)          MARKET GROUP                                  
         DC    V(SPTMGA)          MARKET GROUP ASSIGNMENT                       
         DC    V(SPTCDF)          CLIENT GROUP DEF                              
         DC    V(SPTCGP)          CLIENT GROUPS                                 
         DC    V(SPTCGC)          CLIENT GROUP CLIENTS                          
         DC    V(SPTSDF)          STATION GROUP DEF                             
         DC    V(SPTSGP)          STATION GROUPS                                
         DC    V(SPTSGS)          STATION GROUP STATIONS                        
         DC    V(SPTPGF)          PRODUCT GROUP DEF                             
         DC    V(SPTPGR)          PRODUCT GROUPS                                
         DC    V(SPTPGP)          PRODUCT GROUP PRODUCTS                        
         DC    V(SPTPAR)          PARENT INFO                                   
         DC    V(SPTDEA)          DEAL REACORDS                                 
         DC    V(SPTDST)          DEAL STATIONS                                 
         DC    V(SPTREP)          REP RECORDS                                   
         DC    V(SPTNL)           SYSCODE/NETWORK LIST                          
         DC    V(SPTUCM)          SPOT UCOM RECORD                              
         DC    V(SPTSLK)          STATION LOCKIN RECORD                         
         DC    V(SPTDPM)          DAYPART MENU RECORD                           
         DC    V(SPTDEM)          DEMO CODES AND NAMES                          
         DC    V(SPTDEMT)         TRADITIONAL DEMO CODES AND NAMES              
         DC    V(SPTDEMC)         NON-TRADITIONAL DEMO CODES AND NAMES          
         DC    V(SPTADB)          ADBUYER                                       
         DC    V(SPTEAD)          ESTIMATE AUTHORIZED DOLLARS                   
         DC    V(SPTDAO)          DARE ORDER (BLOCKCHAIN)                       
         DC    V(SPTDMN)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)             
         DC    V(SPTDMX)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)XSP          
         DC    V(SPTBXQ)          BUY EXTRACT REQUEST                           
         DC    V(SPTBIL)          BILLING INFO                                  
         DC    V(SPTFLT)          FLIGHT RECORDS                                
         DC    V(SPTFLF)          FLIGHT RECORD FLIGHTS                         
         DC    V(SPTCMC)          COMSCORE MARKET                               
         DC    V(SPTSPC)          SPILLDEF                                      
         DC    V(SPTSQC)          SQAD DAYPART MENU                             
         DC    V(SPTDMU)          DEMO MENUS                                    
         DC    V(SPTBFM)          BILL FORMULA                                  
         DC    V(SPTPAT)          PATTERN RECORD                                
         DC    V(SPTPPH)          PATTERN HEADER                                
         DC    V(SPTPCM)          PATTERN COMMERCIAL                            
         DC    V(SPTPMK)          PATTERN MARKET                                
         DC    V(SPTPST)          PATTERN STATION                               
         DC    V(SPTPMG)          PATTERN MARKET GROUPS                         
         DC    V(SPTPMA)          PATTERN AFFILIATES                            
         DC    V(SPTPTP)          PATTERN STATION TYPES                         
         DC    V(SPTPCN)          PATTERN COMMENTS                              
         DC    V(SPTSTP)          STEXT PAGE DEFINITION                         
         DC    V(SPTSTL)          STEXT LINE                                    
*                                                                               
ADDRLQ2  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
*                                                                               
ADDRLQ3  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADAGY)          MEDIA/AGENCY                                 
         DC    A(LOADCNT)          CLIENT                                       
         DC    A(LOADPRD)          PRODUCT                                      
         DC    A(LOADPDL)          PRODUCT LIST                                 
         DC    A(LOADEST)          ESTIMATE                                     
         DC    A(LOADMKT)          MARKET                                       
         DC    A(LOADSTA)          STATION                                      
         DC    A(LOADEDL)          ESTIMATE DEMO LIST                           
         DC    A(LOADSAD)          STATION ADDRESS                              
         DC    A(LOADEQV)          EQUIVALENCE FACTORS RECORD                   
         DC    A(LOADCML)          EQUIVALENCE FACTORS RECORD                   
         DC    A(LOADSOW)          STATION OWNERSHIP INFO                       
         DC    A(LOADFOR)          STATION FORMAT                               
         DC    A(LOADOIN)          OWNER INFO                                   
         DC    A(LOADMGD)          MARKET GROUP DEF                             
         DC    A(LOADMGR)          MARKET GROUP                                 
         DC    A(LOADMGA)          MARKET GROUP ASSIGNMENT                      
         DC    A(LOADCGD)          CLIENT GROUP DEF                             
         DC    A(LOADCGP)          CLIENT GROUP                                 
         DC    A(LOADCGC)          CLIENT GROUP CLIENTS                         
         DC    A(LOADSGD)          STATION GROUP DEF                            
         DC    A(LOADSGP)          STATION GROUP                                
         DC    A(LOADSGS)          STATION GROUP STATIONS                       
         DC    A(LOADPGF)          PRODUCT GROUP DEF                            
         DC    A(LOADPGR)          PRODUCT GROUPS                               
         DC    A(LOADPGP)          PRODUCT GROUP PRODUCTS                       
         DC    A(LOADPAR)          PARENT INFO                                  
         DC    A(LOADDEA)          DEAL RECORDS                                 
         DC    A(LOADDST)          DEAL STATIONS                                
         DC    A(LOADREP)          REP RECORDS                                  
         DC    A(LOADSNL)          SYSCODE/NETWORK LIST                         
         DC    A(LOADUCM)          SPOT UCOM RECORD                             
         DC    A(LOADSLK)          STATION LOCKIN RECORD                        
         DC    A(LOADDPM)          DAYPART MENU RECORD                          
         DC    A(LOADDEM)          DEMO CODES AND NAMES                         
         DC    A(LOADDEMT)         TRADITIONAL DEMO CODES AND NAMES             
         DC    A(LOADDEMC)         NON-TRADITIONAL DEMO CODES/NAMES             
         DC    A(LOADADB)          ADBUYER                                      
         DC    A(LOADEAD)          ESTIMATE AUTHORIZED DOLLARS                  
         DC    A(LOADDAO)          DARE ORDER (BLOCKCHAIN)                      
         DC    A(LOADDMN)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)            
         DC    A(LOADDMX)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)XSP         
         DC    A(LOADBIL)          BILLING INFO                                 
         DC    A(LOADFLT)          FLIGHT RCORDS                                
         DC    A(LOADFLF)          FLIGHT RECORD FLIGHTS                        
         DC    A(LOADMKC)          COMSCORE MARKET                              
         DC    A(LOADSPD)          SPILLDEF                                     
         DC    A(LOADSQD)          SQAD DAYPART MENU                            
         DC    A(LOADDMU)          DEMO MENU                                    
         DC    A(LOADBFM)          BILL FORMULA                                 
         DC    A(LOADPAT)          PATTERN RECORD                               
         DC    A(LOADPPH)          PATTERN HEADER                               
         DC    A(LOADPCM)          PATTERN COMMERCIAL                           
         DC    A(LOADPMK)          PATTERN MARKET                               
         DC    A(LOADPST)          PATTERN STATION                              
         DC    A(LOADPMG)          PATTERN MARKET GROUPS                        
         DC    A(LOADPMA)          PATTERN AFFILIATES                           
         DC    A(LOADPST)          PATTERN STATION TYPES                        
         DC    A(LOADPCN)          PATTERN COMMENTS                             
         DC    A(LOADSTD)          STEXT PAGE DEFINITION                        
         DC    A(LOADSTL)          STEXT LINE                                   
*                                                                               
ADDRLQ4  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTAGY)          MEDIA/AGENCY                                 
         DC    A(UPDTCNT)          CLIENT                                       
         DC    A(UPDTPRD)          PRODUCT                                      
         DC    A(UPDTPDL)          PRODUCT LIST                                 
         DC    A(UPDTEST)          ESTIMATE                                     
         DC    A(UPDTMKT)          MARKET                                       
         DC    A(UPDTSTA)          STATION                                      
         DC    A(UPDTEDL)          ESTIMATE DEMO LIST                           
         DC    A(UPDTSAD)          STATION ADDRESS                              
         DC    A(UPDTEQV)          EQUIVALENCE FACTORS                          
         DC    A(UPDTSOW)          STATION OWNERSHIP INFO                       
         DC    A(UPDTFOR)          STATION FORMAT                               
         DC    A(UPDTOIN)          OWNER INFO                                   
         DC    A(UPDTMGD)          MARKET GROUP DEF                             
         DC    A(UPDTMGR)          MARKET GROUP                                 
         DC    A(UPDTMGA)          MARKET GROUP ASSIGNMENT                      
         DC    A(UPDTCGD)          CLIENT GROUP DEF                             
         DC    A(UPDTCGP)          CLIENT GROUP                                 
         DC    A(UPDTCGC)          CLIENT GROUP CLIENTS                         
         DC    A(UPDTSGD)          STATION GROUP DEF                            
         DC    A(UPDTSGP)          STATION GROUP                                
         DC    A(UPDTSGS)          STATION GROUP STATIONS                       
         DC    A(UPDTPGF)          PRODUCT GROUP DEF                            
         DC    A(UPDTPGR)          PRODUCT GROUPS                               
         DC    A(UPDTPGP)          PRODUCT GROUP PRODUCTS                       
         DC    A(UPDTPAR)          PARENT INFO                                  
         DC    A(UPDTDEA)          DEAL RECORDS                                 
         DC    A(UPDTDST)          DEAL STATIONS                                
         DC    A(UPDTREP)          REP RECORDS                                  
         DC    A(UPDTSNL)          SYSCODE/NETWORK LIST                         
         DC    A(UPDTUCM)          SPOT UCOM RECORD                             
         DC    A(UPDTSLK)          STATION LOCKIN RECORD                        
         DC    A(UPDTDPM)          DAYPART MENU RECORD                          
         DC    A(UPDTDEM)          DEMO CODES AND NAMES                         
         DC    A(UPDTDEMT)         TRADITIONAL DEMO CODES AND NAMES             
         DC    A(UPDTDEMC)         NON-TRADITIONAL DEMO CODES AND NAMES         
         DC    A(UPDTADB)          ADBUYER                                      
         DC    A(UPDTEAD)          ESTIMATE AUTHORIZED DOLLARS                  
         DC    A(UPDTDAO)          DARE ORDER (BLOCKCHAIN)                      
         DC    A(UPDTDMN)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)            
         DC    A(UPDTDMX)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)XSP         
         DC    A(UPDTBIL)          BILLING INFO                                 
         DC    A(UPDTFLT)          FLIGHT RECORDS                               
         DC    A(UPDTFLF)          FLIGHT RECORD FLIGHTS                        
         DC    A(UPDTMKC)          COMSCORE MARKET                              
         DC    A(UPDTSPD)          SPILLDEF                                     
         DC    A(UPDTSQD)          SQAD DAYPART MENU                            
         DC    A(UPDTDMU)          DEMO MENU                                    
         DC    A(UPDTBFM)          BILL FORMULA                                 
         DC    A(UPDTPAT)          PATTERN RECORD                               
         DC    A(UPDTPPH)          PATTERN HEADER                               
         DC    A(UPDTPCM)          PATTERN COMMERCIAL                           
         DC    A(UPDTPMK)          PATTERN MARKET                               
         DC    A(UPDTPST)          PATTERN STATION                              
         DC    A(UPDTPMG)          PATTERN MARKET GROUPS                        
         DC    A(UPDTPMA)          PATTERN AFFILIATES                           
         DC    A(UPDTPST)          PATTERN STATION TYPES                        
         DC    A(UPDTPCN)          PATTERN COMMENTS                             
         DC    A(UPDTSTD)          STEXT PAGE DEFINITION                        
         DC    A(UPDTSTL)          STEXT LINE                                   
*                                                                               
ADDRLQ5  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL8'FILTERS'                                                     
         DC    A(FILTAGY)          MEDIA/AGENCY                                 
         DC    A(FILTCNT)          CLIENT                                       
         DC    A(FILTPRD)          PRODUCT                                      
         DC    A(FILTPDL)          PRODUCT LIST                                 
         DC    A(FILTEST)          ESTIMATE                                     
         DC    A(FILTMKT)          MARKET                                       
         DC    A(FILTSTA)          STATION                                      
         DC    A(FILTEDL)          ESTIMATE DEMO LIST                           
         DC    A(FILTSAD)          STATION ADDRESS                              
         DC    A(FILTEQV)          EQUIVALENCE FACTORS                          
         DC    A(FILTCML)          COMMERCIAL RECORDS                           
         DC    A(FILTSOW)          STATION OWNERSHIP INFO                       
         DC    A(FILTFOR)          STATION FORMAT                               
         DC    A(FILTOIN)          OWNER INFO                                   
         DC    A(FILTMGD)          MARKET GROUP DEF                             
         DC    A(FILTMGR)          MARKET GROUP                                 
         DC    A(FILTMGA)          MARKET GROUP ASSIGNMENT                      
         DC    A(FILTCGD)          CLIENT GROUP DEF                             
         DC    A(FILTCGP)          CLIENT GROUP                                 
         DC    A(FILTCGC)          CLIENT GROUP CLIENTS                         
         DC    A(FILTSGD)          STATION GROUP DEF                            
         DC    A(FILTSGP)          STATION GROUP                                
         DC    A(FILTSGS)          STATION GROUP STATIONS                       
         DC    A(FILTPGF)          PRODUCT GROUP DEF                            
         DC    A(FILTPGR)          PRODUCT GROUPS                               
         DC    A(FILTPGP)          PRODUCT GROUP PRODUCTS                       
         DC    A(FILTPAR)          PARENT INFO                                  
         DC    A(FILTDEA)          DEAL RECORDS                                 
         DC    A(FILTDST)          DEAL STATIONS                                
         DC    A(FILTREP)          REP RECORDS                                  
         DC    A(FILTSNL)          SYSCODE/NETWORK LIST                         
         DC    A(FILTUCM)          SPOT UCOM RECORD                             
         DC    A(FILTSLK)          STATION LOCKIN RECORD                        
         DC    A(FILTDPM)          DAYPART MENU RECORD                          
         DC    A(FILTDEMT)         TRADITIONAL DEMO CODES AND NAMES             
         DC    A(FILTDEMC)         NON-TRADITIONAL DEMO CODES AND NAMES         
         DC    A(FILTADB)          ADBUYER                                      
         DC    A(FILTEAD)          ESTIMATE AUTHORIZED DOLLARS                  
         DC    A(FILTDAO)          DARE ORDER (BLOCKCHAIN)                      
         DC    A(FILTDMN)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)            
         DC    A(FILTDMX)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)XSP         
         DC    A(FILTBIL)          BILLING INFO                                 
         DC    A(FILTFLT)          FLIGHT RECORDS                               
         DC    A(FILTFLF)          FLIGHT RECORD FLIGHTS                        
         DC    A(FILTMKT)          COMSCORE MARKET                              
         DC    A(FILTSPD)          SPILLDEF                                     
         DC    A(FILTSQD)          SQAD DAYPART MENU                            
         DC    A(FILTDMU)          DEMO MENU                                    
         DC    A(FILTBFM)          BILL FORMULA                                 
*                                                                               
* NB: FOLLOWING RECORDS ARE ALL EXTRACTED FROM                                  
*     SPOT TRAFFIC PATTERN RECORD.  THEREFORE, FILTER ROUTINE                   
*     IS THE SAME FOR ALL RECORD TYPES.  THE A-TYPES                            
*     ARE NAMED AFTER THE RECORDS (E.G. AFILTPMK, AFILTPST, ETC)                
         DC    A(FILTPPH)          PATTERN HEADER                               
         DC    A(FILTPPH)          PATTERN COMMERCIAL                           
         DC    A(FILTPPH)          PATTERN MARKET                               
         DC    A(FILTPPH)          PATTERN STATION                              
         DC    A(FILTPPH)          PATTERN MARKET GROUPS                        
         DC    A(FILTPPH)          PATTERN AFFILIATES                           
         DC    A(FILTPPH)          PATTERN STATION TYPES                        
         DC    A(FILTPPH)          PATTERN COMMENTS                             
* END OF PATTERN-BASED RECORDS                                                  
*                                                                               
         DC    A(FILTSTD)          STEXT PAGE DEFINITION                        
         DC    A(FILTSTL)          STEXT LINE                                   
*                                                                               
ADDRLQ6  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(INITAGY)          MEDIA/AGENCY                                 
         DC    A(INITCNT)          CLIENT                                       
         DC    A(INITPRD)          PRODUCT                                      
         DC    A(INITPDL)          PRODUCT LIST                                 
         DC    A(INITEST)          ESTIMATE                                     
         DC    A(INITMKT)          MARKET                                       
         DC    A(INITSTA)          STATION                                      
         DC    A(INITEDL)          ESTIMATE DEMO LIST                           
         DC    A(INITSAD)          STATION ADDRESS                              
         DC    A(INITEQV)          EQUIVALENCE FACTORS                          
         DC    A(INITCML)          COMMERCIAL RECORDS                           
         DC    A(INITSOW)          STATION OWNERSHIP INFO                       
         DC    A(INITFOR)          STATION FORMAT                               
         DC    A(INITOIN)          OWNER INFO                                   
         DC    A(INITMGD)          MARKET GROUP DEF                             
         DC    A(INITMGR)          MARKET GROUP                                 
         DC    A(INITMGA)          MARKET GROUP ASSIGNMENT                      
         DC    A(INITCGD)          CLIENT GROUP DEF                             
         DC    A(INITCGP)          CLIENT GROUP                                 
         DC    A(INITCGC)          CLIENT GROUP CLIENTS                         
         DC    A(INITSGD)          STATION GROUP DEF                            
         DC    A(INITSGP)          STATION GROUP                                
         DC    A(INITSGS)          STATION GROUP STATIONS                       
         DC    A(INITPGF)          PRODUCT GROUP DEF                            
         DC    A(INITPGR)          PRODUCT GROUPS                               
         DC    A(INITPGP)          PRODUCT GROUP PRODUCTS                       
         DC    A(INITPAR)          PARENT INFO                                  
         DC    A(INITDEA)          DEAL RECORDS                                 
         DC    A(INITDST)          DEAL STATIONS                                
         DC    A(INITREP)          REP RECORDS                                  
         DC    A(INITSNL)          SYSCODE/NETWORK LIST                         
         DC    A(INITUCM)          SPOT UCOM RECORD                             
         DC    A(INITSLK)          STATION LOCKIN RECORD                        
         DC    A(INITDPM)          DAYPART MENU RECORD                          
         DC    A(INITDEM)          DEMO CODES AND NAMES                         
         DC    A(INITDEMT)         TRADITIONAL DEMO CODES AND NAMES             
         DC    A(INITDEMC)         NON-TRADITIONAL DEMO CODES AND NAMES         
         DC    A(INITADB)          ADBUYER                                      
         DC    A(INITEAD)          ESTIMATE AUTHORIZED DOLLARS                  
         DC    A(INITDAO)          DARE ORDER (BLOCKCHAIN)                      
         DC    A(INITDMN)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)            
         DC    A(INITDMX)          DARE MAKEGOOD NOTICE (BLOCKCHAIN)XSP         
         DC    A(INITBIL)          BILLING INFO                                 
         DC    A(INITFLT)          FLIGHT RECORDS                               
         DC    A(INITFLF)          FLIGHT RECORD FLIGHTS                        
         DC    A(INITMKT)          COMSCORE MARKET                              
         DC    A(INITSPD)          SPILLDEF                                     
         DC    A(INITSQD)          SQAD DAYPART MENU                            
         DC    A(INITDMU)          DEMO MENU                                    
         DC    A(INITBFM)          BILL FORMULA                                 
         DC    A(INITPPH)          PATTERN HEADER                               
         DC    A(INITPCM)          PATTERN COMMERCIAL                           
         DC    A(INITPMK)          PATTERN MARKET                               
         DC    A(INITPST)          PATTERN STATION                              
         DC    A(INITPMG)          PATTERN MARKET GROUPS                        
         DC    A(INITPMA)          PATTERN AFFILIATES                           
         DC    A(INITPST)          PATTERN STATION TYPES                        
         DC    A(INITPCN)          PATTERN COMMENTS                             
         DC    A(INITSTD)          STEXT PAGE DEFINITION                        
         DC    A(INITSTL)          STEXT LINE                                   
                                                                                
ADDRLQ7  EQU   *-ADDRESS                                                        
*                                                                               
         DC    CL7'OPEN'                                                        
         DC    CL7'DMREAD'                                                      
         DC    CL7'DMRSEQ'                                                      
         DC    CL7'DMRDHI'                                                      
         DC    CL7'DMCLSE'                                                      
         DC    CL7'DMFAST'                                                      
         DC    CL7'GETREC'                                                      
         DC    CL7'RECOVER'                                                     
         DC    CL7'CONTROL'                                                     
         DC    CL7'CTFILE'                                                      
         DC    CL7'PRTDIR'                                                      
         DC    CL7'PRTFILE'                                                     
         DC    CL7'SPTDIR'                                                      
         DC    CL7'SPTFIL'                                                      
         DC    CL7'STATION'                                                     
         DC    CL7'TRFDIR'                                                      
         DC    CL7'TRFFILE'                                                     
         DC    CL7'XSPDIR'                                                      
         DC    CL7'XSPFIL'                                                      
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    F'0'                                                             
         DC    CL1'Y'                                                           
         DC    80C' '                                                           
*                                                                               
ADDRLQT  EQU   *-ADDRESS                                                        
*                                                                               
*                                                                               
         LTORG                                                                  
SPTDIRQ  EQU   X'23'                                                            
SPTFILQ  EQU   X'21'                                                            
STAFILQ  EQU   X'22'                                                            
XSPFILQ  EQU   X'37'                                                            
STRFILQ  EQU   X'32'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPARATOR CHR                          
FF       EQU   X'FF'                                                            
*                                                                               
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
*                                                                               
*                                                                               
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
*                                                                               
         DC    CL3'OPT',AL1(00,00,00,00,00),AL4(LOADOPT,UPDTOPT)                
*                                                                               
         DC    CL3'MDM',AL1(00,00,00,00,00),AL4(LOADMDM,UPDTMDM)                
*                                                                               
         DC    CL3'DAR',AL1(00,00,00,00,00),AL4(LOADDAR,UPDTDAR)                
*                                                                               
         DC    CL3'P36',AL1(00,00,00,00,00),AL4(LOADP36,UPDTP36)                
*                                                                               
         DC    CL3'PAT',AL1(00,00,00,00,00),AL4(LOADPAT,UPDTPAT)                
*                                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'CNT',AL1(00,00,00,00,00),AL4(LOADCNT,UPDTCNT)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'PDL',AL1(00,00,00,00,00),AL4(LOADPDL,UPDTPDL)                
         DC    CL3'EST',AL1(00,00,00,00,00),AL4(LOADEST,UPDTEST)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'EDL',AL1(00,00,00,00,00),AL4(LOADEDL,UPDTEDL)                
         DC    CL3'SAD',AL1(00,00,00,00,00),AL4(LOADSAD,UPDTSAD)                
         DC    CL3'EQF',AL1(00,00,00,00,00),AL4(LOADEQV,UPDTEQV)                
         DC    CL3'CML',AL1(00,00,00,00,00),AL4(LOADCML,UPDTCML)                
*                                                                               
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'SOW',AL1(00,00,00,00,00),AL4(LOADSOW,UPDTSOW)                
         DC    CL3'OIN',AL1(00,00,00,00,00),AL4(LOADOIN,UPDTOIN)                
         DC    CL3'FOR',AL1(00,00,00,00,00),AL4(LOADFOR,UPDTFOR)                
*                                                                               
         DC    CL3'MKG',AL1(00,00,00,00,00),AL4(LOADMKG,UPDTMKG)                
         DC    CL3'MGD',AL1(00,00,00,00,00),AL4(LOADMGD,UPDTMGD)                
         DC    CL3'MGR',AL1(00,00,00,00,00),AL4(LOADMGR,UPDTMGR)                
         DC    CL3'MGA',AL1(00,00,00,00,00),AL4(LOADMGA,UPDTMGA)                
*                                                                               
         DC    CL3'CLG',AL1(00,00,00,00,00),AL4(LOADCLG,UPDTCLG)                
         DC    CL3'CGD',AL1(00,00,00,00,00),AL4(LOADCGD,UPDTCGD)                
         DC    CL3'CGP',AL1(00,00,00,00,00),AL4(LOADCGP,UPDTCGP)                
         DC    CL3'CGC',AL1(00,00,00,00,00),AL4(LOADCGC,UPDTCGC)                
*                                                                               
         DC    CL3'STG',AL1(00,00,00,00,00),AL4(LOADSTG,UPDTSTG)                
         DC    CL3'SGD',AL1(00,00,00,00,00),AL4(LOADSGD,UPDTSGD)                
         DC    CL3'SGP',AL1(00,00,00,00,00),AL4(LOADSGP,UPDTSGP)                
         DC    CL3'SGS',AL1(00,00,00,00,00),AL4(LOADSGS,UPDTSGS)                
*                                                                               
         DC    CL3'PRG',AL1(00,00,00,00,00),AL4(LOADPRG,UPDTPRG)                
         DC    CL3'PGF',AL1(00,00,00,00,00),AL4(LOADPGF,UPDTPGF)                
         DC    CL3'PGR',AL1(00,00,00,00,00),AL4(LOADPGR,UPDTPGR)                
         DC    CL3'PGP',AL1(00,00,00,00,00),AL4(LOADPGP,UPDTPGP)                
*                                                                               
         DC    CL3'PAR',AL1(00,00,00,00,00),AL4(LOADPAR,UPDTPAR)                
         DC    CL3'DEA',AL1(00,00,00,00,00),AL4(LOADDEA,UPDTDEA)                
         DC    CL3'DST',AL1(00,00,00,00,00),AL4(LOADDST,UPDTDST)                
*                                                                               
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'SNL',AL1(00,00,00,00,00),AL4(LOADSNL,UPDTSNL)                
         DC    CL3'UCM',AL1(00,00,00,00,00),AL4(LOADUCM,UPDTUCM)                
         DC    CL3'SLK',AL1(00,00,00,00,00),AL4(LOADSLK,UPDTSLK)                
*                                                                               
         DC    CL3'DPM',AL1(00,00,00,00,00),AL4(LOADDPM,UPDTDPM)                
         DC    CL3'DEM',AL1(00,00,00,00,00),AL4(LOADDEM,UPDTDEM)                
         DC    CL3'DMT',AL1(00,00,00,00,00),AL4(LOADDEMT,UPDTDEM)               
         DC    CL3'DMC',AL1(00,00,00,00,00),AL4(LOADDEMC,UPDTDEM)               
         DC    CL3'ADB',AL1(00,00,00,00,00),AL4(LOADADB,UPDTADB)                
*                                                                               
         DC    CL3'BIL',AL1(00,00,00,00,00),AL4(LOADBIL,UPDTBIL)                
*                                                                               
         DC    CL3'EAD',AL1(00,00,00,00,00),AL4(LOADEAD,UPDTEAD)                
*                                                                               
* DAO, DMN - BLOCKCHAIN EXTRACT                                                 
         DC    CL3'DAO',AL1(00,00,00,00,00),AL4(LOADDAO,UPDTDAO)                
         DC    CL3'DMN',AL1(00,00,00,00,00),AL4(LOADDMN,UPDTDMN)                
         DC    CL3'DMX',AL1(00,00,00,00,00),AL4(LOADDMX,UPDTDMX)                
*                                                                               
* RECORDS FOR PM360 PROJECT                                                     
* DARE ORDER CODE IS REUSED, EXCEPT FOR SOME STATUS FILTERING                   
         DC    CL3'ORD',AL1(00,00,00,00,00),AL4(LOADDAO,UPDTDAO)                
         DC    CL3'FLT',AL1(00,00,00,00,00),AL4(LOADFLT,UPDTFLT)                
         DC    CL3'FLF',AL1(00,00,00,00,00),AL4(LOADFLF,UPDTFLF)                
*                                                                               
         DC    CL3'MKC',AL1(00,00,00,00,00),AL4(LOADMKC,UPDTMKC)                
*                                                                               
         DC    CL3'SPD',AL1(00,00,00,00,00),AL4(LOADSPD,UPDTSPD)                
         DC    CL3'SQD',AL1(00,00,00,00,00),AL4(LOADSQD,UPDTSQD)                
         DC    CL3'DMU',AL1(00,00,00,00,00),AL4(LOADDMU,UPDTDMU)                
         DC    CL3'BFM',AL1(00,00,00,00,00),AL4(LOADBFM,UPDTBFM)                
*                                                                               
         DC    CL3'PPH',AL1(00,00,00,00,00),AL4(LOADPPH,UPDTPPH)                
         DC    CL3'PCM',AL1(00,00,00,00,00),AL4(LOADPCM,UPDTPCM)                
         DC    CL3'PMK',AL1(00,00,00,00,00),AL4(LOADPMK,UPDTPMK)                
         DC    CL3'PST',AL1(00,00,00,00,00),AL4(LOADPST,UPDTPST)                
         DC    CL3'PMG',AL1(00,00,00,00,00),AL4(LOADPMG,UPDTPMG)                
         DC    CL3'PMA',AL1(00,00,00,00,00),AL4(LOADPMA,UPDTPMA)                
         DC    CL3'PTP',AL1(00,00,00,00,00),AL4(LOADPTP,UPDTPTP)                
         DC    CL3'PCN',AL1(00,00,00,00,00),AL4(LOADPCN,UPDTPCN)                
*                                                                               
         DC    CL3'STD',AL1(00,00,00,00,00),AL4(LOADSTD,UPDTSTD)                
         DC    CL3'STL',AL1(00,00,00,00,00),AL4(LOADSTL,UPDTSTL)                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
         DROP  RB                                                               
*                                                                               
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         BNE   *+6                                                              
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
*                                                                               
         CLC   TYPENAME,=C'DMX'                                                 
         BE    GTYP05                                                           
         CLC   TYPENAME,=C'SLK'                                                 
         BNE   GTYP06                                                           
*                                                                               
GTYP05   DS    0H                                                               
         OI    FLAGS2,XSPFILEQ                                                  
         J     YES                                                              
*                                                                               
GTYP06   DS    0H                                                               
         CLC   TYPENAME,=C'CML'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PPH'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PCM'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PMK'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PST'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PMG'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PMA'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PTP'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'PCN'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'STD'                                                 
         BE    GTYP07                                                           
         CLC   TYPENAME,=C'STL'                                                 
         BE    GTYP07                                                           
         B     GTYP08                                                           
*                                                                               
GTYP07   DS    0H                                                               
         OI    FLAGS,TRFFILEQ                                                   
         J     YES                                                              
*                                                                               
GTYP08   DS    0H                                                               
         CLC   TYPENAME,=C'OWN'                                                 
         BE    GTYP10                                                           
         CLC   TYPENAME,=C'SOW'                                                 
         BE    GTYP10                                                           
         CLC   TYPENAME,=C'FOR'                                                 
         BE    GTYP10                                                           
         CLC   TYPENAME,=C'OIN'                                                 
         BE    GTYP10                                                           
         CLC   TYPENAME,=C'PAR'                                                 
         BNE   GTYP12                                                           
*                                                                               
GTYP10   DS    0H                                                               
         OI    FLAGS2,CTFILEQ                                                   
         J     YES                                                              
*                                                                               
GTYP12   DS    0H                                                               
         CLC   TYPENAME,=C'REP'                                                 
         BE    GTYP20                                                           
         CLC   TYPENAME,=C'MKT'                                                 
         BE    GTYP20                                                           
         CLC   TYPENAME,=C'MKC'                                                 
         BE    GTYP20                                                           
         CLC   TYPENAME,=C'STA'                                                 
         BE    GTYP20                                                           
         CLC   TYPENAME,=C'SNL'                                                 
         BE    GTYP20                                                           
         CLC   TYPENAME,=C'SAD'                                                 
         BNE   GTYP30                                                           
*                                                                               
GTYP20   DS    0H                                                               
         OI    FLAGS,STAFILEQ                                                   
         J     YES                                                              
*                                                                               
GTYP30   DS    0H                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
*                                                                               
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
*                                                                               
*                                                                               
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       PRIADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
GETIT    NTR1  BASE=*,LABEL=*                                                   
         MVC   VARFIL,SPTFIL                                                    
         TM    FLAGS,TRFFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARFIL,TRFFIL                                                    
         TM    FLAGS2,XSPFILEQ                                                  
         BNO   *+10                                                             
         MVC   VARFIL,XSPFIL                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),VARFIL,PRIADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,PRIADDR,GETDA,L'PRIADDR,0                        
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HIGH                                                             
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
*                                                                               
         MVC   VARDIR,SPTDIR                                                    
*                                                                               
         TM    FLAGS,STAFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,STAFIL                                                    
*                                                                               
         TM    FLAGS,TRFFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,TRFDIR                                                    
*                                                                               
         TM    FLAGS2,CTFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,CTFILE                                                    
*                                                                               
         TM    FLAGS2,XSPFILEQ                                                  
         BNO   *+10                                                             
         MVC   VARDIR,XSPDIR                                                    
*                                                                               
         MVC   SVDIR,VARDIR                                                     
         GOTO1 VDATAMGR,DMCB,DMRDHI,VARDIR,IOKEY,(R2),DMWORK                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV,0                     
*                                                                               
         XR    R0,R0                                                            
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
         LTORG                                                                  
*                                                                               
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
*                                                                               
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
*                                                                               
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES - NO COPY, NOTHING TO COMPARE            
*                                                                               
         L     R2,DXARECB          GET CHANGE RECORD ADDRESS                    
         LA    R2,L'RECVHDR+4(R2)                                               
         L     R6,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R6,L'RECVHDR+4(R6)                                               
*                                                                               
* 02/26/09 RIGHT NOW THE UPDATES WILL ONLY WORK FOR:                            
*          1. SPOT FILE                                                         
*          2. XSPOT FILE                                                        
*          3. SPOT TRAFFIC FILE                                                 
*          4. SYSCODE NETWORK LIST(SNL) RECORD                                  
*          5. REP RECORD                                                        
*                                                                               
         CLI   RFILTY,XSPFILQ                                                   
         BE    RECCMP00                                                         
         CLI   RFILTY,SPTFILQ                                                   
         BE    RECCMP00                                                         
         CLI   RFILTY,STRFILQ                                                   
         BE    RECCMP00                                                         
         CLI   RFILTY,STAFILQ                                                   
         BE    RECCMP00                                                         
         CLI   RFILTY,SPTDIRQ                                                   
         JE    YES                                                              
*                                                                               
         J     NO                                                               
*                                                                               
         CLI   0(R2),C'S'          STATION RECORD?                              
         BE    RECCMP00                                                         
         CLI   0(R2),C'M'          MARKET RECORD?                               
         BE    RECCMP00                                                         
         CLI   0(R2),C'R'          REP RECORD?                                  
         BE    RECCMP00                                                         
         CLI   0(R2),C'A'          STATION ADDRESS RECORD?                      
         JNE   NO                                                               
*                                                                               
RECCMP00 DS    0H                                                               
*                                                                               
* GET CHANGE RECORD'S LENGTH INTO R3, COPY - INTO R7                            
*                                                                               
         SR    R3,R3                                                            
         SR    R7,R7                                                            
*                                                                               
         CLI   RFILTY,XSPFILQ                                                   
         BNE   *+16                                                             
         ICM   R3,3,32(R2)                                                      
         ICM   R7,3,32(R6)                                                      
         B     RECCMP50                                                         
*                                                                               
         CLI   RFILTY,SPTFILQ                                                   
         BNE   *+16                                                             
         ICM   R3,3,13(R2)                                                      
         ICM   R7,3,13(R6)                                                      
         B     RECCMP50                                                         
*                                                                               
         CLI   RFILTY,STAFILQ                                                   
         BNE   *+16                                                             
         ICM   R3,3,15(R2)                                                      
         ICM   R7,3,15(R6)                                                      
         B     RECCMP50                                                         
*                                                                               
         CLI   RFILTY,STRFILQ                                                   
         JNE   NO                                                               
         ICM   R3,3,13(R2)                                                      
         ICM   R7,3,13(R6)                                                      
*                                                                               
RECCMP50 DS    0H                                                               
         CR    R3,R7               RECORD LENGTH CHANGED?                       
         JNE   YES                 YES - RECORD MUST HAVE CHANGED TOO           
*                                                                               
         CLCL  R2,R6               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
PROCCLOX NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'DAR',SXDTTYP                                                  
         JNE   YES                                                              
         MVC   MSGDSP,DXDSPAC                                                   
         MVC   MSGAGY(L'SXDTAGY),SXDTAGY                                        
         MVC   MSGDSN(L'SXDTXDSN),SXDTXDSN                                      
         LA    R2,MSGFLEN                                                       
         WTO   TEXT=(R2),MCSFLAG=HRDCPY                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
MSGFLEN  DC    AL2(MSGFLNQ)                                                     
MSGNUM   DC    C'<0336>'                                                        
         DC    C' DSPACE='                                                      
MSGDSP   DS    C                                                                
         DC    C' AGENCY='                                                      
MSGAGY   DS    CL(L'SXDTAGY)                                                    
         DC    C' DSN='                                                         
MSGDSN   DS    CL(L'SXDTXDSN)                                                   
MSGFLNQ  EQU   *-MSGNUM                                                         
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
*                                                                               
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
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
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'PDL',AL1(0),AL4(LOADPDL) PRODUCT LIST                        
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'MKT',AL1(0),AL4(LOADMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'EDL',AL1(0),AL4(LOADEDL) ESTIMATE DEMO LIST                  
         DC    CL3'SAD',AL1(0),AL4(LOADSAD) STATION ADDRESS                     
         DC    CL3'EQF',AL1(0),AL4(LOADEQV) EQUIVALENCE FACTORS                 
         DC    CL3'CML',AL1(2),AL4(LOADCML) COMMERCIAL RECORDS                  
         DC    CL3'OWN',AL1(2),AL4(LOADOWN) OWNER INFORMATION                   
         DC    CL3'MKG',AL1(2),AL4(LOADMKG) MARKET GROUPS                       
         DC    CL3'CLG',AL1(2),AL4(LOADCLG) CLIENT GROUPS                       
         DC    CL3'PRG',AL1(2),AL4(LOADPRG) PRODUCT GROUPS                      
         DC    CL3'STG',AL1(2),AL4(LOADSTG) STATION GROUPS                      
         DC    CL3'PAR',AL1(2),AL4(LOADPAR) PARENT INFORMATION                  
         DC    CL3'REP',AL1(3),AL4(LOADREP) REP RECORDS                         
         DC    CL3'DEA',AL1(13),AL4(LOADDEA) DEAL                               
         DC    CL3'DST',AL1(13),AL4(LOADDST) DEAL STATIONS                      
         DC    CL3'UCM',AL1(4),AL4(LOADUCM) SPOT UCOM RECORD                    
         DC    CL3'SLK',AL1(4),AL4(LOADSLK) STATION LOCKIN RECORD               
         DC    CL3'SNL',AL1(4),AL4(LOADSNL) SYSCODE/NETWORK LIST                
         DC    CL3'DPM',AL1(6),AL4(LOADDPM) DAYPART MENU RECORD                 
         DC    CL3'DEM',AL1(6),AL4(LOADDEM) DEMO CODES/NAMES                    
         DC    CL3'EAD',AL1(6),AL4(LOADEAD) ESTIMATE AUTHORIZED DOLLARS         
*                                                                               
* RECORDS FOR PM360 PROJECT                                                     
* DARE ORDER CODE IS REUSED, EXCEPT FOR SOME STATUS FILTERING                   
         DC    CL3'ORD',AL1(0),AL4(LOADDAO)                                     
         DC    CL3'FLT',AL1(0),AL4(LOADFLT)                                     
         DC    CL3'FLF',AL1(0),AL4(LOADFLF)                                     
*                                                                               
         DC    CL3'SPD',AL1(0),AL4(LOADSPD) SPILLDEF                            
         DC    CL3'SQD',AL1(0),AL4(LOADSQD) SQAD DAYPART MENUQ                  
         DC    CL3'DMU',AL1(0),AL4(LOADDMU) DEMO MENUS                          
         DC    CL3'BFM',AL1(0),AL4(LOADBFM) BILL FORMULA                        
*                                                                               
         DC    CL3'PPH',AL1(0),AL4(LOADPPH)                                     
         DC    CL3'PCM',AL1(0),AL4(LOADPCM)                                     
         DC    CL3'PMK',AL1(0),AL4(LOADPMK)                                     
         DC    CL3'PST',AL1(0),AL4(LOADPST)                                     
         DC    CL3'PMG',AL1(0),AL4(LOADPMG)                                     
         DC    CL3'PMA',AL1(0),AL4(LOADPMA)                                     
         DC    CL3'PTP',AL1(0),AL4(LOADPTP)                                     
         DC    CL3'PCN',AL1(0),AL4(LOADPCN)                                     
         DC    CL3'STD',AL1(0),AL4(LOADSTD)                                     
         DC    CL3'STL',AL1(0),AL4(LOADSTL)                                     
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* LOAD OPTICA DATA                                                    *         
***********************************************************************         
*                                                                               
LOADOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOODTAB                                                       
*                                                                               
LOOD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOOD04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         OI    FLAGS2,OPTICAQ                                                   
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOOD04   LA    R3,L'LOODTAB(R3)                                                 
         J     LOOD02                                                           
*                                                                               
LOODTAB  DS    0XL8                                                             
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'MKT',AL1(0),AL4(LOADMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'DPM',AL1(0),AL4(LOADDPM) DAYPART MENU RECORD                 
*        DC    CL3'DEM',AL1(0),AL4(LOADDEM) DEMO CODES AND NAMES                
         DC    CL3'ADB',AL1(0),AL4(LOADADB) ADBUYER                             
         DC    CL3'MKG',AL1(0),AL4(LOADMKG) MARKET GROUPS                       
         DC    CL3'CLG',AL1(0),AL4(LOADCLG) CLIENT GROUPS                       
         DC    CL3'SNL',AL1(0),AL4(LOADSNL) CABLE NETWORK LISTS                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* LOAD MDM DATA                                                       *         
***********************************************************************         
*                                                                               
LOADMDM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LMDMTAB                                                       
*                                                                               
LMDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LMDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         OI    FLAGS2,OPTICAQ                                                   
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LMDM04   LA    R3,L'LMDMTAB(R3)                                                 
         J     LMDM02                                                           
*                                                                               
LMDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'MKT',AL1(0),AL4(LOADMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'EDL',AL1(0),AL4(LOADEDL) ESTIMATE DEMO LISTS                 
         DC    CL3'DPM',AL1(0),AL4(LOADDPM) DAYPART MENU RECORD                 
         DC    CL3'DEM',AL1(0),AL4(LOADDEM) DEMOS                               
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD PM360 DATA - ORDERS, FLIGHTS                                   *         
***********************************************************************         
*                                                                               
LOADP36  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LP36TAB                                                       
*                                                                               
*                                                                               
LP3602   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LP3604                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         OI    FLAGS2,OPTICAQ                                                   
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LP3604   LA    R3,L'LP36TAB(R3)                                                 
         J     LP3602                                                           
*                                                                               
LP36TAB  DS    0XL8                                                             
         DC    CL3'ORD',AL1(0),AL4(LOADDAO) ORDER                               
         DC    CL3'FLT',AL1(0),AL4(LOADFLT) FLIGHT                              
         DC    CL3'FLF',AL1(0),AL4(LOADFLF) FLIGHT RECORD FLIGHT                
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE PM360 DATA                                                             
***********************************************************************         
*                                                                               
UPDTP36  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPD3TAB                                                       
UPD302   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPD304                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPD304   LA    R3,L'UPD3TAB(R3)                                                 
         J     UPD302                                                           
                                                                                
UPD3TAB  DS    0XL8                                                             
         DC    CL3'ORD',AL1(0),AL4(UPDTDAO) ORDER                               
         DC    CL3'FLT',AL1(0),AL4(UPDTFLT) FLIGHT                              
         DC    CL3'FLF',AL1(0),AL4(UPDTFLF) FLIGHT RECORD FLIGHT                
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD PATTERN RECORD DATA                                            *         
***********************************************************************         
*                                                                               
LOADPAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LPATTAB                                                       
*                                                                               
LPAT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LPAT04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LPAT04   LA    R3,L'LPATTAB(R3)                                                 
         J     LPAT02                                                           
*                                                                               
LPATTAB  DS    0XL8                                                             
         DC    CL3'PPH',AL1(0),AL4(LOADPPH) PATTERN RECORD HEADER               
         DC    CL3'PCM',AL1(0),AL4(LOADPCM) PATTERN COMMERCIAL                  
         DC    CL3'PMK',AL1(0),AL4(LOADPMK) PATTERN MARKET                      
         DC    CL3'PST',AL1(0),AL4(LOADPST) PATTERN STATION                     
         DC    CL3'PMG',AL1(0),AL4(LOADPMG) PATTERN MARKET GROUPS               
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE PATTERN RECORD DATA                                                    
***********************************************************************         
*                                                                               
UPDTPAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDPATTB                                                      
*                                                                               
UPDPAT10 CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDPAT20                                                         
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDPAT20 LA    R3,L'UPDPATTB(R3)                                                
         J     UPDPAT10                                                         
                                                                                
UPDPATTB DS    0XL8                                                             
         DC    CL3'PPH',AL1(0),AL4(UPDTPPH) PATTERN                             
         DC    CL3'PCM',AL1(0),AL4(UPDTPCM) COMMERCIAL                          
         DC    CL3'PMK',AL1(0),AL4(UPDTPMK) PATTERN MARKET                      
         DC    CL3'PST',AL1(0),AL4(UPDTPST) PATTERN STATION                     
         DC    CL3'PMG',AL1(0),AL4(UPDTPMG) PATTERN MARKET GROUPS               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE BLOCKCHAIN DATA                                                     
***********************************************************************         
*                                                                               
LOADDAR  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
*                                                                               
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT02                                                           
                                                                                
UPDTTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'MKT',AL1(0),AL4(UPDTMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
* EDL HAS BEEN COMMENTED OUT FOR A REASON !!!                                   
* WHEN ESTIMATE IS CHANGED, EDL IS EXTRACTED AUTOMATICALLY                      
*        DC    CL3'EDL',AL1(0),AL4(UPDTEDL) ESTIMATE DEMO LIST                  
         DC    CL3'SAD',AL1(0),AL4(UPDTSAD) STATION ADDRESS                     
         DC    CL3'EQF',AL1(0),AL4(UPDTEQV) EQUIVALENCE FACTORS                 
         DC    CL3'CML',AL1(0),AL4(UPDTCML) COMMERCIAL RECORDS                  
         DC    CL3'PAR',AL1(0),AL4(UPDTPAR) PARENT INFORMATION                  
         DC    CL3'DEA',AL1(13),AL4(UPDTDEA) DEAL                               
         DC    CL3'DST',AL1(13),AL4(UPDTDST) DEAL STATIONS                      
         DC    CL3'REP',AL1(3),AL4(UPDTREP) REP RECORDS                         
         DC    CL3'SNL',AL1(4),AL4(UPDTSNL) SYSCODE/NETWORK LIST                
         DC    CL3'UCM',AL1(4),AL4(UPDTUCM) SPOT UCOM RECORD                    
         DC    CL3'SLK',AL1(4),AL4(UPDTSLK) STATION LOCKIN RECORD               
         DC    CL3'DPM',AL1(6),AL4(UPDTDPM) DAYPART MENU RECORD                 
         DC    CL3'DEM',AL1(0),AL4(UPDTDEM) DEMO CODES AND NAMES                
         DC    CL3'MGA',AL1(0),AL4(UPDTMGA) MARKET GROUP ASSIGNMENTS            
*                                                                               
* RECORDS FOR PM360 PROJECT                                                     
* DARE ORDER CODE IS REUSED, EXCEPT FOR SOME STATUS FILTERING                   
         DC    CL3'ORD',AL1(0),AL4(UPDTDAO)                                     
         DC    CL3'FLT',AL1(0),AL4(UPDTFLT)                                     
         DC    CL3'FLF',AL1(0),AL4(UPDTFLF)                                     
*                                                                               
         DC    CL3'SPD',AL1(0),AL4(UPDTSPD) SPILLDEF                            
         DC    CL3'SQD',AL1(0),AL4(UPDTSQD) SQAD DAYPART MENUQ                  
         DC    CL3'DMU',AL1(0),AL4(UPDTDMU) DEMO MENUS                          
         DC    CL3'BFM',AL1(0),AL4(UPDTBFM) BILL FORMULA                        
*                                                                               
         DC    CL3'PPH',AL1(0),AL4(UPDTPPH)                                     
         DC    CL3'PCM',AL1(0),AL4(UPDTPCM)                                     
         DC    CL3'PMK',AL1(0),AL4(UPDTPMK)                                     
         DC    CL3'PST',AL1(0),AL4(UPDTPST)                                     
         DC    CL3'PMG',AL1(0),AL4(UPDTPMG)                                     
         DC    CL3'PMA',AL1(0),AL4(UPDTPMA)                                     
         DC    CL3'PTP',AL1(0),AL4(UPDTPTP)                                     
         DC    CL3'PCN',AL1(0),AL4(UPDTPCN)                                     
         DC    CL3'STD',AL1(0),AL4(UPDTSTD)                                     
         DC    CL3'STL',AL1(0),AL4(UPDTSTL)                                     
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE OPTICA DATA                                                  *         
***********************************************************************         
*                                                                               
UPDTOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDOTAB                                                       
UPDO02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDO04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         OI    FLAGS2,OPTICAQ                                                   
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDO04   LA    R3,L'UPDOTAB(R3)                                                 
         J     UPDO02                                                           
                                                                                
UPDOTAB  DS    0XL8                                                             
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'MKT',AL1(0),AL4(UPDTMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'DPM',AL1(0),AL4(UPDTDPM) DAYPART MENU RECORD                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE MDM DATA                                                     *         
***********************************************************************         
*                                                                               
UPDTMDM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDMTAB                                                       
UPDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDM04   LA    R3,L'UPDMTAB(R3)                                                 
         J     UPDM02                                                           
                                                                                
UPDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'MKT',AL1(0),AL4(UPDTMKT) MARKET                              
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
         DC    CL3'DPM',AL1(0),AL4(UPDTDPM) DAYPART MENU RECORD                 
* EDL HAS BEEN COMMENTED OUT FOR A REASON !!!                                   
* WHEN ESTIMATE IS CHANGED, EDL IS EXTRACTED AUTOMATICALLY                      
*        DC    CL3'EDL',AL1(0),AL4(UPDTEDL) ESTIMATE DEMO LIST                  
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE BLOCKCHAIN DATA                                                   
***********************************************************************         
*                                                                               
UPDTDAR  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDDTAB                                                       
UPDD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDD04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDD04   LA    R3,L'UPDDTAB(R3)                                                 
         J     UPDD02                                                           
                                                                                
UPDDTAB  DS    0XL8                                                             
         DC    CL3'DAO',AL1(0),AL4(UPDTDAO) DARE ORDERS                         
         DC    CL3'DMN',AL1(0),AL4(UPDTDMN) DARE MAKEGOOD NOTICES               
         DC    CL3'DMX',AL1(0),AL4(UPDTDMX) DARE MAKEGOOD NOTICES XSPT          
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
*                                                                               
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
*                                                                               
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
*                                                                               
         TM    MISCFLG,MFLDUPDQ    LOADING IN UPDATE MODE?                      
         BO    IALL02                                                           
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
*                                                                               
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
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* AGENCY RECORDS                                                                
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD AGENCY RECORDS                                                           
*---------------------------------------------------------------------*         
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
LAGY01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING AGYHDR,R2                                                        
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SXDTAGY                                                  
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTAGYC,AINITAGY,AFILTAGY,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE AGENCY RECORD DATA                                                     
*---------------------------------------------------------------------*         
*                                                                               
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING AGYHDR,R2                                                        
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGY                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTAG10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTAGYC,0,0,(R8)                                  
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTAG10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER AGY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
*                                                                               
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING AGYHDR,R2                                                        
         CLI   AGYKTYPE,X'06'                                                   
         JNE   NO                                                               
         CLC   AGYKAGY,SXDTAGY                                                  
         JE    YES                                                              
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE AGENCY RECORD                                                      
*---------------------------------------------------------------------*         
*                                                                               
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMDDL          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CLIENT RECORDS                                                                
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD CLIENT RECORDS                                                           
*---------------------------------------------------------------------*         
*                                                                               
LOADCNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
LCNT01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CLTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   CKEYTYPE,X'00'                                                   
         MVC   CKEYAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCNT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTCNTC,AINITCNT,AFILTCNT,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCNT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE CNT RECORD DATA                                                        
*---------------------------------------------------------------------*         
*                                                                               
UPDTCNT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CLTHDR,R2                                                        
*                                                                               
         GOTO1 AFILTCNT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCNT                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTCNTC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
* PRODUCT LIST NOT NEEDED FOR MDM, OPT                                          
*                                                                               
         CLC   SXDTTYP,=C'MDM'                                                  
         JE    YES                                                              
         CLC   SXDTTYP,=C'OPT'                                                  
         JE    YES                                                              
*                                                                               
* OTHERWISE, RE-LOAD THIS CLIENT'S PRODUCT LIST                                 
*                                                                               
         OI    MISCFLG,MFLDUPDQ+MFDELADQ                                        
         MVC   SVPRAM,CKEYAM                                                    
         MVC   SVPRBCLT,CKEYCLT                                                 
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         L     RF,ALOADPDL                                                      
         BASR  RE,RF                                                            
         NI    MISCFLG,X'FF'-MFLDUPDQ-MFDELADQ                                  
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CNT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTCNT  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLC   4(9,R2),=XL9'0'                                                  
         JNE   NO                                                               
*                                                                               
         TM    FLAGS2,ORGANIZQ                                                  
         JZ    YES                                                              
         CLI   SXDTCTRY,CTRYCAN                                                 
         JNE   YES                                                              
         MVC   BYTE,1(R2)          A/M                                          
         NI    BYTE,X'0F'          ISOLATE MEDIA                                
         CLI   BYTE,X'03'          NET                                          
         JE    NO                                                               
         CLI   BYTE,X'08'          CABLE                                        
         JE    NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE CNT RECORD                                                         
*---------------------------------------------------------------------*         
INITCNT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTCNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PRODUCT RECORDS                                                               
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PRD RECORDS                                                              
*---------------------------------------------------------------------*         
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRDHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PKEYAM,SXDTAGB                                                   
*                                                                               
         TM    MISCFLG,MFLDUPDQ                                                 
         BZ    LPRD01                                                           
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         BZ    *+10                                                             
         MVC   PKEYAM,SVPRAM                                                    
         OC    SVPRBCLT,SVPRBCLT                                                
         BZ    *+10                                                             
         MVC   PKEYCLT,SVPRBCLT                                                 
*                                                                               
LPRD01   DS    0H                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTPRDC,AINITPRD,AFILTPRD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PRD RECORD DATA                                                        
*---------------------------------------------------------------------*         
*                                                                               
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PRDHDR,R2                                                        
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
*                                                                               
         CLI   DXMODE,DXUPDTQ                                                   
         BNE   UPDTPRD5                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PKEY),0(R2)                                                
         MVC   KEY+PKEYPRD-PKEY(L'PKEYPRD),=C'AAA'                              
         GOTO1 =A(GETBFM),DMCB,KEY,SVAAABFM                                     
*                                                                               
UPDTPRD5 DS    0H                                                               
         GOTO1 AINITPRD                                                         
         GOTO1 AACCUPDT,DMCB,VSPTPRDC,TYPECODE,,(R8)                            
         JNE   NO                                                               
*                                                                               
* SEE IF BILL FORMULA HAS CHANGED                                               
*                                                                               
         L     RF,DXACPYB                                                       
         LA    RF,4+L'RECVHDR(RF)                                               
         CLC   PBILLBAS(L'PBILLBAS+L'PBILLCOM),PBILLBAS-PKEY(RF)                
         JE    YES                                                              
*                                                                               
* IF BILL FORMULA CHANGED, WE NEED TO RE-LOAD ALL OF THIS PRODUCT'S             
* ESTIMATES                                                                     
*                                                                               
         OI    MISCFLG,MFLDUPDQ+MFDELADQ                                        
*                                                                               
         MVC   SVPRAM,PKEYAM                                                    
         MVC   SVPRBCLT,PKEYCLT                                                 
         MVC   SVPRCPRD,PKEYPRD                                                 
*                                                                               
         CLC   PKEYPRD,=C'AAA'                                                  
         BNE   UPDTPRD8                                                         
*                                                                               
* FOR PRODUCT AAA RELOAD ALL PRODUCTS AND ESTIMATES FOR THIS CLIENT             
         XC    SVPRCPRD,SVPRCPRD                                                
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         L     RF,ALOADPRD                                                      
         BASR  RE,RF                                                            
*                                                                               
UPDTPRD8 DS    0H                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         L     RF,ALOADEST                                                      
         BASR  RE,RF                                                            
         NI    MISCFLG,X'FF'-MFLDUPDQ-MFDELADQ                                  
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PRD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING PRDHDR,R2                                                        
*                                                                               
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLC   4(3,R2),=XL3'0'                                                  
         JE    NO                                                               
         CLC   7(6,R2),=XL6'0'                                                  
         JNE   NO                                                               
*                                                                               
         TM    MISCFLG,MFLDUPDQ    INDICATE LOADING IN UPDATIVE MODE            
         BZ    FILTPRD5                                                         
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         BZ    FILTPRD5                                                         
         CLC   PKEYAM,SVPRAM                                                    
         JNE   NO                                                               
*                                                                               
         OC    SVPRBCLT,SVPRBCLT                                                
         BZ    FILTPRD5                                                         
         CLC   PKEYCLT,SVPRBCLT                                                 
         JNE   NO                                                               
*                                                                               
FILTPRD5 DS    0H                                                               
         OI    MISCFLG,MFXPROCQ    TURN ON EXTRA PROCESSING FLAG                
*                                                                               
         CLC   SVCLTPRD(4),0(R2)   A/M,CLT CHANGED?                             
         JE    *+16                                                             
         MVC   SVCLTPRD(4),0(R2)   SAVE LATEST A/M,CLT                          
         XC    SVAAABFM,SVAAABFM   CLEAR PRODUCT AAA'S BILL FORMULA             
*                                                                               
         TM    FLAGS2,ORGANIZQ                                                  
         JZ    YES                                                              
         CLI   SXDTCTRY,CTRYCAN                                                 
         JNE   YES                                                              
         MVC   BYTE,1(R2)          A/M                                          
         NI    BYTE,X'0F'          ISOLATE MEDIA                                
         CLI   BYTE,X'03'          NET                                          
         JE    NO                                                               
         CLI   BYTE,X'08'          CABLE                                        
         JE    NO                                                               
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRD RECORD                                                         
*---------------------------------------------------------------------*         
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PRODUCT LIST RECORDS                                                          
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD RECORDS FOR PRODUCT LIST                                                 
*---------------------------------------------------------------------*         
LOADPDL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRDHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PKEYAM,SXDTAGB                                                   
*                                                                               
         TM    MISCFLG,MFLDUPDQ                                                 
         BZ    LPDL01                                                           
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         BZ    LPDL01                                                           
         MVC   PKEYAM,SVPRAM                                                    
         MVC   PKEYCLT,SVPRBCLT                                                 
         MVI   PKEYPRD+L'PKEYPRD,X'FF'                                          
*                                                                               
LPDL01   DS    0H                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPDL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTPDLC,AINITPDL,AFILTPDL,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPDL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER RECORD FOR PRODUCT LIST                                                
*---------------------------------------------------------------------*         
FILTPDL  NTR1  BASE=*,LABEL=*                                                   
         USING CLTHDR,R2                                                        
*                                                                               
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLC   4(3,R2),=XL3'0'                                                  
         JE    NO                                                               
         CLC   7(6,R2),=XL6'0'                                                  
         JNE   NO                                                               
*                                                                               
         TM    MISCFLG,MFLDUPDQ    INDICATE LOADING IN UPDATIVE MODE            
         JZ    YES                                                              
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         JZ    YES                                                              
         CLC   CKEYAM,SVPRAM                                                    
         JNE   NO                                                               
         CLC   CKEYCLT,SVPRBCLT                                                 
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE RECORD FOR PRODUCT LIST                                            
*---------------------------------------------------------------------*         
INITPDL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PRDODUCT LIST DATA                                                     
*---------------------------------------------------------------------*         
*                                                                               
UPDTPDL  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTPDL                                                         
         JNE   YES                                                              
         GOTO1 AINITPDL                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPDL5 DS    0H                                                               
         GOTO1 AACCUPDT,DMCB,VSPTPDLC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPDL5                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ESTIMATE RECORDS                                                              
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD EST RECORDS                                                              
*---------------------------------------------------------------------*         
LOADEST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ESTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   EKEYAM,SXDTAGB                                                   
*                                                                               
         TM    MISCFLG,MFLDUPDQ                                                 
         BZ    LEST01                                                           
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         BZ    *+10                                                             
         MVC   EKEYAM,SVPRAM                                                    
         OC    SVPRBCLT,SVPRBCLT                                                
         BZ    *+10                                                             
         MVC   EKEYCLT,SVPRBCLT                                                 
         OC    SVPRCPRD,SVPRCPRD                                                
         BZ    *+10                                                             
         MVC   EKEYPRD,SVPRCPRD                                                 
*                                                                               
LEST01   DS    0H                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTESTC,AINITEST,AFILTEST,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEST02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE EST RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTEST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         XC    SVCLTPRD,SVCLTPRD                                                
         XC    SVPRDBFM,SVPRDBFM                                                
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTEST                                                         
         JNE   YES                                                              
         GOTO1 AINITEST                                                         
         GOTO1 AACCUPDT,DMCB,VSPTESTC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         L     RF,DXAXREC                                                       
         CLI   (SPTESACT-SPTESD)(RF),C'D'                                       
         JE    YES                                                              
*                                                                               
* EDL, EAD NOT NEEDED FOR OPT                                                   
*                                                                               
         CLC   SXDTTYP,=C'OPT'                                                  
         JE    YES                                                              
*                                                                               
* IF ESTIMAT RECORD CHANGES, UPDATE ESTIMATE DEMO LIST                          
*                                                                               
         GOTO1 AFILTEDL                                                         
         JNE   YES                                                              
         GOTO1 AINITEDL                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTES10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTEDLC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTES10                                                         
*                                                                               
* IF ESTIMAT RECORD CHANGES, UPDATE AUTHORIZED TOLLARS RECORD                   
*                                                                               
         CLC   =C'MDM',SXDTTYP MDM DATABASE DOESN'T HAVE EAD TABLE              
         JE    YES             DON'T UPDATE EAD RECORD                          
*                                                                               
         GOTO1 AFILTEAD                                                         
         JNE   YES                                                              
         GOTO1 AINITEAD                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTES20 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTEAD,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTES20                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER EST RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTEST  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R2                                                        
*                                                                               
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLI   7(R2),0             ESTIMATE                                     
         JE    NO                                                               
         CLC   8(5,R2),=XL5'0'                                                  
         JNE   NO                                                               
*                                                                               
         TM    MISCFLG,MFLDUPDQ    INDICATE LOADING IN UPDATIVE MODE            
         BZ    FILTEST5                                                         
*                                                                               
         OC    SVPRAM,SVPRAM                                                    
         BZ    FILTEST5                                                         
         CLC   EKEYAM,SVPRAM                                                    
         JNE   NO                                                               
*                                                                               
         OC    SVPRBCLT,SVPRBCLT                                                
         BZ    FILTEST5                                                         
         CLC   EKEYCLT,SVPRBCLT                                                 
         JNE   NO                                                               
*                                                                               
         OC    SVPRCPRD,SVPRCPRD                                                
         BZ    FILTEST5                                                         
         CLC   EKEYPRD,SVPRCPRD                                                 
         JNE   NO                                                               
*                                                                               
FILTEST5 DS    0H                                                               
         CLC   SVCLTPRD,0(R2)      A/M,CLT,PRD CHANGED?                         
         JE    YES                                                              
         MVC   SVCLTPRD,0(R2)                                                   
*                                                                               
         BRAS  RE,GETPRBFM         GET PRODUCT'S BILL FORMULA                   
*                                                                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FLIGHT RECORDS                                                                
***********************************************************************         
*                                                                               
LOADFLT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DFLRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,SXDTAGB                                                 
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LFLT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTFLT,AINITFLT,AFILTFLT,(R8)                     
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LFLT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
UPDTFLT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         XC    SVCLTPRD,SVCLTPRD                                                
         XC    SVPRDBFM,SVPRDBFM                                                
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTFLT                                                         
         JNE   YES                                                              
         GOTO1 AINITFLT                                                         
         GOTO1 AACCUPDT,DMCB,VSPTFLT,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
* IF FLIGHT RECORD CHANGES, UPDATE FLIGHT RECORD FLIGHTS                        
*                                                                               
         GOTO1 AINITFLF                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTFL10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTFLF,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTFL10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
FILTFLT  NTR1  BASE=*,LABEL=*                                                   
         USING DFLRECD,R2                                                       
*                                                                               
         CLI   DFLKTYP,DFLKTYPQ                                                 
         JNE   NO                                                               
         CLI   DFLKSUB,DFLKSUBQ                                                 
         JNE   NO                                                               
         MVC   BYTE,DFLKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JE    YES                                                              
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
                                                                                
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
LOADFLF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DFLRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,SXDTAGB                                                 
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LFLF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTFLF,AINITFLF,AFILTFLF,(R8)                     
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LFLF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
UPDTFLF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTFLF                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTFF10 DS    0H                                                               
         GOTO1 AINITFLF                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTFLF,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTFF10                                                         
         J     YES                                                              
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
FILTFLF  NTR1  BASE=*,LABEL=*                                                   
         USING DFLRECD,R2                                                       
*                                                                               
         CLI   DFLKTYP,DFLKTYPQ                                                 
         JNE   NO                                                               
         CLI   DFLKSUB,DFLKSUBQ                                                 
         JNE   NO                                                               
         MVC   BYTE,DFLKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JE    YES                                                              
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
                                                                                
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
INITFLF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTFLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ESTIMATE AUTHORIZED DOLLARS                                                   
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD EAD RECORDS                                                              
*---------------------------------------------------------------------*         
LOADEAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ESTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   EKEYAM,SXDTAGB                                                   
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEAD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTEAD,AINITEAD,AFILTEAD,(R8)                     
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEAD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE EAD RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTEAD                                                         
         JNE   YES                                                              
         GOTO1 AINITEAD                                                         
         GOTO1 AACCUPDT,DMCB,VSPTEAD,TYPECODE,0,(R8)                            
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER EAD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTEAD  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLI   7(R2),0             ESTIMATE                                     
         JE    NO                                                               
         CLC   8(5,R2),=XL5'0'                                                  
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INITEAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTELDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD BIL RECORDS                                                              
*---------------------------------------------------------------------*         
LOADBIL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         XCEFL SVBILL,L'SVBILL                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING BILLRECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVC   BKEYAM,SXDTAGB                                                   
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBIL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTBIL,AINITBIL,AFILTBIL,(R8)                     
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBIL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE BIL RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTBIL  NTR1  BASE=*,LABEL=*                                                   
         XCEFL SVBILL,L'SVBILL                                                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTBIL                                                         
         JNE   YES                                                              
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTBL10 DS    0H                                                               
         GOTO1 AINITBIL                                                         
         GOTO1 AACCUPDT,DMCB,VSPTBIL,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTBL10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER BIL RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTBIL  NTR1  BASE=*,LABEL=*                                                   
         USING BILLRECD,R2                                                      
*                                                                               
         CLI   BKEYTYPE,X'00'                                                   
         JNE   NO                                                               
         MVC   BYTE,BKEYAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         OC    BKEYYSRV(3),BKEYYSRV BILL RECORD?                                
         JZ    NO                                                               
*                                                                               
         OC    FILTCLI,FILTCLI                                                  
         JZ    YES                                                              
         CLC   BKEYCLT,FILTCLI                                                  
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INITBIL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXG$TRNL         R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DARE ORDER RECORDS                                                            
***********************************************************************         
*---------------------------------------------------------------------*         
* LOAD DAO RECORDS                                                              
*---------------------------------------------------------------------*         
LOADDAO  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         MVC   SVTYPCOD,TYPECODE                                                
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DAREORDD,R2                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,SXDTAGB                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDAO02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTDAOC,AINITDAO,AFILTDAO,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDAO02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE DAO RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTDAO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVTYPCOD,TYPECODE                                                
*                                                                               
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING DAREORDD,R2                                                      
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTDAO                                                         
         JNE   YES                                                              
         GOTO1 AINITDAO                                                         
         GOTO1 AACCUPDT,DMCB,VSPTDAOC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         CLC   TYPECODE,=C'ORD'                                                 
         JE    YES                                                              
*                                                                               
         TM    FLAGS,UPDWRTQ       UPDATE WRITTEN TO OUTPUT?                    
         JZ    YES                                                              
*                                                                               
* GENERATE DB2 BUY EXTRACT REQUEST                                              
*                                                                               
         GOTO1 AACCUPDT,DMCB,VSPTBXQC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5,R2                                                            
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DAO RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTDAO  NTR1  BASE=*,LABEL=*                                                   
         USING DAREORDD,R2                                                      
*                                                                               
         CLI   DOKTYPE,DOKTYPQ                                                  
         JNE   NOMOREQX                                                         
         CLI   DOKSUBTY,DOKSTYPQ                                                
         JNE   NOMOREQX                                                         
         MVC   BYTE,DOKAGMD                                                     
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NOMOREQX                                                         
         CLI   DOKCMT,X'00'                                                     
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
INITDAO  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DARE MAKEGOOD NOTICE RECORDS                                                  
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DMN RECORDS                                                              
*---------------------------------------------------------------------*         
LOADDMN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MNKEY,R2                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   MNKTYPE,MNKTYPQ     X'0D'                                        
         MVI   MNKSUBTY,MNKSTYPQ   X'36'                                        
         MVC   MNKAGMD,SXDTAGB                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDMN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTDMNC,AINITDMN,AFILTDMN,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDMN02                                                           
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE DMN RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTDMN  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING DAREMGND,R2                                                      
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTDMN                                                         
         JNE   YES                                                              
         GOTO1 AINITDMN                                                         
         GOTO1 AACCUPDT,DMCB,VSPTDMNC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,UPDWRTQ       UPDATE WRITTEN TO OUTPUT?                    
         JZ    YES                                                              
*                                                                               
* GENERATE DB2 BUY EXTRACT REQUEST                                              
*                                                                               
         GOTO1 AACCUPDT,DMCB,VSPTBXQC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DMN RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTDMN  NTR1  BASE=*,LABEL=*                                                   
         USING DAREMGND,R2                                                      
*                                                                               
         CLI   MNKTYPE,MNKTYPQ                                                  
         JNE   NOMOREQX                                                         
         CLI   MNKSUBTY,MNKSTYPQ                                                
         JNE   NOMOREQX                                                         
         MVC   BYTE,MNKAGMD                                                     
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NOMOREQX                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INITDMN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DARE MAKEGOOD NOTICE RECORDS - XSPOT                                          
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DMN RECORDS - XSPOT                                                      
*---------------------------------------------------------------------*         
LOADDMX  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE DMN RECORD DATA - XSPOT                                                
*---------------------------------------------------------------------*         
UPDTDMX  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING MNXKEY,R2                                                        
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTDMX                                                         
         JNE   YES                                                              
         GOTO1 AINITDMX                                                         
         GOTO1 AACCUPDT,DMCB,VSPTDMXC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,UPDWRTQ       UPDATE WRITTEN TO OUTPUT?                    
         JZ    YES                                                              
*                                                                               
* GENERATE DB2 BUY EXTRACT REQUEST                                              
*                                                                               
         GOTO1 AACCUPDT,DMCB,VSPTBXQC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DMN RECORD AT R2 - XSPOT                                     *         
*---------------------------------------------------------------------*         
FILTDMX  NTR1  BASE=*,LABEL=*                                                   
         USING MNXKEY,R2                                                        
*                                                                               
         CLI   MNXKTYPE,MNXKTYPQ                                                
         JNE   NOMOREQX                                                         
         CLI   MNXKSBTY,MNXKSBTQ                                                
         JNE   NOMOREQX                                                         
         MVC   BYTE,MNXKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NOMOREQX                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INITDMX  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER SPOT UCOM RECORD                                                       
*---------------------------------------------------------------------*         
FILTUCM  NTR1  BASE=*,LABEL=*                                                   
         USING UCOMHDR,R2                                                       
*                                                                               
         NI    MISCFLG,X'FF'-MFCLUCMQ TURN OFF CLT-LEVEL INDICATOR              
*                                                                               
         CLC   UCOMKTYP,=X'0D0C'    STILL UCOM RECORD?                          
         BNE   FUCMNQXX                                                         
*                                                                               
         MVC   BYTE,UCOMKAGY       A/M BYTE                                     
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FUCMNQXX                                                         
*                                                                               
         CLI   UCOMCTYP,C'U'                                                    
         BNE   FUCMNQX                                                          
*                                                                               
         TM    MISCFLG,MFLDUPDQ    LOADING RECORDS IN UPDATE MODE?              
         BZ    FUCM05              NO                                           
* YES - ONLY LOAD UCOMS FOR ONE SPECIFIC CLIENT (SAVED IN SVUCMCLT)             
         CLC   SVUCMMED,UCOMKAGY   SAME A/M?                                    
         BNE   FUCMNQX                                                          
         CLC   SVUCMCLT,UCOMKCLT   SAME CLIENT?                                 
         BNE   FUCMNQX                                                          
*                                                                               
FUCM05   DS    0H                                                               
         OC    UCOMKPRD(L'UCOMKPRD+L'UCOMKEST+L'UCOMKMKT),UCOMKPRD              
         BNZ   FUCM10              NONZERO - DO PRD/EST LEVEL UCOM              
*                                                                               
* CLT-LEVEL UCOM HERE (FIELD DEFINITIONS)                                       
*                                                                               
         OI    MISCFLG,MFCLUCMQ    INDICATE CLT-LEVEL CHANGES                   
         MVC   SVUCMMED,UCOMKAGY   SAVE A/M                                     
         MVC   SVUCMCLT,UCOMKCLT   SAVE CLIENT CODE                             
         BRAS  RE,UCOMDEF          SAVE FIELD DEFINITIONS                       
*                                                                               
         TM    MISCFLG,MFLDUPDQ    LOADING UCOMS IN UPDATE MODE?                
         BO    FUCMNQX             YES - DO NOT EXTRACT THE DEFINITION          
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         BE    FUCMNQX             NO, DON'T EXTRACT THE DEF RECORD             
         B     FUCMQX                                                           
*                                                                               
* ACTUAL UCOM HERE (PRD OR EST LEVEL)                                           
*                                                                               
FUCM10   CLI   DXMODE,DXLOADQ      LOAD?                                        
         BE    FUCMQX              NO, SKIP - DEFINITIONS ALREADY SAVED         
         TM    MISCFLG,MFLDUPDQ    LOADING UCOMS IN UPDATE MODE?                
         BO    FUCMQX              YES, SKIP - DEFS ALREADY SAVED               
         BRAS  RE,UCOMDEF          UPDATE - READ DEFINITIONS                    
         J     FUCMQX              EXTRAT CURRENT RECORD                        
*                                                                               
FUCMQX   DS    0H                  OK EXIT                                      
         J     YES                                                              
*                                                                               
FUCMNQXX DS    0H                  SET NOMOREQ FLAG                             
         OI    FLAGS,NOMOREQ                                                    
*                                                                               
FUCMNQX  DS    0H                  UNEQUAL EXIT                                 
         J     NO                                                               
         DROP  R2                                                               
                                                                                
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION LOCKIN RECORDS                                                 
*---------------------------------------------------------------------*         
FILTSLK  NTR1  BASE=*,LABEL=*                                                   
         USING SLKRECD,R2                                                       
*                                                                               
         CLI   SLKKTYP,SLKKTYPQ    STILL UCOM RECORD?                           
         JNE   NOMOREQX                                                         
         CLI   SLKKSUB,SLKKSUBQ    STILL UCOM RECORD?                           
         JNE   NOMOREQX                                                         
*                                                                               
         MVC   BYTE,SLKKAGMD       A/M BYTE                                     
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NOMOREQX                                                         
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE EST RECORD                                                         
*---------------------------------------------------------------------*         
INITEST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTESDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE FLT RECORD                                                         
*---------------------------------------------------------------------*         
INITFLT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTFLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* REP RECORDS                                                                   
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD REP RECORDS                                                              
*---------------------------------------------------------------------*         
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REPREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   REPKTYPE,C'R'                                                    
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTREPC,AINITREP,AFILTREP,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE REP RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTREP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTREP                                                         
         JNE   YES                                                              
         GOTO1 AINITREP                                                         
         GOTO1 AACCUPDT,DMCB,VSPTREPC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER REP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTREP  NTR1  BASE=*,LABEL=*                                                   
         USING REPREC,R2                                                        
*                                                                               
         CLI   0(R2),C'R'                                                       
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLC   REPKAGY,SXDTAGY                                                  
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE REP RECORD                                                         
*---------------------------------------------------------------------*         
INITREP  NTR1  BASE=*,LABEL=*                                                   
         LHI   R1,L'REPREC         R1=L'RECORD                                  
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ESTIMATE DEMO LIST                                                            
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD EST DEMO LIST                                                            
*---------------------------------------------------------------------*         
LOADEDL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ESTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   EKEYAM,SXDTAGB                                                   
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEDL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTEDLC,AINITEDL,AFILTEDL,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LEDL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE EST DEMO LIST                                                          
*---------------------------------------------------------------------*         
*                                                                               
UPDTEDL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTEDL                                                         
         JNE   YES                                                              
         GOTO1 AINITEDL                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTEDLC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER EST DEMO LIST                                                          
*---------------------------------------------------------------------*         
FILTEDL  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLI   7(R2),0             ESTIMATE                                     
         JE    NO                                                               
         CLC   8(5,R2),=XL5'0'                                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE EST DEMO LIST                                                      
*---------------------------------------------------------------------*         
*                                                                               
INITEDL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTEDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SPILLDEF                                                                      
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD SPILLDEF                                                                 
*---------------------------------------------------------------------*         
LOADSPD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING SDEFKEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   SDEFKTY,SDEFKTYQ                                                 
         MVI   SDEFKSB,SDEFKSBQ                                                 
         MVC   SDEFKAGY,SXDTAGY                                                 
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSPD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTSPCC,AINITSPD,AFILTSPD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LSPD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE SPILLDEF                                                               
*---------------------------------------------------------------------*         
*                                                                               
UPDTSPD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSPD                                                         
         JNE   YES                                                              
         GOTO1 AINITSPD                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSPCC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER SPILLDEF                                                               
*---------------------------------------------------------------------*         
FILTSPD  NTR1  BASE=*,LABEL=*                                                   
         USING SDEFRECD,R2                                                      
*                                                                               
         CLI   SDEFKTY,SDEFKTYQ                                                 
         JNE   FILTSPDN                                                         
         CLI   SDEFKSB,SDEFKSBQ                                                 
         JNE   FILTSPDN                                                         
         CLC   SDEFKAGY,SXDTAGY                                                 
         JNE   FILTSPDN                                                         
*                                                                               
         J     YES                                                              
*                                                                               
FILTSPDN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE SPILLDEF                                                           
*---------------------------------------------------------------------*         
*                                                                               
INITSPD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SQAD DAYPART MENU                                                             
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD SQAD DAYPART MENU                                                        
*---------------------------------------------------------------------*         
LOADSQD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING SQDKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   SQDKTYP,SQDKTYPQ                                                 
         MVI   SQDKSUB,SQDKSUBQ                                                 
         MVC   SQDKAGMD,SXDTAGB                                                 
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSQD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTSQCC,AINITSQD,AFILTSQD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LSQD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE SQAD DAYPART MENUS                                                     
*---------------------------------------------------------------------*         
*                                                                               
UPDTSQD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSQD                                                         
         JNE   YES                                                              
         GOTO1 AINITSQD                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSQCC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER SQAD DAYPART MENUS                                                     
*---------------------------------------------------------------------*         
FILTSQD  NTR1  BASE=*,LABEL=*                                                   
         USING SQDRECD,R2                                                       
*                                                                               
         CLI   SQDKTYP,SQDKTYPQ                                                 
         JNE   FILTSQDN                                                         
         CLI   SQDKSUB,SQDKSUBQ                                                 
         JNE   FILTSQDN                                                         
         MVC   BYTE,SQDKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   FILTSQDN                                                         
*                                                                               
         J     YES                                                              
*                                                                               
FILTSQDN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE SQAD DAYPART MENUS                                                 
*---------------------------------------------------------------------*         
*                                                                               
INITSQD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSQDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DEMO MENUS                                                               
*---------------------------------------------------------------------*         
LOADDMU  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DMNRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   DMNKTYP,=X'0D26'                                                 
         MVC   DMNKAGMD,SXDTAGB                                                 
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDMU02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTDMUC,AINITDMU,AFILTDMU,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LDMU02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE DEMO MENUS                                                             
*---------------------------------------------------------------------*         
*                                                                               
UPDTDMU  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTDMU                                                         
         JNE   YES                                                              
         GOTO1 AINITDMU                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTDMUC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DEMO MENUS                                                             
*---------------------------------------------------------------------*         
FILTDMU  NTR1  BASE=*,LABEL=*                                                   
         USING DMNRECD,R2                                                       
*                                                                               
         CLC   DMNKTYP,=X'0D26'                                                 
         JNE   FILTDMUN                                                         
         MVC   BYTE,DMNKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   FILTDMUN                                                         
*                                                                               
         J     YES                                                              
*                                                                               
FILTDMUN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE DEMO MENUS                                                         
*---------------------------------------------------------------------*         
*                                                                               
INITDMU  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMUDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD BILL FORMULAS                                                            
*---------------------------------------------------------------------*         
LOADBFM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING BFREC,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,SXDTAGB                                                 
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBFM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTBFMC,AINITBFM,AFILTBFM,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LBFM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE BILL FORMULAS                                                          
*---------------------------------------------------------------------*         
*                                                                               
UPDTBFM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTBFM                                                         
         JNE   YES                                                              
         GOTO1 AINITBFM                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTBFMC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER BILL FORMULAS                                                          
*---------------------------------------------------------------------*         
FILTBFM  NTR1  BASE=*,LABEL=*                                                   
         USING BFREC,R2                                                         
*                                                                               
         CLI   BFKTYPE,BFKTYPEQ                                                 
         JNE   FILTBFMN                                                         
         CLI   BFKSTYPE,BFKSTYPQ                                                
         JNE   FILTBFMN                                                         
         MVC   BYTE,BFKAGYMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   FILTBFMN                                                         
*                                                                               
         J     YES                                                              
*                                                                               
FILTBFMN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE BILL FORMULAS                                                      
*---------------------------------------------------------------------*         
*                                                                               
INITBFM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTBFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* EQUIVALENCE FACTORS                                                           
***********************************************************************         
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD EQUIVALENCE FACTOR LIST                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADEQV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   EQVFLAG,C'N'                                                     
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING EQUHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   EQUKTYPE,X'09'                                                   
         MVC   EQUKAGY,SXDTAGY                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEQV02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTEQVC,AINITEQV,AFILTEQV,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LEQV02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE EQUIVALENCE FACTOR LIST                                                
*---------------------------------------------------------------------*         
*                                                                               
UPDTEQV  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AFILTEQV                                                         
         JNE   YES                                                              
         GOTO1 AINITEQV                                                         
         CLI   DXACTION,C'A'       CHANGE?                                      
         BE    *+8                 NO NEED FOR KILL ON ACTION ADD               
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTEQ10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTEQVC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTEQ10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER EQUIVALENCE FACTORS LIST                                               
*---------------------------------------------------------------------*         
FILTEQV  NTR1  BASE=*,LABEL=*                                                   
         USING EQUHDR,R2                                                        
*                                                                               
         CLI   EQUKTYPE,X'09'                                                   
         BNE   FILTEQNX                                                         
*                                                                               
         CLC   EQUKAGY,SXDTAGY     SAME AGENCY?                                 
         BNE   *+12                NO - SEE WHETHER TO LOOK FOR DEFAULT         
         MVI   EQVFLAG,C'Y'        INDICATE FOUND AGENCY EQU                    
         B     FEQV10              EXTRACT THE DATA                             
*                                                                               
         CLI   EQVFLAG,C'Y'        FOUND AGENCY EQV ALREADY?                    
         JE    FILTEQNX            YES - JUST STOP LOOKING                      
*                                                                               
         CLC   EQUKAGY,=C'00'      LOOK FOR DEFAULT                             
         JNE   NO                                                               
*                                                                               
FEQV10   DS    0H                                                               
*        OC    EQUKCLT,EQUKCLT                                                  
*        JNZ   NO                                                               
*                                                                               
         L     RF,ASLNTAB                                                       
         LR    R1,RF               POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   EQUKMED,C'T'                                                     
         BE    FEQV20                                                           
         CLI   EQUKMED,C'N'                                                     
         BE    FEQV20                                                           
         CLI   EQUKMED,C'C'                                                     
         BE    FEQV20                                                           
*                                                                               
         LA    R0,C'R'                                                          
         CLI   EQUKMED,C'R'                                                     
         BE    FEQV20                                                           
         CLI   EQUKMED,C'X'                                                     
         JNE   NO                                                               
*                                                                               
FEQV20   CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    FEQV40                                                           
         CLC   EQUKAGY,0(R1)        MATCH AGY                                   
         BNE   *+12                                                             
FEQV40   CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    FEQV60                                                           
*                                                                               
         BXLE  R1,RE,FEQV20        NEXT ENTRY                                   
         B     FILTEQNX                                                         
*                                                                               
FEQV60   DS    0H                                                               
         AHI   R1,4                                                             
         STCM  R1,15,ADSLNTAB-MEDPARAM(R8) SAVE A(SLNTAB) IN MEDPARAM           
*                                                                               
         J     YES                                                              
*                                                                               
FILTEQNX DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE EQUIVALENCE FACTORS LIST                                           
*---------------------------------------------------------------------*         
*                                                                               
INITEQV  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTEFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD COMSCORE MARKET RECORDS                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMKC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKTREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   MKTKTYPE,MKTKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VSPTMKCC,AINITMKC,AFILTMKC,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKC02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE COMSCORE MARKET RECORD DATA                                            
*---------------------------------------------------------------------*         
*                                                                               
UPDTMKC  BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD MARKET RECORDS                                                           
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKTREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   MKTKTYPE,MKTKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VSPTMKTC,AINITMKT,AFILTMKT,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE MARKET RECORD DATA                                                     
*---------------------------------------------------------------------*         
*                                                                               
UPDTMKT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTMKT                                                         
         JNE   YES                                                              
         GOTO1 AINITMKT                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTMKTC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER MARKET RECORD AT R2                                                    
*---------------------------------------------------------------------*         
FILTMKT  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),C'M'                                                       
         JNE   NO                                                               
         CLC   6(2,R2),SXDTAGY                                                  
         JE    YES                                                              
         J     NO                                                               
                                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE MARKET RECORD                                                      
*---------------------------------------------------------------------*         
*                                                                               
INITMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMKDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION RECORDS                                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSTA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING STAREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   STAKTYPE,STAKTYPQ                                                
*                                                                               
         TM    MISCFLG,MFLDUPDQ                                                 
         BZ    LSTA01                                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'STAKEY),LOADKEY                                          
*                                                                               
LSTA01   DS    0H                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         GOTO1 AACCLOAD,DMCB,VSPTSTAC,AINITSTA,AFILTSTA,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD SYSCODE/NETWORK LIST                                                     
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSNL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING STAREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   STAKTYPE,STAKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSNL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(L'STAKEY),0(R2)                                            
         BE    LSNL10                                                           
*                                                                               
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
LSNL10   DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTNLC,AINITSNL,AFILTSNL,(R8)                     
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSNL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD SPOT UCOM RECORD                                                         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUCM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R8,MEDPARAM                                                      
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         STCM  RE,15,ABREAKT                                                    
         XCEFL                                                                  
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING UCOMHDR,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   UCOMKTYP,=X'0D0C'                                                
         MVC   UCOMKAGY,SXDTAGB                                                 
         MVI   UCOMCTYP,C'U'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUCM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTUCMC,AINITUCM,AFILTUCM,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUCM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION LOCKIN RECORDS                                                   
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSLK  NTR1  BASE=*,LABEL=*                                                   
         MVC   ANETTABM,ANETTAB                                                 
         XC    SVAMCLT,SVAMCLT                                                  
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R8,MEDPARAM                                                      
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         STCM  RE,15,ABREAKT                                                    
         XCEFL                                                                  
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING SLKRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSLK02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLI   SXDTCTRY,CTRYCAN                                                 
         BNE   LSLK10                                                           
         CLC   SVAMCLT,SLKKAGMD-SLKKEY(R2)                                      
         BE    LSLK10                                                           
         BRAS  RE,BLDNET                                                        
         MVC   SVAMCLT,SLKKAGMD-SLKKEY(R2)                                      
*                                                                               
LSLK10   DS    0H                                                               
         MVC   PRIADDR,36(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTSLKC,AINITSLK,AFILTSLK,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSLK02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION RECORD DATA                                                    
*---------------------------------------------------------------------*         
*                                                                               
UPDTSTA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSTA                                                         
         JNE   YES                                                              
         GOTO1 AINITSTA                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSTAC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE SYSCODE/NETWORK LIST                                                   
*---------------------------------------------------------------------*         
*                                                                               
UPDTSNL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSNL                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTSN10 DS    0H                                                               
         GOTO1 AINITSNL                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTNLC,TYPECODE,0,(R8)                            
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTSN10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE SPOT UCOM RECORD                                                       
*---------------------------------------------------------------------*         
UPDTUCM  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         LA    R8,MEDPARAM                                                      
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         GOTO1 AFILTUCM                                                         
         JNE   YES                                                              
*                                                                               
         TM    MISCFLG,MFCLUCMQ    CLT-LEVEL UCOM?                              
         BZ    UPDTUC05                                                         
*                                                                               
         CLI   DXACTION,C'C'                                                    
         JNE   YES                 ACT ONLY ON ACTION = 'CHANGE'                
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         B     UPDTUC10                                                         
*                                                                               
UPDTUC05 DS    0H                  REGULAR UCOM HERE                            
         CLI   DXACTION,C'A'       CHANGE?                                      
         BE    *+8                 NO NEED FOR KILL ON ACTION ADD               
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
*                                                                               
         CLI   DXACTION,C'D'       UCOM, ACT = DELETE?                          
         BE    *+8                 KILL ONLY ON DELETE                          
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTUC10 DS    0H                                                               
         GOTO1 AINITUCM                                                         
         GOTO1 AACCUPDT,DMCB,VSPTUCMC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTUC10                                                         
*                                                                               
         TM    MISCFLG,MFCLUCMQ    CLT-LEVEL UCOM DEFINITION?                   
         JZ    YES                 NO - EXIT                                    
*                                                                               
         OI    MISCFLG,MFLDUPDQ    YES - NEED TO RE-LOAD ALL UCOMS              
         OI    FLAGS2,UPDLOADQ                                                  
         L     RF,ALOADUCM                                                      
         BASR  RE,RF                                                            
         NI    FLAGS2,X'FF'-UPDLOADQ                                            
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION LOCKIN RECORDS                                                 
*---------------------------------------------------------------------*         
UPDTSLK  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         LA    R8,MEDPARAM                                                      
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         GOTO1 AFILTSLK                                                         
         JNE   YES                                                              
*                                                                               
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTSL10 DS    0H                                                               
         GOTO1 AINITSLK                                                         
         GOTO1 AACCUPDT,DMCB,VSPTSLKC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTSL10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTSTA  NTR1  BASE=*,LABEL=*                                                   
         USING STAREC,R2                                                        
*                                                                               
         CLI   STAKTYPE,STAKTYPQ                                                
         JNE   NO                                                               
         CLC   STAKAGY,SXDTAGY                                                  
         JNE   NO                                                               
*                                                                               
         TM    MISCFLG,MFLDUPDQ    INDICATE LOADING IN UPDATIVE MODE            
         BZ    FILTSTA5                                                         
*                                                                               
         CLC   STAKMED,LOADKEY+STAKMED-STAKEY                                   
         JNE   NO                                                               
         CLC   STAKCALL,LOADKEY+STAKCALL-STAKEY                                 
         JNE   NO                                                               
*                                                                               
FILTSTA5 DS    0H                                                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER SYSCODE/NETWORK LIST AT R2                                             
*---------------------------------------------------------------------*         
FILTSNL  NTR1  BASE=*,LABEL=*                                                   
         USING STAREC,R2                                                        
         CLI   STAKTYPE,C'S'                                                    
         JNE   NO                                                               
         CLI   STAKMED,C'T'                                                     
         JNE   NO                                                               
         CLC   STAKAGY,SXDTAGY                                                  
         JNE   NO                                                               
         CLC   STAKCLT,=C'000'                                                  
         JNE   NO                                                               
         LA    R1,STAKCALL                                                      
         LHI   R0,4                                                             
         BRAS  RE,ISNUM                                                         
         JNE   NO                                                               
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION RECORD                                                     
*---------------------------------------------------------------------*         
*                                                                               
INITSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE SPOT UCOM RECORD                                                   
*---------------------------------------------------------------------*         
INITUCM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTUCMDL         R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION LOCKIN RECORDS                                             
*---------------------------------------------------------------------*         
INITSLK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSLDL         R1=L'RECORD (LONGEST)                         
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE SYSCODE/NETWORK LIST                                               
*---------------------------------------------------------------------*         
*                                                                               
INITSNL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTNLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION ADDRESS RECORDS                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ADDRREC,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   ADDKTYPE,ADDKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSAD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VSPTSADC,AINITSAD,AFILTSAD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSAD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION ADDRESS RECORD DATA                                            
*---------------------------------------------------------------------*         
*                                                                               
UPDTSAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSAD                                                         
         JNE   YES                                                              
         GOTO1 AINITSAD                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSADC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
* RE-LOAD STATION RECORD                                                        
* BUILD STATION RECORD KEY IN LOADKEY                                           
*                                                                               
         OI    MISCFLG,MFLDUPDQ+MFDELADQ                                        
*                                                                               
         XC    LOADKEY,LOADKEY                                                  
         MVC   LOADKEY(L'ADDRKEY),0(R2)                                         
         LA    R3,LOADKEY                                                       
         USING STAREC,R3                                                        
         MVI   STAKTYPE,STAKTYPQ                                                
         XC    STAKCLT,STAKCLT                                                  
         XC    STAKFILL,STAKFILL                                                
         DROP  R3                                                               
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
         OI    FLAGS,STAFILEQ                                                   
         L     RF,ALOADSTA                                                      
         BASR  RE,RF                                                            
         NI    MISCFLG,X'FF'-MFLDUPDQ-MFDELADQ                                  
         NI    FLAGS,X'FF'-STAFILEQ                                             
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION ADDRESS RECORD AT R2                                           
*---------------------------------------------------------------------*         
FILTSAD  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),C'A'                                                       
         JNE   NO                                                               
         CLC   7(2,R2),SXDTAGY                                                  
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION ADDRESS RECORD                                             
*---------------------------------------------------------------------*         
*                                                                               
INITSAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* COMMERCIAL RECORDS                                                            
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD COMMERCIAL RECORDS                                                       
*---------------------------------------------------------------------*         
LOADCML  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         STCM  RE,15,ABREAKT                                                    
         XCEFL                                                                  
*                                                                               
LCML01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CMLKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCML02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTCMLC,AINITCML,AFILTCML,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCML02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE CML RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTCML  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CLTHDR,R2                                                        
         L     RE,ABRKTAB                                                       
         STCM  RE,15,ABREAKT                                                    
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         GOTO1 AFILTCML                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCML                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTCMLC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CML RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTCML  NTR1  BASE=*,LABEL=*                                                   
         USING CMLRECD,R2                                                       
         CLC   =X'0A21',CMLKID                                                  
         JNE   NO                                                               
*                                                                               
         MVC   BYTE,CMLKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         CLC   CMLKCML,=XL8'00'                                                 
         JE    NO                                                               
         CLC   CMLKCML,=8C'9'                                                   
         JE    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE CML RECORD                                                         
*---------------------------------------------------------------------*         
INITCML  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTCMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* STATION OWNERSHIP/FORMAT/OWNER INFO RECORDS                                   
***********************************************************************         
*                                                                               
LOADOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOWNTAB                                                       
*                                                                               
LOWN02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOWN04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         NI    FLAGS,X'FF'-NOMOREQ                                              
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOWN04   LA    R3,L'LOWNTAB(R3)                                                 
         J     LOWN02                                                           
*                                                                               
LOWNTAB  DS    0XL8                                                             
         DC    CL3'SOW',AL1(0),AL4(LOADSOW) STA OWNER                           
         DC    CL3'OIN',AL1(0),AL4(LOADOIN) OWNER INFO                          
         DC    CL3'FOR',AL1(0),AL4(LOADFOR) STA FORMAT                          
         DC    X'00'                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTOWN  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* STATION OWNERSIP RECORDS                                                      
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD STATION OWNERSHIP RECORDS                                                
*---------------------------------------------------------------------*         
LOADSOW  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CT99RECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSRA                                                
         MVC   CT99KSRC,SVAGYSRC                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSOW02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTSOWC,AINITSOW,AFILTSOW,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSOW02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION OWNERSHIP DATA                                                 
*---------------------------------------------------------------------*         
UPDTSOW  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CT99RECD,R2                                                      
*                                                                               
         GOTO1 AFILTSOW                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSOW                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSOWC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION OWNERSHIP RECORD (R2)                                *         
*---------------------------------------------------------------------*         
FILTSOW  NTR1  BASE=*,LABEL=*                                                   
         USING CT99RECD,R2                                                      
*                                                                               
         LHI   R1,CT99KSRA                                                      
         BRAS  RE,FILT99                                                        
         JNE   NO                                                               
*                                                                               
* THERE ARE RECORDS WITH BOTH UNIQUE IDS AND STATION CALL LETTERS               
* THE WAY TO DISTINGUISH THEM IS BY LENGTH                                      
* CALL LETTERS ARE ALWAYS 5 CHARS, SO LAST CHARACTER WILL BE 00/40              
* UNIQUE ID IS ALWAYS 6 CHARACTERS                                              
*                                                                               
         CLI   CT99KUID+5,C' '                                                  
         JNL   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STA OWNERSHIP                                                      
*---------------------------------------------------------------------*         
INITSOW  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSODL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* PARENT INFORMATION RECORDS                                                    
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PARENT INFORMATION RECORDS                                               
*---------------------------------------------------------------------*         
LOADPAR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CT99RECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSMM                                                
         MVC   CT99KSRC,SVAGYSRC                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPAR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPARC,AINITPAR,AFILTPAR,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPAR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PARENT INFORMATION                                                     
*---------------------------------------------------------------------*         
UPDTPAR  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CT99RECD,R2                                                      
*                                                                               
         GOTO1 AFILTPAR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPAR                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPARC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PARENT INFORMATION (R2)                                                
*---------------------------------------------------------------------*         
FILTPAR  NTR1  BASE=*,LABEL=*                                                   
         USING CT99RECD,R2                                                      
*                                                                               
         LHI   R1,CT99KSMM                                                      
         BRAS  RE,FILT99                                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PARENT INFORMATION                                                 
*---------------------------------------------------------------------*         
INITPAR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPRDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* STATION FORMAT RECORDS                                                        
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD STATION FORMAT RECORDS                                                   
*---------------------------------------------------------------------*         
LOADFOR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CT99RECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSFM                                                
         MVC   CT99KSRC,SVAGYSRC                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LFOR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTFORC,AINITFOR,AFILTFOR,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LFOR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION FORMAT DATA                                                    
*---------------------------------------------------------------------*         
UPDTFOR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTFOR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITFOR                                                         
*                                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTFORC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION OWNERSHIP RECORD (R2)                                *         
*---------------------------------------------------------------------*         
FILTFOR  NTR1  BASE=*,LABEL=*                                                   
         USING CT99RECD,R2                                                      
*                                                                               
         LHI   R1,CT99KSFM                                                      
         BRAS  RE,FILT99                                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STA OWNERSHIP                                                      
*---------------------------------------------------------------------*         
INITFOR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* OWNER INFORMATION RECORDS                                                     
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD OWNER INFORMATION RECORDS                                                
*---------------------------------------------------------------------*         
LOADOIN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CT99RECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSOW                                                
         MVC   CT99KSRC,SVAGYSRC                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDOI02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTOINC,AINITOIN,AFILTOIN,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDOI02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE OWNER DATA                                                             
*---------------------------------------------------------------------*         
UPDTOIN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CT99RECD,R2                                                      
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         GOTO1 AFILTOIN                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOIN                                                         
         GOTO1 AACCUPDT,DMCB,VSPTOINC,0,0,(R8)                                  
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER OWNER INFO RECORD (R2)                                                 
*---------------------------------------------------------------------*         
FILTOIN  NTR1  BASE=*,LABEL=*                                                   
         USING CT99RECD,R2                                                      
*                                                                               
         LHI   R1,CT99KSOW                                                      
         BRAS  RE,FILT99                                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE OWNER INFO RECORDS                                                 
*---------------------------------------------------------------------*         
INITOIN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTOIDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD ALL MARKET GROUP RECORDS                                                 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMKG  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,MKGTAB                                                        
*                                                                               
LOMG02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOMG04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         NI    FLAGS,X'FF'-(NOMOREQ+STAFILEQ+TRFFILEQ)                          
         NI    FLAGS2,X'FF'-(CTFILEQ+NOGETQ)                                    
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOMG04   LA    R3,L'MKGTAB(R3)                                                  
         J     LOMG02                                                           
*                                                                               
MKGTAB   DS    0XL8                                                             
         DC    CL3'MGD',AL1(0),AL4(LOADMGD) MARKET GROUP DEF                    
         DC    CL3'MGR',AL1(0),AL4(LOADMGR) MARKET GROUPS                       
         DC    CL3'MGA',AL1(0),AL4(LOADMGA) MARKET GROUPS                       
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTMKG  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD MARKET GROUP DEF RECORDS                                                 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMGD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   MKGKTYP(2),=X'0D02'                                              
         MVC   MKGKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMGD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTMGPC,AINITMGD,AFILTMGD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMGD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE MARKET GROUP DEF RECORDS                                               
*---------------------------------------------------------------------*         
*                                                                               
UPDTMGD  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER MARKET GROUP DEF RECORDS                                               
*---------------------------------------------------------------------*         
FILTMGD  NTR1  BASE=*,LABEL=*                                                   
         USING MKGRECD,R2                                                       
         CLC   MKGKTYP,=X'0D02'                                                 
         BNE   FMGDNO                                                           
*                                                                               
         MVC   BYTE,MKGKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FMGDNO                                                           
*                                                                               
         OC    MKGKPRD,MKGKPRD                                                  
         JNZ   NO                                                               
         OC    MKGKMGRP,MKGKMGRP                                                
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
FMGDNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE MARKET GROUP DEF RECORDS                                           
*---------------------------------------------------------------------*         
*                                                                               
INITMGD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTGDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD MARKET GROUP RECORDS                                                     
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   MKGKTYP(2),=X'0D02'                                              
         MVC   MKGKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMGR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTMGRC,AINITMGR,AFILTMGR,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMGR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE MARKET GROUP RECORDS                                                   
*---------------------------------------------------------------------*         
*                                                                               
UPDTMGR  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER MARKET GROUP RECORDS                                                   
*---------------------------------------------------------------------*         
FILTMGR  NTR1  BASE=*,LABEL=*                                                   
         USING MKGRECD,R2                                                       
         CLC   MKGKTYP,=X'0D02'                                                 
         BNE   FMGRNO                                                           
*                                                                               
         MVC   BYTE,MKGKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FMGRNO                                                           
*                                                                               
         OC    MKGKPRD,MKGKPRD                                                  
         JNZ   NO                                                               
         OC    MKGKMGRP,MKGKMGRP                                                
         JZ    NO                                                               
*                                                                               
         CLC   MKGKAGMD(3),SVBKLEN    SAME A/M,CLT?                             
         BNE   *+14                                                             
         CLC   MKGKMID,SVBKLEN+3      SAME GID?                                 
         JE    YES                                                              
*                                                                               
         BRAS  RE,GETBLEN                                                       
         MVC   SVBKLEN(3),MKGKAGMD                                              
         MVC   SVBKLEN+3(1),MKGKMID                                             
         MVC   SVBKLEN+4(3),BREAKLS                                             
*                                                                               
         J     YES                                                              
*                                                                               
FMGRNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE MARKET GROUP RECORDS                                               
*---------------------------------------------------------------------*         
*                                                                               
INITMGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD MARKET GROUP ASSIGN RECORDS                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMGA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         BRAS  RE,DOBRKTAB                                                      
         L     R1,ABRKTAB                                                       
         STCM  R1,15,ABREAKT                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKARECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   MKAKTYP(2),=X'0D03'                                              
         MVC   MKAKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMGA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTMGAC,AINITMGA,AFILTMGA,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMGA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE MARKET GROUP ASSIGNMENT RECORDS                                        
*---------------------------------------------------------------------*         
*                                                                               
UPDTMGA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING AGYHDR,R2                                                        
*                                                                               
         BRAS  RE,DOBRKTAB                                                      
         L     R1,ABRKTAB                                                       
         STCM  R1,15,ABREAKT                                                    
*                                                                               
         GOTO1 AFILTMGA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMGA                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTMG10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTMGAC,0,0,(R8)                                  
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTMG10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER MARKET GROUP ASSIGNMENT RECORDS                                        
*---------------------------------------------------------------------*         
FILTMGA  NTR1  BASE=*,LABEL=*                                                   
         USING MKARECD,R2                                                       
         CLC   MKAKTYP,=X'0D03'                                                 
         BNE   FMGANO                                                           
*                                                                               
         MVC   BYTE,MKAKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FMGANO                                                           
*                                                                               
         J     YES                                                              
*                                                                               
FMGANO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE MARKET GROUP ASSIGNMENT RECORDS                                    
*---------------------------------------------------------------------*         
*                                                                               
INITMGA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD ALL CLIENT GROUP RECORDS                                                 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCLG  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,CLGTAB                                                        
*                                                                               
LOCG02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOCG04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         NI    FLAGS,X'FF'-(NOMOREQ+STAFILEQ+TRFFILEQ)                          
         NI    FLAGS2,X'FF'-(CTFILEQ+NOGETQ)                                    
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOCG04   LA    R3,L'CLGTAB(R3)                                                  
         J     LOCG02                                                           
*                                                                               
CLGTAB   DS    0XL8                                                             
         DC    CL3'CGD',AL1(0),AL4(LOADCGD) CLIENT GROUP DEF                    
         DC    CL3'CGP',AL1(0),AL4(LOADCGP) CLIENT GROUPS                       
         DC    CL3'CGC',AL1(0),AL4(LOADCGC) CLIENT GROUP CLIENTS                
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTCLG  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD CLIENT GROUP DEF RECORDS                                                 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCGD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D04'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTCGDC,AINITCGD,AFILTCGD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP DEF RECORDS                                               
*---------------------------------------------------------------------*         
*                                                                               
UPDTCGD  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP DEF RECORDS                                               
*---------------------------------------------------------------------*         
FILTCGD  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D04'                                              
         BNE   FCGDNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FCGDNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
FCGDNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP DEF RECORDS                                           
*---------------------------------------------------------------------*         
*                                                                               
INITCGD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTCDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD CLIENT GROUP RECORDS                                                     
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCGP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D04'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTCGPC,AINITCGP,AFILTCGP,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP RECORDS                                                   
*---------------------------------------------------------------------*         
*                                                                               
UPDTCGP  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP RECORDS                                                   
*---------------------------------------------------------------------*         
FILTCGP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D04'                                              
         BNE   FCGPNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FCGPNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JNZ   YES                                                              
*                                                                               
* GROUP ID=0, WE HAVE DEF RECORD.  READ IT AND SAVE BREAK LENGTHS               
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         LR    R6,R2               A(RECORD)                                    
         MVI   ELCODE,GRPBRKCQ     MKTGRP BREAK DESCRIPTION                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         MVC   BREAKLS(1),GRPBK1LN-GRPBRKD(R6)                                  
         MVC   BREAKLS+1(1),GRPBK2LN-GRPBRKD(R6)                                
         MVC   BUYTRKER,GRPBKBTK-GRPBRKD(R6)                                    
*                                                                               
         J     NO                                                               
*                                                                               
FCGPNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP RECORDS                                               
*---------------------------------------------------------------------*         
*                                                                               
INITCGP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTCGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD CLIENT GROUP CLIENT RECORDS                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCGC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         BRAS  RE,DOBRKTAB                                                      
         L     R1,ABRKTAB                                                       
         STCM  R1,15,ABREAKT                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D04'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTCGCC,AINITCGC,AFILTCGC,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGC02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP CLIENT RECORDS                                            
*---------------------------------------------------------------------*         
*                                                                               
UPDTCGC  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP CLIENT RECORDS                                            
*---------------------------------------------------------------------*         
FILTCGC  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D04'                                              
         BNE   FCGCNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FCGCNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
*                                                                               
FCGCNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP CLIENT RECORDS                                        
*---------------------------------------------------------------------*         
*                                                                               
INITCGC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTMADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD ALL PRODUCT GROUP RECORDS                                                
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPRG  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,PRGTAB                                                        
*                                                                               
LOPG02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOPG04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         NI    FLAGS,X'FF'-(NOMOREQ+STAFILEQ+TRFFILEQ)                          
         NI    FLAGS2,X'FF'-(CTFILEQ+NOGETQ)                                    
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOPG04   LA    R3,L'PRGTAB(R3)                                                  
         J     LOPG02                                                           
*                                                                               
PRGTAB   DS    0XL8                                                             
         DC    CL3'PGF',AL1(0),AL4(LOADPGF) PRODUCT GROUP DEF                   
         DC    CL3'PGR',AL1(0),AL4(LOADPGR) PRODUCT GROUPS                      
         DC    CL3'PGP',AL1(0),AL4(LOADPGP) PRODUCT GROUP PRODUCTS              
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTPRG  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PRODUCT GROUP DEF RECORDS                                                
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPGF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PRGKTYP(2),=X'0D01'                                              
         MVC   PRGKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPGFC,AINITPGF,AFILTPGF,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP DEF RECORDS                                              
*---------------------------------------------------------------------*         
*                                                                               
UPDTPGF  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP DEF RECORDS                                              
*---------------------------------------------------------------------*         
FILTPGF  NTR1  BASE=*,LABEL=*                                                   
         USING PRGRECD,R2                                                       
         CLC   PRGKTYP(2),=X'0D01'                                              
         BNE   FPGFNO                                                           
*                                                                               
         MVC   BYTE,PRGKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FPGFNO                                                           
*                                                                               
         OC    PRGKGRP,PRGKGRP                                                  
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
FPGFNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP DEF RECORDS                                          
*---------------------------------------------------------------------*         
*                                                                               
INITPGF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PRODUCT GROUP RECORDS                                                    
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PRGKTYP(2),=X'0D01'                                              
         MVC   PRGKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPGRC,AINITPGR,AFILTPGR,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRG02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP RECORDS                                                  
*---------------------------------------------------------------------*         
*                                                                               
UPDTPGR  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PROPUCT GROUP RECORDS                                                  
*---------------------------------------------------------------------*         
FILTPGR  NTR1  BASE=*,LABEL=*                                                   
         USING PRGRECD,R2                                                       
         CLC   PRGKTYP(2),=X'0D01'                                              
         BNE   FPGRNO                                                           
*                                                                               
         MVC   BYTE,PRGKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FPGRNO                                                           
*                                                                               
         OC    PRGKGRP,PRGKGRP                                                  
         JNZ   YES                                                              
*                                                                               
* GROUP ID=0, WE HAVE DEF RECORD.  READ IT AND SAVE BREAK LENGTHS               
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         LR    R6,R2               A(RECORD)                                    
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         MVC   BREAKLS(1),PRGBK1LN-PRGEL01(R6)                                  
         MVC   BREAKLS+1(1),PRGBK2LN-PRGEL01(R6)                                
         MVC   BREAKLS+2(1),PRGBK3LN-PRGEL01(R6)                                
*                                                                               
         J     NO                                                               
*                                                                               
FPGRNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP RECORDS                                              
*---------------------------------------------------------------------*         
*                                                                               
INITPGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PRODUCT GROUP PRODUCTS                                                   
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPGP  NTR1  BASE=*,LABEL=*                                                   
         OI    FLAGS2,NOGETQ                                                    
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         BRAS  RE,DOBRKTAB                                                      
         L     R1,ABRKTAB                                                       
         STCM  R1,15,ABREAKT                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PRGKTYP(2),=X'0D81'                                              
         MVC   PRGKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPGPC,AINITPGP,AFILTPGP,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP PRODUCTS                                                 
*---------------------------------------------------------------------*         
*                                                                               
UPDTPGP  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PROPUCT GROUP PRODUCTS                                                 
*---------------------------------------------------------------------*         
FILTPGP  NTR1  BASE=*,LABEL=*                                                   
         USING PRGRECD,R2                                                       
         CLC   PRGKTYP(2),=X'0D81'                                              
         BNE   FPGPNO                                                           
*                                                                               
         MVC   BYTE,PRGKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FPGPNO                                                           
*                                                                               
         OC    PRGKGRP,PRGKGRP                                                  
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
*                                                                               
FPGPNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP PRODUCT RECORDS                                      
*---------------------------------------------------------------------*         
*                                                                               
INITPGP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD ALL STATION GROUP RECORDS                                                
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSTG  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,STGTAB                                                        
*                                                                               
LOST02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOST04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         NI    FLAGS,X'FF'-(NOMOREQ+STAFILEQ+TRFFILEQ)                          
         NI    FLAGS2,X'FF'-(CTFILEQ+NOGETQ)                                    
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOST04   LA    R3,L'STGTAB(R3)                                                  
         J     LOST02                                                           
*                                                                               
STGTAB   DS    0XL8                                                             
         DC    CL3'SGD',AL1(0),AL4(LOADSGD) STATION GROUP DEF                   
         DC    CL3'SGP',AL1(0),AL4(LOADSGP) STATION GROUPS                      
         DC    CL3'SGS',AL1(0),AL4(LOADSGS) STATION GROUP STATIONS              
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTSTG  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION GROUP DEF RECORDS                                                
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSGD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D05'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSGD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTSGDC,AINITSGD,AFILTSGD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSGD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION GROUP DEF RECORDS                                              
*---------------------------------------------------------------------*         
*                                                                               
UPDTSGD  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION GROUP DEF RECORDS                                              
*---------------------------------------------------------------------*         
FILTSGD  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D05'                                              
         BNE   FSGDNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FSGDNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
FSGDNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION GROUP DEF RECORDS                                          
*---------------------------------------------------------------------*         
*                                                                               
INITSGD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTCDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION GROUP RECORDS                                                    
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSGP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D05'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSGP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTSGPC,AINITSGP,AFILTSGP,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSGP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION GROUP RECORDS                                                  
*---------------------------------------------------------------------*         
*                                                                               
UPDTSGP  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION GROUP RECORDS                                                  
*---------------------------------------------------------------------*         
FILTSGP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D05'                                              
         BNE   FSGPNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FSGPNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JNZ   YES                                                              
*                                                                               
* GROUP ID=0, WE HAVE DEF RECORD.  READ IT AND SAVE BREAK LENGTHS               
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         LR    R6,R2               A(RECORD)                                    
         MVI   ELCODE,GRPBRKCQ     STAGRP BREAK DESCRIPTION                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         MVC   BREAKLS(1),GRPBK1LN-GRPBRKD(R6)                                  
         MVC   BREAKLS+1(1),GRPBK2LN-GRPBRKD(R6)                                
*                                                                               
         J     NO                                                               
*                                                                               
FSGPNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION GROUP RECORDS                                              
*---------------------------------------------------------------------*         
*                                                                               
INITSGP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION GROUP STATION RECORDS                                            
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSGS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         BRAS  RE,DOBRKTAB                                                      
         L     R1,ABRKTAB                                                       
         STCM  R1,15,ABREAKT                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPKTYP(2),=X'0D05'                                              
         MVC   GRPKAGMD,SXDTAGB                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSGS02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTSGSC,AINITSGS,AFILTSGS,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSGS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION GROUP STATION RECORDS                                          
*---------------------------------------------------------------------*         
*                                                                               
UPDTSGS  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STATION GROUP STATION RECORDS                                          
*---------------------------------------------------------------------*         
FILTSGS  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKTYP(2),=X'0D05'                                              
         BNE   FSGSNO                                                           
*                                                                               
         MVC   BYTE,GRPKAGMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FSGSNO                                                           
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
*                                                                               
FSGSNO   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STATION GROUP STATION RECORDS                                      
*---------------------------------------------------------------------*         
*                                                                               
INITSGS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTSSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* DEAL RECORDS                                                                  
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DEAL RECORDS                                                             
*---------------------------------------------------------------------*         
*                                                                               
LOADDEA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
LDEA01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING SDLKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   SDLKTYPE,SDLKTYPQ                                                
         MVI   SDLKSTYP,SDLKSTYQ                                                
         MVC   SDLKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDEA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTDEAC,AINITDEA,AFILTDEA,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE DEAL RECORD DATA                                                       
*---------------------------------------------------------------------*         
*                                                                               
UPDTDEA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING SDLKEY,R2                                                        
*                                                                               
         GOTO1 AFILTDEA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITDEA                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTDEAC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DEAL RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTDEA  NTR1  BASE=*,LABEL=*                                                   
         USING SDLKEY,R2                                                        
         CLI   SDLKTYPE,SDLKTYPQ                                                
         BNE   FILTDEAN                                                         
         CLI   SDLKSTYP,SDLKSTYQ                                                
         BNE   FILTDEAN                                                         
*                                                                               
         MVC   BYTE,SDLKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JE    YES                                                              
*                                                                               
FILTDEAN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE DEAL RECORD                                                        
*---------------------------------------------------------------------*         
INITDEA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DEAL STATIONS                                                                 
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DEAL STATIONS                                                            
*---------------------------------------------------------------------*         
*                                                                               
LOADDST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
LDST01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING SDLKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   SDLKTYPE,SDLKTYPQ                                                
         MVI   SDLKSTYP,SDLKSTYQ                                                
         MVC   SDLKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTDSTC,AINITDST,AFILTDST,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDST02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE DEAL STATION DATA                                                      
*---------------------------------------------------------------------*         
*                                                                               
UPDTDST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING SDLKEY,R2                                                        
*                                                                               
         GOTO1 AFILTDST                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITDST                                                         
*                                                                               
         CLI   RRECTY,X'03' ACTION ADD?                                         
         BE    *+8                 YES - DON'T GENERATE ACTION KILL             
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDDST10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTDSTC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDDST10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DEAL STATIONS AT R2                                                    
*---------------------------------------------------------------------*         
FILTDST  NTR1  BASE=*,LABEL=*                                                   
         USING SDLKEY,R2                                                        
         CLI   SDLKTYPE,SDLKTYPQ                                                
         BNE   FILTDSTN                                                         
         CLI   SDLKSTYP,SDLKSTYQ                                                
         BNE   FILTDSTN                                                         
*                                                                               
         MVC   BYTE,SDLKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JE    YES                                                              
*                                                                               
FILTDSTN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE DEAL STATIONS                                                      
*---------------------------------------------------------------------*         
INITDST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DAYPART MENU RECORDS                                                          
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD DAYPART MENU RECORDS                                                     
*---------------------------------------------------------------------*         
LOADDPM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
LDPM01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DPTHDR,R2                                                        
         XC    DPTKEY,DPTKEY                                                    
         MVI   DPTKTYPE,X'08'                                                   
         MVC   DPTKAGY,SXDTAGY                                                  
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDPM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTDPMC,AINITDPM,AFILTDPM,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDPM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE DAYPART MENU RECORDS                                                   
*---------------------------------------------------------------------*         
*                                                                               
UPDTDPM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTDPM                                                         
         JNE   YES                                                              
         GOTO1 AINITDPM                                                         
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
*                                                                               
UPDTDP10 DS    0H                                                               
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTDPMC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTDP10                                                         
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER DAYPART MENU RECORDS                                         *         
*---------------------------------------------------------------------*         
*                                                                               
FILTDPM  NTR1  BASE=*,LABEL=*                                                   
         USING DPTHDR,R2                                                        
*                                                                               
         CLI   DPTKTYPE,X'08'                                                   
         JNE   NO                                                               
         CLC   DPTKAGY,=X'0000'                                                 
         JE    NO                                                               
         CLC   DPTKAGY,SXDTAGY                                                  
         BE    *+12                                                             
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         CLI   DPTKMENU,00                                                      
         JE    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE DAYPART MENU RECORDS                                               
*---------------------------------------------------------------------*         
*                                                                               
INITDPM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDMDL          R1=L'DPM RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DEMO CODES AND NAMES                                                          
***********************************************************************         
*                                                                               
*&&DO                                                                           
*---------------------------------------------------------------------*         
* LOAD DEMO CODES AND NAMES                                                     
*---------------------------------------------------------------------*         
LOADDEM  NTR1  BASE=*,LABEL=*                                                   
         OI    FLAGS2,NOGETQ                                                    
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   ELCOUNT,X'01'                                                    
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
LDEM02   DS    0H                                                               
         GOTO1 AACCLOAD,DMCB,VSPTDEMC,AINITDEM,AFILTDEM,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*&&                                                                             
*                                                                               
*                                                                               
*                                                                               
LOADDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LDEMTAB                                                       
*                                                                               
LDEM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         MVI   FLAGS,0                                                          
         MVI   FLAGS2,0                                                         
*                                                                               
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
         LA    R3,L'LDEMTAB(R3)                                                 
         J     LDEM02                                                           
*                                                                               
LDEMTAB  DS    0XL8                                                             
         DC    CL3'DMT',AL1(0),AL4(LOADDEMT) TRADITIONAL                        
         DC    CL3'DMC',AL1(0),AL4(LOADDEMC) NON-TRADITIONAL                    
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE DEMO CODES AND NAMES                                               
*---------------------------------------------------------------------*         
*                                                                               
INITDEMT DS    0H                                                               
INITDEMC DS    0H                                                               
INITDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDEDL          R1=L'DEMO RECORD (LONGEST)                   
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
UPDTDEM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTDMC                                                         
         JNE   YES                                                              
         GOTO1 AINITDEM                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTDMCC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD TRADITIONAL DEMO CODES AND NAMES                                         
*---------------------------------------------------------------------*         
LOADDEMT NTR1  BASE=*,LABEL=*                                                   
         OI    FLAGS2,NOGETQ                                                    
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   ELCOUNT,X'01'                                                    
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
LDEMT02  DS    0H                                                               
         GOTO1 AACCLOAD,DMCB,VSPTDMTC,AINITDMT,AFILTDMT,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEMT02                                                          
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
FILTDEMT SR    RF,RF               RETURN CC EQUAL                              
         BR    RE                                                               
*                                                                               
UPDTDEMT SR    RF,RF               RETURN CC EQUAL                              
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD NON-TRADITIONAL DEMO CODES AND NAMES                                     
*---------------------------------------------------------------------*         
LOADDEMC NTR1  BASE=*,LABEL=*                                                   
         OI    FLAGS2,NOGETQ                                                    
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
LDMC01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NTSQKEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   NTSQKTYP,NTSQKTYPQ  0D                                           
         MVI   NTSQKSUB,NTSQKSUBQ  2A                                           
         MVI   NTSQRTGSV,C'C'                                                   
         MVC   NTSQKAGMD,SXDTAGB   MOVE AGY CODE (LEFT ALIGNED)                 
         OI    NTSQKAGMD,X'01'     SET MEDIA=TV                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDMC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTDMCC,AINITDMC,AFILTDMC,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDMC02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
FILTDEMC NTR1  BASE=*,LABEL=*      RETURN CC EQUAL                              
         USING NTSQKEY,R2                                                       
*                                                                               
         CLI   NTSQKTYP,NTSQKTYPQ  0D                                           
         JNE   FILTDMC7                                                         
         CLI   NTSQKSUB,NTSQKSUBQ  2A                                           
         JNE   FILTDMC7                                                         
         CLI   NTSQRTGSV,C'C'                                                   
         JNE   FILTDMC7                                                         
*                                                                               
         MVC   BYTE,NTSQKAGMD                                                   
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   FILTDMC7                                                         
*                                                                               
         J     YES                                                              
*                                                                               
FILTDMC7 OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
*                                                                               
*                                                                               
UPDTDEMC SR    RF,RF               RETURN CC EQUAL                              
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ADBUYER RECORDS                                                               
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD ADBUYER RECORDS                                                          
*---------------------------------------------------------------------*         
*                                                                               
LOADADB  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING BYRREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LADB02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTADBC,AINITADB,AFILTADB,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LADB02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE ADBUYER RECORDS                                                        
*---------------------------------------------------------------------*         
*                                                                               
UPDTADB  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING BYRREC,R2                                                        
*                                                                               
         GOTO1 AFILTADB                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITADB                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTADBC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* FILTER ADBUYER RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTADB  NTR1  BASE=*,LABEL=*                                                   
         USING BYRREC,R2                                                        
*                                                                               
         CLI   BYRKTYP,BYRKTYPQ                                                 
         BNE   FADB90                                                           
         CLI   BYRKSUB,BYRKSUBQ                                                 
         BNE   FADB90                                                           
*                                                                               
         MVC   BYTE,BYRKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FADB90                                                           
*                                                                               
         CLC   6(7,R2),=7X'00'    BUYERS RECORDS ONLY                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
FADB90   DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE ADBUYER RECORD                                                     
*---------------------------------------------------------------------*         
INITADB  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTABDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* USED TO BE SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE       *         
* NOW ITS SUBROUTINE FOR ALL RECORDS THAT HAVE (1-1) DIFFICULTY LEVEL *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(OF MEDIA TABLE WHICH CONTANE LOTS OF USEFULL STUFF) ALWAYS R8*         
* ALSO PRIPFLG (GLOBAL) IS A SWICH BETWEEN DIFFERENT RECORDS          *         
* REPMED IS LOCAL STORAGE                                             *         
***********************************************************************         
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         L     R8,12(R1)                                                        
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
*                                                                               
         NI    MISCFLG,X'FF'-MFXPROCQ TURN OFF EXTRA PROCESSING                 
*                                                                               
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   DS    0H                                                               
         TM    FLAGS2,NOGETQ                                                    
         BO    ALOA03                                                           
         TM    FLAGS,STAFILEQ                                                   
         BO    ALOA03              WHOLE RECORD IS IN DIR, SKIP GETREC          
         TM    FLAGS2,CTFILEQ                                                   
         BO    ALOA03              WHOLE RECORD IS IN DIR, SKIP GETREC          
*                                                                               
         TM    FLAGS,NORDSEQ                                                    
         BO    ALOA03                                                           
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA03   DS    0H                                                               
         BRAS  RE,CLTFILT                                                       
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                                                               
* NO CLIENT CODE IN KEY, DON'T GET CLIENT PROFILE                               
*                                                                               
         CLC   TYPECODE,=CL3'ADB'                                               
         BE    ALOA04                                                           
         CLC   TYPECODE,=CL3'DEM'                                               
         BE    ALOA04                                                           
*                                                                               
         MVI   CPROF7,X'00'                                                     
         BRAS  RE,GETCPROF                                                      
         BNE   ALOA06              PROBLEM WITH CLT RECORD - SKIP               
         MVC   CPROF7,BYTE                                                      
*                                                                               
* CALL RECORD EXTRACT ROUTINE                                                   
*                                                                               
ALOA04   DS    0H                                                               
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8)                               
         BNE   ALOA06                                                           
*                                                                               
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,X'FF'                                                     
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
*                                                                               
         TM    MISCFLG,MFXPROCQ    EXTRA PROCESSING?                            
         BZ    *+8                                                              
         BRAS  RE,XTRAPROC                                                      
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
         TM    FLAGS2,NOWRITEQ     SKIP WRITE THIS TIME?                        
         BZ    *+12                                                             
         NI    FLAGS2,X'FF'-NOWRITEQ                                            
         J     ALOA06                                                           
*                                                                               
         CLI   SXDTPLFM,0          PLATFORM SET?                                
         JNE   ALOA05    NO: WRITE UNCONVERTED RECORD (NO SPXCNVX CALL)         
         GOTO1 DXPUT,DMCB,DXAXREC,(R7)                                          
         J     ALOA06                                                           
*                                  CONVERT RECORD TO SQL BUFFER                 
ALOA05   DS    0H                                                               
         GOTO1 VSPXCNVX,DMCB,(R7)                                               
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                  PUT CONVERTED RECORD TO FILE                 
*                                                                               
         TM    MISCFLG,MFLDUPDQ+MFDELADQ LOAD IN UPDATE MODE?                   
         BZ    ALOA05A             NO - PROCEED AS USUAL                        
*                                                                               
* LOADING RECORDS IN UPDATE MODE HERE                                           
* NEED TO GENERATE DELETE-ADD PAIRS FOR EACH                                    
*                                                                               
         L     RF,DXASQLB                                                       
         USING SPTGRD,RF           GENERIC RECORD                               
         MVI   SPTGRACT,C'D'       ACTION=DELETE                                
         DROP  RF                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         L     RF,DXASQLB                                                       
         USING SPTGRD,RF           GENERIC RECORD                               
         MVI   SPTGRACT,C'A'       ACTION=ADD                                   
         DROP  RF                                                               
*                                                                               
ALOA05A  DS    0H                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
ALOA06   DS    0H                                                               
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         TM    FLAGS,STAFILEQ                                                   
         BNO   *+14                                                             
         MVC   IOKEY(15),0(R2)     L(STAFILE KEY)                               
         B     ALOA10                                                           
*                                                                               
         TM    FLAGS2,CTFILEQ                                                   
         BNO   *+14                                                             
         MVC   IOKEY(25),0(R2)     L(CTFILE KEY)                                
         B     ALOA10                                                           
*                                                                               
         TM    FLAGS2,XSPFILEQ                                                  
         BNO   *+14                                                             
         MVC   IOKEY(32),0(R2)     L(XSPFILE KEY)                               
         B     ALOA10                                                           
*                                                                               
         MVC   IOKEY(13),0(R2)     L(SPOTDIR,TRFDIR KEY)                        
*                                                                               
ALOA10   DS    0H                                                               
         TM    FLAGS,NORDSEQ                                                    
         JO    YES                                                              
*                                                                               
ALOA15   DS    0H                                                               
         MVC   VARDIR,SPTDIR                                                    
         TM    FLAGS,STAFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,STAFIL                                                    
         TM    FLAGS,TRFFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,TRFDIR                                                    
         TM    FLAGS2,CTFILEQ                                                   
         BNO   *+10                                                             
         MVC   VARDIR,CTFILE                                                    
         TM    FLAGS2,XSPFILEQ                                                  
         BNO   *+10                                                             
         MVC   VARDIR,XSPDIR                                                    
*                                                                               
* ONLY FOR PATTERNS.  SKIP READ TO NEXT RECORD                                  
         CLC   =X'0A22',IOKEY                                                   
         BNE   ALOA17                                                           
*                                                                               
         ICM   R1,7,IOKEY+PATKREF-PATKEY                                        
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         SH    R1,=H'1'            BUILD NEXT KEY                               
         BP    ALOA16A             IF STILL NOT ZERO, OK                        
         LLC   R1,IOKEY+PATKCODE-PATKEY                                         
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,IOKEY+PATKCODE-PATKEY                                         
         CLI   IOKEY+PATKCODE-PATKEY,0 IF OVER 255, BUMP SPOT LEN               
         BNE   ALOA16                                                           
         LLC   R1,IOKEY+PATKSLN2-PATKEY BUMP SPOT LEN2                          
         LA    R1,1(,R1)                     BY 1                               
         STC   R1,IOKEY+PATKSLN2-PATKEY                                         
ALOA16   SR    R1,R1               SET BREF                                     
         B     *+8                         ZERO AND LEAVE IT ZERO               
ALOA16A  X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,IOKEY+PATKREF-PATKEY                                        
         GOTO1 VDATAMGR,DMCB,DMRDHI,VARDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
ALOA17   DS    0H                                                               
         TM    FLAGS2,BUMPQ                                                     
         BZ    ALOA20                                                           
         NI    FLAGS2,X'FF'-BUMPQ                                               
*                                                                               
         CLC   TYPECODE,=C'SGP'                                                 
         BE    ALOA18                                                           
         CLC   TYPECODE,=C'CGP'                                                 
         BNE   ALOA20                                                           
*                                                                               
ALOA18   DS    0H                                                               
         MVC   IOKEY+GRPKELCD-GRPKEY(7),=7X'FF'                                 
         GOTO1 VDATAMGR,DMCB,DMRDHI,VARDIR,IOKEY,(R2),DMWORK                    
*                                                                               
ALOA20   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,VARDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
REPMED   DC    C' '           STORAGE FOR ONE BYTE MEDIA                        
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
* P4 = A(OF MEDIA TABLE WHICH CONTANE LOTS OF USEFULL STUFF) ALWAYS R8*         
***********************************************************************         
*                                                                               
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         L     R8,12(R1)                                                        
*                                                                               
         NI    FLAGS,X'FF'-UPDWRTQ TURN OFF "UPDATE WRITTEN" FLAG               
*                                                                               
UPDL01   DS    0H                                                               
         MVI   CPROF7,X'00'                                                     
         BRAS  RE,GETCPROF                                                      
         JNE   YES                 PROBLEM WITH CLT RECORD - SKIP               
         MVC   CPROF7,BYTE                                                      
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8)   EXTRACT                     
         JNE   YES                                                              
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    YES                                                              
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         TM    FLAGS2,NOWRITEQ     SKIP WRITE THIS TIME?                        
         BZ    *+12                                                             
         NI    FLAGS2,X'FF'-NOWRITEQ                                            
         J     YES                                                              
*                                                                               
         L     RF,DXAXREC                                                       
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UPDL04              DO NOT CONVERT RECORD                        
*                                                                               
* MULTIPLE LINES ARE EXTRACTED FOR THESE RECORDS,                               
* SO, DON'T COMPARE COPY, CHANGE                                                
*                                                                               
UPDL01A  DS    0H                                                               
         USING SPTGRD,RF                                                        
*                                                                               
         CLC   SPDMQ,SPTGRTYP      DAYPART MENU RECORDS?                        
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPEDQ,SPTGRTYP      ESTIMATE DEMO LIST?                          
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPELQ,SPTGRTYP      ESTIMATE AUTHORIZED DOLLARS?                 
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPSLQ,SPTGRTYP      STA LOCKIN RECORD?                           
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPNLQ,SPTGRTYP      SYSCODE/NETWORK RECORD?                      
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPUCQ,SPTGRTYP      UCOM RECORD?                                 
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPEFQ,SPTGRTYP      EQUIV FACTOR?                                
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPDSQ,SPTGRTYP      DEAL STATIONS?                               
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPMDQ,SPTGRTYP      MEDIA?                                       
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
         CLC   SPBHQ,SPTGRTYP      BILL HEADER?                                 
         JE    UPDL02              SKIP: SOME BH RECS DON'T HAVE COPIES         
         CLC   SPPLQ,SPTGRTYP      PRODUCT LIST?                                
         JE    UPDL02                                                           
         CLC   SPFFQ,SPTGRTYP      FLIGHT RECORD FLIGHTS?                       
         JE    UPDL02                                                           
         CLC   SPPCQ,SPTGRTYP      PATTERN COMMERCIALS?                         
         JE    UPDL02                                                           
         CLC   SPPMQ,SPTGRTYP      PATTERN MARKETS?                             
         JE    UPDL02                                                           
         CLC   SPPSQ,SPTGRTYP      PATTERN STATIONS?                            
         JE    UPDL02                                                           
         CLC   SPPKQ,SPTGRTYP      PATTERN MARKET GROUPS?                       
         JE    UPDL02                                                           
         CLC   SPPAQ,SPTGRTYP      PATTERN AFFILIATES?                          
         JE    UPDL02                                                           
         CLC   SPPTQ,SPTGRTYP      PATTERN STATION TYPES?                       
         JE    UPDL02                                                           
         CLC   SPPNQ,SPTGRTYP      PATTERN COMMENTS?                            
         JE    UPDL02                                                           
*                                                                               
* BUY EXTRACT REQUEST IS GENERATED FROM DAO/DMN RECORDS                         
* THERE IS NO COPY FOR IT                                                       
         CLC   SPXQQ,SPTGRTYP      BUY RECORD EXTRACT REQUEST?                  
         JE    UPDL02              SKIP SPECIAL CODE FOR CHANGES                
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL02                                                           
*                                                                               
         CLC   SPSTQ,SPTGRTYP      STATION?                                     
         BNE   *+12                                                             
         CLI   SPTGRACT,C'D'                                                    
         BE    UPDL02                                                           
*                                                                               
         DROP  RF                  SPTGRD,RF                                    
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(L'COPYBUFF)                                                
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6),(R8)    BUILD COPY REC            
*                                                                               
* THE ABOVE CALL TO EXTRACT THE RECORD COPY CAN ALSO SET NOWRITEQ               
* MAKE SURE IT IS TURNED OFF, SINCE AT THIS POINT WE CARE ABOUT                 
* THE CHANGE, AND NOT THE COPY                                                  
*                                                                               
         NI    FLAGS2,X'FF'-NOWRITEQ                                            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(SPTGRAGY-SPTGRD) DISP TO AGENCY ALPHA                    
         AR    R0,R1               BUMP TO PRIALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               COPY, CHANGE RECORDS IDENTICAL?              
         JE    YES                 YES - SKIP THEM                              
*                                                                               
UPDL02   DS    0H                                                               
         GOTO1 VSPXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
         USING SPTGRD,RF                                                        
*                                                                               
         CLC   SPBHQ,SPTGRTYP      BILL HEADER?                                 
         BE    UPDL04                                                           
*                                                                               
         CLC   SPUCQ,SPTGRTYP      UCOM RECORD?                                 
         BNE   *+12                                                             
         CLI   SPTGRACT,C'A'       ACTION ADD?                                  
         BE    UPDL02B             GENERATE "DELETE" FOR EVERY ADD              
*                                                                               
         CLC   SPESQ,SPTGRTYP      ESTIMATE RECORD?                             
         BNE   *+12                                                             
         CLI   SPTGRACT,C'A'       ACTION ADD?                                  
         BE    UPDL02B             GENERATE "DELETE" FOR EVERY ADD              
* SPEC-42146                                                                    
* BILL FORMULA CHANGE ON PRODUCT AAA TRIGGERS UPDATES TO PRODUCTS               
* AND ESTIMATES.  IF A PRODUCT HAS BEEN ADDED *AFTER* BILL FORMULA              
* CHANGE, THIS WILL CAUSE DUPLICATE KEYS.                                       
* AS A RESULT, WE'LL ALWAYS GENERATE DELETE-ADD PAIRS FOR PRD, EST              
         CLC   SPPDQ,SPTGRTYP      PRODUCT RECORD?                              
         BNE   *+12                                                             
         CLI   SPTGRACT,C'A'       ACTION ADD?                                  
         BE    UPDL02A             GENERATE "DELETE" FOR EVERY ADD              
*                                                                               
         CLI   SPTGRACT,C'C'                                                    
         BNE   UPDL04                                                           
*                                                                               
* FOR DARE BLOCKCHAIN RECORDS:                                                  
* REMOVE SEMICOLON SEPARATING RECORD DATE AND TIME FIELDS                       
* DARE ORDER RECORD FORMAT HAS TO MATCH THE BUY EXTRACT,                        
* AND BUY RECORDS DO NOT SEPARATE DATE AND TIME                                 
* DON'T GENERATE ADD-DELETE PAIRS                                               
*                                                                               
         CLC   SPDOQ,SPTGRTYP      DARE ORDER?                                  
         BE    UPDL02A                                                          
         CLC   SPMNQ,SPTGRTYP      DARE MAKEGOOD NOTICE?                        
         BE    UPDL02A                                                          
         CLC   SPXQQ,SPTGRTYP      BUY RECORD EXTRACT REQUEST?                  
         BNE   UPDL02B                                                          
*                                                                               
UPDL02A  DS    0H                                                               
         L     RF,DXASQLB                                                       
         MVI   SPTGRDTS,C' '       REMOVE SEMICOLON                             
         B     UPDL04                                                           
*                                                                               
UPDL02B  DS    0H                                                               
         MVI   SPTGRACT,C'D'                                                    
         GOTO1 DXPUT,DMCB,(RF),(R7),(R8)                                        
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
*                                                                               
         L     RF,DXASQLB                                                       
         MVI   SPTGRACT,C'A'                                                    
         DROP  RF                  SPTGRD,RF                                    
*                                                                               
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7),(R8)                                        
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
*                                                                               
         OI    FLAGS,UPDWRTQ       INDICATE UPDATE WRITTEN TO OUTPUT            
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
       ++INCLUDE SPXRECID                                                       
*                                                                               
*                                                                               
***********************************************************************         
* GETCPROF                                                            *         
* OBTAINS CLIENT RECORD AND SAVES CPROF+6(DISPLAY CLT CODE AS AAN)              
* IN BYTE                                                                       
* ON ENTRY R2 EXPECTED TO HAVE THE RECORD                                       
* UNEQUAL CONDITION IF CPROF CAN'T BE OBTAINED.  EQUAL OTHERWISE                
*                                                                               
* !!! IMPORTANT REGISTER USAGE INFO:                                            
*     R3 = SAVED KEY LENGTH, DO NOT USE!!!                                      
***********************************************************************         
GETCPROF NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,X'00'                                                       
         MVI   DMINBITS,X'00'                                                   
         OI    DMINBITS,X'08'      ALWAYS PASS BACK DELETES 5/20/2015           
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL1(??),CL2(AGY),CL1(MED),CL3(CLT)                                            
         CLC   TYPECODE,=CL3'CNT'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'PRD'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'PDL'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'EST'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'EDL'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'EAD'                                               
         BE    GETCP10                                                          
         CLC   TYPECODE,=CL3'BIL'                                               
         BE    GETCP10                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL1(??),CL2(AGY),CL1(MED),CL3(CLT)                                            
         CLC   TYPECODE,=CL3'EQF'                                               
         BE    GETCP20                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL2(??),XL1(A/M),XL2(CLT)                                                     
         CLC   TYPECODE,=CL3'CML'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'PGF'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'PGR'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'PGP'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'MGD'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'MGR'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'FLT'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'FLF'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'BFM'                                               
         BE    GETCP30                                                          
         CLC   TYPECODE,=CL3'PPH'                                               
         BE    GETCP30                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL8(??),XL1(A/M),XL2(CLT)                                                     
         CLC   TYPECODE,=CL3'MGA'                                               
         BE    GETCP50                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL2(??),XL1(A/M),XL1(??),XL2(CLT)                                             
         CLC   TYPECODE,=CL3'UCM'                                               
         BE    GETCP60                                                          
*                                                                               
* KEY LENGTH = 32                                                               
* XL17(??),XL1(A/M),XL2(CLT)                                                    
         CLC   TYPECODE,=CL3'SLK'                                               
         BE    GETCP70                                                          
*                                                                               
         B     GETCPQX                                                          
*                                                                               
* KEY LENGTH = 13                                                               
* XL1(??),XL1(A/M),XL2(CLT)                                                     
*                                                                               
GETCP10  DS    0H                                                               
         LHI   R3,13               KEY LENGTH                                   
         MVC   KEY(4),0(R2)                                                     
         B     GETCP100                                                         
*                                                                               
* KEY LENGTH = 13                                                               
* XL1(??),CL2(AGY),CL1(MED),CL3(CLT)                                            
*                                                                               
GETCP20  DS    0H                                                               
         OC    4(2,R2),4(R2)       DO WE HAVE CLIENT CODE?                      
         BZ    GETCPQX             NO - DON'T BOTHER WITH PROFILE               
*                                                                               
         LHI   R3,13               KEY LENGTH                                   
*                                                                               
         LA    R1,3(R2)            MEDIA CODE                                   
         ICM   R1,8,=C'C'                                                       
         BRAS  RE,GETMED                                                        
         BE    *+14                                                             
         XC    SVCPROF7,SVCPROF7                                                
         B     GETCPNQX                                                         
*                                                                               
         MVC   KEY+1(1),SXDTAGB    AGENCY NIBBLE                                
         OC    KEY+1(1),BYTE       MEDIA NIBBLE                                 
*                                                                               
* BYTE IS THE OUTPUT AREA FOR CPROF7 - RESTORE IT TO ZERO!!!                    
         MVI   BYTE,X'00'          RESTORE BYTE TO 0                            
*                                                                               
         MVC   KEY+2(2),4(R2)      CLT                                          
         B     GETCP100                                                         
*                                                                               
* KEY LENGTH = 13                                                               
* XL2(??),XL1(A/M),XL2(CLT)                                                     
*                                                                               
GETCP30  DS    0H                                                               
         OC    3(2,R2),3(R2)       DO WE HAVE CLIENT CODE?                      
         BZ    GETCPQX             NO - DON'T BOTHER WITH PROFILE               
*                                                                               
         LHI   R3,13               KEY LENGTH                                   
         MVC   KEY+1(3),2(R2)                                                   
         B     GETCP100                                                         
*                                                                               
* KEY LENGTH = 13                                                               
* XL8(??),XL1(A/M),XL2(CLT)                                                     
*                                                                               
GETCP50  DS    0H                                                               
         OC    9(2,R2),3(R2)       HAVE CLIENT?                                 
         BZ    GETCPQX             NO                                           
*                                                                               
         LHI   R3,13               KEY LENGTH                                   
         MVC   KEY+1(3),8(R2)                                                   
         B     GETCP100                                                         
*                                                                               
* KEY LENGTH = 13                                                               
* XL2(??),XL1(A/M),XL1(??),XL2(CLT)                                             
*                                                                               
GETCP60  DS    0H                                                               
         LHI   R3,13               KEY LENGTH                                   
         MVC   KEY+1(1),2(R2)                                                   
         MVC   KEY+2(2),4(R2)                                                   
         B     GETCP100                                                         
*                                                                               
* KEY LENGTH = 32                                                               
* XL17(??),XL1(A/M),XL2(CLT)                                                    
*                                                                               
GETCP70  DS    0H                                                               
         LHI   R3,32               KEY LENGTH                                   
         MVC   KEY+1(3),17(R2)                                                  
         B     GETCP100                                                         
*                                                                               
* CLIENT RECORD KEY BUILT HERE, READ THE RECORD                                 
*                                                                               
GETCP100 DS    0H                                                               
         CLC   SVCPROF7(L'SVCPROF7-1),1(R2) SAME A/M,CLT AS BEFORE?             
         BNE   GETCP105            NO - READ THE CLIENT RECORD                  
*                                                                               
* A/M,CLT ARE SAME                                                              
         CLI   SVCPROF7+3,X'FF'    IS THE CLT RECORD BAD?                       
         BE    GETCPNQX            YES - SKIP THIS RECORD                       
         B     GETCPQX             NO - USE SAVED CPROF DATA                    
*                                                                               
GETCP105 DS    0H                                                               
* SAVE A/M,CLT IMMEDIATELY, IN CASE CLT IS NOT FOUND                            
         MVC   SVCPROF7(3),1(R6)   A/M,CLT                                      
         MVI   SVCPROF7+3,X'FF'    IN CASE CLT REC NOT FOUND                    
*                                                                               
* SAVE RECORD KEY, TO RESTORE READ SEQUENCE LATER                               
         XC    IOKEYSAV,IOKEYSAV                                                
         BCTR  R3,0                R3 = KEY LENGTH                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   IOKEYSAV(0),0(R2)                                                
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(L'CKEY),KEY                                              
*                                                                               
* READ THE CLIENT RECORD                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBITS,DMRDHI),SPTDIR,KEY,KEY,DMWORK            
         CLI   8(R1),0                                                          
         BE    GETCP110            NO ERRORS, ALL OK                            
*                                                                               
* IF READING FOR DELETES - CHECK FOR DMGR'S RETURN X'02'                        
         CLI   8(R1),X'02'         RETURN ERROR =RECORD DELETED?                
         BNE   *+12                NO - SOMETHING IS WRONG                      
         CLI   DMINBITS,X'08'      ARE WE READING FOR DELETES?                  
         BNE   GETCP200            RESTORE READ SEQUENCE                        
*                                                                               
GETCP110 DS    0H                                                               
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GETCP200            RESTORE READ SEQUENCE                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBITS,GETREC),SPTFIL,KEY+14,IO,DMWORK          
         CLI   8(R1),0                                                          
         BE    GETCP120                                                         
*                                                                               
* IF READING FOR DELETES - CHECK FOR DMGR'S RETURN X'02'                        
         CLI   8(R1),X'02'         RETURN ERROR =RECORD DELETED?                
         BNE   *+12                NO - SOMETHING IS WRONG                      
         CLI   DMINBITS,X'08'      ARE WE READING FOR DELETES?                  
         BNE   GETCP200            RESTORE READ SEQUENCE                        
*                                                                               
GETCP120 DS    0H                                                               
         LA    R6,IO                                                            
         MVC   SVCPROF7+3(1),(CPROF+6-CLTHDR)(R6)                               
*                                                                               
* FOR THESE RECORDS SAVE CLIST IN BRKTAB                                        
*                                                                               
         CLC   TYPECODE,=CL3'SLK'                                               
         BE    GETCP130                                                         
         CLC   TYPECODE,=CL3'CML'                                               
         BE    GETCP130                                                         
         CLC   TYPECODE,=CL3'PPH'                                               
         BE    GETCP130                                                         
*                                                                               
         B     GETCP200                                                         
*                                                                               
GETCP130 DS    0H                                                               
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
* SAVE CLIST                                                                    
         L     R0,ABRKTAB                                                       
         LHI   R1,880              L'CLIST                                      
         LA    RE,(CLIST-CLTHDR)(R6) A(CLIST)                                   
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
* SAVE CLIST2                                                                   
         L     R0,ABRKTAB                                                       
         AHI   R0,880              ADVANCE PAST SAVED CLIST                     
         LHI   R1,140              L'CLIST2                                     
         LA    RE,(CLIST2-CLTHDR)(R6) A(CLIST2)                                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
* RESTORE READ SEQUENCE                                                         
*                                                                               
GETCP200 DS    0H                                                               
         CLI   DXMODE,DXUPDTQ      MODE = UPDATE?                               
         BNE   *+12                NO                                           
         TM    MISCFLG,MFLDUPDQ    LOADING IN UPDATE MODE?                      
         BZ    GETCP220            NO - NO NEED TO RESTORE SEQUENCE             
*                                                                               
         XC    KEY,KEY                                                          
         EX    R3,*+8              R3 BCTR'D WHEN SAVING KEY                    
         B     *+10                                                             
         MVC   KEY(0),IOKEYSAV                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),SVDIR,KEY,KEY,DMWORK                
         CLI   8(R1),0                                                          
         BNE   GETCPNQX            SEQ RESTORE FAILED                           
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),IOKEYSAV                                                  
         BNE   GETCPNQX            SEQ RESTORE FAILED                           
*                                                                               
* WE COULD BE RESTORING THE REQD SEQUENCE AFTER READHI ERROR                    
* CHECK WHAT KIND OF EXIT CONDITION CODE TO SET - EQ OR NEQ                     
*                                                                               
GETCP220 DS    0H                                                               
         CLI   SVCPROF7+3,X'FF'                                                 
         BE    GETCPNQX                                                         
*                                                                               
GETCPQX  DS    0H                                                               
         MVC   BYTE,SVCPROF7+3                                                  
         J     YES                                                              
*                                                                               
GETCPNQX DS    0H                                                               
         MVI   BYTE,X'00'                                                       
         MVI   SVCPROF7+3,X'FF'                                                 
         J     NO                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
* R1 EXPECTED TO ADDRESS EITHER A/M BYTE OR MEDIA CODE                          
* R1 HIGH-ORDER BYTE MUST HAVE INPUT TYPE C=CODE A=A/M                          
* ON EXIT, BYTE CONTAINS MEDIA CODE OR MEDIA HALFWORD FOR A/M                   
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
GETMED   NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,0(R1)          INPUT                                        
*                                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   *+8                 NO                                           
         NI    BYTE,X'0F'          YES - TURN OFF AGENCY BITS                   
*                                                                               
         LA    R5,GMTAB                                                         
*                                                                               
GM10     DS    0H                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   GM15                NO                                           
*                                  YES                                          
         CLC   BYTE,1(R5)          COMPARE ON A/M BYTE COLUMN                   
         BE    GM20                                                             
         B     GM18                                                             
*                                                                               
GM15     CLC   BYTE,0(R5)          COMPARE ON MEDIA LETTER COLUMN               
         BE    GM20                                                             
*                                                                               
GM18     LA    R5,GMTABLQ(R5)      ADVANCE TO NEXT LINE IN THE TABLE            
         CLI   0(R5),X'FF'                                                      
         BNE   GM10                                                             
*                                                                               
         MVI   BYTE,X'00'                                                       
         J     NO                                                               
*                                                                               
GM20     DS    0H                                                               
         MVC   BYTE,0(R5)          COPY MEDIA CODE                              
         CLM   R1,8,=C'C'          INPUT = A/M BYTE?                            
         BNE   *+10                NO                                           
         MVC   BYTE,1(R5)          COPY A/M HALFWORD                            
*                                                                               
         J     YES                                                              
*                                                                               
GMTAB    DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
GMTABLQ  EQU   *-GMTAB                                                          
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
PROCOPEN NTR1  BASE=*,LABEL=*      SET UTL SENUM                                
         MVC   MAXIOSW,DXMAXREC                                                 
         BRAS  RE,CHKADDRD                                                      
*                                                                               
         L     RE,VUTL                                                          
         MVI   4(RE),X'0A'         CONTROL                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,=CL8'CONTROL',CFLIST,IO                     
*                                                                               
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
         GOTO1 VDATAMGR,DMCB,DMOPEN,=CL8'SPOT',FILELIST,IO                      
*                                                                               
         OC    AOFFCR,AOFFCR                                                    
         BNZ   PROCOP10                                                         
         MVC   DUB,=CL8'T00A38'    OFFICER                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   AOFFCR,DMCB+4                                                    
*                                                                               
PROCOP10 DS    0H                                                               
*                                                                               
PROCOPX  DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
AOFFCR   DC    A(0)                                                             
MAXIOSW  DC    AL4(0)                                                           
*                                                                               
CFLIST   DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL10'X       '                                                   
*                                                                               
FILELIST DC    CL8'NSPTFIL'                                                     
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NXSPFIL'                                                     
         DC    CL8'NXSPDIR'                                                     
         DC    CL8'NSTAFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NSTRFFL'                                                     
         DC    CL8'NSTRFDR'                                                     
         DC    CL10'X       '                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
PROCCLOS NTR1  BASE=*,LABEL=*                                                   
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,=CL8'PRINT',0,IO                            
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
*                                                                               
GENINIT  NTR1  BASE=*,LABEL=*                                                   
         MVI   MISCFLG,X'00'                                                    
*                                                                               
         XC    SVCLTPRD,SVCLTPRD                                                
*                                                                               
         L     R1,=V(UTL)                                                       
         ST    R1,VUTL                                                          
         L     RF,=A(TYPTAB)                                                    
         ST    RF,ATYPTAB                                                       
*                                                                               
         LA    R1,WORKD                                                         
         AHI   R1,BREAKTAB-WORKD                                                
         ST    R1,ABRKTAB                                                       
*                                                                               
         LA    R1,WORKD                                                         
         AHI   R1,NETTAB-WORKD                                                  
         ST    R1,ANETTAB                                                       
*                                                                               
         XC    SVCPROF7,SVCPROF7                                                
         XC    SVBKLEN,SVBKLEN                                                  
*                                                                               
         MVI   FLAGS,X'0'                                                       
         MVI   FLAGS2,X'0'                                                      
         MVI   ELCOUNT,X'0'                                                     
         XC    BREAKLS,BREAKLS                                                  
*                                                                               
         L     R1,=A(AOFFCR)                                                    
         MVC   AOFFICER,0(R1)                                                   
         MVC   AOFCR,AOFFICER                                                   
*                                                                               
         L     R4,=A(VDEMADDR)                                                  
         OC    0(4,R4),0(R4)       IS DEMADDR ALREADY LOADED?                   
         BNZ   GENINI10            YES: V(DEMADDR) ALREADY IN COMFACS           
*                                                                               
         MVC   DUB,=CL8'T00ADE'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI10 DS    0H                                                               
         L     R4,=A(CRCPK)                                                     
         OC    0(4,R4),0(R4)       IS RCPACK ALREADY LOADED?                    
         BNZ   GENINI20                                                         
*                                                                               
         MVC   DUB,=CL8'T00ABC'    RCPACK                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI20 DS    0H                                                               
         MVC   VRCPACK,0(R4)                                                    
*                                                                               
         L     R4,=A(CSTPK)                                                     
         OC    0(4,R4),0(R4)       IS STAPACK ALREADY LOADED?                   
         BNZ   GENINI30                                                         
*                                                                               
         MVC   DUB,=CL8'T00A7A'    STAPACK                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
         MVC   VSTAPACK,0(R4)                                                   
*                                                                               
* IMMEDIATELY OBTAIN ADDRESS OF CABLETAB AND PASS IT TO STAPACK                 
*                                                                               
         MVC   DUB,=CL8'T00A9E'    CABLETAB                                     
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   ACBLTAB,DMCB+4                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,QSTP_T00A9E                                              
         MVC   STAPACOM,ACBLTAB                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         DROP  R1                                                               
*                                                                               
         B     *+10                                                             
*                                                                               
GENINI30 DS    0H                                                               
         MVC   VSTAPACK,0(R4)                                                   
*                                                                               
         L     R4,=A(DEMOCON)                                                   
         OC    0(4,R4),0(R4)       IS DEMOCON ALREADY LOADED?                   
         BNZ   GENINI40                                                         
*                                                                               
         MVC   DUB,=CL8'T00AE0'    DEMOCON                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI40 DS    0H                                                               
         MVC   VDEMOCON,0(R4)                                                   
*                                                                               
         L     R2,=V(COMFACS)                                                   
         USING COMFACSD,R2                                                      
         OC    CDATCON,CDATCON                                                  
         BNZ   GENINI50                                                         
*                                                                               
         MVC   DUB,=CL8'T00AE0'    DEMOCON                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI50 DS    0H                                                               
         DROP  R2                                                               
*                                                                               
         OC    ASLNTAB,ASLNTAB                                                  
         BNZ   GENINI60                                                         
*                                                                               
         MVC   DUB,=CL8'T00A57'    SLENTAB                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   ASLNTAB,DMCB+4                                                   
*                                                                               
GENINI60 DS    0H                                                               
         L     R4,=A(UNTIME)                                                    
         OC    0(4,R4),0(R4)       IS UNTIME ALREADY LOADED?                    
         BNZ   GENINI70                                                         
*                                                                               
         MVC   DUB,=CL8'T00A11'    UNTIME                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI70 DS    0H                                                               
         MVC   AUNTIME,0(R4)                                                    
*                                                                               
         L     R4,=A(TRPACK)                                                    
         OC    0(4,R4),0(R4)       IS TRPACK ALREADY LOADED?                    
         BNZ   GENINI80                                                         
*                                                                               
         MVC   DUB,=CL8'T00AFE'    TRPACK                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINI80 DS    0H                                                               
         MVC   ATRPACK,0(R4)                                                    
*                                                                               
         MVC   APIDTAB,VPIDTAB                                                  
         CLI   SXDTCTRY,CTRYCAN                                                 
         BNE   *+10                                                             
         MVC   APIDTAB,VPIDTABC                                                 
*                                                                               
         CLI   DXMODE,DXLOADQ                                                   
         BE    *+12                                                             
         CLI   DXMODE,DXUPDTQ                                                   
         BNE   *+8                                                              
         BRAS  RE,READACC                                                       
*                                                                               
         GOTO1 =V(CLPACK),DMCB,DXCLI,FILTCLI                                    
*                                                                               
GENINITX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CHECK THE INTEGRITY OF ADDRESS SPACE AND ADDRESSD                             
***********************************************************************         
CHKADDRD NTR1  BASE=*,LABEL=*                                                   
         CLC   =AL2(ADDRLQ1),=AL2(ADDRDLQ1)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ2),=AL2(ADDRDLQ2)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ3),=AL2(ADDRDLQ3)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ4),=AL2(ADDRDLQ4)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ5),=AL2(ADDRDLQ5)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ6),=AL2(ADDRDLQ6)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQ7),=AL2(ADDRDLQ7)                                     
         BNE   CHKADXXX                                                         
         CLC   =AL2(ADDRLQT),=AL2(ADDRDLQT)                                     
         BNE   CHKADXXX                                                         
*                                                                               
         J     YES                                                              
*                                                                               
* ADDRESS AND ADDRESSD ARE OUT OF SYNC                                          
*                                                                               
CHKADXXX DC    H'0'                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* BUILD TABLE OF BREAK LENGTHS                                                  
***********************************************************************         
DOBRKTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
* COMPUTE TABLE INDEX FIRST                                                     
*                                                                               
         LHI   R0,DOBKTBNQ                                                      
         LA    R1,DOBKTAB                                                       
         USING DOBKTD,R1                                                        
         CLC   DOBKTTYP,TYPECODE                                                
         BE    *+16                                                             
         LA    R1,DOBKTBLQ(R1)                                                  
         BCT   R0,*-14                                                          
         J     NO                                                               
         DROP  R1                                                               
*                                                                               
         LHI   RF,DOBKTBNQ                                                      
         SR    RF,R0                                                            
         STC   RF,TYPEINDX                                                      
*                                                                               
         MVI   DOBRKCOD,C'Y'                                                    
         L     R8,ABRKTAB                                                       
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEYSAV(13),0(R2)  TO RESTORE READ SEQ LATER                    
*                                                                               
* BUILD GROUP DEF KEY                                                           
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         ZIC   R1,TYPEINDX                                                      
         MHI   R1,DOBKTBLQ                                                      
         LA    R1,DOBKTAB(R1)                                                   
         MVC   KEY(2),DOBKTKEY-DOBKTD(R1)                                       
*                                                                               
         MVC   KEY+2(1),SXDTAGB                                                 
         MVC   SVAGYMD,SXDTAGB                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY,DMWORK                       
         CLI   8(R1),0                                                          
         BE    DOBRK15                                                          
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
DOBRK10  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,KEY,KEY,DMWORK                       
*                                                                               
DOBRK15  DS    0H                                                               
*                                                                               
* CHECK IF STILL HAVE SAME KEY TYPE                                             
*                                                                               
         ZIC   R1,TYPEINDX                                                      
         MHI   R1,DOBKTBLQ                                                      
         LA    R1,DOBKTAB(R1)                                                   
         CLC   KEY(2),DOBKTKEY-DOBKTD(R1)                                       
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
* CHECK IF AGENCY STILL THE SAME                                                
*                                                                               
         MVC   BYTE,KEY+2                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SVAGYMD                                                     
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
         LA    R2,KEY                                                           
         ZIC   R1,TYPEINDX                                                      
         MHI   R1,4                                                             
         B     DOBRK20(R1)                                                      
*                                                                               
DOBRK20  B     DOBRK20P                                                         
         B     DOBRK20M                                                         
         B     DOBRK20C                                                         
         B     DOBRK20S                                                         
*                                                                               
* FILTER KEYS FOR SPECIFIC RECORD TYPE                                          
*                                                                               
DOBRK20P DS    0H                  PRODUCT GROUPS                               
         USING PRGRECD,R2                                                       
         OC    PRGKGRP,PRGKGRP                                                  
         BNZ   DOBRK10                                                          
         B     DOBRK30                                                          
         DROP  R2                                                               
*                                                                               
DOBRK20M DS    0H                                                               
         USING MKGRECD,R2                                                       
         OC    MKGKPRD,MKGKPRD                                                  
         BNZ   DOBRK10                                                          
         OC    MKGKMGRP,MKGKMGRP                                                
         BNZ   DOBRK10                                                          
         B     DOBRK30                                                          
         DROP  R2                                                               
*                                                                               
DOBRK20C DS    0H                                                               
DOBRK20S DS    0H                                                               
         USING GRPRECD,R2                                                       
         OC    GRPKCODE,GRPKCODE                                                
         BNZ   DOBRK10                                                          
         DROP  R2                                                               
*                                                                               
DOBRK30  DS    0H                                                               
         LA    R2,IO                                                            
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,KEY+14,IO,DMWORK             
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
         ZIC   R1,TYPEINDX                                                      
         MHI   R1,4                                                             
         B     DOBRK40(R1)                                                      
*                                                                               
DOBRK40  B     DOBRK40P                                                         
         B     DOBRK40M                                                         
         B     DOBRK40C                                                         
         B     DOBRK40S                                                         
*                                                                               
DOBRK40P DS    0H                  PRODUCT GROUPS                               
         USING PRGRECD,R2                                                       
         USING BRKTABD,R8                                                       
         MVC   BRKTAM,PRGKAGMD                                                  
         MVC   BRKTCLT,PRGKCLT                                                  
         MVC   BRKTGID,PRGKID                                                   
         DROP  R2                                                               
*                                                                               
* GET THE '01' ELEMENT AND ADD UP THE BREAK LENGTHS                             
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
         MVC   BRKTBK1,PRGBK1LN-PRGEL01(R6)                                     
         MVC   BRKTBK2,PRGBK2LN-PRGEL01(R6)                                     
         MVC   BRKTBK3,PRGBK3LN-PRGEL01(R6)                                     
         B     DOBRK90                                                          
*                                                                               
DOBRK40M DS    0H                  MARKET  GROUPS                               
         USING MKGRECD,R2                                                       
         MVC   BRKTAM,MKGKAGMD                                                  
         MVC   BRKTCLT,MKGKCLT                                                  
         MVC   BRKTGID,MKGKMID                                                  
         DROP  R2                                                               
*                                                                               
* GET THE '01' ELEMENT AND ADD UP THE BREAK LENGTHS                             
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
         MVC   BRKTBK1,MKGBK1LN-MKGEL01(R6)                                     
         MVC   BRKTBK2,MKGBK2LN-MKGEL01(R6)                                     
         MVC   BRKTBK3,MKGBK3LN-MKGEL01(R6)                                     
         B     DOBRK90                                                          
*                                                                               
DOBRK40C DS    0H                  CLIENT GROUPS                                
DOBRK40S DS    0H                  STATION GROUPS                               
         USING GRPRECD,R2                                                       
         MVC   BRKTAM,GRPKAGMD                                                  
         MVC   BRKTGID,GRPKID                                                   
         DROP  R2                                                               
*                                                                               
* GET THE '10' ELEMENT AND ADD UP THE BREAK LENGTHS                             
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,GRPBRKCQ                                                  
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         MVI   DOBRKCOD,C'N'                                                    
         B     DOBRK100                                                         
*                                                                               
         MVC   BRKTBK1,GRPBK1LN-GRPBRKD(R6)                                     
         MVC   BRKTBK2,GRPBK2LN-GRPBRKD(R6)                                     
         B     DOBRK90                                                          
         DROP  R8                                                               
*                                                                               
* READ THE NEXT DEF RECORD                                                      
*                                                                               
DOBRK90  DS    0H                  STATION GROUPS                               
         LA    R8,BRKTABLQ(R8)                                                  
         B     DOBRK10                                                          
*                                                                               
* RESTORE READ SEQUENCE                                                         
*                                                                               
DOBRK100 DS    0H                                                               
         CLI   DOBRKCOD,C'Y'                                                    
         BNE   DOBRKNQX                                                         
*                                                                               
DOBRKQX  DS    0H                                                               
         J     YES                                                              
*                                                                               
DOBRKNQX DS    0H                                                               
         J     NO                                                               
DOBRKCOD DS    X                                                                
         LTORG                                                                  
*                                                                               
DOBKTAB  DC    C'PGP',X'0D01'                                                   
DOBKTBLQ EQU   *-DOBKTAB                                                        
         DC    C'MGA',X'0D02'                                                   
         DC    C'CGC',X'0D04'                                                   
         DC    C'SGS',X'0D05'                                                   
DOBKTBNQ EQU   (*-DOBKTAB)/DOBKTBLQ                                             
*                                                                               
DOBKTD   DS    0X                                                               
DOBKTTYP DS    CL3                 RECORD TYPE                                  
DOBKTKEY DS    CL3                 KEY TYPE                                     
DOBKTGCD DS    X                   D(GROUP CODE)                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* READ ACCESS RECORD                                                            
* 1. SAVE AGENCY SOURCE IN SVAGYSRC                                             
***********************************************************************         
READACC  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SXDTAGY                                                 
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(L'CT5KEY),KEY                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,IO,DMWORK                        
         CLI   8(R1),0                                                          
         JNE   NO                                                               
         TM    8(R1),X'80'                                                      
         JO    NO                                                               
         CLC   KEYSAVE(L'CT5KEY),IO                                             
         JNE   NO                                                               
*                                                                               
         LA    R6,IO                                                            
         MVC   DATADISP,=X'001C'                                                
         MVI   ELCODE,CTSYSELQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
READAC10 CLI   CTSYSNUM-CTSYSD(R6),X'02'         SPOT                           
         BE    READAC30                                                         
         BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
         B     READAC10                                                         
*                                                                               
READAC30 TM    CTSYSIND-CTSYSD(R6),CTSYSRAD                                     
         BZ    *+8                                                              
         MVI   SVAGYSRC,CT99KOMS                                                
*                                                                               
         TM    CTSYSIND-CTSYSD(R6),CTSYSMF                                      
         BZ    *+8                                                              
         MVI   SVAGYSRC,CT99KOMF                                                
*                                                                               
         TM    CTSYSIND-CTSYSD(R6),CTSYSOWN                                     
         BZ    *+8                                                              
         MVI   SVAGYSRC,CT99KOSD                                                
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UCOMDEF - SAVE UCOM DEFINITION ELEMENTS IN ABRKTAB                            
* R2 EXPECTED TO ADDRESS UCOM RECORD                                            
***********************************************************************         
UCOMDEF  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         XCEFL                                                                  
*                                                                               
         USING UCOMHDR,R2                                                       
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEYSAV(13),0(R2)  SAVE KEY FOR READ SEQ RESTORE                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(UCOMKPRD-UCOMHDR),0(R2)    CLT-LEVEL KEY                     
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(13),KEY                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),=C'SPTDIR ',KEY,KEY,DMWORK          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',GETREC),=C'SPTFIL ',KEY+14,        X        
               IO,DMWORK                                                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'02'         RECORD DELETED - IT IS OK                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* HAVE CLIENT-LEVEL UCOM RECORD HERE.  PROCESS ELEMENTS                         
*                                                                               
         LA    R6,IO                                                            
         LA    R6,(SUCMELEM-UCOMHDR)(R6)                                        
         ICM   R5,15,ABREAKT                                                    
*                                                                               
UCOMD100 DS    0H                                                               
         CLI   0(R6),X'00'         EOR                                          
         BE    UCOMD300                                                         
*                                                                               
         ZIC   RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)                                                    
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         AR    R6,RF                                                            
         B     UCOMD100                                                         
*                                                                               
* RESTORE READ SEQUENCE                                                         
*                                                                               
UCOMD300 DS    0H                                                               
         CLI   DXMODE,DXUPDTQ                                                   
         BE    UCOMDQX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),IOKEYSAV                                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'SPTDIR ',KEY,KEY,DMWORK                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SEQ RESTORE FAILED                           
*                                                                               
         CLC   KEY(13),IOKEYSAV                                                 
         BE    *+6                                                              
         DC    H'0'                SEQ RESTORE FAILED                           
*                                                                               
UCOMDQX  J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN HEADERS                                                          
*---------------------------------------------------------------------*         
LOADPPH  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     RE,ABRKTAB                                                       
         LHI   RF,BREAKTBX-BREAKTAB                                             
         STCM  RE,15,ABREAKT                                                    
         XCEFL                                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPPH02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPPHC,AINITPPH,AFILTPPH,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPPH02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN HEADERS                                                        
*---------------------------------------------------------------------*         
UPDTPPH  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPPH                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPPH                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPPHC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER PATTERN HEADER RECORD AT 0(R2)                               *         
*---------------------------------------------------------------------*         
FILTPPH  NTR1  BASE=*,LABEL=*                                                   
         USING PATRECD,R2                                                       
         CLC   =X'0A22',PATKID                                                  
         JNE   FILTPPHN                                                         
*                                                                               
         MVC   BYTE,PATKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FILTPPHN                                                         
*                                                                               
         TM    FLAGS,NORDSEQ                                                    
         JO    YES                                                              
         TM    PATKEY+L'PATKEY,X'02' INCOMPLETE RECORD?                         
         JO    NO                                                               
         TM    PATKEY+L'PATKEY,X'01' BPAT RECORD?                               
         JO    NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
FILTPPHN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN HEADERS                                                    
*---------------------------------------------------------------------*         
INITPPH  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPHDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN COMMERCIALS                                                      
*---------------------------------------------------------------------*         
LOADPCM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPCM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPCMC,AINITPCM,AFILTPCM,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPCM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN COMMERCIALS                                                    
*---------------------------------------------------------------------*         
UPDTPCM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPCM                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPCM10 DS   0H                                                               
         GOTO1 AINITPCM                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPCMC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPCM10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN COMMERCIALS                                                
*---------------------------------------------------------------------*         
INITPCM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPCDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN MARKETS                                                          
*---------------------------------------------------------------------*         
LOADPMK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPMK02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPMKC,AINITPMK,AFILTPMK,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPMK02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN MARKETS                                                        
*---------------------------------------------------------------------*         
UPDTPMK  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPMK                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPMK10 DS   0H                                                               
         GOTO1 AINITPMK                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPMKC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPMK10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN MARKETS                                                    
*---------------------------------------------------------------------*         
INITPMK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN STATION                                                          
*---------------------------------------------------------------------*         
LOADPST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPSTC,AINITPST,AFILTPST,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPST02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN STATIONS                                                       
*---------------------------------------------------------------------*         
UPDTPST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPST                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPST10 DS   0H                                                               
         GOTO1 AINITPST                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPSTC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPST10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN STATIONS                                                   
*---------------------------------------------------------------------*         
INITPST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN MARKET GROUPS                                                    
*---------------------------------------------------------------------*         
LOADPMG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPMG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPMGC,AINITPMG,AFILTPMG,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPMG02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN MARKET GROUPS                                                  
*---------------------------------------------------------------------*         
UPDTPMG  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPMG                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPMG10 DS   0H                                                               
         GOTO1 AINITPMG                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPMGC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPMG10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN MARKET GROUPS                                              
*---------------------------------------------------------------------*         
INITPMG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPKDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN AFFILIATES                                                       
*---------------------------------------------------------------------*         
LOADPMA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPMA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPMAC,AINITPMA,AFILTPMA,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPMA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN AFFILIATES                                                     
*---------------------------------------------------------------------*         
UPDTPMA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPMA                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPMA10 DS   0H                                                               
         GOTO1 AINITPMA                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPMAC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPMA10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN AFFILIATES                                                 
*---------------------------------------------------------------------*         
INITPMA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN STATION TYPES                                                    
*---------------------------------------------------------------------*         
LOADPTP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPTP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPTPC,AINITPTP,AFILTPTP,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPTP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN STATION TYPES                                                  
*---------------------------------------------------------------------*         
UPDTPTP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPTP                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPTP10 DS   0H                                                               
         GOTO1 AINITPTP                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPTPC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPTP10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN STATION TYPES                                              
*---------------------------------------------------------------------*         
INITPTP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD PATTERN COMMENTS                                                         
*---------------------------------------------------------------------*         
LOADPCN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PATKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPCN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTPCNC,AINITPCN,AFILTPCN,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPCN02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE PATTERN COMMENTS                                                       
*---------------------------------------------------------------------*         
UPDTPCN  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PATRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPCN                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTPCN10 DS   0H                                                               
         GOTO1 AINITPCN                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTPCNC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTPCN10                                                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE PATTERN COMMENTS                                                   
*---------------------------------------------------------------------*         
INITPCN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTPNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
* LOAD STEXT PAGE DEFINITION                                                    
*---------------------------------------------------------------------*         
LOADSTD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DTXRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   DTXKID,=X'0A2D'                                                  
         MVC   DTXKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCLOAD,DMCB,VSPTSTDC,AINITSTD,AFILTSTD,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE STEXT PAGE DEFINITION                                                  
*---------------------------------------------------------------------*         
UPDTSTD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING DTXRECD,R2                                                       
*                                                                               
         GOTO1 AFILTSTD                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSTD                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSTDC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER STEXT PAGE DEFINITION                                                  
*---------------------------------------------------------------------*         
FILTSTD  NTR1  BASE=*,LABEL=*                                                   
         USING DTXRECD,R2                                                       
         CLC   =X'0A2D',DTXKID                                                  
         JNE   FILTSTDN                                                         
*                                                                               
         MVC   BYTE,DTXKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FILTSTDN                                                         
*                                                                               
         CLI   DTXKDESC,C'-'                                                    
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
FILTSTDN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE STEXT PAGE DEFINITION                                              
*---------------------------------------------------------------------*         
INITSTD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTDPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
LOADSTL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING DTXRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   DTXKID,=X'0A2D'                                                  
         MVC   DTXKAM,SXDTAGB                                                   
*                                                                               
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VSPTSTLC,AINITSTL,AFILTSTL,(R8)                    
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,NOMOREQ                                                    
         JO    YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BNZ   LSTL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
UPDTSTL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSTL                                                         
         JNE   YES                                                              
         OI    FLAGS,KILLQ         GENERATE ACTION KILL                         
         OI    FLAGS,MULTIQ        INDICATE DOING MULTIPLE PUTS PER REC         
         MVI   ELCOUNT,X'00'                                                    
*                                                                               
UPDTSTL10 DS  0H                                                                
         GOTO1 AINITSTL                                                         
         LA    R8,MEDPARAM                                                      
         GOTO1 AACCUPDT,DMCB,VSPTSTLC,TYPECODE,0,(R8)                           
         JNE   NO                                                               
*                                                                               
         TM    FLAGS,MULTIQ                                                     
         BO    UPDTSTL10                                                        
         J     YES                                                              
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
FILTSTL  NTR1  BASE=*,LABEL=*                                                   
         USING DTXRECD,R2                                                       
         CLC   =X'0A2D',DTXKID                                                  
         JNE   FILTSTLN                                                         
*                                                                               
         MVC   BYTE,DTXKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         BNE   FILTSTLN                                                         
*                                                                               
         CLI   DTXKDESC,C'-'                                                    
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
FILTSTLN DS    0H                                                               
         OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
INITSTL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPTLNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GET BREAK LENGTHS FOR MARKET GROUP RECORD                                     
* MARKET GROUP RECORD KEY EXPECTED IN 0(R2)                                     
***********************************************************************         
GETBLEN  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING MKGRECD,R3                                                       
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD(3),2(R2)   A/M,CLT                                      
         MVC   MKGKMID,MKGKMID-MKGKEY(R2)                                       
*                                                                               
         CLI   MKGKMID-MKGKEY(R2),X'40'                                         
         BL    *+12                                                             
         CLI   MKGKMID-MKGKEY(R2),C'F'                                          
         BNH   *+10                                                             
         XC    MKGKCLT,MKGKCLT                                                  
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(L'MKGKEY),KEY                                            
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY,DMWORK                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,KEY+14,IO,DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO               A(RECORD)                                    
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         MVC   DATADISP,=X'0018'                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BREAKLS(1),MKGBK1LN-MKGEL01(R6)                                  
         MVC   BREAKLS+1(1),MKGBK2LN-MKGEL01(R6)                                
         MVC   BREAKLS+2(1),MKGBK3LN-MKGEL01(R6)                                
*                                                                               
         MVC   BUYTRKER,MKGBKBTK-MKGEL01(R6)                                    
*                                                                               
* RESTORE READ SEQUENCE                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'MKGKEY),0(R2)                                              
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(L'MKGKEY),KEY                                            
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY,DMWORK                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GBLENQX  DS    0H                                                               
         J     YES                                                              
GBLENNQX DS    0H                                                               
         J     NO                                                               
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R2 EXPECTED TO ADDRESS THE RECORD                                             
* R1 LOB EXPECTED TO HAVE SUBTYPE                                               
***********************************************************************         
FILT99   NTR1  BASE=*,LABEL=*                                                   
         USING CT99RECD,R2                                                      
*                                                                               
         CLI   SVAGYSRC,X'00'                                                   
         BE    F990050                                                          
*                                                                               
         CLI   CT99KTYP,CT99KTYQ                                                
         BNE   F990050                                                          
         CLM   R1,1,CT99KSUB                                                    
         BNE   F990050                                                          
         CLC   CT99KSRC,SVAGYSRC   REC SOURCE = AGY SOURCE?                     
         BNE   F990050                                                          
*                                                                               
         CLC   =C'DDS',CT99KUID                                                 
         JE    NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
F990050  OI    FLAGS,NOMOREQ                                                    
         J     NO                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDNET   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ANETTAB                                                       
         ICM   RF,15,=AL4(NETTABLQ)                                             
         XCEFL                                                                  
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(L'SLKKEY),0(R2)                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'   RECORD TYPE                                  
         MVC   NDEFKAGY,SXDTAGY    AGENCY                                       
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),SPTDIR,KEY,KEY,DMWORK                   
         B     BLDN04B                                                          
*                                                                               
BLDN04   GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),SPTDIR,KEY,KEY,DMWORK                   
*                                                                               
BLDN04B  DS    0H                                                               
         CLC   NDEFKTYP,=X'0D11'                                                
         BNE   BLDN20                                                           
         CLC   NDEFKAGY,SXDTAGY      REC TYPE, QAGY                             
         BNE   BLDN20                                                           
         OC    NDEFKNET,NDEFKNET   AGENCY-LEVEL NETDEF?                         
         BZ    BLDN04              YES -- SKIP                                  
         CLC   NDEFKCLT,KEYSAVE+SLKKCLT-SLKKEY                                  
         BE    *+14                                                             
         OC    NDEFKCLT,NDEFKCLT   OR AGENCY DEFAULT                            
         BNZ   BLDN04                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBITS,GETREC),SPTFIL,KEY+14,IO,DMWORK          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IO                                                            
         LA    R3,NDEFEL-NDEFRECD(R3)                                           
*                                                                               
BLDN08   DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    BLDN04              NEXT RECORD                                  
         CLI   0(R3),NDEFNELQ      02 ELEM HAS NETWORK NUMBER                   
         BE    BLDN09                                                           
*                                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BLDN08                                                           
*                                                                               
BLDN09   DS    0H                                                               
         USING NDEFEL02,R3                                                      
         CLI   NDEFNET,MAXNETS     TOO MANY NETWORKS?                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R4,NDEFNET          NETWORK NUMBER                               
         MHI   R4,NETTABL                                                       
         A     R4,ANETTAB                                                       
         DROP  R3                                                               
*                                                                               
         OC    0(NETTABL,R4),0(R4)  IF WE ALREADY HAVE AN ENTRY FOR             
         BNZ   BLDN04               THIS NET #, USE IT.                         
*                                                                               
         USING NETTABD,R4                                                       
         MVC   NTBCALL,NDEFKNET    SET NETWORK CODE                             
         B     BLDN04              NEXT RECORD                                  
         DROP  R4                                                               
*                                                                               
BLDN20   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SLKKEY),KEYSAVE                                            
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),XSPDIR,KEY,KEY,DMWORK                   
         CLC   KEY(L'SLKKEY),KEYSAVE                                            
         JE    YES                                                              
         DC    H'0'                KEY RESTORE FAILED                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* EXTRA POST-EXTRACT PROCESSING                                                 
***********************************************************************         
XTRAPROC NTR1  BASE=*,LABEL=*                                                   
         CLC   TYPECODE,=CL3'PRD'                                               
         BE    XPPRD                                                            
         J     YES                                                              
*                                                                               
XPPRD    DS    0H                                                               
         L     R2,DXAXREC                                                       
         USING SPTPDD,R2                                                        
*                                                                               
         CLC   =C'AAA',SPTPDPRD                                                 
         JNE   YES                                                              
         MVC   SVAAABFM,SPTPDBFM                                                
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTERING FOR RECORDS WITH CLIENT CODE IN ELEMENTS                            
***********************************************************************         
CLTFILT  NTR1  BASE=*,LABEL=*                                                   
         OC    FILTCLI,FILTCLI     IS THERE A CLIENT FILTER?                    
         JZ    YES                                                              
*                                                                               
         CLC   TYPECODE,=CL3'ORD'                                               
         BE    CLTF10                                                           
         B     CLTFX                                                            
*                                                                               
CLTF10   DS    0H                                                               
         USING DAREORDD,R2                                                      
*                                                                               
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         CLC   DOIDCLT,FILTCLI                                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R6,R2                                                            
*                                                                               
CLTFX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* OBTAIN BILL FORMULA FOR ESTIMATE'S PRODUCT                                    
***********************************************************************         
GETPRBFM NTR1  BASE=*,LABEL=*                                                   
         MVC   SVPRDBFM,SPACES     INIT BILL FORMULA TO BLANKS                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDR,R6                                                        
         MVC   KEY(EKEYEST-EKEY),0(R2)                                          
         GOTO1 =A(GETBFM),DMCB,KEY,SVPRDBFM                                     
         BE    GETPRBFM5                                                        
*                                                                               
         MVC   PKEYPRD,=C'AAA'                                                  
         GOTO1 =A(GETBFM),DMCB,KEY,SVPRDBFM                                     
*                                                                               
GETPRBFM5 DS   0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PKEY),0(R2)                                                
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY,DMWORK               
         CLC   KEY(L'PKEY),0(R2)                                                
         JE    YES                                                              
         DC    H'0'                KEY RESTORE FAILED                           
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* P1 - A(PRODUCT KEY)                                                           
* P2 - A(BILL FORMULA OUTPUT AREA, 18 BYTES)                                    
***********************************************************************         
GETBFM   NTR1  BASE=*,LABEL=*,WORK=(R8,4)                                       
         L     R6,0(R1)            A(PRODUCT KEY)                               
         L     R3,4(R1)            A(OUTPUT)                                    
         MVC   0(L'SVPRDBFM,R3),SPACES INITIALIZE OUTPUT                        
         MVC   0(L'PKEY,R8),0(R6)  SAVE PRODUCT KEY                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),SPTDIR,(R6),IO,DMWORK                   
         CLC   IO(L'PKEY),0(R8)                                                 
         JNE   NEQXIT                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBITS,GETREC),SPTFIL,IO+14,IO,DMWORK           
         CLI   8(R1),0                                                          
         JNE   NEQXIT                                                           
*                                                                               
         LA    R6,IO                                                            
         USING PRDHDR,R6                                                        
         OC    PBILLBAS(L'PBILLBAS+L'PBILLCOM),PBILLBAS                         
         JZ    NEQXIT                                                           
*                                                                               
         GOTO1 =V(DISBFM),DMCB,PBILLBAS,(R3)                                    
         CLC   0(L'SVPRDBFM,R3),SPACES                                          
         JH    EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
COPYBUFF DS    CL10000                                                          
*                                                                               
***********************************************************************         
* TF SSB                                                              *         
***********************************************************************         
*                                                                               
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      CSECT                                                                  
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
*                                                                               
*                                                                               
***********************************************************************         
* COPY OF COMFACS FOR THOSE DAMN DEMOS                                *         
***********************************************************************         
*                                                                               
COMFACS  CSECT                     COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                                                             
         DC    A(0)                GETMSG)                                      
         DC    A(0)                GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    A(0)                HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    A(0)                HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    A(0)                GETPROF)                                     
         DC    V(PERVERT)                                                       
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    V(GENERAL)                                                       
         DC    18A(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    A(0)                DDISPSRT)                                    
VDEMADDR DC    A(0)                DEMADDR                                      
         DC    A(0)                DEMDISP)                                     
         DC    A(0)                DBOOK)                                       
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
         DC    A(0)                DEMOUT)                                      
         DC    A(0)                DEMEL)                                       
         DC    A(0)                DEMAINT)                                     
         DC    A(0)                DEMAND)                                      
         DC    A(0)                DEMOMATH)                                    
         DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    V(PERVAL)                                                        
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
*&&                                                                             
         DC    A(0)                GLOBBER)                                     
         DC    A(0)                MINIO)                                       
         DC    A(0)                PARSNIP)                                     
         DC    A(0)                DICTATE)                                     
         DC    A(0)                EDITOR)                                      
         DC    A(0)                GETHELP)                                     
         DC    A(0)                CUREDIT)                                     
         DC    A(0)                GETRET)                                      
         DC    A(0)                REPORT)                                      
         DC    A(0)                BLDCUR)                                      
         DC    A(0)                GETCUR)                                      
         DC    A(0)                GETNARR)                                     
         DC    A(0)                DEJAVU)                                      
         DC    A(0)                SECRET)                                      
         DC    A(0)                BILLIT)                                      
         DC    A(0)                                                             
         DC    A(0)                PQPROF)                                      
         DC    2A(0)                                                            
         DC    A(0)                BINSRCH)                                     
         DC    V(PROTON)           PROTON)                                      
         DC    V(PROTOFF)          PROTOFF)                                     
         DC    A(0)                HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    24A(0)              SPARE                                        
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CSECT-SAVED STORAGE                                                           
***********************************************************************         
SXTRACT  CSECT                                                                  
CSTPK    DC    A(0)                                                             
CRCPK    DC    A(0)                                                             
DEMOCON  DC    A(0)                                                             
UNTIME   DC    A(0)                                                             
TRPACK   DC    A(0)                                                             
*                                                                               
*                                                                               
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
*                                                                               
*                                                                               
*                                                                               
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
*                                                                               
WORK     DS    XL64                                                             
*                                                                               
VUTL     DS    A                                                                
AOFFICER DS    A                                                                
ABRKTAB  DS    A                                                                
ANETTAB  DS    A                                                                
*                                                                               
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
PRIADDR  DS    CL4                                                              
KEY      DS    CL48                                                             
KEYSAVE  DS    CL48                                                             
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL64                                                             
BYTE     DS    XL1                                                              
DMINBITS DS    X                                                                
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
PRIALPHA DS    XL2                                                              
VERSION  DS    XL1                                                              
PLATFORM DS    XL1                                                              
SVUCMCLT DS    XL2                                                              
SVUCMMED DS    X                                                                
*                                                                               
MISCFLG  DS    XL1                 VARIOUS FLAGS, LOCAL TO SPXTRACT             
MFCLUCMQ EQU   X'80'               CLT-LEVEL UCOM RECORD CHANGED                
MFLDUPDQ EQU   X'40'               LOADING RECORDS IN UPDATE MOVE               
MFXPROCQ EQU   X'20'               EXTRA POST-EXTRACT PROCESSING                
MFDELADQ EQU   X'10'               WRITE DELETE-ADD PAIRS                       
*                                                                               
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEINDX DS    X                                                                
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
SVAGYMD  DS    X                                                                
SVAGYSRC DS    X                                                                
EQVFLAG  DS    C                                                                
*                                                                               
SVCPROF7 DS    XL4                 A/M,CLT,CLT,CPROF+6                          
SVBKLEN  DS    XL7                 A/M,CLT,GROUP ID,BREAKLS(3)                  
SVCLTPRD DS    XL7                 A/M,CLT,PRODUCT                              
*                                                                               
SVAMCLT  DS    XL3                 SAVED A/M,BCLT FOR STA LOCKINS               
*                                                                               
SVPRAM   DS    X                                                                
SVPRBCLT DS    XL2                                                              
SVPRCPRD DS    CL3                                                              
*                                                                               
LOADKEY  DS    XL32                                                             
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
*                                                                               
SAVEAREC DS    A                                                                
VARDIR   DS    CL7                                                              
VARFIL   DS    CL7                                                              
SVDIR    DS    CL7                                                              
*                                                                               
FILTCLI  DS    XL2                                                              
*                                                                               
* MEDPARAM BLOCK                                                                
*                                                                               
         DS    0F                  PUT ON A FULLWORD BOUNDARY                   
       ++INCLUDE SPXPARM                                                        
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
SVCLTKEY DS    XL4                                                              
IO       DS    6144X               SPOT HAS 6K RECS                             
*                                                                               
BREAKTAB DS    (BRKTABNQ*BRKTABLQ)X                                             
BREAKTBX DS    0X                                                               
*                                                                               
NETTAB   DS    0D                                                               
         DS    CL(MAXNETS*NETTABL)                                              
         DC    X'00'                                                            
NETTABLQ EQU   *-NETTAB                                                         
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
*                                                                               
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
         DS    XL3                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DSECT TO COVER COMMON ADDRESSES                                     *         
***********************************************************************         
*                                                                               
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VSPXCNVX DS    V                                                                
VPIDTAB  DS    A                                                                
VPIDTABC DS    A                                                                
ACBLTAB  DS    A                                                                
*                                                                               
ADDRDLQ1 EQU   *-ADDRESSD                                                       
*                                                                               
VSPTAGYC DS    V          MEDIA/AGENCY                                          
VSPTCNTC DS    V          CLIENT                                                
VSPTPRDC DS    V          PRODUCT                                               
VSPTPDLC DS    V          PRODUCT                                               
VSPTESTC DS    V          ESTIMATE                                              
VSPTMKTC DS    V          MARKET                                                
VSPTSTAC DS    V          STATION                                               
VSPTEDLC DS    V          ESTIMATE DEMO LIST                                    
VSPTSADC DS    V          STATION ADDRESS                                       
VSPTEQVC DS    V          EQUIVALENCE FACTORS                                   
VSPTCMLC DS    V          COMMERCIAL RECORDS                                    
VSPTSOWC DS    V          STATION OWNERSHIP                                     
VSPTFORC DS    V          STATION FORMAT                                        
VSPTOINC DS    V          OWNER INFO                                            
VSPTMGPC DS    V          MARKET GROUP DEF                                      
VSPTMGRC DS    V          MARKET GROUP                                          
VSPTMGAC DS    V          MARKET GROUP ASSIGNMENT                               
VSPTCGDC DS    V          CLIENT GROUP DEF                                      
VSPTCGPC DS    V          CLIENT GROUP                                          
VSPTCGCC DS    V          CLIENT GROUP                                          
VSPTSGDC DS    V          STATION GROUP DEF                                     
VSPTSGPC DS    V          STATION GROUP                                         
VSPTSGSC DS    V          STATION GROUP STATIONS                                
VSPTPGFC DS    V          PRODUCT GROUP DEF                                     
VSPTPGRC DS    V          PRODUCT GROUP                                         
VSPTPGPC DS    V          PRODUCT GROUP PRODUCTS                                
VSPTPARC DS    V          PARENT INFO                                           
VSPTDEAC DS    V          DEAL RECORDS                                          
VSPTDSTC DS    V          DEAL STATIONS                                         
VSPTREPC DS    V          REP RECORDS                                           
VSPTNLC  DS    V          SYSCODE/NETWORK LIST                                  
VSPTUCMC DS    V          SPOT UCOM RECORD                                      
VSPTSLKC DS    V          STATION LOCKIN RECORD                                 
VSPTDPMC DS    V          DAYPART MENU RECORD                                   
VSPTDEMC DS    V          DEMO CODES AND NAMES                                  
VSPTDMTC DS    V          TRATITIONAL DEMO CODES AND NAMES                      
VSPTDMCC DS    V          NON-TRADITIONAL DEMO CODES AND NAMES                  
VSPTADBC DS    V          ADBUYER                                               
VSPTEAD  DS    V          ESTIMATE AUTHORIZED DOLLARS                           
VSPTDAOC DS    V          DARE ORDER (BLOCKCHAIN)                               
VSPTDMNC DS    V          DARE MAKEGOOD NOTICE (BLOCKCHAIN)                     
VSPTDMXC DS    V          DARE MAKEGOOD NOTICE (BLOCKCHAIN) XSPT                
VSPTBXQC DS    V          BUY EXTRACT REQUEST                                   
VSPTBIL  DS    V          BILLING INFO                                          
VSPTFLT  DS    V          FLIGHT RECORDS                                        
VSPTFLF  DS    V          FLIGHT RECORD FLIGHTS                                 
VSPTMKCC DS    V          COMSCORE MARKET                                       
VSPTSPCC DS    V          SPILLDEF                                              
VSPTSQCC DS    V          SQAD DAYPART MENU                                     
VSPTDMUC DS    V          DEMO MENUS                                            
VSPTBFMC DS    V          BILL FORMULA                                          
VSPTPATC DS    V          PATTERN RECORD                                        
VSPTPPHC DS    V          PATTERN HEADER                                        
VSPTPCMC DS    V          PATTERN COMMERCIAL                                    
VSPTPMKC DS    V          PATTERN MARKET                                        
VSPTPSTC DS    V          PATTERN STATION                                       
VSPTPMGC DS    V          PATTERN MARKET GROUPS                                 
VSPTPMAC DS    V          PATTERN AFFILIATES                                    
VSPTPTPC DS    V          PATTERN STATION TYPES                                 
VSPTPCNC DS    V          PATTERN COMMENTS                                      
VSPTSTDC DS    V          STEXT PAGE DEFINITION                                 
VSPTSTLC DS    V          STEXT LINE                                            
*                                                                               
ADDRDLQ2 EQU   *-ADDRESSD                                                       
*                                                                               
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
         DS    A                                                                
         DS    A                   SPARE                                        
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                                                                
ARECCMP  DS    A                                                                
*                                                                               
ADDRDLQ3 EQU   *-ADDRESSD                                                       
*                                                                               
         DS    CL8                 LOAD ROUTINES                                
ALOADAGY DS    A          MEDIA/AGENCY                                          
ALOADCNT DS    A          CLIENT                                                
ALOADPRD DS    A          PRODUCT                                               
ALOADPDL DS    A          PRODUCT LIST                                          
ALOADEST DS    A          ESTIMATE                                              
ALOADMKT DS    A          MARKET                                                
ALOADSTA DS    A          STATION                                               
ALOADEDL DS    A          ESTIMATE DEMO LIST                                    
ALOADSAD DS    A          STATIO ADDRESS                                        
ALOADEQV DS    A          EQUIVALENCE FACTORS                                   
ALOADCML DS    A          COMMERCIAL RECORDS                                    
ALOADSOW DS    A          STATION OWNERSHIP INFO                                
ALOADFOR DS    A          STATION FORMAT                                        
ALOADOIN DS    A          OWNER INFO                                            
ALOADMGD DS    A          MARKET GROUP DEF                                      
ALOADMGR DS    A          MARKET GROUP                                          
ALOADMGA DS    A          MARKET GROUP ASSIGNMENT                               
ALOADCGD DS    A          CLIENT GROUP DEF                                      
ALOADCGP DS    A          CLIENT GROUP                                          
ALOADCGC DS    A          CLIENT GROUP                                          
ALOADSGD DS    A          STATION GROUP DEF                                     
ALOADSGP DS    A          STATION GROUP                                         
ALOADSGS DS    A          STATION GROUP STATIONS                                
ALOADPGF DS    A          PRODUCT GROUP DEF                                     
ALOADPGR DS    A          PRODUCT GROUP                                         
ALOADPGP DS    A          PRODUCT GROUP PRODUCTS                                
ALOADPAR DS    A          PARENT INFO                                           
ALOADDEA DS    A          DEAL RECORDS                                          
ALOADDST DS    A          DEAL STATIONS                                         
ALOADREP DS    A          REP RECORDS                                           
ALOADSNL DS    A          SYSCODE/NETWORK LIST                                  
ALOADUCM DS    A          SPOT UCOM RECORD                                      
ALOADSLK DS    A          STATION LOCKIN RECORD                                 
ALOADDPM DS    A          DAYPART MENU RECORD                                   
ALOADDEM DS    A          DEMO CODES AND NAMES                                  
ALOADDMT DS    A          TRADITIONAL DEMO CODES AND NAMES                      
ALOADDMC DS    A          NON-TRADITIONAL DEMO CODES AND NAMES                  
ALOADADB DS    A          ADBUYER                                               
ALOADEAD DS    A          ESTIMATE AUTHORIZED DOLLARS                           
ALOADDAO DS    A          DARE ORDER (BLOCKCHAIN)                               
ALOADDMN DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN)                     
ALOADDMX DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN) XSPT                
ALOADBIL DS    A          BILLING INFO                                          
ALOADFLT DS    A          FLIGHT RECORDS                                        
ALOADFLF DS    A          FLIGHT RECORD FLIGHTS                                 
ALOADMKC DS    A          COMSCORE MARKET                                       
ALOADSPD DS    A          SPILLDEF                                              
ALOADSQD DS    A          SQAD DAYPART MENU                                     
ALOADDMU DS    A          DEMO MENUS                                            
ALOADBFM DS    A          BILL FORMULA                                          
ALOADPAT DS    A          PATTERN RECORD                                        
ALOADPPH DS    A          PATTERN HEADER                                        
ALOADPCM DS    A          PATTERN COMMERCIAL                                    
ALOADPMK DS    A          PATTERN MARKET                                        
ALOADPST DS    A          PATTERN STATION                                       
ALOADPMG DS    A          PATTERN MARKET GROUPS                                 
ALOADPMA DS    A          PATTERN AFFILIATES                                    
ALOADPTP DS    A          PATTERN STATION TYPES                                 
ALOADPCN DS    A          PATTERN COMMENTS                                      
ALOADSTD DS    A          STEXT PAGE DEFINITION                                 
ALOADSTL DS    A          STEXT LINE                                            
*                                                                               
ADDRDLQ4 EQU   *-ADDRESSD                                                       
*                                                                               
         DS    CL8                 UPDATE ROUTINES                              
AUPDTAGY DS    A          MEDIA/AGENCY                                          
AUPDTCNT DS    A          CLIENT                                                
AUPDTPRD DS    A          PRODUCT                                               
AUPDTPDL DS    A          PRODUCT LIST                                          
AUPDTEST DS    A          ESTIMATE                                              
AUPDTMKT DS    A          MARKET                                                
AUPDTSTA DS    A          STATION                                               
AUPDTEDL DS    A          ESTIMATE DEMO LIST                                    
AUPDTSAD DS    A          STATION ADDRESS                                       
AUPDTEQV DS    A          EQUIVALENCE FACTORS                                   
AUPDTSOW DS    A          STATION OWNERSHIP INFO                                
AUPDTFOR DS    A          STATION FORMAT                                        
AUPDTOIN DS    A          OWNER INFO                                            
AUPDTMGD DS    A          MARKET GROUP DEF                                      
AUPDTMGR DS    A          MARKET GROUP                                          
AUPDTMGA DS    A          MARKET GROUP ASSIGNMENT                               
AUPDTCGD DS    A          CLIENT GROUP DEF                                      
AUPDTCGP DS    A          CLIENT GROUP                                          
AUPDTCGC DS    A          CLIENT GROUP                                          
AUPDTSGD DS    A          STATION GROUP DEF                                     
AUPDTSGP DS    A          STATION GROUP                                         
AUPDTSGS DS    A          STATION GROUP STATIONS                                
AUPDTPGF DS    A          PRODUCT GROUP DEF                                     
AUPDTPGR DS    A          PRODUCT GROUP                                         
AUPDTPGP DS    A          PRODUCT GROUP PRODUCTS                                
AUPDTPAR DS    A          PARENT INFO                                           
AUPDTDEA DS    A          DEAL RECORDS                                          
AUPDTDST DS    A          DEAL STATIONS                                         
AUPDTREP DS    A          REP RECORDS                                           
AUPDTSNL DS    A          SYSCODE/NETWORK LIST                                  
AUPDTUCM DS    A          SPOT UCOM RECORD                                      
AUPDTSLK DS    A          STATION LOCKIN RECORD                                 
AUPDTDPM DS    A          DAYPART MENU RECORD                                   
AUPDTDEM DS    A          DEMO CODES AND NAMES                                  
AUPDTDMT DS    A          TRADITIONAL DEMO CODES AND NAMES                      
AUPDTDMC DS    A          NON-TRADITIONAL DEMO CODES AND NAMES                  
AUPDTADB DS    A          ADBUYER                                               
AUPDTEAD DS    A          ESTIMATE AUTHORIZED DOLLARS                           
AUPDTDAO DS    A          DARE ORDER (BLOCKCHAIN)                               
AUPDTDMN DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN)                     
AUPDTDMX DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN) XSPT                
AUPDTBIL DS    A          BILLING INFO                                          
AUPDTFLT DS    A          FLIGHT RECORDS                                        
AUPDTFLF DS    A          FLIGHT RECORD FLIGHTS                                 
AUPDTMKC DS    A          COMSCORE MARKET                                       
AUPDTSPD DS    A          SPILLDEF                                              
AUPDTSQD DS    A          SQAD DAYPART MENU                                     
AUPDTDMU DS    A          DEMO MENU                                             
AUPDTBFM DS    A          BILL FORMULA                                          
AUPDTPAT DS    A          PATTERN RECORD                                        
AUPDTPPH DS    A          PATTERN HEADER                                        
AUPDTPCM DS    A          PATTERN COMMERCIAL                                    
AUPDTPMK DS    A          PATTERN MARKET                                        
AUPDTPST DS    A          PATTERN STATION                                       
AUPDTPMG DS    A          PATTERN MARKET GROUPS                                 
AUPDTPMA DS    A          PATTERN AFFILIATES                                    
AUPDTPTP DS    A          PATTERN STATION TYPES                                 
AUPDTPCN DS    A          PATTERN COMMENTS                                      
AUPDTSTD DS    A          STEXT PAGE DEFINITION                                 
AUPDTSTL DS    A          STEXT LINE                                            
*                                                                               
ADDRDLQ5 EQU   *-ADDRESSD                                                       
*                                                                               
         DS    CL8                 FILTER ROUTINES                              
AFILTAGY DS    A          MEDIA/AGENCY                                          
AFILTCNT DS    A          CLIENT                                                
AFILTPRD DS    A          PRODUCT                                               
AFILTPDL DS    A          PRODUCT LIST                                          
AFILTEST DS    A          ESTIMATE                                              
AFILTMKT DS    A          MARKET                                                
AFILTSTA DS    A          STATION                                               
AFILTEDL DS    A          ESTIMATE DEMO LIST                                    
AFILTSAD DS    A          STATION ADDRESS                                       
AFILTEQV DS    A          EQUIVALENCE FACTORS                                   
AFILTCML DS    A          COMMERCIAL RECORDS                                    
AFILTSOW DS    A          STATION OWNERSHIP INFO                                
AFILTFOR DS    A          STATION FORMAT                                        
AFILTOIN DS    A          OWNER INFO                                            
AFILTMGD DS    A          MARKET GROUP DEF                                      
AFILTMGR DS    A          MARKET GROUP                                          
AFILTMGA DS    A          MARKET GROUP ASSIGNMENT                               
AFILTCGD DS    A          CLIENT GROUP DEF                                      
AFILTCGP DS    A          CLIENT GROUP                                          
AFILTCGC DS    A          CLIENT GROUP                                          
AFILTSGD DS    A          STATION GROUP DEF                                     
AFILTSGP DS    A          STATION GROUP                                         
AFILTSGS DS    A          STATION GROUP STATIONS                                
AFILTPGF DS    A          PRODUCT GROUP DEF                                     
AFILTPGR DS    A          PRODUCT GROUP                                         
AFILTPGP DS    A          PRODUCT GROUP PRODUCTS                                
AFILTPAR DS    A          PARENT INFO                                           
AFILTDEA DS    A          DEAL RECORDS                                          
AFILTDST DS    A          DEAL STATIONS                                         
AFILTREP DS    A          REP RECORDS                                           
AFILTSNL DS    A          SYSCODE/NETWORK LIST                                  
AFILTUCM DS    A          SPOT UCOM RECORD                                      
AFILTSLK DS    A          STATION LOCKIN RECORD                                 
AFILTDPM DS    A          DAYPART MENU RECORD                                   
AFILTDMT DS    A          TRADITIONAL DEMO CODES AND NAMES                      
AFILTDMC DS    A          NON-TRADITIONAL DEMO CODES AND NAMES                  
AFILTADB DS    A          ADBUYER                                               
AFILTEAD DS    A          ESTIMATE AUTHORIZED DOLLARS                           
AFILTDAO DS    A          DARE ORDER (BLOCKCHAIN)                               
AFILTDMN DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN)                     
AFILTDMX DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN) XSPT                
AFILTBIL DS    A          BILLING INFO                                          
AFILTFLT DS    A          FLIGHT RECORDS                                        
AFILTFLF DS    A          FLIGHT RECORD FLIGHTS                                 
AFILTMKC DS    A          COMSCORE MARKET                                       
AFILTSPD DS    A          SPILLDEF                                              
AFILTSQD DS    A          SQAD DAYPART MENU                                     
AFILTDMU DS    A          DEMO MENU                                             
AFILTBFM DS    A          BILL FORMULA                                          
AFILTPPH DS    A          PATTERN HEADER                                        
AFILTPCM DS    A          PATTERN COMMERCIAL                                    
AFILTPMK DS    A          PATTERN MARKET                                        
AFILTPST DS    A          PATTERN STATION                                       
AFILTPMG DS    A          PATTERN MARKET GROUPS                                 
AFILTPMA DS    A          PATTERN AFFILIATES                                    
AFILTPTP DS    A          PATTERN STATION TYPES                                 
AFILTPCN DS    A          PATTERN COMMENTS                                      
AFILTSTD DS    A          STEXT PAGE DEFINITION                                 
AFILTSTL DS    A          STEXT LINE                                            
*                                                                               
ADDRDLQ6 EQU   *-ADDRESSD                                                       
*                                                                               
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITAGY DS    A          MEDIA/AGENCY                                          
AINITCNT DS    A          CLIENT                                                
AINITPRD DS    A          PRODUCT                                               
AINITPDL DS    A          PRODUCT LIST                                          
AINITEST DS    A          ESTIMATE                                              
AINITMKT DS    A          MARKET                                                
AINITSTA DS    A          STATION                                               
AINITEDL DS    A          ESTIMATE DEMO LIST                                    
AINITSAD DS    A          STATION ADDRESS                                       
AINITEQV DS    A          EQUIVALENCE FACTORS                                   
AINITCML DS    A          COMMERCIAL RECORDS                                    
AINITSOW DS    A          STATION OWNERSHIP INFO                                
AINITFOR DS    A          STATION FORMAT                                        
AINITOIN DS    A          OWNER INFO                                            
AINITMGD DS    A          MARKET GROUP DEF                                      
AINITMGR DS    A          MARKET GROUP                                          
AINITMGA DS    A          MARKET GROUP ASSIGNMENT                               
AINITCGD DS    A          CLIENT GROUP DEF                                      
AINITCGP DS    A          CLIENT GROUP                                          
AINITCGC DS    A          CLIENT GROUP                                          
AINITSGD DS    A          STATION GROUP DEF                                     
AINITSGP DS    A          STATION GROUP                                         
AINITSGS DS    A          STATION GROUP STATIONS                                
AINITPGF DS    A          PRODUCT GROUP DEF                                     
AINITPGR DS    A          PRODUCT GROUP                                         
AINITPGP DS    A          PRODUCT GROUP PRODUCTS                                
AINITPAR DS    A          PARENT INFO                                           
AINITDEA DS    A          DEAL RECORDS                                          
AINITDST DS    A          DEAL STATIONS                                         
AINITREP DS    A          REP RECORDS                                           
AINITSNL DS    A          SYSCODE/NETWORK LIST                                  
AINITUCM DS    A          SPOT UCOM RECORD                                      
AINITSLK DS    A          STATION LOCKIN RECORD                                 
AINITDPM DS    A          DAYPART MENU RECORD                                   
AINITDEM DS    A          DEMO CODES AND NAMES                                  
AINITDMT DS    A          TRADITIONAL DEMO CODES AND NAMES                      
AINITDMC DS    A          NON-TRADITIONAL DEMO CODES AND NAMES                  
AINITADB DS    A          ADBUYER                                               
AINITEAD DS    A          ESTIMATE AUTHORIZED DOLLARS                           
AINITDAO DS    A          DARE ORDER (BLOCKCHAIN)                               
AINITDMN DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN)                     
AINITDMX DS    A          DARE MAKEGOOD NOTICE (BLOCKCHAIN) XSPT                
AINITBIL DS    A          BILLING INFO                                          
AINITFLT DS    A          FLIGHT RECORDS                                        
AINITFLF DS    A          FLIGHT RECORD FLIGHTS                                 
AINITMKC DS    A          COMSCORE MARKET                                       
AINITSPD DS    A          SPILLDEF                                              
AINITSQD DS    A          SQAD DAYPART MENU                                     
AINITDMU DS    A          DEMO MENU                                             
AINITBFM DS    A          BILL FORMULA                                          
AINITPPH DS    A          PATTERN HEADER                                        
AINITPCM DS    A          PATTERN COMMERCIAL                                    
AINITPMK DS    A          PATTERN MARKET                                        
AINITPST DS    A          PATTERN STATION                                       
AINITPMG DS    A          PATTERN MARKET GROUPS                                 
AINITPMA DS    A          PATTERN AFFILIATES                                    
AINITPTP DS    A          PATTERN STATION TYPES                                 
AINITPCN DS    A          PATTERN COMMENTS                                      
AINITSTD DS    A          STEXT PAGE DEFINITION                                 
AINITSTL DS    A          STEXT LINE                                            
*                                                                               
ADDRDLQ7 EQU   *-ADDRESSD                                                       
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
PRTDIR   DS    CL7                                                              
PRTFIL   DS    CL7                                                              
SPTDIR   DS    CL7                                                              
SPTFIL   DS    CL7                                                              
STAFIL   DS    CL7                                                              
TRFDIR   DS    CL7                                                              
TRFFIL   DS    CL7                                                              
XSPDIR   DS    CL7                                                              
XSPFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
ASLNTAB  DS    A                                                                
ACTIVITY DS    CL1                                                              
SPACES   DS    CL80                                                             
*                                                                               
ADDRDLQT EQU   *-ADDRESSD                                                       
*                                                                               
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
*                                                                               
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE SPGENMKA                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENDEAL                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENUCOM                                                      
       ++INCLUDE SPGENXLK                                                       
       ++INCLUDE RXUSERD                                                        
       ++INCLUDE SPXRECD                                                        
       ++INCLUDE DXDSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE FACTRYEQUS                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE SPGENEQU                                                       
       ++INCLUDE SPXBTABD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENDAYPT                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE SPADBUYER                                                      
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPGENNTDEM                                                     
       ++INCLUDE SPGENDRFLT                                                     
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE SPGENSQAD                                                      
       ++INCLUDE SPGENDMN                                                       
       ++INCLUDE SPGENBFML                                                      
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRDTXT                                                       
*                                                                               
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
*                                                                               
       ++INCLUDE AXTRBDST                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPXTRACT  02/24/21'                                      
         END                                                                    
