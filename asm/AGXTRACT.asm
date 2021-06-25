*          DATA SET AGXTRACT   AT LEVEL 048 AS OF 08/24/20                      
*PHASE AGXTRCTA                                                                 
                                                                                
         TITLE 'AGXTRACT - Warehouse Data Extract'                              
                                                                                
***********************************************************************         
* Who  Lvl Date    Change(s)                                 Audit              
* ---- --- ------- ----------------------------------------- ----------         
* MPEN 044 20MAY20 New fields for orders                     DSRD-25739         
* NSHE 044 05May20 Create client summary records             DSRD-26144         
* VGUP 043 05MAY20 Fix the timeoff record issue              DSRD-26383         
* YNGX 042 10Mar20 New Timeoff for Load/Update purposes      DSRD-23410         
* YNGX 041 10JAN20 FIX BLANK VALUES IN HIGHER LEVEL ACCOUNTS DSRD-25135         
*                  TREAT PEELED TRANSACTIONS AS DELETED                         
*                  NEW MOA AND TRANSACTION MODULE FIELS                         
*                  Fix duplicate CUT records sent to an agy                     
* VGUP 040 19DEC19 Fix for estimate / order out/in entries   DSRD-24839         
*                  to ensure in entry has the 1sec delay                        
* NSHE 039 06Nov19 Extend extract to include expense trans   DSRD-23387         
* MPEN 038 07Aug19 New company userid column                 DSRD-23461         
* MPEN 037 01Oct19 Fix for delete job in Aura                DSRD-24109         
* YNGX 036 03May19 Include est deleted/restored in =Prod     DSRD-22214         
* YNGX 035 06Aug19 relink to increase SJOTMAX                ITMF-38626         
* VGUP 034 21May19 Fix for CpyUpdateDimension time           DSRD-22598         
*                  used new DMRCVRHDR equates                                   
* TKLU 033 14Mar19 ACRTACTH for ACC - TYPTABD change         DSRD-21820         
*                  RSIN=RCVSEQNO changes TCLE                DSRD-21790         
* TKLU 032 21Feb19 Fix issue with RETBYTE value - RETBFFQ    DSRD-21704         
*                  remained from previous record when this                      
*                  e.g. an order 'status change' where                          
*                  AGXORDC still sets RETBFFQ though facts                      
*                  won't get processed. Could alos use code                     
*                  at UORD55 (+ more tricky equivalent in                       
*                  UPDTEST)                                                     
* TKLU 031 08Feb19 New data fields for high level accounts   DSRD-21185         
* CPAT              US XOUT -> HEXOUT changes                                   
* TKLU 030 23Jan19 AGXROUTS.CHKEIM adjustments               DSRD-21047         
* TKLU 029 06Dec18 Increase SJOTMAX                          ITMF-31733         
* TKLU 028 26Nov18 SETSJO coding for TRNFOFF iuf update mode DSRD-20807         
*                  AGXCUTQ call fix                                             
* TKLU 027 31Oct18 Change SETACTV as required now            DSRD-20556         
* TKLU 026 16Oct18 US transaction SCITSJXP change            DSRD-20460         
* TKLU 025 06Oct18 US draft transaction filter fix           DSRD-20140         
*                  Fix hole in IO logic if no data           DSRD-20012         
* TKLU 024 05Oct18 DSNNAMC change (from AURADEV to AURACSC)  SPEC-12211         
* TKLU 023 01Oct18 US transaction reference adjustment       DSRD-20140         
*                  Exclude CUT if Currency Load running                         
*                  Various other improvements, see AGXROUTS                     
* TKLU 022 14Sep18 Various data fixes + add SR ledger        DSRD-20140         
* TKLU 021 04Sep18 Extra date validation + US merge +        DSRD-20010         
*                  New System data field (+ GCPY07 jump fix) DSRD-19961         
* TKLU 020 17Aug18 Dummy US w/c 99 entry on load             DSRD-19969         
* TKLU 019 15Aug18 Set ESTFEXI with estimate number          DSRD-19937         
*                  + US only 6 hours Mediaocean time shift                      
* TKLU 018 14Aug18 New CUT dimension added to LOAD, too      DSRD-19926         
*                  and changed SETACTV globally incl.        DSRD-19938         
*                  SV_REDAT/TIM logic                                 f         
* TKLU 017 30Jul18 New CUT dimension for update purposes     DSRD-19766         
* TKLU             Est/Ord DimStatusUpdate: skip 'delete' if DSRD-19623         
*                  'add' follows                                                
* TKLU 016 10May18 SCMEL/ARTEL sub sequence logic change     DSRD-18977         
*                  ARTMULT dec pl changes due to web fix     DSRD-18931         
* TKLU 015 23Apr18 Currency order add up to avoid 'pennies'  DSRD-18850         
*                  Assume ARTMULT of zero is 100 really      DSRD-18850         
*                  Skip zero timesheet rows (TIMEL clusters) DSRD-18836         
*                  Include Expense POs without cli/pro/job   DSRD-13993         
* TKLU 014 05Apr18 AGFORDC EUREKA call adjustments (again)   DSRD-18788         
* TKLU 013 29Mar18 Estimates and order time manipulation     DSRD-18638         
*                  FILTTIM clean up to make code neater      DSRD-18696         
* TKLU 012 26Mar18 AGXCLIC: label typo for FFTELQ            DSRD-18510         
*                  SJ office from current level only - old   DSRD-18534         
*                  code not removed fully as GETSJO reused                      
*                  but with different code (old saved)                          
*                  Expenses line manager/fin approver rework DSRD-18578         
*                  All ledgers - take accounts of any level  DSRD-18635         
*                  Job approver lookup GETJAP                DSRD-18604         
* TKLU 011 08Mar18 AGXROUTS GETMAP call parameter fix        DSRD-18430         
* TKLU 010 28Feb18 Estimate item data retrieval bug fix      DSRD-18342         
* TKLU 009 21Feb18 Various bug fixes (see AGXROUTS)          DSRD-18117         
* TKLU 008 26Jan18 Transaction billing enhancements          DSRD-18079         
* TKLU 007 11Jan18 Added EXPRRWNR and EXPRX01N length fix    DSRD-17946         
* TKLU 006 09Jan18 GETOPT IO improvements                    DSRD-17593         
* TKLU 005 08Jan18 UFSELD JOBDU* bug fix                     DSRD-17881         
* TKLU 004 05Jan18 Add TYPE=TRN and TYPE=EXP                 DSRD-17376         
* TKLU 003 13Dec17 Enlarge estimate facts 1R to incl. u/l    ITMF-21419         
* TKLU 002 15Nov17 Order update bug fix                      DSRD-16206         
* TKLU 001 27Oct17 Initial Version for BUlkAPI Extract       DSRD-13993         
*                                                                               
***********************************************************************         
* Issues                                                              *         
* ------                                                              *         
* >>> IMPORTANCE RERUN update jobs will not work for Timeoff module   *         
* >>> (Use TYPE=API to exclude Timeoff when RERUN update jobs)        *         
* >>> CHAMODE settings/mode - both available but to keep flexibility  *         
* >>> XPFM    settings/mode - both available to keep flexibility      *         
* >>> DATE= card values taken out of JCL (as decided by Product)      *         
* >>> Uses TRUEQ/FALSEQ not YESQ/NOQ for Boolean                      *         
* >>> BLDCUR Call for 'ALL' not 'Production only' currencies          *         
* >>> RETCODE magnitude is irrelevant, this is ignored by DXTRACT     *         
*                                                                     *         
***********************************************************************         
* General                                                             *         
* -------                                                             *         
* AGXRECID uses record ID range 05300 to 05399                        *         
* JCL: YNGX.DDS.JCL(GPX*)                                             *         
* PROCSTAT: not coded/supported                                       *         
*                                                                     *         
* DSNFILE must be set if running from TSO                             *         
* There is new TSTRUN= parameter for use in AGXTRACT:                 *         
* - TSTRUN+0=TSTJCLQ/T to set TSO test run                            *         
*                                                                     *         
* Documentation confluence page:                                      *         
*                                                                     *         
* https://confluence.mediaocean.com/display/RD/Bulk+API+mainframe+side*         
*                                                                     *         
* ------------------------------------------------------------------- *         
* Field initialization per type request for Talend DB (YNGX,14AUG17): *         
*                                                                               
*   => see following routines in AGX-book...                                    
* 1.Text fields: colon+colon if no data, a space if we have a value             
*   => see routine ????? in ?????                                               
* 2.Boolean: 0 or 1                                                             
*   => see routine ????? in ?????                                               
* 3.Date field: colon+colon                                                     
*   => see routine ????? in ?????                                               
* 4.Monetary amout fields: 0.0                                                  
*   => see routine ????? in ?????                                               
*   Examples: CLIDCRL CL13 T01D126 Credit limit,                                
*             ESTFCAM CL12 T15F117 Est currency amount,ORDFCAM CL12 AMT         
* 5.Integer fields: 0                                                           
*   => see routine ????? in ?????                                               
*                                                                               
***********************************************************************         
* Warehouse SQL database extract control module                       *         
* ---------------------------------------------                       *         
* This version will pass kill mode records so there are no cascading  *         
* releationships or triggers.                                         *         
*                                                                     *         
* Control is passed from DXTRACT with following parms:                *         
* R1=A(Extract control data block - see DSECT DXBLOCKD)               *         
*                                                                     *         
* Module entered with one of following modes in DXMODE:               *         
*   DXOPENQ  - open system files                                      *         
*   DXCLOSEQ - close system files                                     *         
*   DXLOADQ  - extract from (generic) files in load mode              *         
*   DXUPDTQ  - extract from (recovery) files in update mode           *         
*                                                                     *         
* For DXLOADQ and DXUPDTQ modes, DXBLOCKD contains DXSTPTR which is:  *         
* A(Current entry in sub system extract driver table, see SYSTABD)    *         
*                                                                     *         
* Return CC .NE. if error condition else CC .EQ. for OK               *         
*                                                                               
* Order of modes from DXTRACT is as follows for update mode                     
*                                                                               
* DXOPENQ                                                                       
* DXUPDTQ - for each recovery change/add                                        
* DXCLOSXDQ  - driven by parm CLOXMODE=Y and each extract file created          
* DXENDQ - for each alpha id in the extract                                     
* DXCLOSEQ                                                                      
* DXCLOSXDQ  - for last file                                                    
*                                                                               
* For load                                                                      
* DXOPENQ                                                                       
* DXLOADQ                                                                       
* DXCLOSEQ                                                                      
* DXCLOSXDQ                                                                     
*                                                                     *         
* >>> Note that USERPARM= for multiple agencies DOES NOT WORK |       *         
* DXUSER    = 32 byte input card from USERPARM=                       *         
* DXUSER+00 = C'XX,XX,XX...' AGYALPHA list if multiple agencies (in   *         
*             update mode only)                                       *         
*                                                                     *         
***********************************************************************         
                                                                                
*INCLUDE ACRECTYP                                                               
*INCLUDE ACLDLANG                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BLDCUR                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE CONVERT                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQ                                                                 
*INCLUDE DMPRTQB                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE GETFACT                                                                
*INCLUDE GETOPT                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE LOGO                                                                   
*INCLUDE LOGOC                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRORATA                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNSCAN                                                                 
*INCLUDE XSORT                                                                  
*&&US                                                                           
*INCLUDE CATCALL                                                                
*&&                                                                             
                                                                                
*INCLUDE AGXCNVX                                                                
*INCLUDE AGXROUTS                                                               
                                                                                
AGXTRACT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*AGXTR**,CLEAR=YES                                         
         USING WORKD,RC            RC=A(Local/Global W/s)                       
                                                                                
         ENTRY READ                                                             
         ENTRY READD                                                            
         ENTRY READHI                                                           
         ENTRY GETIT                                                            
         ENTRY SSB                                                              
         ENTRY SPLITSJ                                                          
         ENTRY SPLIT1R                                                          
         ENTRY SPLITAC                                                          
         ENTRY CHKDATE                                                          
         ENTRY SET1DAT                                                          
         ENTRY SET2DAT                                                          
         ENTRY SETAMNT                                                          
         ENTRY SQUASHIT                                                         
         ENTRY GETSPD                                                           
         ENTRY GETPER                                                           
         ENTRY GETPSD                                                           
         ENTRY GETSJO                                                           
         ENTRY SETSJO                                                           
         ENTRY SETACTV                                                          
         ENTRY SETESTK                                                          
         ENTRY GETXDF                                                           
         ENTRY GETACN                                                           
         ENTRY GETMAP                                                           
         ENTRY GETFAP                                                           
         ENTRY GETJAP                                                           
         ENTRY GETUID                                                           
         ENTRY ADDBUF                                                           
         ENTRY GETBUF                                                           
         ENTRY PUTBUF                                                           
         ENTRY CLIREC                                                           
                                                                                
         LARL  R9,GWORK                                                         
         USING GWORKD,R9           R9=A(Global W/s)                             
         LARL  RA,ADDRESS          RA=A(Common addresses)                       
         USING ADDRESS,RA                                                       
                                                                                
         MVC   ACOMFAC,=A(COMFACS)                                              
         MVC   AIOAREA,=A(IO)                                                   
         MVC   AIOGEN,=A(IOGEN)                                                 
         MVC   APRORATA,=A(PRORATST)                                            
         MVC   AGOBLOCB,=A(GOBLOCKB)                                            
         MVC   AGOXBLCK,=A(GOXBLCKA)                                            
         MVC   AGOBBLCK,=A(GOBBLCKA)                                            
         MVC   ASVREC,=A(SVREC)                                                 
         MVC   ASVOREC,=A(SVOREC)                                               
         MVC   ATYPTAB,=A(TYPTAB)                                               
         MVC   AODBUF,=A(ODBUFFER)                                              
         MVC   AODOBUF,=A(ODBUFOLD)                                             
                                                                                
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
                                                                                
         LA    R1,GLOBALSL                                                      
         LA    RF,LITVALSL                                                      
         CR    RF,R1                                                            
         JNE   *+2                                                              
                                                                                
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         JNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         J     MAIN                                                             
                                                                                
YES      LHI   R1,1                Return CC equal                              
         J     EXITC                                                            
                                                                                
NO       LHI   R1,0                Return CC not equal                          
                                                                                
EXITC    CHI   R1,1                                                             
                                                                                
EXIT     XIT1  ,                   Return                                       
                                                                                
***********************************************************************         
* Main control code                                                   *         
***********************************************************************         
                                                                                
MAIN     JAS   RE,GENINIT                                                       
                                                                                
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         JNE   MCLOSE                                                           
         JAS   RE,PROCOPEN         Open system files                            
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         JNE   MLOAD                                                            
         JAS   RE,PROCLOSE         Close system files                           
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         JNE   MUPDT                                                            
         MVI   PROCMODE,PROCMLQ                                                 
         MVI   PROCMOD2,0                                                       
         MVI   PROCMOD3,0                                                       
         JAS   RE,PROCLOAD         Extract from generic files                   
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         JNE   MSTAT                                                            
         TM    PROCMODE,PROCMUQ                                                 
         JNZ   MUPDT2                                                           
         MVI   PROCMODE,PROCMUQ                                                 
                                                                                
MUPDT2   JAS   RE,PROCUPDT         Extract from recovery files                  
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MSTAT    CLI   DXMODE,DXSTATQ                                                   
         JNE   MFCLOS                                                           
         JAS   RE,PROCSTAT         Send stats from previous steps               
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MFCLOS   CLI   DXMODE,DXCLOXDQ                                                  
         JNE   MEND                                                             
         JAS   RE,PROCFCLO         File close control break                     
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MEND     CLI   DXMODE,DXENDQ                                                    
         JNE   MERR                                                             
         JAS   RE,PROCEND                                                       
         JNE   MERR                                                             
         J     MXIT                                                             
                                                                                
MERR     MVI   RETCODE,0                                                        
                                                                                
MXIT     L     R0,=A(SSB)                                                       
         LA    R1,SSBLQ                                                         
         ICM   RE,B'1111',DXSSB                                                 
         LR    RF,R1                                                            
         MVCL  RE,R0               Copy SSB back to DXTRACT                     
                                                                                
         CLI   RETCODE,0                                                        
         XMOD1 1                                                                
                                                                                
***********************************************************************         
* General initialisation                                              *         
***********************************************************************         
                                                                                
GENINIT  NTR1                                                                   
                                                                                
         L     R1,=V(ADDSIO)       R1=V(A(DDSIO)) in DMDMGRL                    
         MVC   0(4,R1),DXADDSIO    Use DDSIO loaded by DXTRACT                  
         L     R0,=A(SSB)                                                       
         LA    R1,SSBLQ                                                         
         ICM   RE,B'1111',DXSSB                                                 
         LR    RF,R1                                                            
         MVCL  R0,RE               Copy DXTRACT's SSB, Warts and all            
                                                                                
         J     YES                                                              
                                                                                
***********************************************************************         
* DXMODE = DXOPENQ - Open account system files                        *         
***********************************************************************         
                                                                                
PROCOPEN NTR1                                                                   
         L     RE,VUTL             Set UTL SENUM                                
         MVC   4(1,RE),DXSENUM                                                  
                                                                                
         XC    IOKEY,IOKEY         Get DTF address                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,ACCDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          Open system disk files                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILELD,AIOAREA                    
                                                                                
***      MVI   CHAMODE,CHAMCHAQ    <<< Change mode setting:                     
         MVI   CHAMODE,CHAMDARQ    <<< Here for now                             
                                                                                
***      MVI   XPFM,YESQ           <<< Change mode setting:                     
         MVI   XPFM,NOQ            <<< Here for now                             
                                                                                
         JAS   RE,TSAINIT          Acquire storage + init TSAR                  
                                                                                
         L     R0,=A(WCACUML)                                                   
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AWCACUM                                                       
                                                                                
         L     R0,=A(PBULENQ)                                                   
         ST    R0,APBULEN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,APERBUF                                                       
                                                                                
         L     R0,=A(BUFLENQ)                                                   
         ST    R0,ABUFLEN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUFFER                                                       
                                                                                
         L     R0,=A(BUFLENQ)                                                   
         ST    R0,ABUFOLN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUFOLD                                                       
                                                                                
         L     R0,=A(PIDBSIZQ)                                                  
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,APIDBUF                                                       
                                                                                
         LHI   R0,BIGLENQ          See BIGAREA                                  
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ACURBUF                                                       
                                                                                
         LHI   R0,UIDTLNQ*UIDTMAX                                               
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AUIDTAB                                                       
         XC    0(L'UIDTHEX,R1),0(R1)                                            
                                                                                
         SAM31   ,                                                              
         L     R0,=A(EXFMAXLQ)     Max per 1 dataset DCB parm UK/US             
         ST    R0,ACLCBLN          Store length          in DXTRACT             
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ACLCBUF          Compare record buffer                        
         SAM24   ,                                                              
                                                                                
         SAM31   ,                                                              
*        LHI   R0,SJOTLNQ*SJOTMAX                                               
         LHI   R0,SJOTLNQ                                                       
         MHI   R0,SJOTMAX                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ASJOTAB          SJ/Office table                              
         MVI   0(R1),0             (set EoT)                                    
         SAM24   ,                                                              
                                                                                
         JAS   RE,AGLINIT          Init agency list                             
                                                                                
         MVC   IOKEY,GSPACES                                                    
         L     R6,DXSTSPTR         R6=A(Start of system driver table)           
                                                                                
POPEN04  C     R6,DXSTEPTR         Check we haven't reached the end             
         JH    YES                                                              
         CLI   SXDTSYS,ACCSYSQ     Check it is accounting                       
         JNE   POPEN06                                                          
         CLI   SXDTSUB,WAREHSEQ    Check sub system matches                     
         JE    POPEN10                                                          
                                                                                
POPEN06  AHI   R6,SXDTABL          Next table entry                             
         J     POPEN04                                                          
                                                                                
POPEN10  TIME                                                                   
         LTR   R0,R0                                                            
         JZ    *+2                 BAD RETURN FROM MACRO                        
         ST    R0,FULL                                                          
         LR    R4,R1                                                            
*&&US                                                                           
         MVI   BYTE,0              US Mediaocean time offset                    
         MVC   PKWORK2(1),FULL                                                  
         MVI   PKWORK2+1,X'0C'                                                  
         AP    PKWORK2,=P'60'      Add 6 hours to SV_LDTIM                      
         CP    PKWORK2,=P'240'     Check if it's over 24 hours                  
         JL    POPEN12                                                          
         SP    PKWORK2,=P'240'     Yes - subtract 24 hours                      
         MVI   BYTE,1                                                           
                                                                                
POPEN12  DS    0H                                                               
         MVC   FULL(1),PKWORK2                                                  
*&&                                                                             
                                                                                
         L     R1,FULL                                                          
         SRL   R1,28                                                            
         STC   R1,SV_LDTIM                                                      
         OI    SV_LDTIM,X'F0'                                                   
         L     R1,FULL                                                          
         SRL   R1,24                                                            
         STC   R1,SV_LDTIM+1                                                    
         OI    SV_LDTIM+1,X'F0'                                                 
         MVI   SV_LDTIM+2,DOUBLEQ                                               
         L     R1,FULL                                                          
         SRL   R1,20                                                            
         STC   R1,SV_LDTIM+3                                                    
         OI    SV_LDTIM+3,X'F0'                                                 
         L     R1,FULL                                                          
         SRL   R1,16                                                            
         STC   R1,SV_LDTIM+4                                                    
         OI    SV_LDTIM+4,X'F0'                                                 
         MVI   SV_LDTIM+5,DOUBLEQ                                               
         L     R1,FULL                                                          
         SRL   R1,12                                                            
         STC   R1,SV_LDTIM+6                                                    
         OI    SV_LDTIM+6,X'F0'                                                 
         L     R1,FULL                                                          
         SRL   R1,8                                                             
         STC   R1,SV_LDTIM+7                                                    
         OI    SV_LDTIM+7,X'F0'                                                 
                                                                                
         ST    R4,FULL                                                          
*&&US                                                                           
         CLI   BYTE,1                                                           
         JNE   POPEN14                                                          
         GOTO1 VDATCON,DMCB,(6,FULL),(0,DUBNET)                                 
         GOTO1 VADDAY,DMCB,(C'D',DUBNET),DUBCOM,1                               
         GOTO1 VDATCON,DMCB,(0,DUBCOM),(20,DUB)                                 
         J     POPEN16                                                          
*&&                                                                             
                                                                                
POPEN14  GOTO1 VDATCON,DMCB,(6,FULL),(20,DUB)                                   
                                                                                
POPEN16  MVC   SV_LDDAT(4),DUB+0                                                
         MVI   SV_LDDAT+4,MINUSQ                                                
         MVC   SV_LDDAT+5(2),DUB+4                                              
         MVI   SV_LDDAT+7,MINUSQ                                                
         MVC   SV_LDDAT+8(2),DUB+6                                              
                                                                                
POPENX   J     YES                                                              
                                                                                
***********************************************************************         
* DXMODE = DXCLOSEQ - Close account system files                      *         
***********************************************************************         
                                                                                
PROCLOSE NTR1  ,                                                                
                                                                                
         L     R6,DXSTSPTR         R6=A(Start of system driver table)           
                                                                                
PCLOS02  C     R6,DXSTEPTR         Check we haven't reached the end             
         JH    YES                                                              
         CLI   SXDTSYS,ACCSYSQ     Check it is accounting                       
         JNE   PCLOS04                                                          
         CLI   SXDTSUB,WAREHSEQ    Check sub system matches                     
         JE    PCLOS06                                                          
                                                                                
PCLOS04  AHI   R6,SXDTABL          Next table entry                             
         J     PCLOS02                                                          
                                                                                
PCLOS06  GOTO1 APRTSUM             Print summary                                
                                                                                
         L     R0,ABUFOLN                                                       
         L     R1,ABUFOLD                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,ABUFLEN                                                       
         L     R1,ABUFFER                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,APBULEN                                                       
         L     R1,APERBUF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,=A(PIDBSIZQ)                                                  
         L     R1,APIDBUF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         LHI   R0,2*2048           L'SVREC+L'SVOREC                             
         L     R1,ACURBUF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,=A(WCACUML)                                                   
         L     R1,AWCACUM                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,ACLCBLN                                                       
         L     R1,ACLCBUF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         LHI   R0,UIDTLNQ*UIDTMAX                                               
         L     R1,AUIDTAB                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
*        LHI   R0,SJOTLNQ*SJOTMAX                                               
         LHI   R0,SJOTLNQ                                                       
         MHI   R0,SJOTMAX                                                       
         L     R1,ASJOTAB                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     RE,VUTL             Close done by DXTRACT                        
         MVC   4(1,RE),DXSENUM                                                  
         J     YES                                                              
                                                                                
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFILES DC    C'NACCDIR NACCMST NACCARC NACCRCV X'                             
ACFILELD DC    C'NACCDIR NACCMST NACCARC NCTFILE X'                             
VUTL     DC    V(UTL)                                                           
                                                                                
***********************************************************************         
* Process account file data in LOAD mode                              *         
***********************************************************************         
                                                                                
PROCLOAD NTR1                                                                   
                                                                                
         OI    RUNINDS,RUNLOAD                                                  
         MVC   CUR_AGY,SXDTAGB     Set company from system table                
         MVC   COMPANY,CUR_AGY                                                  
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETCPY             Add/get company details from TSAR            
                                                                                
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
                                                                                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
                                                                                
***********************************************************************         
* Process Account file data in Update mode using Recovery records     *         
* If running on multiple agencies, DXTRACT will pass the same RCV rec *         
* for each agency in the list. PROCKEY weeds out mismatches.          *         
***********************************************************************         
                                                                                
PROCUPDT NTR1                                                                   
                                                                                
         OI    RUNINDS,RUNUPDT                                                  
         LLC   R0,AGYLISN                                                       
         LA    RF,AGYLIST          Filter agencies to update this file          
         USING AGYLSTD,RF                                                       
                                                                                
PUPDT02  CLC   AGYLAID,GSPACES                                                  
         JNH   PUPDOK              unwanted                                     
         CLC   AGYLAID,SXDTAGY                                                  
         JE    PUPDT04                                                          
         AHI   RF,AGYLLNQ                                                       
         JCT   R0,PUPDT02                                                       
         J     PUPDOK              unwanted                                     
         DROP  RF                                                               
                                                                                
         USING RCVRECD,R2                                                       
PUPDT04  L     R2,DXARECB          Point to recovery record buffer              
         CLI   XPFM,YESQ           Proces PFM records?                          
         JE    PUPDT06                                                          
         CLI   RCVSEQNO,ROCPYCHG   Offline?                                     
         JE    PUPDT06                                                          
         CLI   RCVSEQNO,ROCHGOLY                                                
         JE    PUPDT06                                                          
         CLC   RCVSEQNO+1(3),=AL3(1)                                            
         JNH   PUPDT06                                                          
         CLI   RCVPRGNO,RCVPPFMQ                                                
         JE    PUPDOK              (=PFM?)                                      
                                                                                
PUPDT06  MVC   CUR_AGY,SXDTAGB     Multiple agencies support                    
         CLI   AGYLISN,1                                                        
         JE    PUPDT12                                                          
         GOTO1 VRECTYP,DMCB,(C'D',RCVRECRD)                                     
                                                                                
         LLC   R0,AGYLISN                                                       
         LA    RF,AGYLIST                                                       
         USING AGYLSTD,RF                                                       
                                                                                
PUPDT08  CLC   AGYLAID,GSPACES                                                  
         JNH   PUPDOK                                                           
         CLC   AGYLBIN,1(R1)                                                    
         JE    PUPDT10                                                          
         AHI   RF,AGYLLNQ                                                       
         JCT   R0,PUPDT08                                                       
         J     PUPDOK              (=PFM?)                                      
         DROP  RF                                                               
                                                                                
PUPDT10  MVC   CUR_AGY,1(R1)                                                    
                                                                                
*UPDT12  MVC   COMPANY,SXDTAGB     Set company code from system table           
PUPDT12  MVC   COMPANY,CUR_AGY     Set company code from system table           
         GOTO1 AGETCPY             Get company details                          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             Set type table data                          
         CLI   RCVFILTY,ACCMSTQ    only want data records                       
         JNE   PUPDOK                                                           
         JAS   RE,PROCKEY          Process record key values                    
         JNE   PUPDOK              unwanted                                     
         MVI   RETBYTE,0                                                        
         L     RF,TYPEAUPD                                                      
         BASR  RE,RF                                                            
         MVC   LT_SIN,RCVSEQNO     Save SIN for next time                       
         MVC   LT_GIN,DXRGIN                                                    
         MVC   LT_ACT,SVACTION                                                  
         MVC   LT_RTYP,RECTYPE                                                  
         JNE   PUPDNO              Exit error                                   
                                                                                
PUPDOK   J     YES                 Ok exit                                      
PUPDNO   J     NO                  Error exit                                   
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Process Account file data in STAT mode                              *         
***********************************************************************         
                                                                                
PROCSTAT NTR1                                                                   
                                                                                
         OPEN  (AUDFILE)           Open tape                                    
                                                                                
PSTAT02  GET   AUDFILE,TREC                                                     
         CLC   TALPHA,SXDTAGY      Match AUDREC to agency alpha                 
         JNE   PSTAT02                                                          
                                                                                
         USING DXSQLD,R2                                                        
         L     R2,DXASQLB                                                       
         LA    R0,DXSQLLEN                                                      
         LHI   R1,DXSQLDL+19+TRECACL                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LHI   RE,DXSQLDL+17+TRECACL                                            
         STCM  RE,B'0011',DXSQLLEN                                              
         MVC   DXSQLTYP,DXSQLDQ                                                 
         MVI   DXSQLRTY-1,MXTRTQ                                                
         MVI   DXSQLRTY,C'S'                                                    
         MVI   DXSQLLIN-1,MXTRTQ                                                
         MVC   DXSQLLIN(14),=CL14'Bulk API Check'                               
         MVI   DXSQLLIN+14,X'7D'                                                
         MVC   DXSQLLIN+15(1),TRECTY                                            
         MVI   DXSQLLIN+16,X'7D'                                                
         MVI   DXSQLLIN+17+TRECACL,MXTRTQ                                       
         MVI   DXSQLLIN+18+TRECACL,MXTRTQ                                       
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     PSTAT02                                                          
                                                                                
PSTATX   CLOSE (AUDFILE)           Close tape                                   
         J     YES                                                              
                                                                                
***********************************************************************         
* Process File Close Control Break mode                               *         
***********************************************************************         
                                                                                
PROCFCLO NTR1                                                                   
                                                                                
                                                                                
                                                                                
PFCLOX   DS    0H                                                               
         J     YES                                                              
                                                                                
***********************************************************************         
* Process Account file data in End mode                               *         
***********************************************************************         
                                                                                
PROCEND  NTR1                                                                   
         C     R6,DXSTEPTR         Check we haven't reached the end             
         JH    *+2                                                              
         CLI   SXDTSYS,ACCSYSQ     Check it is accounting                       
         JNE   *+2                                                              
         CLI   SXDTSUB,WAREHSEQ    Check sub system matches                     
         JNE   *+2                                                              
                                                                                
         MVC   SAVCOMP,SXDTAGB     Save company code                            
         GOTOR APROCCSM            Process client summary records               
                                                                                
PROCENDY J     YES                                                              
                                                                                
***********************************************************************         
* Process key values in Recovery record                               *         
***********************************************************************         
                                                                                
         USING RCVRECD,R2                                                       
PROCKEY  NTR1                                                                   
                                                                                
         GOTO1 VRECTYP,DMCB,(C'D',RCVRECRD)                                     
         MVC   RECTYPE,0(R1)                                                    
         MVI   RECATYP,0                                                        
         CLI   RECTYPE,ACRTAUDT                                                 
         JNE   PKEY060                                                          
         MVC   RECATYP,RCVRECRD+AUDKAUDT-AUDKEY                                 
                                                                                
PKEY060  CLC   COMPANY,1(R1)       Company match?                               
         JNE   PKEYNO                                                           
         CLI   TYPERECT,0          Test record type code                        
         JE    PKEY100                                                          
         CLC   TYPERECT,0(R1)                                                   
         JE    PKEY100                                                          
         CLC   TYPEREC2,0(R1)                                                   
         JE    PKEY100                                                          
         TM    TYPETYPE,AUDITQ     Audit required?                              
         JZ    PKEY080                                                          
         CLI   RECTYPE,ACRTAUDT                                                 
         JNE   PKEYNO                                                           
         CLC   TYPEATYP,RECATYP                                                 
         JNE   PKEYNO                                                           
         J     PKEY100                                                          
                                                                                
PKEY080  CLI   TYPERECT,ACRTTRN    Handle archive                               
         JNE   PKEYNO                                                           
         CLI   0(R1),ACRTTRNA                                                   
         JNE   PKEYNO                                                           
                                                                                
PKEY100  TM    RCVRECRD+ACTRSTAT-ACTKEY,TRNSDELT                                
         JNZ   PKEY102             Test for deleted record                      
*&&UK*&& CLI   RECTYPE,ACRTTRNA    Test for archived transaction                
*&&UK*&& JE    *+12                                                             
         CLI   RECTYPE,ACRTTRN     Test for transaction                         
         JNE   PKEY110                                                          
         TM    RCVRECRD+TRNRSTA2-TRNKEY,TRNSPEEL                                
         JZ    PKEY110             Treat peeled as deleted                      
PKEY102  CLI   DXACTION,C'C'                                                    
         JNE   PKEYNO                                                           
         CLI   RCVRECTY,RCVRCHAQ                                                
         JNE   PKEYNO                                                           
         L     R4,DXACPYB                                                       
         TM    L'RCVHDR+ACTRSTAT-ACTKEY+4(R4),TRNSDELT                          
         JNZ   PKEYNO              Avoid deleted 'Changed'                      
*&&UK*&& CLI   RECTYPE,ACRTTRNA    Test for archived transaction                
*&&UK*&& JE    *+12                                                             
         CLI   RECTYPE,ACRTTRN     Test for transaction                         
         JNE   PKEY104                                                          
         TM    L'RCVHDR+TRNRSTA2-TRNKEY+4(R4),TRNSPEEL                          
         JNZ   PKEYNO              Treat peeled as deleted                      
PKEY104  MVI   DXACTION,C'D'                                                    
         J     PKEYOK                                                           
                                                                                
PKEY110  DS    0H                  Test restored record using saved             
         CLI   RCVRECTY,RCVRCHAQ   recoverey Copy record with Change            
         JNE   PKEYOK              recovery record type not Change              
         L     R4,DXACPYB                                                       
         TM    L'RCVHDR+ACTRSTAT-ACTKEY+4(R4),TRNSDELT                          
         JNZ   PKEY112                                                          
*&&UK*&& CLI   RECTYPE,ACRTTRNA    Test for archived transaction                
*&&UK*&& JE    *+12                                                             
         CLI   RECTYPE,ACRTTRN     Test for transaction                         
         JNE   PKEY120                                                          
         TM    L'RCVHDR+TRNRSTA2-TRNKEY+4(R4),TRNSPEEL                          
         JZ    PKEY120             Treat peeled as deleted                      
PKEY112  MVI   DXACTION,C'A'                                                    
         J     PKEYOK                                                           
                                                                                
PKEY120  CLI   RECTYPE,ACRTTRN     'Draft to Live' check (see FILTTRN)          
         JNE   PKEYOK                                                           
*&&UK                                                                           
         CLC   RCVRECRD+TRNKUNT-TRNKEY(2),PRODUL                                
         JNE   *+12                                                             
         CLI   RCVRECRD+TRNRSTYP-TRNKEY,TRNTFBAD                                
         JE    PKEYOK              (advance billing on SJ included)             
*&&                                                                             
         TM    RCVRECRD+TRNRSTAT-TRNKEY,TRNSDRFT                                
         JNZ   PKEYOK              (every other draft skipped)                  
         L     R4,DXACPYB                                                       
         TM    L'RCVHDR+TRNRSTAT-TRNKEY+4(R4),TRNSDRFT                          
         JZ    PKEYOK                                                           
         MVI   DXACTION,C'A'       Change action to Add                         
                                                                                
PKEYOK   J     YES                                                              
PKEYNO   J     NO                                                               
                                                                                
***********************************************************************         
* Initialise agency alpha list                                        *         
* Load:   S/b 'AGENCY=XX' card in JCL, so SXDTAGY must be = 'XX'      *         
* Update: If 'DXUSER=AA,BB,..' card build list, else like Load above  *         
***********************************************************************         
                                                                                
AGLINIT  NTR1                                                                   
                                                                                
         MVC   AGYLIST(2),SXDTAGY  Default to supplied agency alpha             
         MVI   AGYLISN,1                                                        
         CLC   DXUSER,GSPACES      Else agency alpha list (update only)         
         JNH   YES                 empty - use default AGENCY= card             
         LA    RE,DXUSER                                                        
         LA    RF,AGYLIST                                                       
         USING AGYLSTD,RF                                                       
         LHI   R0,AGYLMAXQ         MAX N'ENTRIES                                
                                                                                
AGLIN02  CLC   0(2,RE),GSPACES     EoL                                          
         JNH   AGLIN04                                                          
         MVC   AGYLAID,0(RE)       Save agency alpha (minus comma)              
         AHI   RE,3                                                             
         AHI   RF,AGYLLNQ                                                       
         JCT   R0,AGLIN02                                                       
         DROP  RF                                                               
                                                                                
AGLIN04  LHI   RF,AGYLMAXQ                                                      
         SR    RF,R0                                                            
         STC   RF,AGYLISN          (# of entries)                               
                                                                                
         LLC   R4,AGYLISN                                                       
         LA    R3,AGYLIST                                                       
         USING AGYLSTD,R3                                                       
                                                                                
         USING CT5REC,R2                                                        
AGLIN06  L     R2,AIOAREA                                                       
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYLAID                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                        
         CLI   8(R1),0                                                          
         JE    AGLIN10                                                          
         CLI   8(R1),X'10'                                                      
         JE    AGLIN20                                                          
         DC    H'0'                Die on any other I/O error                   
                                                                                
AGLIN10  LA    R2,CT5DATA          Find system                                  
         MVI   BYTE,0                                                           
                                                                                
         USING CTSYSD,R2                                                        
AGLIN12  CLI   CTSYSEL,0           EoR?                                         
         JE    AGLIN20                                                          
         CLI   CTSYSEL,CTSYSELQ                                                 
         JE    AGLIN16                                                          
                                                                                
AGLIN14  LLC   R0,CTSYSLEN                                                      
         AR    R2,R0                                                            
         J     AGLIN12                                                          
                                                                                
AGLIN16  CLC   CTSYSNUM,SXDTSYS    SXDTSYS set by DXTRACT yet?                  
         JNE   AGLIN14                                                          
         MVC   AGYLBIN,CTSYSAGB                                                 
                                                                                
AGLIN20  AHI   R3,AGYLLNQ                                                       
         JCT   R4,AGLIN06                                                       
                                                                                
         LA    R3,AGYLIST                                                       
         LLC   R4,AGYLISN                                                       
                                                                                
AGLIN22  MVC   COMPANY,AGYLBIN                                                  
         GOTO1 AGETCPY                                                          
         AHI   R3,AGYLLNQ                                                       
         JCT   R4,AGLIN22                                                       
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
* TSAR initialisation                                                 *         
***********************************************************************         
                                                                                
TSAINIT  NTR1                                                                   
                                                                                
         MVC   DUB,=CL8'T00A5D'    Load in TSAR                                 
         GOTO1 VLOADER,DMCB,DUB,0,0                                             
         OC    VTSAR,4(R1)                                                      
         JZ    *+2                 (failed)                                     
                                                                                
         GOTO1 AINIBUF                                                          
                                                                                
         J     YES                                                              
                                                                                
***********************************************************************         
* Print summary report to pq                                          *         
***********************************************************************         
                                                                                
PRTSUM   NTR1                                                                   
                                                                                
* see APXTRACT.PRTSUM for report to pq handling                                 
                                                                                
         USING PLINED,R6                                                        
         L     R6,VCPRINT                                                       
                                                                                
         MVI   COMPANY,FSTCPYQ                                                  
                                                                                
         MVC   PLINED(132),GSPACES                                              
         MVC   PLINED(30),=CL30'*** PRTSUM TOTALS ***'                          
         ZAP   EDDUB,PZERO                                                      
                                                                                
         GOTO1 VPRINTER                                                         
                                                                                
AGY      USING OB_D,OBTTAREA                                                    
PSUM02   DS    0H                                                               
         LA    R0,OBTTAREA                                                      
         LHI   R1,OB_LNQ-1                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   AGY.OB_KCPY,COMPANY                                              
         GOTOR AGETBUF,AGY.OB_D    Get company via TSAR                         
         JNE   PSUM10                                                           
                                                                                
         MVC   PLINED(132),GSPACES                                              
                                                                                
         GOTO1 VPRINTER                                                         
                                                                                
PSUM04   LA    R2,AGY.OB_RC300                                                  
         LARL  R4,PSLABTAB                                                      
         XR    R5,R5                                                            
                                                                                
PSUM06   MVC   PLINED(132),GSPACES                                              
         MVC   PLINAGY,AGY.OB_CPY+SAVCALP-SAVCPY                                
         MVC   PLINCOD,00(R4)                                                   
         MVC   PLINTYP,10(R4)                                                   
         EDITR (P6,0(R2)),(L'PLINNUM,PLINNUM),0,ZERO=NOBLANK                    
                                                                                
         GOTO1 VPRINTER                                                         
                                                                                
PSUM08   AP    EDDUB,0(L'OB_RC300,R2)                                           
         AHI   R2,L'OB_RC300                                                    
         AHI   R4,PSLABTLQ                                                      
         AHI   R5,1                                                             
         CHI   R5,OB_RCMAX                                                      
         JL    PSUM06                                                           
                                                                                
PSUM10   LLC   R1,COMPANY                                                       
         AHI   R1,1                                                             
         CHI   R1,FFQ                                                           
         JE    PSUM12                                                           
         STC   R1,COMPANY                                                       
         J     PSUM02                                                           
                                                                                
PSUM12   MVC   PLINED(132),GSPACES                                              
         MVC   PLINAGY(13),=C'Control total'                                    
         EDITR (P8,EDDUB),(L'PLINNUM,PLINNUM),0,ZERO=NOBLANK                    
                                                                                
                                                                                
PSUM14   GOTO1 VPRINTER                                                         
                                                                                
         MVC   PLINED(132),GSPACES                                              
                                                                                
         GOTO1 VPRINTER                                                         
                                                                                
PSUM16   DS    0H                                                               
         DROP  AGY,R6                                                           
                                                                                
PRTSUMX  J     YES                                                              
                                                                                
PSLABTAB DS    0H                                                               
         DC    CL10'AGXCPYDQ',CL5'05300'                                        
PSLABTLQ EQU   *-PSLABTAB                                                       
         DC    CL10'AGXCLIDQ',CL5'05301'                                        
         DC    CL10'AGXPRODQ',CL5'05302'                                        
         DC    CL10'AGXJOBDQ',CL5'05303'                                        
         DC    CL10'AGXETYDQ',CL5'05304'                                        
         DC    CL10'AGXWCODQ',CL5'05305'                                        
         DC    CL10'AGXCATDQ',CL5'05306'                                        
         DC    CL10'AGXACCDQ',CL5'05307'                                        
         DC    CL10'AGXPERDQ',CL5'05308'                                        
         DC    CL10'AGXEST2Q',CL5'05309'                                        
         DC    CL10'AGXTRNDQ',CL5'05310'                                        
         DC    CL10'AGXTRNFQ',CL5'05311'                                        
         DC    CL10'AGXORDDQ',CL5'05312'                                        
         DC    CL10'AGXORDFQ',CL5'05313'                                        
         DC    CL10'AGXESTDQ',CL5'05314'                                        
         DC    CL10'AGXESTFQ',CL5'05315'                                        
         DC    CL10'AGXEXPDQ',CL5'05316'                                        
         DC    CL10'AGXEXPFQ',CL5'05317'                                        
         DC    CL10'AGXTIMDQ',CL5'05318'                                        
         DC    CL10'AGXTIMFQ',CL5'05319'                                        
         DC    CL10'AGXITMDQ',CL5'05320'                                        
         DC    CL10'AGXCURDQ',CL5'05321'                                        
         DC    CL10'AGXETXDQ',CL5'05322'                                        
         DC    CL10'AGXORD2Q',CL5'05323'                                        
         DC    CL10'AGXBILDQ',CL5'05324'                                        
         DC    CL10'AGXTIMRQ',CL5'05325'                                        
         DC    CL10'AGXEXPRQ',CL5'05326'                                        
         DC    CL10'AGXORDSQ',CL5'05327'                                        
         DC    CL10'AGXESTSQ',CL5'05328'                                        
         DC    CL10'AGXOTXDQ',CL5'05329'                                        
         DC    CL10'AGXOFFDQ',CL5'05330'                                        
         DC    CL10'AGXCUTDQ',CL5'05336'                                        
         DC    CL10'AGXXTRFQ',CL5'05337'                                        
         DC    CL10'AGXTOFFQ',CL5'05339'                                        
         DC    CL10'AGXCSMFQ',CL5'05340'                                        
         DC    AL1(FFQ)                                                         
                                                                                
***********************************************************************         
* Common addresses for all routines - covered by ADDRESS DSECT        *         
* (ADDRESS is at AADDRESS in w/s)                                     *         
***********************************************************************         
                                                                                
**********************************************************************          
* Literals                                                           *          
**********************************************************************          
                                                                                
ADDRESS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG                                                                  
                                                                                
ACCMSTQ  EQU   X'6A'                                                            
MXTRTQ   EQU   X'5E'               Field separator                              
ONEK     EQU   1024                                                             
                                                                                
AUDFILE  DCB   DDNAME=AUDFILE,DSORG=PS,RECFM=FB,LRECL=604,BLKSIZE=3020,X        
               MACRF=(PM,GM),EODAD=PSTATX                                       
                                                                                
DDNAME   DC    C'AUDFILE '                                                      
DSSRT    DC    C'ACCGX.'                                                        
DSNAM    DS    CL30' '                                                          
NOCYS    DC    XL6'000004000002'                                                
DTSYSPRG DC    C'AGX'              System and program ID                        
REPLIT   DC    C'Bulk API'         PQ report title                              
BC01     DC    C'BC01'                                                          
PRTCLOSE DC    C'CLOSE'                                                         
ERRCPYID DC    C'**WARNING** Invalid ID on Company record nn'                   
                                                                                
***********************************************************************         
* Common addresses for all routines                                   *         
***********************************************************************         
                                                                                
         DS    0D                                                               
                                                                                
         DC    CL8'Externs'        External routine addresses                   
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VBLDCUR  DC    V(BLDCUR)                                                        
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VRECTYP  DC    V(ACRECTYP)                                                      
VLDLANG  DC    V(LDLANG)                                                        
VMASTC   DC    V(MASTC)                                                         
VCPRINT  DC    V(CPRINT)                                                        
CUREDIT  DC    V(CUREDIT)                                                       
VDICTATE DC    V(DICTATE)                                                       
VPRINTER DC    V(PRINTER)                                                       
VPRINT   DC    V(PPGPRINT)                                                      
VCHOPPER DC    V(CHOPPER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VBOXAREA DC    V(BOXAREA)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
VPQBUFF  DC    V(PQBUFF)                                                        
VPQOPEN  DC    V(PQOPEN)                                                        
VLOADER  DC    V(LOADER)                                                        
VMQRPT   DC    V(MQRPT)                                                         
VSQUASHR DC    V(SQUASHER)                                                      
VSCANNER DC    V(SCANNER)                                                       
*&&US                                                                           
VCATCALL DC    V(CATCALL)                                                       
*&&                                                                             
                                                                                
VAGXCNVX DC    V(AGXCNVX)          A(Routines from AGXROUTS)                    
VAGXCPYC DC    V(AGXCPYC)                                                       
VAGXOFFC DC    V(AGXOFFC)                                                       
VAGXCLIC DC    V(AGXCLIC)                                                       
VAGXPROC DC    V(AGXPROC)                                                       
VAGXJOBC DC    V(AGXJOBC)                                                       
VAGXETYC DC    V(AGXETYC)                                                       
VAGXWCOC DC    V(AGXWCOC)                                                       
VAGXCATC DC    V(AGXCATC)                                                       
VAGXACCC DC    V(AGXACCC)                                                       
VAGXPERC DC    V(AGXPERC)                                                       
VAGXTRNC DC    V(AGXTRNC)                                                       
VAGFTRNC DC    V(AGFTRNC)                                                       
VAGFXTRC DC    V(AGFXTRC)                                                       
VAGXORDC DC    V(AGXORDC)                                                       
VAGFORDC DC    V(AGFORDC)                                                       
VAGXESTC DC    V(AGXESTC)                                                       
VAGFESTC DC    V(AGFESTC)                                                       
VAGXEXPC DC    V(AGXEXPC)                                                       
VAGFEXPC DC    V(AGFEXPC)                                                       
VAGXTIMC DC    V(AGXTIMC)                                                       
VAGFTIMC DC    V(AGFTIMC)                                                       
VAGXITMC DC    V(AGXITMC)                                                       
VAGXCURC DC    V(AGXCURC)                                                       
VAGXCUTC DC    V(AGXCUTC)                                                       
VAGXETXC DC    V(AGXETXC)                                                       
VAGXOTXC DC    V(AGXOTXC)                                                       
VAG2ESTC DC    V(AG2ESTC)                                                       
VAG2ORDC DC    V(AG2ORDC)                                                       
VAGXBILC DC    V(AGXBILC)                                                       
VAGRTIMC DC    V(AGRTIMC)                                                       
VAGREXPC DC    V(AGREXPC)                                                       
VAGFTOFC DC    V(AGFTOFC)                                                       
                                                                                
         DC    CL8'For all'        Common routines used by all subs             
AACCLOAD DC    A(ACCLOAD)                                                       
AACCUPDT DC    A(ACCUPDT)                                                       
ADECIOC  DC    A(DECIOC)                                                        
ACHKSEQ  DC    A(CHKSEQIO)                                                      
AGETTYP  DC    A(GETTYP)                                                        
AGETIT   DC    A(GETIT)                                                         
AREAD    DC    A(READ)                                                          
AREADD   DC    A(READD)                                                         
AREADHI  DC    A(READHI)                                                        
ARECNUMS DC    A(RECNUMS)                                                       
APRTSUM  DC    A(PRTSUM)                                                        
AINIBUF  DC    A(INIBUF)                                                        
AADDBUF  DC    A(ADDBUF)                                                        
AGETBUF  DC    A(GETBUF)                                                        
AGETNXT  DC    A(GETNXT)                                                        
APUTBUF  DC    A(PUTBUF)                                                        
AGETCPY  DC    A(GETCPY)                                                        
AGETACN  DC    A(GETACN)                                                        
APROCCSM DC    A(PROCCSM)                                                       
ASETAMNT DC    A(SETAMNT)                                                       
ASETACTV DC    A(SETACTV)                                                       
ASETESTK DC    A(SETESTK)                                                       
                                                                                
         DC    CL8'Loading'        Addresses of LOAD routines                   
ADUMMYL  DC    A(0)                (dummy)                                      
                                                                                
         DC    CL8'Updatng'        Addresses of UPDATE routines                 
ADUMMYU  DC    A(0)                (dummy)                                      
                                                                                
         DC    CL8'Filters'        Addresses of FILTER routines                 
AFILTCPY DC    A(FILTCPY)                                                       
AFILTCLI DC    A(FILTCLI)                                                       
AFILTPRO DC    A(FILTPRO)                                                       
AFILTJOB DC    A(FILTJOB)                                                       
AFILTETY DC    A(FILTETY)                                                       
AFILTWCO DC    A(FILTWCO)                                                       
AFILTOFF DC    A(FILTOFF)                                                       
AFILTCAT DC    A(FILTCAT)                                                       
AFILTPER DC    A(FILTPER)                                                       
AFILTACC DC    A(FILTACC)                                                       
AFILTTRN DC    A(FILTTRN)                                                       
AFILTXTR DC    A(FILTXTR)                                                       
AFILTORD DC    A(FILTORD)                                                       
AFILTEST DC    A(FILTEST)                                                       
AFILTEXP DC    A(FILTEXP)                                                       
AFILTTIM DC    A(FILTTIM)                                                       
AFILTITM DC    A(FILTITM)                                                       
AFILTCUR DC    A(FILTCUR)                                                       
AFILTTOF DC    A(FILTTOF)                                                       
                                                                                
         DC    CL8'Initials'       Addresses of INIT routines                   
AINITALL DC    A(INITALL)                                                       
AINITCPY DC    A(INITCPYD)                                                      
AINITCLI DC    A(INITCLID)                                                      
AINITPRO DC    A(INITPROD)                                                      
AINITJOB DC    A(INITJOBD)                                                      
AINITETY DC    A(INITETYD)                                                      
AINITWCO DC    A(INITWCOD)                                                      
AINITOFF DC    A(INITOFFD)                                                      
AINITCAT DC    A(INITCATD)                                                      
AINITPER DC    A(INITPERD)                                                      
AINITACC DC    A(INITACCD)                                                      
AINITTRN DC    A(INITTRND)                                                      
AINITRNF DC    A(INITTRNF)                                                      
AINIXTRF DC    A(INITXTRF)                                                      
AINITORD DC    A(INITORDD)                                                      
AINIORDF DC    A(INITORDF)                                                      
AINITEST DC    A(INITESTD)                                                      
AINIESTF DC    A(INITESTF)                                                      
AINITEXP DC    A(INITEXPD)                                                      
AINIEXPF DC    A(INITEXPF)                                                      
AINITTIM DC    A(INITTIMD)                                                      
AINITIMF DC    A(INITTIMF)                                                      
AINITITM DC    A(INITITMD)                                                      
AINITCUR DC    A(INITCURD)                                                      
AINITETX DC    A(INITETXD)                                                      
AINITOTX DC    A(INITOTXD)                                                      
AINITES2 DC    A(INITESD2)                                                      
AINITOR2 DC    A(INITORD2)                                                      
AINITBIL DC    A(INITBILD)                                                      
AINITIMR DC    A(INITTIMR)                                                      
AINIEXPR DC    A(INITEXPR)                                                      
AINITCUT DC    A(INITCUTD)                                                      
AINITTOF DC    A(INITTOFF)                                                      
                                                                                
DMDA     DC    F'0'                DMDA                                         
DTFADDR  DC    F'0'                DTF                                          
                                                                                
VTSAR    DS    A                                                                
TSARINFL DS    CL1                 TSAR INITIALISED FLAG                        
TSARINFY EQU   C'Y'                YES                                          
                                                                                
         DS    0D                                                               
GWORK    DS    XL(GWORKDL-GLOBALSL)                                             
                                                                                
GLOBALS  DC    C'OPEN   '                                                       
         DC    C'DMREAD '                                                       
         DC    C'DMRSEQ '                                                       
         DC    C'DMRDHI '                                                       
         DC    C'DMCLSE '                                                       
         DC    C'DMFAST '                                                       
         DC    C'DMPRINT'                                                       
         DC    C'PRTQUE '                                                       
         DC    C'GETREC '                                                       
         DC    C'RECOVER'                                                       
         DC    C'CONTROL'                                                       
         DC    C'CTFILE '                                                       
         DC    C'ACCDIR '                                                       
         DC    C'ACCMST '                                                       
         DC    C'ACCARC '                                                       
         DC    256C' '                                                          
         DC    PL1'0'                                                           
         DC    PL1'-1'                                                          
         DC    PL1'1'                                                           
         DC    PL2'10'                                                          
         DC    PL2'100'                                                         
         DC    C'000000'                                                        
         DC    C'MIX'                                                           
         DC    C'GBP'                                                           
         DC    C'USD'                                                           
         DC    C'EU'                                                            
         DC    C'NA'                                                            
         DC    C'ALL'                                                           
         DC    C'SJ'                                                            
         DC    C'SE'                                                            
         DC    C'SA'                                                            
*&&UK*&& DC    C'SQ'                                                            
*&&US*&& DC    C'SB'                                                            
         DC    C'SC'                                                            
         DC    C'SG'                                                            
         DC    C'SI'                                                            
         DC    C'SK'                                                            
*&&US*&& DC    C'SQ'                                                            
         DC    C'SR'                                                            
         DC    C'SV'                                                            
         DC    C'SX'                                                            
         DC    C'SF'                                                            
*&&US*&& DC    C'SW'                                                            
*&&US*&& DC    C'SY'                                                            
         DC    C'1C'                                                            
         DC    C'1N'                                                            
         DC    C'1R'                                                            
         DC    C'2D'                                                            
         DC    C'2P'                                                            
         DC    X'00'                                                            
         DC    X'00'                                                            
         DC    C'99'                                                            
GLOBALSL EQU   *-GLOBALS                                                        
                                                                                
*PREFIX=ACC                                                                     
       ++INCLUDE AGXRECID                                                       
*PREFIX=                                                                        
                                                                                
       ++INCLUDE DXRECID                                                        
                                                                                
         DS    0D                                                               
TSAROBUF DC    (TSPXTNL)X'00'      TSAR block for optimisation buffer           
                                                                                
AGYLIST  DC    (AGYLSTLQ)C' '      Agency alpha list if multiple agies          
AGYLISN  DC    X'00'               # of agencies                                
                                                                                
CUR_AGY  DC    XL1'00'                                                          
                                                                                
IO       DC    2048X'00'                                                        
                                                                                
IOGEN    DC    2048X'00'                                                        
                                                                                
PRORATST DC    (JBLOCKL)X'00'      PRORATA & GETOPT storage                     
                                                                                
GOBLOCKB DC    (GOBLOCKX-GOBLOCK)X'00'     (400)                                
GOXBLCKA DS    (GOXBLKX-GOXBLOCK)X'00'     (400)                                
GOBBLCKA DS    (GOBBLKXX-GOBBLOCK)X'00'    (600)                                
                                                                                
BIGAREA  DS    0X                  Note: Start of ABIGAREA                      
SVREC    DC    2048X'00'                                                        
                                                                                
SVOREC   DC    2048X'00'                                                        
                                                                                
XSPARE   DC    6144X'00'                                                        
BIGAREAX DS    0X                  Note: End of ABIGAREA                        
                                                                                
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
                                                                                
* AGXTYPTAB                                                                     
       ++INCLUDE AGXTYPTAB                                                      
                                                                                
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
                                                                                
         USING DXBLOCKD,R7                                                      
         USING SXDTABD,R6                                                       
CHKSEQIO NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R2,DTFADDR                                                       
         USING ISDTF,R2                                                         
         L     RF,ISPDKEY                                                       
         LH    RE,ISKEYLN1                                                      
         EX    RE,SEQCLC                                                        
         JNE   NO                                                               
         J     YES                                                              
                                                                                
SEQCLC   CLC   IOKEY(0),0(RF)                                                   
*EQCLC   CLC   0(0,R2),0(RF)       Bad code inherited from APXTRACT|            
         DROP  R2,R6,R7                                                         
                                                                                
***********************************************************************         
* Get type table action from 3 character code                         *         
***********************************************************************         
                                                                                
GETTYP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TYPTABD,RF                                                       
         L     RF,ATYPTAB                                                       
                                                                                
GTYP02   CLI   0(RF),FFQ           EoT?                                         
         JE    *+2                                                              
                                                                                
         CLC   TYPECODE,TYPNAME    Compare namne                                
         JE    GTYP04                                                           
         AHI   RF,TYPTABLQ         next entry                                   
         J     GTYP02                                                           
                                                                                
GTYP04   MVC   TYPENAME,TYPNAME    Save table enry information                  
         MVC   TYPERECT,TYPRTYP                                                 
         MVC   TYPEREC2,TYPRTY2                                                 
         MVC   TYPETYPE,TYPTYPE                                                 
         MVC   TYPETYP2,TYPTYPE2                                                
         MVC   TYPEVERS,TYPVERS                                                 
         MVC   TYPEATYP,TYPATYP                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
         DROP  RF                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Decrement maximum IO count                                          *         
***********************************************************************         
                                                                                
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,B'1111',MAXIOS                                                
         SHI   RF,1                                                             
         STCM  RF,B'1111',MAXIOS                                                
         JNZ   YES                                                              
                                                                                
         LA    R3,DECMSG           Output IO count exceeded message             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
                                                                                
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO count exceeded - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
                                                                                
***********************************************************************         
* Split into SJ levels                                                *         
* Entry: R1           = A(ACTKACT)                                    *         
* Exit:  WORK+30+00   = Client code                                   *         
*        WORK+30+08   = Product code                                  *         
*        WORK+30+16   = Job code                                      *         
***********************************************************************         
                                                                                
SPLITSJ  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK+30(8+8+8),GSPACES                                           
         MVC   WORK(L'ACTKACT),0(R1)                                            
         MVC   WORK+L'ACTKACT(8),GSPACES                                        
         LA    R2,WORK                                                          
                                                                                
         LLC   RE,CLILEN                                                        
         SHI   RE,1                                                             
         MVC   WORK+30(0),0(R2)                                                 
         EX    RE,*-6                                                           
                                                                                
         LA    R2,1(RE,R2)                                                      
         LLC   RE,PROLEN                                                        
         SHI   RE,1                                                             
         MVC   WORK+30+8(0),0(R2)                                               
         EX    RE,*-6                                                           
                                                                                
         LA    R2,1(RE,R2)                                                      
         LLC   RE,JOBLEN                                                        
         SHI   RE,1                                                             
         MVC   WORK+30+16(0),0(R2)                                              
         EX    RE,*-6                                                           
                                                                                
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Split account into level accounts                                   *         
* Entry: R1           = A(ACTKULA)                                    *         
* Exit:  WORK+25      = Level number of account (1-4)                 *         
*        WORK+30+00   = Level 1 account                               *         
*        WORK+30+15   = Level 2 account                               *         
*        WORK+30+30   = Level 3 account                               *         
*        WORK+30+45   = Level 4 account                               *         
***********************************************************************         
                                                                                
SPLITAC  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK+30(4*15),GSPACES                                            
         MVI   WORK+25,C'0'                                                     
                                                                                
         LR    R2,R1                                                            
         LA    R3,SAVCLVLA                                                      
                                                                                
SPLITAC2 CLI   0(R3),0             EoT?                                         
         JE    SPLITACX                                                         
         CLC   0(2,R3),0(R2)       Match on U/L?                                
         JE    SPLITAC4                                                         
         AHI   R3,6                                                             
         J     SPLITAC2                                                         
                                                                                
SPLITAC4 CLI   2(R3),0             Level 1 length                               
         JNH   SPLITAC6                                                         
         LLC   RE,2(R3)                                                         
         AHI   RE,2-1                                                           
         MVC   WORK+30+00(0),0(R2)                                              
         EX    RE,*-6                                                           
                                                                                
         CLI   3(R3),0             Level 2 length                               
         JNH   SPLITAC6                                                         
         LLC   RE,3(R3)                                                         
         AHI   RE,2-1                                                           
         MVC   WORK+30+15(0),0(R2)                                              
         EX    RE,*-6                                                           
                                                                                
         CLI   4(R3),0             Level 3 length                               
         JNH   SPLITAC6                                                         
         LLC   RE,4(R3)                                                         
         AHI   RE,2-1                                                           
         MVC   WORK+30+30(0),0(R2)                                              
         EX    RE,*-6                                                           
                                                                                
         CLI   5(R3),0             Level 4 length                               
         JNH   SPLITAC6                                                         
         LLC   RE,5(R3)                                                         
         AHI   RE,2-1                                                           
         MVC   WORK+30+45(0),0(R2)                                              
         EX    RE,*-6                                                           
                                                                                
SPLITAC6 MVI   WORK+25,C'1'        Determine account level 1-4                  
         CLC   0(L'ACTKULA,R2),WORK+30+00                                       
         JE    SPLITACX                                                         
         MVI   WORK+25,C'2'                                                     
         CLC   0(L'ACTKULA,R2),WORK+30+15                                       
         JE    SPLITACX                                                         
         MVI   WORK+25,C'3'                                                     
         CLC   0(L'ACTKULA,R2),WORK+30+30                                       
         JE    SPLITACX                                                         
         MVI   WORK+25,C'4'                                                     
         CLC   0(L'ACTKULA,R2),WORK+30+45                                       
         JNE   *+2                 ???                                          
                                                                                
SPLITACX DS    0H                                                               
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Split into 1R levels                                                *         
* Entry: R1           = A(ACTKACT)                                    *         
* Exit:  WORK+00   = Office code                                      *         
*        WORK+05   = Department code                                  *         
*        WORK+10   = Sub department code                              *         
*        WORK+15   = Person code                                      *         
***********************************************************************         
                                                                                
SPLIT1R  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK+00(6+6+6+10),GSPACES                                        
         MVC   WORK+30(L'ACTKACT),0(R1)                                         
         MVC   WORK+30+L'ACTKACT(10),GSPACES                                    
                                                                                
         LA    R2,WORK+30                                                       
                                                                                
         LLC   RE,OFFLEN           1R office length                             
         SHI   RE,1                                                             
         MVC   WORK+0(0),0(R2)                                                  
         EX    RE,*-6                                                           
                                                                                
         LA    R2,1(RE,R2)                                                      
         LLC   RE,DEPLEN           1R department length                         
         SHI   RE,1                                                             
         MVC   WORK+6(0),0(R2)                                                  
         EX    RE,*-6                                                           
                                                                                
         LA    R2,1(RE,R2)                                                      
         LLC   RE,SUBLEN           1R sub-department length                     
         SHI   RE,1                                                             
         MVC   WORK+12(0),0(R2)                                                 
         EX    RE,*-6                                                           
                                                                                
         LA    R2,1(RE,R2)                                                      
         LLC   RE,PERLEN           1R person length                             
         SHI   RE,1                                                             
         MVC   WORK+18(0),0(R2)                                                 
         EX    RE,*-6                                                           
                                                                                
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Set estimate internal key                                           *         
* Entry: R1           = ESTKEY                                        *         
* Exit:  SAV_ECKY     = 13 byte internal key                          *         
***********************************************************************         
                                                                                
SETESTK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ESTRECD,R2                                                       
         LR    R2,R1                                                            
                                                                                
         MVC   WORK+00(20),GSPACES                                              
                                                                                
         LA    R1,WORK+00                                                       
         LLC   RE,CLILEN                                                        
         SHI   RE,1                                                             
         MVC   0(0,R1),ESTKCLI                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RE,PROLEN                                                        
         SHI   RE,1                                                             
         MVC   0(0,R1),ESTKPRO                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         MVC   0(L'ESTKJOB,R1),ESTKJOB                                          
                                                                                
         MVC   WORK+00+12(1),ESTKLNO                                            
                                                                                
         MVC   SAV_ECKY,WORK+00                                                 
                                                                                
         CLI   ESTKPRO,ESTKBPQ     Set ballpark estimates                       
         JE    SETESTKB                                                         
         CLI   ESTKJOB,ESTKBPQ                                                  
         JNE   EXIT                                                             
                                                                                
SETESTKB OI    SAV_EIND,SAV_EIBQ                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Set generalised activity timestamp                                  *         
* (Uses standard recovery file date/time now in UPDATE mode)          *         
* *ntry: R1           = A(3 byte date type 1 + 3 byte time type PL3)  *         
* Exit:  WORK+00      = Activity                                      *         
*        R1=1 will set WORK in LOAD mode, too                                   
***********************************************************************         
                                                                                
SETACTV  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK+00(20),GSPACES                                              
*        MVC   WORK+30(6),0(R1)                                                 
*                                                                               
*        OC    WORK+30(6),WORK+30                                               
*        JZ    SETACTVX                                                         
*                                                                               
*        GOTO1 =V(DATCON),DMCB,(1,WORK+30),(23,WORK+00)                         
*                                                                               
*        MVC   WORK+40(3),WORK+30+3                                             
*                                                                               
*        LHI   RF,3                                                             
*        LA    R1,WORK+40                                                       
*                                                                               
*ETACTV2 MVC   BYTE,0(R1)          ensure second nibble can't higher            
*        NI    BYTE,X'0F'          than nine for hours mins secs                
*        CLI   BYTE,9                                                           
*        JNH   SETACTV4                                                         
*        NI    0(R1),X'F0'         remove if it is and make zero                
*                                                                               
*ETACTV4 AHI   R1,1                                                             
*        JCT   RF,SETACTV2                                                      
*                                                                               
*        MVI   WORK+40+3,X'0D'     trick into being packed decimal              
*        UNPK  WORK+50(6+1),WORK+40(3+1)                                        
*        MVC   WORK+11+00(2),WORK+50+00                                         
*        MVI   WORK+11+02,DOUBLEQ                                               
*        MVC   WORK+11+03(2),WORK+50+02                                         
*        MVI   WORK+11+05,DOUBLEQ                                               
*        MVC   WORK+11+06(2),WORK+50+04                                         
                                                                                
         CHI   R1,1                CUT dimension use run date/time              
         JE    SETACTV2                                                         
                                                                                
         TM    PROCMODE,PROCMUQ    Null return if not UPDATE mode               
*        JZ    SETACTV2                                                         
         JZ    SETACTVX                                                         
                                                                                
***      MVC   WORK(L'SV_REDAT),SV_REDAT                                        
***      MVC   WORK+L'SV_REDAT+1(L'SV_RETIM),SV_RETIM                           
*        MVC   WORK(L'SV_LDDAT),SV_LDDAT                                        
*        MVC   WORK+L'SV_LDDAT+1(L'SV_LDTIM),SV_LDTIM                           
         MVC   WORK(L'SV_REDAT),SV_REDAT use recovery date/time                 
         MVC   WORK+L'SV_REDAT+1(L'SV_RETIM),SV_RETIM                           
         J     SETACTVX                                                         
                                                                                
SETACTV2 CHI   R1,1                                                             
         JNE   SETACTVX                                                         
                                                                                
         MVC   WORK(L'SV_LDDAT),SV_LDDAT                                        
         MVC   WORK+L'SV_LDDAT+1(L'SV_LDTIM),SV_LDTIM                           
                                                                                
SETACTVX DS    0H                                                               
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Date check                                                          *         
* Entry: R1:DATCON in type + A(Date)                                  *         
* Exit:  Date being manipulated but will abend                        *         
***********************************************************************         
                                                                                
CHKDATE  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R2,0(R1)                                                         
         LLC   R3,0(R1)                                                         
         LHI   RF,3-1                                                           
         CLI   0(R1),X'01'                                                      
         JE    CHKDAT2                                                          
         LHI   RF,2-1                                                           
         CLI   0(R1),X'02'                                                      
         JNE   *+2                                                              
                                                                                
CHKDAT2  EX    RF,CHKDATC                                                       
         JE    CHKDATX                                                          
         MVC   WORK+50(24),DMCB                                                 
         GOTO1 =V(DATCON),(R1),((R3),(R2)),(20,WORK+80)                         
         MVC   WORK+90(6),WORK+80+2                                             
         CLC   WORK+80(2),=C'19'                                                
         JE    CHKDAT4                                                          
         CLC   WORK+80(2),=C'20'                                                
         JNE   CHKDATP                                                          
                                                                                
CHKDAT4  GOTO1 =V(DATVAL),DMCB,(0,WORK+80),WORK+100                             
         OC    DMCB(4),DMCB                                                     
         JNZ   CHKDAT6                                                          
                                                                                
         USING PLINED,R6                                                        
CHKDATP  L     R6,=V(CPRINT)                                                    
         MVC   PLINED(132),GSPACES                                              
         MVC   PLINED(13),=C'CHKDATE ERROR'                                     
         STC   R3,WORK+110                                                      
*&&UK                                                                           
         XOUT  WORK+110,PLINED+15,1                                             
         XOUT  0(R2),PLINED+18,3                                                
*&&                                                                             
*&&US                                                                           
         GOTO1 =V(HEXOUT),PARM,WORK+110,PLINED+15,1                             
         GOTO1 =V(HEXOUT),PARM,0(R2),PLINED+18,3                                
*&&                                                                             
         MVC   PLINED+26(8),WORK+80                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   PLINED(132),GSPACES                                              
*&&UK                                                                           
         XOUT  0(R4),PLINED+10,42                                               
*&&                                                                             
*&&US                                                                           
         GOTO1 =V(HEXOUT),PARM,0(R4),PLINED+10,42                               
*&&                                                                             
         GOTO1 =V(PRINTER)                                                      
         CLI   RETCODE,7                                                        
         JNL   CHKDAT6                                                          
         MVI   RETCODE,7                                                        
         DROP  R6                                                               
                                                                                
CHKDAT6  MVC   DMCB(24),WORK+50                                                 
                                                                                
CHKDATX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
CHKDATC  CLC   0(0,R2),=8X'00'                                                  
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ISO 8601 date conversion (1)                                        *         
* Entry: R1:P'YMD'    = A(type 1 date)                                *         
* Exit:  WORK+00      = ISO 8601 date                                 *         
***********************************************************************         
                                                                                
SET1DAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK(10),GSPACES                                                 
         OC    0(3,R1),0(R1)                                                    
         JZ    EXIT                                                             
         MVC   WORK+10(3),0(R1)                                                 
         GOTO1 =V(CHKDATE),DMCB,(1,WORK+10)                                     
         GOTO1 =V(DATCON),DMCB,(1,WORK+10),(23,WORK)                            
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ISO 8601 date conversion (2)                                        *         
* Entry: R1           = A(type 2 date)                                *         
* Exit:  WORK+00      = ISO 8601 date                                 *         
***********************************************************************         
                                                                                
SET2DAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK(10),GSPACES                                                 
         OC    0(2,R1),0(R1)                                                    
         JZ    EXIT                                                             
         MVC   WORK+10(3),0(R1)                                                 
         GOTO1 =V(CHKDATE),DMCB,(2,WORK+10)                                     
         GOTO1 =V(DATCON),DMCB,(2,WORK+10),(23,WORK)                            
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Squash out any spaces (for ID/key fields)                           *         
* Entry: P1 B0        = L(Field)                                      *         
*        P1 B1-B3     = A(Field)                                      *         
* Exit:  (field)                                                      *         
***********************************************************************         
                                                                                
SQUASHIT NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LLC   R3,0(R1)                                                         
         L     R2,0(R1)                                                         
         LA    R2,0(R2)                                                         
                                                                                
         CHI   R3,L'WORK                                                        
         JH    *+2                                                              
                                                                                
         LA    R4,WORK                                                          
                                                                                
         MVC   WORK,GSPACES                                                     
         SHI   R3,1                                                             
         EXRL  R3,SQITEX1                                                       
         EXRL  R3,SQITEX2                                                       
         AHI   R3,1                                                             
                                                                                
SQITLOOP CLI   0(R4),SPACEQ                                                     
         JNH   SQITSKIP                                                         
         MVC   0(1,R2),0(R4)                                                    
         AHI   R2,1                                                             
                                                                                
SQITSKIP AHI   R4,1                                                             
         JCT   R3,SQITLOOP                                                      
                                                                                
         J     EXIT                                                             
                                                                                
SQITEX1  MVC   WORK(0),0(R2)                                                    
                                                                                
SQITEX2  MVC   0(0,R2),GSPACES                                                  
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* EDIT an amount field   ??????                                       *         
* Entry: P1 B0-B3  A(Currency)                                        *         
*        P2 B0     L(Output Field)                                    *         
*        P2 B1-B3  A(Output Field)                                    *         
* Exit:  (Field)                                                      *         
***********************************************************************         
                                                                                
SETAMNT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R2,0(R1)                                                         
         LLC   R4,4(R1)                                                         
         L     R5,4(R1)                                                         
         LA    R5,0(R5)                                                         
                                                                                
         MVC   WORK(20),GSPACES                                                 
                                                                                
         CLC   SAVCCUR,0(R2)       Agency currency?                             
         JE    SETAMN3                                                          
*&&US*&& DC    H'0'                                                             
*&&UK                                                                           
         SAM31 ,                                                                
         USING CURTABD,R3                                                       
         L     R3,ACURBUF                                                       
                                                                                
SETAMN1  OC    CURTCUR,CURTCUR     Not found?                                   
         JZ    *+2                                                              
         CLC   CURTCUR,0(R2)                                                    
         JE    SETAMN2                                                          
         AHI   R3,CURTABL+L'CURTLONG                                            
         J     SETAMN1             Try next                                     
*                                                                               
SETAMN2  MVC   WORK+20(CURTABL+L'CURTLONG),0(R3)                                
         DROP  R3                                                               
*&&                                                                             
         CP    EDDUB,PZERO         Empty?                                       
         JE    SETAMN5                                                          
         J     SETAMN4                                                          
*                                                                               
SETAMN3  DS    0H                  Agency currency                              
         CURED (P8,EDDUB),(16,WORK),2,ALIGN=LEFT,FLOAT=-,              +        
               MODADDR=VCUREDIT                                                 
         J     SETAMN7                                                          
                                                                                
SETAMN4  DS    0H                  Foreign currency non zero                    
         CURED (P8,EDDUB),(16,WORK),WORK+20,ALIGN=LEFT,FLOAT=-,        +        
               CURSYMB=NO,MODADDR=VCUREDIT                                      
         J     SETAMN7                                                          
                                                                                
SETAMN5  DS    0H                  Foreign currency is zero                     
         CURED (P8,EDDUB),(16,WORK),WORK+20,ALIGN=LEFT,CURSYMB=NO,     +        
               MODADDR=VCUREDIT                                                 
                                                                                
SETAMN7  CR    R0,R4              Check versus field length                     
         JH    *+2                                                              
         LR    R4,R0                                                            
         LA    RF,WORK                                                          
         CLI   0(RF),SPACEQ                                                     
         JH    SETAMN9                                                          
         CLI   1(RF),CZEROQ                                                     
         JNL   SETAMN9                                                          
         MVI   0(RF),CZEROQ                                                     
         AHI   R4,1                                                             
                                                                                
SETAMN9  SHI   R4,1                                                             
         EXRL  R4,SETAMEX                                                       
         J     EXIT                                                             
                                                                                
SETAMEX  MVC   0(0,R5),0(RF)                                                    
VCUREDIT DC    V(CUREDIT)                                                       
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get security person details via Control file                        *         
* Entry: R1           = A(hex pid)                                    *         
* Exit:  WORK+100     = PID code                                      *         
*        WORK+00      = First name                                    *         
*        WORK+32      = Middle name                                   *         
*        WORK+16      = Last name                                     *         
*        WORK+48      = Name                                          *         
***********************************************************************         
                                                                                
GETSPD   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    MYSECAGY,MYSECAGY   Security agency - if present - else          
         OC    MYSECAGY,SAVCSEC    use native agency                            
         JNZ   GETSPD02                                                         
         MVC   MYSECAGY,SAVCALP                                                 
                                                                                
GETSPD02 MVC   WORK,GSPACES                                                     
         MVC   FULL(2),0(R1)                                                    
         OC    0(2,R1),0(R1)                                                    
         JZ    GETSPDX                                                          
                                                                                
         SAM31 ,                                                                
         USING PIDBUFD,R4                                                       
         L     R4,APIDBUF                                                       
         LHI   R0,PIDB#Q-1                                                      
                                                                                
GETSPD04 OC    PIDBSAGY,PIDBSAGY   EoT?                                         
         JZ    GETSPD10                                                         
         CLC   PIDBSAGY,MYSECAGY                                                
         JNE   GETSPD06                                                         
         CLC   PIDBPIDX,FULL                                                    
         JE    GETSPD08                                                         
                                                                                
GETSPD06 AHI   R4,PIDBLNQ                                                       
         JCT   R0,GETSPD04                                                      
         L     R4,APIDBUF                                                       
         J     GETSPD10                                                         
                                                                                
GETSPD08 MVC   DUB,PIDBPIDC                                                     
         MVC   WORK+100(8),PIDBPIDC                                             
         MVC   WORK+00(16),PIDBFNAM                                             
         MVC   WORK+32(16),PIDBMNAM                                             
         MVC   WORK+16(16),PIDBLNAM                                             
         MVC   WORK+48(20),PIDBNAME                                             
         SAM24 ,                                                                
         J     GETSPDX                                                          
                                                                                
GETSPD10 DS    0H                                                               
         SAM24 ,                                                                
                                                                                
         USING SA0REC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,MYSECAGY                                                 
         MVC   SA0KNUM,FULL                                                     
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,SA0KEY,AIOGEN                     
         JNE   GETSPD40                                                         
                                                                                
         L     R2,AIOGEN                                                        
         LA    R3,SA0DATA                                                       
         USING SAPALD,R3                                                        
                                                                                
GETSPD14 CLI   SAPALEL,SAPALELQ                                                 
         JE    GETSPD16                                                         
         CLI   SAPALEL,0                                                        
         JE    GETSPD40                                                         
         LLC   R1,SAPALLN                                                       
         AR    R3,R1                                                            
         J     GETSPD14                                                         
                                                                                
GETSPD16 MVC   DUB,SAPALPID        Save 8 byte security person code             
         MVC   WORK+100(8),DUB                                                  
         DROP  R2,R3                                                            
                                                                                
         USING SAPEREC,R2                                                       
GETSPD17 LA    R2,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,MYSECAGY                                                 
         MVC   SAPEPID,DUB                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,SAPEKEY,AIOGEN                    
         JNE   GETSPD40                                                         
                                                                                
         L     R2,AIOGEN                                                        
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),IOKEY                               
         JNE   GETSPD40                                                         
                                                                                
         USING SANAMD,R3                                                        
         LA    R3,SAPEDATA                                                      
                                                                                
GETSPD20 CLI   SANAMEL,SANAMELQ                                                 
         JE    GETSPD24                                                         
         CLI   SANAMEL,0                                                        
         JE    GETSPD40                                                         
                                                                                
GETSPD22 LLC   R1,SANAMLN                                                       
         AR    R3,R1                                                            
         J     GETSPD20                                                         
                                                                                
GETSPD24 LA    R1,SANAMELN                                                      
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   First name present?                          
         JZ    GETSPD28                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,MAXSECLQ         Max length check                             
         JNH   GETSPD26                                                         
         LA    RF,MAXSECLQ                                                      
                                                                                
GETSPD26 SHI   RF,1                                                             
         MVC   WORK+00(0),SANAME                                                
         EX    RF,*-6                                                           
         LLC   RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
GETSPD28 TM    SANAMIND,SANAMIMN   Middle name present?                         
         JZ    GETSPD32                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,MAXSECLQ         Max length check                             
         JNH   GETSPD30                                                         
         LA    RF,MAXSECLQ                                                      
                                                                                
GETSPD30 SHI   RF,1                                                             
         MVC   WORK+32(0),SANAME                                                
         EX    RF,*-6                                                           
         LLC   RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
GETSPD32 TM    SANAMIND,SANAMILN   Last name present?                           
         JZ    GETSPD40                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,MAXSE2LQ         Max length check                             
         JNH   GETSPD34                                                         
         LHI   RF,MAXSE2LQ                                                      
                                                                                
GETSPD34 SHI   RF,1                                                             
         MVC   WORK+48(0),SANAME                                                
         EX    RF,*-6                                                           
                                                                                
         CHI   RF,MAXSECLQ-1       Test > max length                            
         JNH   GETSPD36                                                         
         LA    RF,MAXSECLQ-1                                                    
                                                                                
GETSPD36 MVC   WORK+16(0),SANAME                                                
         EX    RF,*-6                                                           
                                                                                
GETSPD40 DS    0H                                                               
         SAM31 ,                                                                
         MVC   PIDBPIDC,DUB                                                     
         MVC   PIDBPIDC,WORK+100                                                
         MVC   PIDBFNAM,WORK+00                                                 
         MVC   PIDBMNAM,WORK+32                                                 
         MVC   PIDBLNAM,WORK+16                                                 
         MVC   PIDBNAME,WORK+48                                                 
         AHI   R4,PIDBLNQ                                                       
         XC    PIDBSAGY,PIDBSAGY                                                
         SAM24 ,                                                                
                                                                                
GETSPDX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
MYSECAGY DS    XL2                                                              
         DROP  R1,R2,R3,R4                                                      
                                                                                
MAXSECLQ EQU   16                                                               
MAXSE2LQ EQU   58                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get Extra Data entry name                                           *         
* Entry: R1           = A(Pointer data)                               *         
* Exit:  WORK+00      = Name                                          *         
***********************************************************************         
                                                                                
GETXDF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK,GSPACES                                                     
         LR    R4,R1                                                            
                                                                                
         USING XDFPASD,R3                                                       
         LA    R3,IOKEY            READ XDFRECD USING PASSIVES                  
         XC    IOKEY,IOKEY                                                      
         MVI   XDFPTYP,XDFPTYPQ                                                 
         MVI   XDFPSUB,XDFPSUBQ                                                 
         MVC   XDFPCPY,SAVCOMP                                                  
         MVC   XDFPPTR(L'XDFPPTR+L'XDFPSEQ),0(R4)                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,XDFPAS,XDFPAS                     
         JNE   GETXDFX                                                          
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,XDFPDA,AIOGEN,DMWORK              
         JNE   GETXDFX                                                          
                                                                                
         USING XDFRECD,R3                                                       
         L     R3,AIOGEN                                                        
         USING XDFEL,R2                                                         
         LA    R2,XDFRFST                                                       
                                                                                
GETXDF02 CLI   XDFEL,XDFELQ                                                     
         JNE   GETXDF04                                                         
         CLC   XDFSEQ,L'XDFPPTR(R4)                                             
         JE    GETXDF08                                                         
         J     GETXDF06                                                         
                                                                                
GETXDF04 CLI   XDFEL,0                                                          
         JE    GETXDFX                                                          
                                                                                
GETXDF06 LLC   R0,XDFLN                                                         
         AR    R2,R0                                                            
         J     GETXDF02                                                         
                                                                                
GETXDF08 LLC   RF,XDFLN                                                         
         SHI   RF,XDFLN1Q+1                                                     
         CHI   RF,0                                                             
         JL    GETXDFX                                                          
         MVC   WORK(0),XDFNAME                                                  
         EX    RF,*-6                                                           
                                                                                
GETXDFX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get Account Name                                                    *         
* Entry: R1           = A(ACTKULA)                                    *         
* Exit:  WORK+00      = Name                                          *         
***********************************************************************         
                                                                                
GETACN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK,GSPACES                                                     
         LR    R4,R1                                                            
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY            READ XDFRECD USING PASSIVES                  
         MVC   ACTKEY,GSPACES                                                   
         MVC   ACTKCPY,SAVCOMP                                                  
         MVC   ACTKULA,0(R4)                                                    
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY                     
         JNE   GETACNX                                                          
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,ACTKDA,AIOGEN,DMWORK              
         JNE   GETACNX                                                          
                                                                                
         L     R3,AIOGEN                                                        
         USING NAMEL,R2                                                         
         LA    R2,ACTRFST                                                       
                                                                                
GETACN02 CLI   NAMEL,NAMELQ                                                     
         JE    GETACN06                                                         
         CLI   NAMEL,0                                                          
         JE    GETACNX                                                          
                                                                                
GETACN04 LLC   R0,NAMLN                                                         
         AR    R2,R0                                                            
         J     GETACN02                                                         
                                                                                
GETACN06 LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         CHI   RF,0                                                             
         JL    GETACNX                                                          
         MVC   WORK(0),NAMEREC                                                  
         EX    RF,*-6                                                           
                                                                                
GETACNX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Process client summary records                                      *         
***********************************************************************         
                                                                                
PROCCSM  NTR1  BASE=*,LABEL=*                                                   
         USING DXBLOCKD,R7                                                      
         USING SXDTABD,R6                                                       
         USING AGXRECD,R3                                                       
         MVC   TEMPSTOR,GSPACES    Clear temp storage for client code           
         OI    RUNINDS,RUNHDR      Set Header as Load as no recovery            
         NI    RUNINDS,X'FF'-RUNCUT                          record             
*                                                                               
OB       USING OB_D,OBTTAREA                                                    
         XC    OB.OB_KEY,OB.OB_KEY                                              
         MVC   OB.OB_LEN,=AL2(OB_CSLNQ)                                         
         MVI   OB.OB_KTYP,OB_KTYPQ                                              
*&&UK*&& MVC   OB.OB_KAGY(2),EUROPE   Set general data                          
*&&US*&& MVC   OB.OB_KAGY(2),NORTHAM                                            
         MVI   OB.OB_KAGY+2,UNDSCQ                                              
         MVC   OB.OB_KAGY+2+1(2),SXDTAGY                                        
         GOTOR AGETBUF,OB.OB_D     Get client summary for this alphaid          
         J     PCSM020                                                          
                                                                                
PCSM010  MVC   OB.OB_D(L'OB_LEN+L'OB_KEY),STSRKEY                               
         GOTOR AGETBUF,OB.OB_D     Get client summary for this alphaid          
         GOTOR AGETNXT,OB.OB_D                                                  
PCSM020  TM    OBTSERR,TSEEOF      Have we hit end of file                      
         JNZ   PROCCSMY             if so exit                                  
*        CLC   OB.OB_KAGY+3(2),SXDTAGY                                          
*        JNE   PROCCSMY                                                         
         CLI   OB.OB_KTYP,OB_KTYPQ                                              
         JNE   PROCCSMY                                                         
                                                                                
         MVC   STSRKEY,OB.OB_D     Save key                                     
         LHI   R1,CSMFLENQ         R1=Max len w/o key:CSMF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CSMFLENQ         R1=Max len of client summary fact            
         GOTO1 AINITALL                                                         
         L     R3,DXAXREC                                                       
         USING AGXRECD,R3                                                       
         XC    AGXRELEN,AGXRELEN   Record length/type/eor                       
         LHI   RF,CSMFLENQ                                                      
         STCM  RF,B'0011',AGXRELEN                                              
         MVC   AGXRETYP,AGXCSMFQ                                                
         MVC   CSMFAGY,OB.OB_KAGY                                               
         MVC   CSMFCOD,OB.OB_KCLI                                               
         CLC   OB.OB_KCLI,TEMPSTOR Have we read for client before               
         JE    PCSM030             Yes - reuse stored named                     
         MVC   TEMPSTOR(L'PRODUL),PRODUL                                        
         MVC   TEMPSTOR+L'PRODUL(L'OB_KCLI),OB.OB_KCLI                          
                                                                                
         GOTOR AGETACN,TEMPSTOR                                                 
                                                                                
PCSM030  MVC   CSMFNAM,WORK                                                     
         MVC   CSMFOFF,OB.OB_KOFF                                               
         MVC   FULL(L'OB_KMOA),OB.OB_KMOA                                       
         MVI   FULL+L'OB_KMOA,X'01'                                             
         GOTO1 VDATCON,DMCB,(1,FULL),(20,CSMFYR)                                
                                                                                
         LA    R5,OB.OB_BHRS                                                    
         LA    R4,CSMFBHRS                                                      
         LHI   R2,7                                                             
PCSM050  ZAP   EDDUB,0(8,R5)                                                    
         GOTOR ASETAMNT,DMCB,SAVCCUR,(L'CSMFBHRS,0(R4))                         
         LA    R4,L'CSMFBHRS+1(R4)                                              
         LA    R5,L'OB_BHRS(R5)                                                 
         JCT   R2,PCSM050                                                       
                                                                                
         GOTO1 VAGXCNVX,DMCB,(R7)  Put unconverted record only                  
                                                                                
         GOTO1 ARECNUMS,DXASQLB          record counter                         
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)   write to dsn=                          
                                                                                
         CLC   TYPECODE,=C'CSM'    If client summary only ensure we             
         JNE   PCSM010              put out company update time                 
         TM    RUNINDS,RUNCUT      Have we sent company                         
         JNZ   PCSM010                                                          
         LHI   R1,CUTDLENQ         R1=Max len w/o key:CUTD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CUTDLENQ         R1=Max len of company update time            
         GOTO1 AINITALL                                                         
         L     R3,DXAXREC                                                       
         USING AGXRECD,R3                                                       
         XC    AGXRELEN,AGXRELEN   Record length/type/eor                       
         LHI   RF,CUTDLENQ                                                      
         STCM  RF,B'0011',AGXRELEN                                              
         MVC   AGXRETYP,AGXCUTDQ                                                
*&&UK*&& MVC   CUTDAGY(2),EUROPE   Set data                                     
*&&US*&& MVC   CUTDAGY(2),NORTHAM                                               
         MVI   CUTDAGY+2,UNDSCQ                                                 
         MVC   CUTDAGY+2+1(2),SXDTAGY                                           
         GOTOR ASETACTV,1          Use run date/time right now                  
         MVC   CUTDTIM,WORK                                                     
                                                                                
         GOTO1 VAGXCNVX,DMCB,(R7)  Put unconverted record only                  
                                                                                
         GOTO1 ARECNUMS,DXASQLB          record counter                         
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)   write to dsn=                          
         OI    RUNINDS,RUNCUT                                                   
         J     PCSM010                                                          
                                                                                
PROCCSMY DS    0H                                                               
         J     EXIT                                                             
         DROP  OB,R3,R6,R7                                                      
         LTORG                                                                  
***********************************************************************         
* Get approver PID for time/expenses (line manager)                   *         
* Entry: R1           = A(TIMKCULC)                                   *         
* Exit:  HALF         = Hex PID                                       *         
***********************************************************************         
                                                                                
GETMAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    HALF,HALF                                                        
                                                                                
         USING TIMKCULA,R4                                                      
         LR    R4,R1                                                            
                                                                                
         USING AGXRECD,R3                                                       
         DS    0H                  R3 incoming from AGXROUTS                    
                                                                                
         USING DPAPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVI   DPAPAPPL,DPAPATIM                                                
         CLC   AGXRETYP,AGXTIMDQ                                                
         JE    GETMAP00                                                         
         MVI   DPAPAPPL,DPAPAEXP                                                
         CLC   AGXRETYP,AGXEXPDQ                                                
         JNE   *+2                                                              
                                                                                
GETMAP00 MVC   DPAPCPY,SAVCOMP                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         DROP  R3                                                               
                                                                                
         LA    R3,SAVC1RD          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETMAP02 MVC   DPAP1RAC,GSPACES                                                 
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   DPAP1RAC(0),TIMKACT                                              
         EX    RF,0(RE)                                                         
         MVC   WORK(L'DPAPAS),DPAPAS                                            
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS                     
         JNE   GETMAPX                                                          
         J     GETMAP06                                                         
                                                                                
GETMAP04 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ACCDIR,DPAPAS,DPAPAS                     
         JNE   GETMAPX                                                          
                                                                                
GETMAP06 CLC   DPAPAS(DPAPPIDB-DPAPASD),WORK                                    
         JNE   GETMAP08                                                         
         MVC   HALF,DPAPPIDB                                                    
         J     GETMAP10                                                         
                                                                                
GETMAP08 MVC   DPAPAS,WORK         Reset key                                    
         SHI   R3,L'SAVC1RD        Point to previous key length                 
         JCT   R0,GETMAP02         Do for number of 1R levels                   
         J     GETMAPX                                                          
                                                                                
GETMAP10 DS    0H                  (ACBRA27.GETMAP - stop here as no            
         DS    0H                   need for back up approver)                  
                                                                                
GETMAPX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R4                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get finance approver PID for expenses                               *         
* Entry: R1           = A(TIMKCULC)                                   *         
* Exit:  HALF         = Hex PID                                       *         
***********************************************************************         
                                                                                
GETFAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    HALF,HALF                                                        
                                                                                
         USING TIMKCULA,R4                                                      
         LR    R4,R1                                                            
                                                                                
         USING DPAPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVI   DPAPAPPL,DPAPAEXF                                                
         MVC   DPAPCPY,SAVCOMP                                                  
         ZAP   DPAPXVAL,PZERO                                                   
                                                                                
         LA    R3,SAVC1RD          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETFAP02 MVC   DPAP1RAC,GSPACES                                                 
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   DPAP1RAC(0),TIMKACT                                              
         EX    RF,0(RE)                                                         
         MVC   WORK(L'DPAPAS),DPAPAS                                            
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS                     
         JNE   GETFAPX                                                          
         J     GETFAP06                                                         
                                                                                
GETFAP04 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ACCDIR,DPAPAS,DPAPAS                     
         JNE   GETFAPX                                                          
                                                                                
GETFAP06 CLC   DPAPAS(DPAPPIDB-DPAPASD),WORK                                    
         JNE   GETFAP08                                                         
         MVC   HALF,DPAPPIDB                                                    
         J     GETFAP10                                                         
                                                                                
GETFAP08 MVC   DPAPAS,WORK         Reset key                                    
         SHI   R3,L'SAVC1RD        Point to previous key length                 
         JCT   R0,GETFAP02         Do for number of 1R levels                   
         J     GETFAPX                                                          
                                                                                
GETFAP10 DS    0H                  (ACBRA15.GETFAP)                             
                                                                                
GETFAPX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R4                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get job approver PIN for jobs (ACBRA2D.GETAPP)                      *         
* Entry: R1           = A(ACTKACT) and HALF for office                *         
* Exit:  HALF         = Hex PID                                       *         
***********************************************************************         
                                                                                
GETJAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   FULL(2),HALF        save office                                  
         XC    HALF,HALF                                                        
         MVI   FULL+3,NOQ                                                       
                                                                                
         USING ACTKACT,R3                                                       
         LR    R3,R1                                                            
         LLC   RE,SAVCSJB                                                       
         LA    RE,ACTKACT(RE)                                                   
         MVC   FULL+2(1),0(RE)     save media code                              
                                                                                
         USING APRTABD,R4                                                       
         LA    R4,APRTAB                                                        
                                                                                
         USING JOBPASD,R2                                                       
GJAP02   LA    R2,IOKEY            Look for Cli/Pro job appr passive            
         XC    JOBPAS,JOBPAS                                                    
         MVI   JOBPTYP,JOBPTYPQ                                                 
         MVI   JOBPSUB,JOBPSUBQ                                                 
         MVC   JOBPCPY,SAVCOMP                                                  
         MVI   JOBPAPPL,JOBPAJOB                                                
         MVI   JOBPVIEW,JOBPVOFF                                                
         MVC   JOBPCPJ,GSPACES                                                  
         MVC   JOBPCOFF,GSPACES                                                 
         MVC   JOBPCMED,GSPACES                                                 
         OC    APRSTAT,APRSTAT                                                  
         JNZ   GJAP04                                                           
         MVI   JOBPCODE,FFQ                                                     
         MVC   JOBPCODE+1(L'JOBPCODE-1),JOBPCODE                                
         J     GJAP12                                                           
                                                                                
GJAP04   TM    APRSTAT,APRPRO                                                   
         JZ    GJAP06                                                           
         LLC   RF,SAVCSJA                                                       
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),SPACEQ        Have we got a product                        
         JNH   GJAP18              No                                           
         LLC   RF,SAVCSJB                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   JOBPCPJ(0),ACTKACT                                               
         EX    RF,0(RE)                                                         
         J     GJAP08                                                           
                                                                                
GJAP06   TM    APRSTAT,APRCLI                                                   
         JZ    GJAP08                                                           
         LLC   RF,SAVCSJA                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   JOBPCPJ(0),ACTKACT                                               
         EX    RF,0(RE)                                                         
                                                                                
GJAP08   TM    APRSTAT,APRMED                                                   
         JZ    GJAP10                                                           
         CLI   FULL+3,SPACEQ                                                    
         JNH   GJAP18                                                           
         MVC   JOBPCMED,FULL+2                                                  
                                                                                
GJAP10   TM    APRSTAT,APROFF                                                   
         JZ    GJAP12                                                           
         CLC   FULL(2),GSPACES                                                  
         JNH   GJAP18                                                           
         MVC   JOBPCOFF,FULL                                                    
                                                                                
GJAP12   OC    JOBPCODE,GSPACES                                                 
         MVC   WORK(L'JOBPAS),JOBPAS                                            
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,ACCDIR,JOBPAS,JOBPAS                     
         J     GJAP16                                                           
                                                                                
GJAP14   MVC   WORK(L'JOBPAS),JOBPAS                                            
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ACCDIR,JOBPAS,JOBPAS                     
                                                                                
GJAP16   CLC   JOBPAS(JOBPPIDB-JOBPASD),WORK                                    
         JE    GJAP20                                                           
         CLI   FULL+3,YESQ         Did we find an approver                      
         JNE   GJAP18              No                                           
         OC    HALF,HALF           Yes - did we find default                    
         JNZ   GETJAPY             Yes                                          
         J     GETJAPN             No - error as there should be one            
                                                                                
GJAP18   AHI   R4,APRTABL                                                       
         CLI   APRSTAT,FFQ                                                      
         JNE   GJAP02                                                           
         J     GETJAPN                                                          
                                                                                
GJAP20   MVI   FULL+3,YESQ                                                      
         TM    JOBPSTAT,JOBPDFLT   Is it default approver                       
         JZ    GJAP14                                                           
         OC    HALF,HALF           Did we already find default                  
         JNZ   GJAP14              Yes                                          
         MVC   HALF,JOBPPIDB                                                    
         J     GJAP14                                                           
                                                                                
GETJAPN  DS    0H                                                               
         J     NO                                                               
                                                                                
GETJAPY  DS    0H                                                               
         J     YES                                                              
         DROP  R2,R3,R4                                                         
***********************************************************************         
* Get userID record for UserID hex code and get its code              *         
* This uses UserID buffering in UIDTAB                                *         
* - Parm1=A(UserID hex)                                               *         
* - Parm2=A(Return field for code)                                    *         
*                                                                     *         
***********************************************************************         
                                                                                
GETUID   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LM    R2,R3,0(R1)                                                      
                                                                                
         SAM31 ,                                                                
                                                                                
         MVC   0(L'UIDTCOD,R3),GSPACES                                          
         OC    0(L'UIDTHEX,R2),0(R2)                                            
         JZ    GETUIDN                                                          
                                                                                
         USING UIDTABD,R4                                                       
         L     R4,AUIDTAB          Check for table                              
         LHI   R0,UIDTMAX                                                       
                                                                                
GUID02   OC    UIDTHEX,UIDTHEX     EoT?                                         
         JZ    GUID10                                                           
         CLC   UIDTHEX,0(R2)       Correct userID?                              
         JE    GUID06                                                           
                                                                                
GUID04   AHI   R4,UIDTLNQ                                                       
         JCT   R0,GUID02                                                        
         J     *+2                 (increase UIDTMAX)                           
                                                                                
GUID06   MVC   0(L'UIDTCOD,R3),UIDTCOD                                          
         CLI   UIDTSTA,YESQ                                                     
         JE    GETUIDY                                                          
         J     GETUIDN                                                          
                                                                                
GUID10   XC    UIDTLNQ(L'UIDTHEX,R4),UIDTLNQ(R4)   Set new EoT                  
         MVC   UIDTHEX,0(R2)                                                    
         MVI   UIDTSTA,NOQ                                                      
         MVC   UIDTCOD,GSPACES                                                  
                                                                                
         USING CTIREC,R2                                                        
         L     R2,AIOGEN                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UIDTHEX                                                  
                                                                                
         SAM24 ,                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,CTIREC,CTIREC                     
         SAM31 ,                                                                
         JNE   GETUIDN                                                          
                                                                                
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
                                                                                
GUID14   CLI   CTDSCEL,CTDSCELQ                                                 
         JE    GUID16                                                           
         CLI   CTDSCEL,0                                                        
         JE    GUID20                                                           
         LLC   R1,CTDSCLEN                                                      
         AR    R2,R1                                                            
         J     GUID14                                                           
                                                                                
GUID16   MVC   UIDTCOD,CTDSC                                                    
         MVI   UIDTSTA,YESQ                                                     
                                                                                
GUID20   MVC   0(L'UIDTCOD,R3),UIDTCOD                                          
         DROP  R2                                                               
                                                                                
GETUIDY  SAM24 ,                                                                
         J     YES                                                              
                                                                                
GETUIDN  SAM24 ,                                                                
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
                                                                                
         LTORG                                                                  
                                                                                
         DS    0H                                                               
APRTAB   DS    0XL1                                                             
         DC    B'11011000'         Office/client/product/media                  
         DC    C'1'                                                             
         DC    B'10011000'         Office/client/media                          
         DC    C'2'                                                             
         DC    B'10010000'         Client/media                                 
         DC    C'4'                                                             
         DC    B'00011000'         Office/media                                 
         DC    C'3'                                                             
         DC    B'00010000'         Media                                        
         DC    C'M'                                                             
         DC    B'11001000'         Office/client/product                        
         DC    C'P'                                                             
         DC    B'10001000'         Office/client                                
         DC    C'C'                                                             
         DC    B'10000000'         Client                                       
         DC    C'C'                                                             
         DC    B'00001000'         Office                                       
         DC    C'O'                                                             
         DC    B'00000000'         Agency                                       
         DC    C'A'                                                             
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* Get person data for current TIMKEY                                  *         
* Entry: R1           = A(TIMKEY)                                     *         
* Exit:  WORK+00      = Period end date                               *         
*        WORK+03      = Period start date                             *         
*        WORK+06      = Location end date                             *         
*        WORK+09      = Location start date                           *         
*        WORK+12      = Hex PID form PERRECD                          *         
***********************************************************************         
                                                                                
GETPER   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TIMRECD,R4                                                       
         LR    R4,R1                                                            
         XC    WORK(40),WORK                                                    
         MVC   WORK+40(L'WORK-40),GSPACES                                       
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PERKEY,GSPACES                                                   
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,TIMKCPY                                                  
         MVC   WORK+40(L'TIMKACT),TIMKACT                                       
         LLC   R1,SAVC1RC                                                       
         LA    R1,WORK+40(R1)                                                   
         MVC   PERKCODE,0(R1)                                                   
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,PERKEY,PERKEY                     
         JNE   GETPER20                                                         
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,PERKDA,AIOGEN,DMWORK              
         JNE   GETPER20                                                         
                                                                                
         USING LOCELD,R3                                                        
         L     R2,AIOGEN                                                        
         LA    R3,PERRFST                                                       
                                                                                
GETPER02 CLI   LOCEL,0                                                          
         JE    GETPER20                                                         
         CLI   LOCEL,LOCELQ                                                     
         JE    GETPER06                                                         
         CLI   LOCEL,PIDELQ                                                     
         JE    GETPER10                                                         
                                                                                
GETPER04 LLC   R1,LOCLN                                                         
         AR    R3,R1                                                            
         J     GETPER02                                                         
                                                                                
GETPER06 OC    LOCEND,LOCEND       Get location dates                           
         JZ    GETPER08                                                         
         CLC   TIMKPEDT,LOCEND                                                  
         JH    GETPER04                                                         
                                                                                
GETPER08 CLC   TIMKPEDT,LOCSTART                                                
         JL    GETPER04                                                         
         MVC   WORK+06(3),LOCEND                                                
         MVC   WORK+09(3),LOCSTART                                              
         J     GETPER04                                                         
                                                                                
         USING PIDELD,R3                                                        
GETPER10 MVC   WORK+12(2),PIDNO                                                 
         J     GETPER04                                                         
                                                                                
         USING TMPELD,R2                                                        
GETPER20 DS    0H                                                               
         SAM31 ,                                                                
         L     R2,APERBUF                                                       
         XC    WORK+60(20),WORK+60                                              
         MVC   HALF,GSPACES        Determine office                             
         LLC   R1,OFFLEN                                                        
         SHI   R1,1                                                             
         MVC   HALF(0),TIMKACT                                                  
         EX    R1,*-6                                                           
                                                                                
GETPER22 CLI   TMPEL,0                                                          
         JE    GETPER28                                                         
         CLI   TMPEL,TMPELQ                                                     
         JNE   *+2                 (integrity check)                            
                                                                                
         CLC   TIMKPEDT,TMPSTART                                                
         JL    GETPER26                                                         
         CLC   TIMKPEDT,TMPEND                                                  
         JH    GETPER26                                                         
         CLC   TMPEND+L'TMPEND(2),GSPACES                                       
         JNH   GETPER24                                                         
         CLC   TMPEND+L'TMPEND(0),HALF                                          
         JNE   GETPER26                                                         
         MVC   WORK+60(L'TMPSTART+L'TMPEND),TMPSTART                            
         J     GETPER26                                                         
                                                                                
GETPER24 MVC   WORK+60(L'TMPSTART+L'TMPEND),TMPSTART                            
                                                                                
GETPER26 AHI   R2,TMPLNQ                                                        
         J     GETPER22                                                         
                                                                                
GETPER28 DS    0H                                                               
         SAM24 ,                                                                
         LA    RF,WORK+70                                                       
         OC    WORK+70(L'TMPSTART+L'TMPEND),WORK+70                             
         JNZ   GETPER30                                                         
         LA    RF,WORK+60                                                       
                                                                                
GETPER30 MVC   WORK+00(3),L'TMPSTART(RF)                                        
         MVC   WORK+03(3),0(RF)                                                 
                                                                                
GETPERX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get period start date via calendar                                  *         
* Entry: R1           = A(TSWKEY)                                     *         
* Exit:  WORK+00      = Period start date                             *         
***********************************************************************         
                                                                                
GETPSD   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TSWRECD,R3                                                       
         LR    R3,R1                                                            
                                                                                
         XR    R1,R1                                                            
         ICM   R1,B'0111',TSWKEND                                               
         LNR   R1,R1                                                            
         STCM  R1,B'0111',FULL                                                  
                                                                                
         XC    WORK(10),WORK                                                    
         MVC   HALF,GSPACES        Determine office                             
         LLC   R1,OFFLEN                                                        
         SHI   R1,1                                                             
         MVC   HALF(0),TSWKODS                                                  
         EX    R1,*-6                                                           
                                                                                
         USING TMPELD,R2                                                        
         SAM31 ,                                                                
         L     R2,APERBUF                                                       
                                                                                
GETPSD02 CLI   TMPEL,0                                                          
         JE    GETPSD08                                                         
         CLI   TMPEL,TMPELQ                                                     
         JNE   *+2                 (integrity check)                            
                                                                                
         CLC   FULL(3),TMPSTART                                                 
         JL    GETPSD06                                                         
         CLC   FULL(3),TMPEND                                                   
         JH    GETPSD06                                                         
         CLC   TMPEND+L'TMPEND(2),GSPACES                                       
         JNH   GETPSD04                                                         
         CLC   TMPEND+L'TMPEND(0),HALF                                          
         JNE   GETPSD06                                                         
         MVC   WORK+6(3),TMPSTART                                               
         J     GETPSD06                                                         
                                                                                
GETPSD04 MVC   WORK+3(3),TMPSTART                                               
                                                                                
GETPSD06 AHI   R2,TMPLNQ                                                        
         J     GETPSD02                                                         
                                                                                
GETPSD08 DS    0H                                                               
         SAM24 ,                                                                
         LA    RF,WORK+6                                                        
         OC    WORK+6(3),WORK+6                                                 
         JNZ   GETPSD10                                                         
         LA    RF,WORK+3                                                        
                                                                                
GETPSD10 MVC   WORK+00(3),0(RF)                                                 
                                                                                
GETPSDX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Get SJ office from account                                          *         
* (see also SETSJO which buffers - buffer used in here)               *         
* Entry: R1           = A(ACTKEY)                                     *         
* Exit:  HALF         = Office code                                   *         
***********************************************************************         
                                                                                
GETSJO   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ACTRECD,R3                                                       
         LR    R3,R1                                                            
                                                                                
         USING PPRELD,R4                                                        
         LA    R4,ACTRFST                                                       
         MVC   HALF,GSPACES        Check current record has office set          
                                                                                
GSJO02   CLI   PPREL,PPRELQ                                                     
         JE    GSJO04                                                           
         CLI   PPREL,0                                                          
         JE    GSJO06                                                           
         LLC   R1,PPRLN                                                         
         AR    R4,R1                                                            
         J     GSJO02                                                           
                                                                                
GSJO04   CLC   PPRGAOFF,GSPACES                                                 
         JNH   GSJO06                                                           
         MVC   HALF,PPRGAOFF                                                    
         J     GETSJOX                                                          
         DROP  R4                                                               
                                                                                
GSJO06   TM    PROCMODE,PROCMUQ    Update mode?                                 
         JNZ   GSJO08                                                           
         MVC   HALF,LAST_POF       If Load mode get office from saved           
         CLC   HALF,GSPACES        values                                       
         JH    GETSJOX                                                          
         MVC   HALF,LAST_COF                                                    
         J     GETSJOX                                                          
                                                                                
GSJO08   XC    DUB,DUB             See if in ASJOBUF - clear entries            
         MVC   WORK(40),GSPACES    build client/product accounts                
         LLC   R1,SAVCSJA                                                       
         SHI   R1,1                                                             
         MVC   WORK+00(0),ACTKACT                                               
         EX    R1,*-6                                                           
         LLC   R1,SAVCSJB                                                       
         SHI   R1,1                                                             
         MVC   WORK+20(0),ACTKACT                                               
         EX    R1,*-6                                                           
                                                                                
         USING SJOTABD,R2                                                       
         SAM31 ,                                                                
         L     R2,ASJOTAB                                                       
                                                                                
GSJO10   CLI   SJOTCPY,0           EoT?                                         
         JE    GSJO16                                                           
         CLC   ACTKCPY,SJOTCPY                                                  
         JNE   GSJO14                                                           
         CLC   SJOTACT,WORK+20     Match on product?                            
         JNE   GSJO12                                                           
         ST    R2,DUB+4                                                         
         J     GSJO14                                                           
                                                                                
GSJO12   CLC   SJOTACT,WORK+00     Match on client?                             
         JNE   GSJO14                                                           
         ST    R2,DUB+0                                                         
                                                                                
GSJO14   AHI   R2,SJOTLNQ                                                       
         J     GSJO10                                                           
                                                                                
GSJO16   OC    DUB+0(4),DUB+0      Now see what we have - client?               
         JZ    GSJO20                                                           
         OC    DUB+4(4),DUB+4      - product?                                   
         JZ    GSJO20                                                           
         L     R2,DUB+4                                                         
         MVC   HALF,SJOTOFF        Seed from product                            
         CLC   HALF,GSPACES                                                     
         JH    GSJO18                                                           
         L     R2,DUB+0                                                         
         MVC   HALF,SJOTOFF        Seed from client                             
         DROP  R2                                                               
                                                                                
GSJO18   SAM24 ,                                                                
         J     GETSJOX             Done                                         
                                                                                
HIGH     USING ACTRECD,R2                                                       
GSJO20   LA    R2,IOKEY            Get product record                           
         MVC   HIGH.ACTKEY,GSPACES                                              
         LLC   R1,SAVCSJB                                                       
         AHI   R1,1+2-1                                                         
         MVC   HIGH.ACTKEY(0),ACTKEY                                            
         EX    R1,*-6                                                           
         MVC   WORK+2(2),GSPACES                                                
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,HIGH.ACTKEY,HIGH.ACTKEY           
         JNE   GSJO26                                                           
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,HIGH.ACTKDA,AIOGEN,DMWORK         
         JNE   GSJO26                                                           
                                                                                
         USING PPRELD,R4                                                        
         L     R2,AIOGEN                                                        
         LA    R4,HIGH.ACTRFST                                                  
                                                                                
GSJO22   CLI   PPREL,PPRELQ                                                     
         JE    GSJO24                                                           
         CLI   PPREL,0                                                          
         JE    GSJO26                                                           
         LLC   R1,PPRLN                                                         
         AR    R4,R1                                                            
         J     GSJO22                                                           
                                                                                
GSJO24   CLC   PPRGAOFF,GSPACES                                                 
         JNH   GSJO26                                                           
         MVC   HALF,PPRGAOFF                                                    
         J     GETSJOX                                                          
                                                                                
GSJO26   LA    R2,IOKEY            Get client record                            
         MVC   HIGH.ACTKEY,GSPACES                                              
         LLC   R1,CLILEN                                                        
         AHI   R1,1+2-1                                                         
         MVC   HIGH.ACTKEY(0),ACTKEY                                            
         EX    R1,*-6                                                           
         MVC   WORK+2(2),GSPACES                                                
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,HIGH.ACTKEY,HIGH.ACTKEY           
         JNE   GETSJOX                                                          
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,HIGH.ACTKDA,AIOGEN,DMWORK         
         JNE   GETSJOX                                                          
                                                                                
         USING PPRELD,R4                                                        
         L     R2,AIOGEN                                                        
         LA    R4,HIGH.ACTRFST                                                  
                                                                                
GSJO28   CLI   PPREL,PPRELQ                                                     
         JE    GSJO30                                                           
         CLI   PPREL,0                                                          
         JE    GETSJOX                                                          
         LLC   R1,PPRLN                                                         
         AR    R4,R1                                                            
         J     GSJO28                                                           
                                                                                
GSJO30   CLC   PPRGAOFF,GSPACES                                                 
         JNH   GETSJOX                                                          
         MVC   HALF,PPRGAOFF                                                    
                                                                                
GETSJOX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R3,R4,HIGH                                                       
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Set SJ office for TRNKEY into SAV_TOFF                              *         
* (see also GETSJO which doesn't buffer)                              *         
* Entry: R1           = A(TRNKACT)                                    *         
* Exit:  (SAV_TOFF set and ASJOTAB updated)                           *         
***********************************************************************         
                                                                                
SETSJO   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SAM31 ,                                                                
         USING TRNRECD,R3                                                       
         LR    R3,R1                                                            
         MVC   SAV_TOFF,GSPACES    Init return                                  
         XC    DUB,DUB             clear client/product entries                 
         MVC   TEMPSTOR,GSPACES    build client/product accounts                
         LLC   R1,SAVCSJA                                                       
         SHI   R1,1                                                             
         MVC   TEMPSTOR+00(0),TRNKACT                                           
         EX    R1,*-6                                                           
         LLC   R1,SAVCSJB                                                       
         SHI   R1,1                                                             
         MVC   TEMPSTOR+20(0),TRNKACT                                           
         EX    R1,*-6                                                           
                                                                                
         USING SJOTABD,R2                                                       
         L     R2,ASJOTAB                                                       
         LHI   R0,SJOTMAX                                                       
         SHI   R0,1                                                             
                                                                                
SSJO02   CLI   SJOTCPY,0           EoT?                                         
         JE    SSJO10                                                           
         CLC   TRNKCPY,SJOTCPY                                                  
         JNE   SSJO08                                                           
         CLC   SJOTACT,TRNKACT     Match on job?                                
         JE    SSJO50                                                           
         CLC   SJOTACT,TEMPSTOR+20 Match on product?                            
         JNE   SSJO04                                                           
         ST    R2,DUB+4                                                         
         J     SSJO08                                                           
                                                                                
SSJO04   CLC   SJOTACT,TEMPSTOR+00 Match on client?                             
         JNE   SSJO08                                                           
         ST    R2,DUB+0                                                         
                                                                                
SSJO08   AHI   R2,SJOTLNQ                                                       
         JCT   R0,SSJO02                                                        
         J     *+2                 Increase SJOTMAX                             
                                                                                
SSJO10   OC    DUB+0(4),DUB+0      Now see what we have - client?               
         JNZ   SSJO18                                                           
                                                                                
CLI      USING ACTRECD,R4                                                       
         LA    R4,IOKEY            Get client record                            
         MVC   CLI.ACTKEY,GSPACES                                               
         MVC   CLI.ACTKCPY,TRNKCPY                                              
         MVC   CLI.ACTKULA(2),PRODUL                                            
         MVC   CLI.ACTKACT,TEMPSTOR+00                                          
         MVC   HALF,GSPACES                                                     
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,CLI.ACTKEY,CLI.ACTKEY             
         JNE   SSJO16                                                           
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,CLI.ACTKDA,AIOGEN,DMWORK          
         JNE   SSJO16                                                           
                                                                                
         USING PPRELD,RF                                                        
         L     R4,AIOGEN                                                        
         LA    RF,CLI.ACTRFST                                                   
                                                                                
SSJO12   CLI   PPREL,PPRELQ                                                     
         JE    SSJO14                                                           
         CLI   PPREL,0                                                          
         JE    SSJO16                                                           
         LLC   R1,PPRLN                                                         
         AR    RF,R1                                                            
         J     SSJO12                                                           
                                                                                
SSJO14   MVC   HALF,PPRGAOFF                                                    
         DROP  CLI,RF                                                           
                                                                                
SSJO16   ST    R2,DUB+0                                                         
         MVC   SJOTCPY,TRNKCPY                                                  
         MVC   SJOTACT,TEMPSTOR+00                                              
         MVC   SJOTOFF,HALF                                                     
         AHI   R2,SJOTLNQ                                                       
         MVI   SJOTCPY,0                                                        
         JCT   R0,SSJO20                                                        
         J     *+2                 Increase SJOTMAX                             
                                                                                
SSJO18   L     R1,DUB+0            Preset client office                         
         MVC   HALF,SJOTOFF-SJOTABD(R1)                                         
                                                                                
SSJO20   OC    DUB+4(4),DUB+4      Product?                                     
         JNZ   SSJO28                                                           
                                                                                
PRO      USING ACTRECD,R4                                                       
         LA    R4,IOKEY            Get client record                            
         MVC   PRO.ACTKEY,GSPACES                                               
         MVC   PRO.ACTKCPY,TRNKCPY                                              
         MVC   PRO.ACTKULA(2),PRODUL                                            
         MVC   PRO.ACTKACT,TEMPSTOR+20                                          
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,PRO.ACTKEY,PRO.ACTKEY             
         JNE   SSJO26                                                           
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,PRO.ACTKDA,AIOGEN,DMWORK          
         JNE   SSJO26                                                           
                                                                                
         USING PPRELD,RF                                                        
         L     R4,AIOGEN                                                        
         LA    RF,PRO.ACTRFST                                                   
                                                                                
SSJO22   CLI   PPREL,PPRELQ                                                     
         JE    SSJO24                                                           
         CLI   PPREL,0                                                          
         JE    SSJO26                                                           
         LLC   R1,PPRLN                                                         
         AR    RF,R1                                                            
         J     SSJO22                                                           
                                                                                
SSJO24   CLC   PPRGAOFF,GSPACES                                                 
         JNH   SSJO26                                                           
         MVC   HALF,PPRGAOFF                                                    
         DROP  PRO,RF                                                           
                                                                                
SSJO26   ST    R2,DUB+4                                                         
         MVC   SJOTCPY,TRNKCPY                                                  
         MVC   SJOTACT,TEMPSTOR+20                                              
         MVC   SJOTOFF,HALF                                                     
         AHI   R2,SJOTLNQ                                                       
         MVI   SJOTCPY,0                                                        
         JCT   R0,SSJO30                                                        
         J     *+2                 Increase SJOTMAX                             
                                                                                
SSJO28   L     R1,DUB+4            Preset product office                        
         MVC   HALF,SJOTOFF-SJOTABD(R1)                                         
                                                                                
JOB      USING ACTRECD,R4                                                       
SSJO30   LA    R4,IOKEY            Now get Job and establish office             
         MVC   JOB.ACTKEY,GSPACES                                               
         MVC   JOB.ACTKCPY,TRNKCPY                                              
         MVC   JOB.ACTKULA(2),PRODUL                                            
         MVC   JOB.ACTKACT,TRNKACT                                              
                                                                                
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,JOB.ACTKEY,JOB.ACTKEY             
         JNE   SSJO36                                                           
                                                                                
         GOTO1 =V(DATAMGR),DMCB,GETREC,ACCMST,JOB.ACTKDA,AIOGEN,DMWORK          
         JNE   SSJO36                                                           
                                                                                
         USING PPRELD,RF                                                        
         L     R4,AIOGEN                                                        
         LA    RF,JOB.ACTRFST                                                   
                                                                                
SSJO32   CLI   PPREL,PPRELQ                                                     
         JE    SSJO34                                                           
         CLI   PPREL,0                                                          
         JE    SSJO36                                                           
         LLC   R1,PPRLN                                                         
         AR    RF,R1                                                            
         J     SSJO32                                                           
                                                                                
SSJO34   CLC   PPRGAOFF,GSPACES                                                 
         JNH   SSJO36                                                           
         MVC   HALF,PPRGAOFF                                                    
         DROP  JOB,RF                                                           
                                                                                
SSJO36   MVC   SJOTCPY,TRNKCPY                                                  
         MVC   SJOTACT,TRNKACT                                                  
         MVC   SJOTOFF,HALF                                                     
         AHI   R2,SJOTLNQ                                                       
         MVI   SJOTCPY,0                                                        
         SHI   R2,SJOTLNQ                                                       
                                                                                
SSJO50   MVC   SAV_TOFF,SJOTOFF    Get it from job and return                   
                                                                                
         DROP  R2,R3                                                            
         SAM24 ,                                                                
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Call DMGR to get a record                                           *         
* Entry: R2           = A(Record buffer)                              *         
*        ACCADDR      = D/A to read                                   *         
* Exit:  CC equal     = Record read OK                                *         
*        CC not equal = Error on read                                 *         
***********************************************************************         
                                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,ACCMST                                                        
         TM    TRNKSTAT-TRNRECD(R2),TRNSARCH                                    
         JZ    GETIT2                                                           
         LA    R0,ACCARC                                                        
                                                                                
GETIT2   GOTO1 =V(DATAMGR),DMCB,(X'00',GETREC),(R0),ACCADDR,(R2),DMWORK         
         JE    YES                                                              
                                                                                
         GOTO1 =V(HEXOUT),PARM,ACCADDR,GETDA,L'ACCADDR                          
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
                                                                                
         LA    R3,GETMSG           Output d/a error message                     
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
                                                                                
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
                                                                                
         LTORG                                                                  
***********************************************************************         
* Call DMGR to perform a read high                                    *         
* Entry: R2           = A(Record buffer)                              *         
*        IOKEY        = Key to be read                                *         
* Exit:  CC equal     = Record read OK                                *         
*        CC not equal = Error on read                                 *         
***********************************************************************         
                                                                                
READHI   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,ACCDIR,IOKEY,(R2),DMWORK                 
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
                                                                                
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV                       
                                                                                
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
                                                                                
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI error key HEXOUT follows'                       
                                                                                
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Call DMGR to perform a read                                         *         
* Entry: R2           = A(Record buffer)                              *         
*        IOKEY        = Key to be read                                *         
* Exit:  CC equal     = Record read OK or EoF set                     *         
*        CC not equal = Error on read                                 *         
***********************************************************************         
                                                                                
READ     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 =V(DATAMGR),DMCB,DMREAD,ACCDIR,IOKEY,(R2),DMWORK                 
         JE    YES                                                              
                                                                                
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,REDKEY,L'IOKEYSAV                       
                                                                                
         XR    R0,R0                                                            
         WTO   TEXT=((REDHL,C),(RED1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
                                                                                
REDHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READ error key HEXOUT follows'                         
                                                                                
RED1L    DC    AL2(90)                                                          
RED1M    DC    CL90' '                                                          
         ORG   RED1M                                                            
         DC    C'KEY='                                                          
REDKEY   DC    CL84' '                                                          
         ORG   RED1M+L'RED1M                                                    
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Call DMGR to perform a read including deleted records               *         
* Entry: R2           = A(Record buffer)                              *         
*        IOKEY        = Key to be read                                *         
* Exit:  CC equal     = Record read OK or EoF set                     *         
*        CC not equal = Error on read                                 *         
***********************************************************************         
                                                                                
READD    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'08',DMREAD),ACCDIR,IOKEY,(R2),DMWORK         
         TM    8(R1),255-X'02'                                                  
         JZ    YES                                                              
                                                                                
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,REDDKEY,L'IOKEYSAV                      
                                                                                
         XR    R0,R0                                                            
         WTO   TEXT=((REDDHL,C),(REDD1L,D),(0,E)),MCSFLAG=HRDCPY                
         J     NO                                                               
                                                                                
REDDHL   DC    AL2(40)                                                          
         DC    CL40'DMGR READ (Del) error key HEXOUT follows'                   
                                                                                
REDD1L   DC    AL2(90)                                                          
REDD1M   DC    CL90' '                                                          
         ORG   REDD1M                                                           
         DC    C'KEY='                                                          
REDDKEY  DC    CL84' '                                                          
         ORG   REDD1M+L'REDD1M                                                  
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Routine to count number of records for add, change and delete       *         
***********************************************************************         
                                                                                
         USING SXDTABD,R6                                                       
         USING DXBLOCKD,R7                                                      
RECNUMS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING DXHDRD,R3                                                        
         ICM   R3,B'1111',0(R1)                                                 
                                                                                
AGY      USING OB_D,OBTTAREA                                                    
         LA    R0,OBTTAREA                                                      
         LHI   R1,OB_LNQ-1                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*        MVC   COMPANY,SXDTAGB                                                  
         MVC   COMPANY,CUR_AGY                                                  
         MVC   AGY.OB_KCPY,COMPANY                                              
         GOTOR AGETBUF,AGY.OB_D    Get company via TSAR                         
         JNE   *+2                                                              
                                                                                
         PACK  FULL,DXHDRTYP                                                    
         ZAP   DUB,FULL                                                         
         CVB   R1,DUB                                                           
         S     R1,AGXCPYOF            Subtract CPY Offset                       
         CHI   R1,36                  Take 31-36 gap into account               
         JL    RECNUM2                                                          
         LR    RF,R1                  Save R1                                   
         SHI   R1,36-30-1                                                       
         CHI   RF,39                  Take 38 gap into account                  
         JL    RECNUM2                                                          
         SHI   R1,39-37-1                                                       
                                                                                
RECNUM2  MHI   R1,L'OB_RC300                                                    
         LA    R1,AGY.OB_RC300(R1)                                              
         AP    0(L'OB_RC300,R1),PONE     Increment record type count            
                                                                                
         GOTOR APUTBUF,AGY.OB_D                                                 
         J     YES                                                              
         DROP  AGY,R3                                                           
                                                                                
***********************************************************************         
* Subroutine to extract account records in Load mode                  *         
* R2 = A(Account directory record buffer)                             *         
* P1 = A(Extract routine)                                             *         
* P2 = A(Extract record initialisation routine)                       *         
* P3 = A(Record filter routine)                                       *         
* P4 = A(Format convert routine)                                      *         
* R6 = A(Extract system table entry)                                  *         
* R7 = A(Extract control block data)                                  *         
***********************************************************************         
                                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
ALOA00   MVC   ALPARMS,0(R1)                                                    
                                                                                
         OC    ALPAFILT,ALPAFILT   Filter routine?                              
         JZ    ALOA10                                                           
         GOTO1 ALPAFILT            Filter record                                
         JNE   ALOA50              (get next if invalid)                        
                                                                                
ALOA10   TM    PROCMODE,PROCCLQ    Currency load mode?                          
         JNZ   ALOA22                                                           
         TM    PROCMOD3,PROCTOFQ   Timeoff mode?                                
         JNZ   ALOA22                                                           
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JNZ   ALOA22                                                           
         TM    PROCMODE,PROC2DQ    Extra Dim processing?                        
         JNZ   ALOA22                                                           
         TM    PROCMODE,PROCBNQ    Bill Number Dim processing?                  
         JNZ   ALOA22                                                           
         TM    PROCMOD3,PROCCUTQ   Don't do another GETIT under CUT             
         JNZ   ALOA22                                                           
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JNZ   ALOA20                                                           
***      TM    SUBIOIND,SUBIOYES   Record already got if Sub required           
***      JNZ   ALOA20                                                           
         TM    PROCMODE,PROCMLQ+PROCMPQ                                         
         JNO   ALOA12                                                           
         L     R1,DXARECB                                                       
         MVC   0(ACCKLEN,R1),0(R2)                                              
         LR    R2,R1                                                            
                                                                                
ALOA12   GOTO1 AGETIT                                                           
         JNE   NO                                                               
                                                                                
ALOA20   TM    PROCMODE,PROCMLQ+PROCMPQ                                         
         JNO   ALOA22                                                           
         L     R2,DXARECB                                                       
                                                                                
ALOA22   GOTO1 ALPAINIT            Initialise extract buffer                    
                                                                                
*        No ignore double 'D'-recs + LEN_DATA compare for Load                  
         DS    0H                  Call record extract routine                  
         GOTO1 ALPAEXTR,DMCB,DXAXREC,(R2),0,(R6),0,ASUBLAST                     
                                                                                
         TM    PROCMODE,PROCMLQ+PROCMPQ                                         
         JNO   ALOA24                                                           
         LA    R2,IOKEY                                                         
                                                                                
ALOA24   MVC   RETBYTE,DMCB+8                                                   
         TM    RETBYTE,RETBNWQ                                                  
         JNZ   ALOA36              Don't write                                  
         TM    RETBYTE,RETBNCQ                                                  
         JNZ   ALOA50              Data not complete - no write                 
         CLC   TYPECODE,=C'CSM'    If client summary only ensure we             
         JE    ALOA40               skip putting records we have read           
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA50                                                           
         CLI   SXDTPLFM,0          Test extract file platform                   
         JE    ALOA30              Put unconverted record only                  
                                                                                
         GOTO1 ALPACNVX,DMCB,(R7)                                               
                                                                                
ALOA30   NI    PROCMOD3,FFQ-PROCCUTQ                                            
                                                                                
         TM    TYPETYPE,CURRYQ+TIMOFFQ                                          
         JNZ   ALOA34              Exclude from currency/Timeoff LOAD           
*                                                                               
         LLC   R0,AGYLISN                                                       
         LA    RF,AGYLIST          Filter agencies to update this file          
         USING AGYLSTD,RF                                                       
                                                                                
ALOA32   CLC   AGYLAID,GSPACES                                                  
         JNH   *+2                 Can't find agency in AGYLIST                 
         CLC   AGYLAID,SAVCALP                                                  
         JE    *+14                                                             
         AHI   RF,AGYLLNQ                                                       
         JCT   R0,ALOA32                                                        
         DC    H'0'                Can't find agency in AGYLIST                 
*                                                                               
         CLI   AGYLCUT,YESQ        Have we done the CUT record for this         
         JE    ALOA34              company?                                     
         MVI   AGYLCUT,YESQ                                                     
         OI    PROCMOD3,PROCCUTQ   trigger it                                   
         DROP  RF                                                               
                                                                                
ALOA34   GOTO1 ARECNUMS,DXASQLB                                                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
                                                                                
         TM    PROCMOD3,PROCCUTQ   do CUT record?                               
         JZ    ALOA40                                                           
         GOTO1 ,DMCB,VAGXCUTC,AINITCUT,0,VAGXCNVX                               
         J     ALOA00              and process it                               
                                                                                
ALOA36   DS    0H                                                               
         TM    PROCMOD3,PROCTOFQ                                                
         JZ    ALOA40              Time off rec not written,clear flag          
         NI    RUNINDS,X'FF'-RUNTIMOF                                           
                                                                                
ALOA40   TM    RETBYTE,RETBCBQ     Call back required?                          
         JO    ALOA20                                                           
         TM    RETBYTE,RETBERQ     Did we error from the routine?               
         JZ    ALOA50                                                           
         CLI   RETCODE,5                                                        
         JNL   ALOA50                                                           
         MVI   RETCODE,5           Set error condition                          
                                                                                
ALOA50   DS    0H                                                               
***      TM    SUBIOIND,SUBIOYES   Sub IO required?                             
***      JZ    ALOA60                                                           
***      GOTO1 (R5)                Filter record will find next sub             
***      JE    ALOA20              Continue with Sub records                    
                                                                                
ALOA60   GOTO1 ADECIOC             Decrement IO count                           
         JNE   NO                  (too many IOs)                               
                                                                                
         TM    PROCMODE,PROCCLQ    Currency load mode?                          
         JNZ   ALOA80                                                           
         TM    PROCMOD3,PROCTOFQ   Timeoff mode?                                
         JNZ   ALOA80                                                           
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JNZ   ALOA80                                                           
         TM    PROCMODE,PROC2DQ    Extra Dim processing?                        
         JNZ   ALOA80                                                           
         TM    PROCMODE,PROCBNQ    Bill Number processing                       
         JNZ   ALOA80                                                           
                                                                                
         TM    PROCMODE,PROCMLQ+PROCMPQ                                         
         JNO   ALOA62                                                           
         MVC   IOKEY,IOKEYPAS      Use passive                                  
         LA    R2,IOKEY                                                         
         J     ALOA64                                                           
                                                                                
ALOA62   DS    0H                  Read next record sequentially                
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
                                                                                
ALOA64   GOTO1 ACHKSEQ             See if read sequence broken                  
         JE    ALOA70                                                           
         GOTO1 AREADHI                                                          
         JNE   *+2                                                              
                                                                                
ALOA70   TM    RETBYTE,RETBFFQ     Straight out as fact to follow?              
         JNZ   ALOA80                                                           
         GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,(R2),DMWORK                    
                                                                                
ALOA80   NI    PROCMOD3,FFQ-PROCCUTQ                                            
         J     YES                                                              
                                                                                
***********************************************************************         
* Subroutine to extract account record in update mode                 *         
* P1 = A(Extract routine)                                             *         
* P2 = A(Extract record initialisation routine)                       *         
* P3 = A(Record filter routine)                                       *         
* P4 = A(Format convert routine)                                      *         
* R6 = A(Extract system table entry)                                  *         
* R7 = A(Extract control data block)                                  *         
***********************************************************************         
                                                                                
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
AUPD000  L     R2,DXARECB                                                       
         TM    PROCMODE,PROCOPQ    'Copy' record mode?                          
         JZ    AUPD002                                                          
         L     R2,DXACPYB                                                       
                                                                                
         USING RCVRECD,R2                                                       
AUPD002  LA    R2,RCVRECRD         R2=A(Acc recovery record)                    
         USING ACTRECD,R2                                                       
         MVC   ALPARMS,0(R1)                                                    
                                                                                
         OC    ALPAFILT,ALPAFILT   Filter routine?                              
         JZ    AUPD010                                                          
         GOTO1 ALPAFILT            Filter record                                
         JNE   AUPD060             (get next)                                   
                                                                                
AUPD010  DS    0H                                                               
                                                                                
AUPD020  DS    0H                  Call record extract routine                  
         GOTO1 ALPAINIT            Initialise extract buffer                    
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JNZ   AUPD024                                                          
         TM    PROCMODE,PROC2DQ    Extra dimension?                             
         JNZ   AUPD024                                                          
         TM    PROCMODE,PROCBNQ    Bill Number dimension?                       
         JNZ   AUPD024                                                          
         TM    PROCMOD2,PROCMRQ    Multiple record processing?                  
         JZ    AUPD024                                                          
         TM    PROCMOD2,PROCMFQ    OK to go?                                    
         JNZ   AUPD022                                                          
         MVI   RETBYTE,0                                                        
         J     AUPD050                                                          
                                                                                
AUPD022  L     R2,ASVREC           Use saved copy of (main) record              
         TM    PROCMODE,PROCOPQ                                                 
         JZ    AUPD024                                                          
         L     R2,ASVOREC                                                       
                                                                                
AUPD024  DS    0H                  Call extract routine                         
                                                                                
         GOTO1 ALPAEXTR,DMCB,DXAXREC,(R2),0,(R6),0,ASUBLAST                     
         MVC   RETBYTE,DMCB+8      Save return code for call back test          
         L     R8,DXAXREC                                                       
         CLI   DXACTION,C'D'       If 'delete' a dimension in upload            
         JNE   AUPD026             mode skip here - don't write it              
         CLI   IS_DIM,YESQ                                                      
         JNE   AUPD026                                                          
                                                                                
         USING DXHDRD,R8                                                        
         L     R0,ACLCBUF          If a delete record, always                   
         XR    R1,R1                                                            
         ICM   R1,B'0011',LEN_DATA Get current data len w/o key                 
         AHI   R1,DXHDRHL          Add header(key) len +28/1C                   
         LR    R2,R8               DXAXREC                                      
         LR    R3,R1               Same len                                     
         MVCL  R0,R2               ...move DXAXREC to ACLCBUF.                  
                                                                                
         TM    PROCMODE,PROCMUQ    Updates?                                     
         JZ    AUPD026                                                          
         J     AUPD040                                                          
                                                                                
AUPD026  TM    RETBYTE,RETBNWQ                                                  
         JO    AUPD040             Don't write                                  
         CLI   DXWRITE,C'Y'                                                     
         JNE   AUPD050             Data not complete - no write                 
         CLI   SXDTPLFM,0          Test extract file platform                   
         JE    AUPD034                                                          
                                                                                
         CLI   DXACTION,C'A'       If 'add' a dimension in upload               
         JNE   AUPD028             mode skip here - don't write it              
         CLI   IS_DIM,YESQ                                                      
         JNE   AUPD028             mode skip here - don't write it              
                                                                                
         L     R1,ACLCBUF          Get compare Buffer                           
         CLC   DXHDRTYP,4(R1)      Same record type?                            
         JNE   AUPD028             Put rec if new type                          
         CLC   DXACTION,10(R1)     DXACTION insync with DXHDRACT?               
         JE    *+2                 Differs normally: 'D' follows 'A'            
         DS    0H                  If same type compare data                    
         L     R0,ACLCBUF          If a delete record, always                   
         AHI   R0,DXHDRHL          Add header len to point AGXREDET             
         XR    R1,R1                                                            
         ICM   R1,B'0011',LEN_DATA Get current data len w/o key                 
         L     R2,DXAXREC                                                       
         AHI   R2,DXHDRHL          Add header len to point AGXREDET             
         LR    R3,R1                                                            
         CLCL  R0,R2               To compare in LEN_DATA offset +28            
         JE    AUPD040             Ignore if its equal                          
                                                                                
AUPD028  DS    0H                  Different record or type                     
                                                                                
         USING AGXRECD,R1                                                       
         L     R1,DXAXREC          For DSRD-19623 we need to skip               
         CLC   AGXRETYP,AGXORDSQ   Status_Update dimensions 'delete'            
         JE    AUPD030             if 'add' follows                             
         CLC   AGXRETYP,AGXESTSQ                                                
         JNE   AUPD032                                                          
                                                                                
AUPD030  CLI   SVACTION,C'D'                                                    
         JE    AUPD034                                                          
         CLI   DXACTION,C'D'                                                    
         JE    AUPD040                                                          
         DROP  R1                                                               
                                                                                
AUPD032  GOTO1 VAGXCNVX,DMCB,(R7)  Put unconverted record only                  
                                                                                
AUPD034  NI    PROCMOD3,FFQ-PROCCUTQ                                            
*                                                                               
         TM    TYPETYPE,TIMOFFQ    Exclude from Timeoff Update                  
         JNZ   AUPD038                                                          
*                                                                               
         LLC   R0,AGYLISN                                                       
         LA    RF,AGYLIST          Filter agencies to update this file          
         USING AGYLSTD,RF                                                       
                                                                                
AUPD036  CLC   AGYLAID,GSPACES                                                  
         JNH   *+2                 Can't find agency in AGYLIST                 
         CLC   AGYLAID,SAVCALP                                                  
         JE    *+14                                                             
         AHI   RF,AGYLLNQ                                                       
         JCT   R0,AUPD036                                                       
         DC    H'0'                Can't find agency in AGYLIST                 
                                                                                
         CLI   AGYLCUT,YESQ        Have we done the CUT record for this         
         JE    AUPD038             company?                                     
         MVI   AGYLCUT,YESQ                                                     
         OI    PROCMOD3,PROCCUTQ   trigger it                                   
         DROP  RF                                                               
                                                                                
AUPD038  GOTO1 ARECNUMS,DXASQLB          record counter                         
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)   write to dsn=                          
         DROP  R8                                                               
                                                                                
         TM    PROCMOD3,PROCCUTQ   do CUT record?                               
         JZ    AUPD040                                                          
         MVC   SVBYTE,DXACTION     save current and make it A                   
         MVI   DXACTION,C'A'                                                    
         GOTO1 ,DMCB,VAGXCUTC,AINITCUT,0,VAGXCNVX                               
         MVC   DXACTION,SVBYTE                                                  
         J     AUPD000             and process it                               
                                                                                
AUPD040  TM    RETBYTE,RETBCBQ     Test if extract call back required           
         JNZ   AUPD020                                                          
         TM    RETBYTE,RETBFFQ     Straight out as fact to follow?              
         JNZ   AUPD050                                                          
                                                                                
AUPD050  MVI   IS_DIM,NOQ          (no difference really)                       
         NI    PROCMOD3,FFQ-PROCCUTQ                                            
         J     YES                                                              
                                                                                
AUPD060  MVI   IS_DIM,NOQ                                                       
         NI    PROCMOD3,FFQ-PROCCUTQ                                            
         J     NO                                                               
         DROP  R2                  ACTRECD                                      
                                                                                
***********************************************************************         
* Initialise all extract records                                      *         
* Entry: R1 = length of extract record                                *         
***********************************************************************         
                                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R0,DXAXREC          R0=A(Extract record)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,GSPACES                                                     
         MVCL  R0,RE               Set record to all spaces                     
                                                                                
         USING DXHDRD,R3                                                        
         L     R3,DXAXREC          R3=A(Extract record area)                    
                                                                                
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         Set minimum record length in header          
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      Load mode?                                   
         JE    IALL06                                                           
         TM    RUNINDS,RUNHDR      Set Header as Load?                          
         JNZ   IALL06                                                           
                                                                                
         USING RCVRECD,R5                                                       
         L     R5,DXARECB          Update mode                                  
         GOTO1 VDATCON,DMCB,(3,RCVDATE),(20,DXHDRCDT)                           
         GOTO1 VDATCON,DMCB,(3,RCVDATE),(0,RCVDATEC)                            
         GOTO1 VADDAY,DMCB,(C'Y',RCVDATEC),WORK,-1                              
         GOTO1 VDATCON,DMCB,(0,WORK),(2,LESS1YR)                                
                                                                                
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,B'1111',RCVTIME  Format date + time from recovery             
         TM    RCVTIME,X'80'                                                    
         JZ    IALL02                                                           
         SLL   RF,1                                                             
         SRL   RF,5                                                             
                                                                                
IALL02   XC    DUB,DUB                                                          
         STCM  RF,B'1111',DUB+4                                                 
         OI    DUB+7,X'0C'                                                      
         TM    PROCMOD3,PROCAASQ                                                
         JZ    IALL04                                                           
         AP    DUB+4(4),PONE                                                    
         TM    DUB+4+2,X'06'       overrun seconds?                             
         JNO   IALL04                                                           
         AP    DUB+4(4),=P'40'                                                  
         TM    DUB+4+1,X'06'       overrun minutes?                             
         CP    DUB+4(4),PZERO                                                   
         JNO   IALL04                                                           
         AP    DUB+4(4),=P'4000'                                                
         CLC   DUB+4+0(2),=X'0240' day overrun?                                 
         JNE   IALL04                                                           
         ZAP   DUB+4(4),PZERO                                                   
         GOTO1 VDATCON,DMCB,(3,RCVDATE),(0,TEMPSTOR)                            
         GOTO1 VADDAY,DMCB,(C'D',TEMPSTOR),TEMPSTOR+6,-1                        
         GOTO1 VDATCON,DMCB,(0,TEMPSTOR+6),(20,DXHDRCDT)                        
                                                                                
IALL04   UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         MVC   SV_REDAT(4),DXHDRCDT                                             
         MVI   SV_REDAT+4,MINUSQ                                                
         MVC   SV_REDAT+5(2),DXHDRCDT+4                                         
         MVI   SV_REDAT+7,MINUSQ                                                
         MVC   SV_REDAT+8(2),DXHDRCDT+6                                         
         MVC   SV_RETIM(2),DXHDRCTI                                             
         MVI   SV_RETIM+2,DOUBLEQ                                               
         MVC   SV_RETIM+3(2),DXHDRCTI+2                                         
         MVI   SV_RETIM+5,DOUBLEQ                                               
         MVC   SV_RETIM+6(2),DXHDRCTI+4                                         
         J     IALL10                                                           
                                                                                
IALL06   MVC   DXHDRCDT,DXDATEN    Load mode                                    
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
                                                                                
         CLI   DXDAFORM,C'Y'                                                    
         JNE   IALL10                                                           
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),GSPACES                                           
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
                                                                                
IALL10   DS    0H                                                               
                                                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
         DROP  R3,R5                                                            
                                                                                
***********************************************************************         
* Initialise CUT records                                              *         
***********************************************************************         
                                                                                
INITCUTD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,CUTDLEDQ         R1=Max len w/o key:CUTD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CUTDLENQ         R1=Max len of currency dimension             
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Load Client records                                                 *         
***********************************************************************         
                                                                                
LOADCLI  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,GSPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),PRODUL                                                
         OI    ACTKACT+L'ACTKACT-1,X'01'                                        
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LCLI02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 All done if C/U/L change                     
                                                                                
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXCLIC,AINITCLI,AFILTCLI,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LCLI02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Client record at R2                                          *         
***********************************************************************         
                                                                                
FILTCLI  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   ACTKUNT(2),PRODUL   Unit/Ledger Ok?                              
         JNE   NO                                                               
         CLC   ACTKACT,GSPACES     Ensure account code                          
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),GSPACES             
         JNE   NO                  And nothing 'below'                          
                                                                                
         LA    RE,ACTKSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FCLI02                                                           
         LA    RE,ACTRSTAT                                                      
                                                                                
FCLI02   TM    0(RE),ACTSABLP                                                   
         JNZ   NO                                                               
                                                                                
         LHI   RF,L'ACTKACT                                                     
         LLC   RE,CLILEN                                                        
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(Data after client code)                 
         SHI   RF,1                                                             
         EX    RF,FCLICLC                                                       
         JNE   NO                  Skip products and jobs                       
         J     YES                                                              
         DROP  R2                                                               
                                                                                
FCLICLC  CLC   0(0,RE),GSPACES     Ensure no product or job                     
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Client record                                            *         
***********************************************************************         
                                                                                
INITCLID NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,CLIDLEDQ         R1=Max len w/o key:CLID                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CLIDLENQ         R1=Max len of client dimension               
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Client records record type ACRTACTH                          *         
***********************************************************************         
                                                                                
UPDTCLI  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         CLC   RCVRECRD+ACTKUNT-ACTKEY(2),PRODUL                                
         JNE   NO                  Exit if not SJ ledger                        
         DROP  R2                                                               
*                                                                               
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMOD2,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UCLI02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UCLI04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UCLI04                                                           
                                                                                
UCLI02   CLI   SVACTION,C'D'                                                    
         JNE   UCLI04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UCLI04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXCLIC,AINITCLI,AFILTCLI,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    UPDTCLIX                                                         
         CLI   SVACTION,C'D'                                                    
         JE    UPDTCLIX                                                         
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UCLI04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UCLI04                                                           
                                                                                
UPDTCLIX DS    0H                                                               
         MVC   DXACTION,SVACTION   RESTORE ACTION                               
         J     YES                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Product records                                                *         
***********************************************************************         
                                                                                
LOADPRO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,GSPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),PRODUL                                                
         OI    ACTKACT+L'ACTKACT-1,X'01'                                        
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LPRO02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 All done if C/U/L change                     
                                                                                
         LLC   R1,CLILEN           If client record extract office              
         LA    R1,ACTKACT(R1)                                                   
         CLC   0(6,R1),GSPACES                                                  
         JNE   LPRO10                                                           
         CLC   ACTKACT,GSPACES                                                  
         JNH   LPRO10                                                           
         CLC   LAST_CKY,ACTKEY     Same as before?                              
         JE    LPRO10                                                           
                                                                                
         MVC   LAST_CKY,ACTKEY                                                  
         MVC   LAST_COF,GSPACES                                                 
                                                                                
         MVC   ACCADDR,ACTKDA                                                   
         L     RE,AIOGEN                                                        
         MVC   0(L'ACCKEY+L'ACCKSTA+L'ACCKDA,RE),ACTKEY                         
         LR    R2,RE                                                            
         GOTO1 AGETIT                                                           
         JNE   LPRO10                                                           
         USING PPRELD,R3                                                        
         LA    R3,ACTRFST                                                       
                                                                                
LPRO04   CLI   PPREL,PPRELQ                                                     
         JE    LPRO06                                                           
         CLI   PPREL,0                                                          
         JE    LPRO10                                                           
         LLC   R1,PPRLN                                                         
         AR    R3,R1                                                            
         J     LPRO04                                                           
                                                                                
LPRO06   MVC   LAST_COF,PPRGAOFF                                                
         DROP  R3                                                               
                                                                                
LPRO10   L     R2,DXARECB                                                       
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXPROC,AINITPRO,AFILTPRO,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LPRO02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Product record at R2                                         *         
***********************************************************************         
                                                                                
FILTPRO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   ACTKUNT(2),PRODUL   Unit/Ledger Ok?                              
         JNE   NO                                                               
         CLC   ACTKACT,GSPACES     Ensure account code                          
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),GSPACES             
         JNE   NO                  And nothing 'below'                          
                                                                                
         LA    RE,ACTKSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FPRO02                                                           
         LA    RE,ACTRSTAT                                                      
                                                                                
FPRO02   TM    0(RE),ACTSABLP                                                   
         JNZ   NO                                                               
                                                                                
         LLC   RE,SAVCSJA                                                       
         LA    RE,ACTKACT(RE)                                                   
         LLC   RF,PROLEN                                                        
         SHI   RF,1                                                             
         EX    RF,FPROCLC                                                       
         JNH   NO                  Skip clients                                 
                                                                                
         LHI   RF,L'ACTKACT                                                     
         LLC   RE,SAVCSJB                                                       
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(Data after product code)                
         SHI   RF,1                                                             
         EX    RF,FPROCLC                                                       
         JNE   NO                  Skip Jobs                                    
         J     YES                                                              
         DROP  R2                                                               
                                                                                
FPROCLC  CLC   0(0,RE),GSPACES     Ensure no job                                
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Product record                                           *         
***********************************************************************         
                                                                                
INITPROD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,PRODLEDQ         R1=Max len w/o key:PROD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,PRODLENQ         R1=Max len of product dimension              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Product records record type ACRTACTH                         *         
***********************************************************************         
                                                                                
UPDTPRO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         CLC   RCVRECRD+ACTKUNT-ACTKEY(2),PRODUL                                
         JNE   NO                  Exit if not SJ ledger                        
         DROP  R2                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UPRO02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UPRO04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UPRO04                                                           
                                                                                
UPRO02   CLI   SVACTION,C'D'                                                    
         JNE   UPRO04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UPRO04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXPROC,AINITPRO,AFILTPRO,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    UPDTPROX                                                         
         CLI   SVACTION,C'D'                                                    
         JE    UPDTPROX                                                         
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UPRO04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UPRO04                                                           
                                                                                
UPDTPROX DS    0H                                                               
         MVC   DXACTION,SVACTION   RESTORE ACTION                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Job records                                                    *         
***********************************************************************         
                                                                                
LOADJOB  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,GSPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),PRODUL                                                
         OI    ACTKACT+L'ACTKACT-1,X'01'                                        
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LJOB02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 All done if C/U/L change                     
                                                                                
         CLC   ACTKACT,GSPACES     If client or product record extract          
         JNH   LJOB20              office                                       
         CLC   TRNKWORK-TRNRECD+ACTKEY(TRNKSBR-TRNKWORK),GSPACES                
         JNE   LJOB20                                                           
         TM    ACTKSTAT,ACTSABLP                                                
         JNZ   LJOB20                                                           
         LA    R4,LAST_POF                                                      
         LLC   R1,CLILEN                                                        
         LA    R1,ACTKACT(R1)                                                   
         CLC   0(6,R1),GSPACES                                                  
         JH    LJOB04                                                           
         LA    R4,LAST_COF         Client record                                
                                                                                
         CLC   LAST_CKY,ACTKEY                                                  
         JE    LJOB20                                                           
                                                                                
         MVC   LAST_CKY,ACTKEY                                                  
         MVC   LAST_COF,GSPACES                                                 
         XC    LAST_PRO,LAST_PRO                                                
         J     LJOB10                                                           
                                                                                
LJOB04   LLC   R1,CLILEN           Product record                               
         AHI   R1,1+2-1                                                         
         EX    R1,LJOBCLI          Same client?                                 
         JNE   *+2                 (should have got it before?)                 
         J     LJOB06                                                           
                                                                                
LJOBCLI  CLC   ACTKEY(0),LAST_CKY                                               
                                                                                
LJOB06   MVC   LAST_PKY,ACTKCPY                                                 
         MVC   LAST_POF,GSPACES                                                 
                                                                                
LJOB10   MVC   ACCADDR,ACTKDA                                                   
         L     RE,AIOGEN                                                        
         MVC   0(L'ACCKEY+L'ACCKSTA+L'ACCKDA,RE),ACTKEY                         
         LR    R2,RE                                                            
         GOTO1 AGETIT                                                           
         JNE   LJOB20                                                           
         USING PPRELD,R3                                                        
         LA    R3,ACTRFST                                                       
                                                                                
LJOB12   CLI   PPREL,PPRELQ                                                     
         JE    LJOB14                                                           
         CLI   PPREL,0                                                          
         JE    LJOB20                                                           
         LLC   R1,PPRLN                                                         
         AR    R3,R1                                                            
         J     LJOB12                                                           
                                                                                
LJOB14   MVC   0(L'LAST_COF,R4),PPRGAOFF                                        
         DROP  R3                                                               
                                                                                
LJOB20   L     R2,DXARECB                                                       
         MVC   ACCADDR,ACTKDA                                                   
                                                                                
         GOTO1 AACCLOAD,DMCB,VAGXJOBC,AINITJOB,AFILTJOB,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LJOB02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Job records at R2                                            *         
***********************************************************************         
                                                                                
FILTJOB  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   ACTKUNT(2),PRODUL   Unit/Ledger Ok?                              
         JNE   NO                                                               
         CLC   ACTKACT,GSPACES     Ensure account code                          
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),GSPACES             
         JNE   NO                  And nothing 'below'                          
                                                                                
         LA    RE,ACTKSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FJOB02                                                           
         LA    RE,ACTRSTAT                                                      
                                                                                
FJOB02   TM    0(RE),ACTSABLP                                                   
         JZ    NO                                                               
                                                                                
         DS    0H                  Any DXF/TDATEP/C filtering?                  
         DS    0H                  (no GETREC done here yet ...)                
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Job record                                               *         
***********************************************************************         
                                                                                
INITJOBD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,JOBDLEDQ         R1=Max len w/o key:JOBD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,JOBDLENQ         R1=Max len of job dimension                  
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Job records record type ACRTACTL                             *         
***********************************************************************         
                                                                                
UPDTJOB  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         CLC   RCVRECRD+ACTKUNT-ACTKEY(2),PRODUL                                
         JNE   NO                  Exit if not SJ ledger                        
         DROP  R2                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UJOB02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UJOB04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UJOB04                                                           
                                                                                
UJOB02   CLI   SVACTION,C'D'                                                    
         JNE   UJOB04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UJOB04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXJOBC,AINITJOB,AFILTJOB,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    UPDTJOBX                                                         
         CLI   SVACTION,C'D'                                                    
         JE    UPDTJOBX                                                         
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UJOB04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UJOB04                                                           
                                                                                
UPDTJOBX DS    0H                                                               
         MVC   DXACTION,SVACTION   RESTORE ACTION                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Job Transaction Records                                        *         
***********************************************************************         
                                                                                
LOADTRN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,GSPACES                                                   
         MVC   TRNKCPY,COMPANY                                                  
         MVC   TRNKUNT(2),PRODUL                                                
         OI    TRNKACT+L'TRNKACT-1,X'01'                                        
         MVC   FULL(3),TRNKEY                                                   
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
         CLC   FULL(3),TRNKEY      Need to check against no TRX at all          
         JNE   YES                                                              
                                                                                
LTRN02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   TRNKEY(TRNKACT-TRNKEY),IOKEY                                     
         JNE   YES                 All done if C/U/L change                     
                                                                                
         XC    SAV_TRN(SAV_TRNL),SAV_TRN                                        
         MVC   SAV_TOFF,LAST_JOF   Set office from saved values                 
         CLC   LAST_JOF,GSPACES                                                 
         JH    LTRN04                                                           
         MVC   SAV_TOFF,LAST_POF                                                
         CLC   LAST_POF,GSPACES                                                 
         JH    LTRN04                                                           
         MVC   SAV_TOFF,LAST_COF                                                
                                                                                
LTRN04   LLC   R1,CLILEN           Retrieve office from Cli/Pro/Job             
         SHI   R1,1                                                             
         LA    RE,TRNKACT                                                       
         EXRL  R1,LTRNXRE                                                       
         JNH   LTRN20                                                           
         LA    R4,LAST_COF                                                      
         LA    R1,TRNKACT+1(R1)                                                 
         LLC   RE,CLILEN                                                        
         LHI   RF,TRNKSTA-TRNKACT-1                                             
         SR    RF,RE                                                            
         EXRL  RF,LTRNXR1                                                       
         JNE   LTRN06                                                           
         XC    LAST_COF,LAST_COF                                                
         XC    LAST_POF,LAST_POF                                                
         XC    LAST_JOF,LAST_JOF                                                
         J     LTRN10                                                           
                                                                                
LTRN06   LLC   RE,CLILEN                                                        
         LA    RE,TRNKACT(RE)                                                   
         LLC   R1,PROLEN                                                        
         SHI   R1,1                                                             
         EXRL  R1,LTRNXRE                                                       
         JNH   LTRN20                                                           
         LA    R4,LAST_POF                                                      
         LLC   RE,SAVCSJB                                                       
         LR    R1,RE                                                            
         LA    R1,TRNKACT(R1)                                                   
         LHI   RF,TRNKSTA-TRNKACT-1                                             
         SR    RF,RE                                                            
         EXRL  RF,LTRNXR1                                                       
         JNE   LTRN08                                                           
         XC    LAST_POF,LAST_POF                                                
         XC    LAST_JOF,LAST_JOF                                                
         J     LTRN10                                                           
                                                                                
LTRN08   LLC   RE,SAVCSJB                                                       
         LA    RE,TRNKACT(RE)                                                   
         LLC   R1,JOBLEN                                                        
         SHI   R1,1                                                             
         EXRL  R1,LTRNXRE                                                       
         JNH   LTRN20                                                           
         LA    R4,LAST_JOF                                                      
         CLC   TRNKWORK(TRNKSTA-TRNKWORK),GSPACES                               
         JNE   LTRN20                                                           
         XC    LAST_JOF,LAST_JOF                                                
                                                                                
LTRN10   MVC   ACCADDR,TRNKDA                                                   
         L     RE,AIOGEN                                                        
         MVC   0(L'ACCKEY+L'ACCKSTA+L'ACCKDA,RE),TRNKEY                         
         LR    R2,RE                                                            
         GOTO1 AGETIT                                                           
         JNE   LTRN20                                                           
         USING PPRELD,R3                                                        
         LA    R3,TRNRFST                                                       
                                                                                
LTRN12   CLI   PPREL,PPRELQ                                                     
         JE    LTRN14                                                           
         CLI   PPREL,0                                                          
         JE    LTRN20                                                           
         LLC   R1,PPRLN                                                         
         AR    R3,R1                                                            
         J     LTRN12                                                           
                                                                                
LTRN14   MVC   0(L'LAST_COF,R4),PPRGAOFF                                        
         J     LTRN20                                                           
         DROP  R3                                                               
                                                                                
LTRNXRE  CLC   0(0,RE),GSPACES                                                  
LTRNXR1  CLC   0(0,R1),GSPACES                                                  
                                                                                
LTRN20   DS    0H                  Dimension:                                   
         MVC   ACCADDR,TRNKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXTRNC,AINITTRN,AFILTTRN,VAGXCNVX                
         JNE   NO                                                               
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LTRN24                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCLOAD,DMCB,VAGFTRNC,AINITRNF,0,VAGXCNVX                       
         JNE   NO                                                               
                                                                                
LTRN22   TM    PROCMODE,PROCBNQ                                                 
         JZ    LTRN06                                                           
         GOTO1 AACCLOAD,DMCB,VAGXBILC,AINITBIL,0,VAGXCNVX                       
         JNE   NO                                                               
         J     LTRN22                                                           
                                                                                
LTRN24   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LTRN02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Job Transaction Records at R2                                *         
***********************************************************************         
                                                                                
FILTTRN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TRNRECD,R2                                                       
         CLC   TRNKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   TRNKUNT(2),PRODUL   Unit/Ledger Ok?                              
         JNE   NO                                                               
                                                                                
         LA    R1,RECTYPE                                                       
                                                                                
         CLI   DXMODE,DXLOADQ                                                   
         JNE   FTRN02                                                           
                                                                                
         GOTO1 VRECTYP,DMCB,(C'I',TRNRECD)                                      
                                                                                
FTRN02   CLI   0(R1),ACRTTRN                                                    
         JE    FTRN04                                                           
         CLI   0(R1),ACRTTRNA                                                   
         JNE   NO                                                               
                                                                                
FTRN04   CLC   TRNKDATE,DXFDATEP   Date filtering                               
         JL    NO                                                               
         OC    DXTDATEC,DXTDATEC                                                
         JZ    FTRN06                                                           
         CLC   TRNKDATE,DXTDATEP                                                
         JH    NO                                                               
                                                                                
FTRN06   LA    R1,TRNKSTAT                                                      
         LA    RE,TRNKSTYP                                                      
         LA    RF,TRNKSTA2                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FTRN08                                                           
         LA    R1,TRNRSTAT                                                      
         LA    RE,TRNRSTYP                                                      
         LA    RF,TRNRSTA2                                                      
                                                                                
FTRN08   TM    0(R1),TRNSDRFT      Skip drafts                                  
*&&US*&& JNZ   NO                                                               
*&&UK*&& JZ    FTRN10                                                           
*&&UK*&& CLI   0(RE),TRNTFBAD      unless advance billing                       
*&&UK*&& JNE   NO                                                               
                                                                                
FTRN10   DS    0H                                                               
         TM    0(RF),TRNSPEEL      Skip peeled transactions                     
         JNZ   NO                                                               
         CLI   0(RE),TRNTORD       Skip order transctions                       
         JE    NO                                                               
                                                                                
FTRN12   DS    0H                                                               
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Transaction Records                                      *         
***********************************************************************         
                                                                                
INITTRND NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TRNDLEDQ         R1=Max len w/o key:TRND                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TRNDLENQ         R1=Max len of transaction dimension          
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Job Transaction Records for Facts                        *         
***********************************************************************         
                                                                                
INITTRNF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TRNFLEDQ         R1=Max len w/o key:TRNF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TRNFLENQ         R1=Max len of transaction facts              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Job Transaction Records - record type ACRTTRN                *         
***********************************************************************         
                                                                                
UPDTTRN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         CLC   RCVRECRD+TRNKUNT-TRNKEY(2),PRODUL                                
         JNE   NO                  Exit if not SJ transactions                  
         DROP  R2                                                               
*                                                                               
         MVC   S_PROCMO,PROCMODE   Save PROCMODE for Order/Order trx            
         MVC   S_PROCM2,PROCMOD2                                                
         MVC   S_PROCM3,PROCMOD3                                                
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UTRN02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UTRN04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UTRN04                                                           
                                                                                
UTRN02   CLI   SVACTION,C'D'                                                    
         JNE   UTRN04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UTRN04   XC    SAV_TRN(SAV_TRNL),SAV_TRN                                        
                                                                                
         DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXTRNC,AINITTRN,AFILTTRN,VAGXCNVX                
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UTRN06                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFTRNC,AINITRNF,0,VAGXCNVX                       
                                                                                
UTRN06   TM    PROCMODE,PROCBNQ                                                 
         JZ    UTRN08                                                           
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXBILC,AINITBIL,0,VAGXCNVX                       
         JNE   NO                                                               
         J     UTRN06                                                           
                                                                                
UTRN08   TM    PROCMODE,PROCOPQ                                                 
         JZ    UTRN10                                                           
         CLI   SVACTION,C'D'                                                    
         JE    UTRN10                                                           
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UTRN04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UTRN04                                                           
                                                                                
UTRN10   MVC   PROCMODE,S_PROCMO   Reset PROCMODE for Order/Order trx           
         MVC   PROCMOD2,S_PROCM2                                                
         MVC   PROCMOD3,S_PROCM3                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load expense transaction records                                    *         
***********************************************************************         
                                                                                
LOADXTR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         LA    R3,EXPLDGS                                                       
                                                                                
LXTR02   CLC   0(L'SCUL,R3),SCUL   (EXPLDGX=EoT)                                
         JE    YES                                                              
                                                                                
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,GSPACES                                                   
         MVC   TRNKCPY,COMPANY                                                  
         MVC   TRNKUNT(2),0(R3)                                                 
         OI    TRNKACT+L'TRNKACT-1,X'01'                                        
         MVC   FULL(3),TRNKEY                                                   
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
         CLC   FULL(3),TRNKEY      Need to check against no TRX at all          
         JNE   LXTR06                                                           
                                                                                
LXTR04   TM    DMCB+8,X'80'        All done if EoF                              
         JO    LXTR06                                                           
         CLC   TRNKEY(TRNKACT-TRNKEY),IOKEY                                     
         JE    LXTR08                                                           
LXTR06   AHI   R3,L'SEUL           Next ledger                                  
         J     LXTR02                                                           
                                                                                
LXTR08   DS    0H                  Dimension:                                   
         MVC   ACCADDR,TRNKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXTRNC,AINITTRN,AFILTXTR,VAGXCNVX                
         JNE   NO                                                               
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LXTR10                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCLOAD,DMCB,VAGFXTRC,AINIXTRF,0,VAGXCNVX                       
         JNE   NO                                                               
                                                                                
LXTR10   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LXTR04                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Expense Transaction Records at R2                            *         
***********************************************************************         
                                                                                
FILTXTR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TRNRECD,R2                                                       
         CLC   TRNKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         LA    R3,EXPLDGS                                                       
                                                                                
FXTR02   CLC   0(L'SCUL,R3),SCUL   (EXPLDGX=EoT)                                
         JE    NO                                                               
         CLC   TRNKUNT(2),0(R3)                                                 
         JE    FXTR04                                                           
         AHI   R3,L'SEUL                                                        
         J     FXTR02                                                           
                                                                                
FXTR04   LA    R1,RECTYPE                                                       
                                                                                
         CLI   DXMODE,DXLOADQ                                                   
         JNE   FXTR06                                                           
                                                                                
         GOTO1 VRECTYP,DMCB,(C'I',TRNRECD)                                      
                                                                                
FXTR06   CLI   0(R1),ACRTTRN                                                    
         JE    FXTR08                                                           
         CLI   0(R1),ACRTTRNA                                                   
         JNE   NO                                                               
                                                                                
FXTR08   CLC   TRNKDATE,DXFDATEP   Date filtering                               
         JL    NO                                                               
         OC    DXTDATEC,DXTDATEC                                                
         JZ    FXTR10                                                           
         CLC   TRNKDATE,DXTDATEP                                                
         JH    NO                                                               
                                                                                
FXTR10   LA    R1,TRNKSTAT                                                      
         LA    RF,TRNKSTA2                                                      
         LA    RE,TRNKSTYP                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FXTR12                                                           
         LA    R1,TRNRSTAT                                                      
         LA    RF,TRNRSTA2                                                      
         LA    RE,TRNRSTYP                                                      
                                                                                
FXTR12   TM    0(R1),TRNSDRFT      Skip drafts                                  
         JNZ   NO                                                               
         TM    0(RF),TRNSPEEL      Skip peeled transactions                     
         JNZ   NO                                                               
         CLI   0(RE),TRNTORD       Skip order transctions                       
         JE    NO                                                               
                                                                                
FXTR16   DS    0H                                                               
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Expense Transaction Records for Facts                    *         
***********************************************************************         
                                                                                
INITXTRF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,XTRFLEDQ         R1=Max len w/o key:TRNF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,XTRFLENQ         R1=Max len of transaction facts              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Expense Transaction Records - record type ACRTTRN            *         
***********************************************************************         
                                                                                
UPDTXTR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         LA    RF,EXPLDGS                                                       
UXTR01   CLC   0(L'SCUL,RF),SCUL   (EXPLDGX=EoT)                                
         JE    NO                  Exit is not expense transaction              
         CLC   RCVRECRD+TRNKUNT-TRNKEY(2),0(RF)                                 
         JE    UXTR01A                                                          
         AHI   RF,L'SEUL                                                        
         J     UXTR01                                                           
         DROP  R2                                                               
*                                                                               
UXTR01A  MVC   S_PROCMO,PROCMODE   Save PROCMODE for Order/Order trx            
         MVC   S_PROCM2,PROCMOD2                                                
         MVC   S_PROCM3,PROCMOD3                                                
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UXTR02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UXTR04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UXTR04                                                           
                                                                                
UXTR02   CLI   SVACTION,C'D'                                                    
         JNE   UXTR04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UXTR04   XC    SAV_TRN(SAV_TRNL),SAV_TRN                                        
                                                                                
         DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXTRNC,AINITTRN,AFILTXTR,VAGXCNVX                
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UXTR08                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFXTRC,AINIXTRF,0,VAGXCNVX                       
                                                                                
UXTR08   TM    PROCMODE,PROCOPQ                                                 
         JZ    UXTR10                                                           
         CLI   SVACTION,C'D'                                                    
         JE    UXTR10                                                           
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UXTR04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UXTR04                                                           
                                                                                
UXTR10   MVC   PROCMODE,S_PROCMO   Reset PROCMODE for Order/Order trx           
         MVC   PROCMOD2,S_PROCM2                                                
         MVC   PROCMOD3,S_PROCM3                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Order Records                                                  *         
***********************************************************************         
                                                                                
LOADORD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ORDRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   FULL(3),ORDKEY                                                   
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
         CLC   FULL(3),ORDKEY      Need to check against no ORD at all          
         JNE   YES                                                              
                                                                                
LORD02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ORDKEY(ORDKORD-ORDKEY),IOKEY                                     
         JNE   YES                 All done if not orders anymore               
                                                                                
         DS    0H                  Dimension:                                   
         MVC   ACCADDR,ORDKDA                                                   
                                                                                
         GOTO1 AACCLOAD,DMCB,VAGXORDC,AINITORD,AFILTORD,VAGXCNVX                
         JNE   NO                                                               
                                                                                
         TM    RETBYTE,RETBFFQ     Only do extra dim if fact return             
         JZ    LORD04                                                           
                                                                                
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCLOAD,DMCB,VAG2ORDC,AINITOR2,0,VAGXCNVX                       
         IPM   R0                                                               
         NI    PROCMODE,FFQ-PROC2DQ                                             
         SPM   R0                                                               
         JNE   NO                                                               
                                                                                
LORD04   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LORD06                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCLOAD,DMCB,VAGFORDC,AINIORDF,0,VAGXCNVX                       
         JNE   NO                                                               
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    LORD04                                                           
         GOTO1 AACCLOAD,DMCB,VAGXOTXC,AINITOTX,0,VAGXCNVX                       
         J     LORD04              as long as there is data ...                 
                                                                                
LORD06   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LORD02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Order Records at R2                                          *         
***********************************************************************         
                                                                                
FILTORD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECTYPE,ACRTAUDT    Take Audit records on update                 
         JNE   FORD10                                                           
         CLI   DXMODE,DXUPDTQ                                                   
         JNE   NO                                                               
         J     YES                                                              
                                                                                
         USING ORDRECD,R2                                                       
FORD10   CLI   ORDKTYP,ORDKTYPQ    Orders?                                      
         JNE   NO                                                               
         CLC   ORDKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   ORDKORD,ORDCNT      Skip control                                 
         JE    NO                                                               
                                                                                
         CLI   DXMODE,DXLOADQ      For load skip sequentials                    
         JNE   FORD12                                                           
         CLI   ORDKSEQ,0                                                        
         JNE   NO                                                               
                                                                                
FORD12   DS    0H                                                               
                                                                                
         DS    0H                  Any DXF/TDATEP/C filtering?                  
         DS    0H                  (no GETREC done here yet ...)                
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Order Records                                            *         
***********************************************************************         
                                                                                
INITORDD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ORDDLEDQ         R1=Max len w/o key:ORDD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ORDDLENQ         R1=Max len of order dimension                
         GOTO1 AINITALL                                                         
                                                                                
         CLI   DXMODE,DXLOADQ      For load get sequentials                     
         JNE   IORD30                                                           
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,DXARECB          (not AIOAREA)                                
         OI    PROCMOD2,PROCSFQ    (load is always facts like)                  
                                                                                
         USING ORDELD,R4                                                        
IORD08   DS    0H                                                               
         SAM31 ,                                                                
         L     R4,ABUFOLD          Fill buffer                                  
         TM    PROCMODE,PROCOPQ                                                 
         JNZ   IORD10                                                           
         L     R4,ABUFFER                                                       
         TM    ORDRSTA2,ORDSSTAT   New 'Status' indicator                       
         JNZ   IORD10                                                           
         OI    PROCMOD2,PROCSFQ    If not on assume 'maint'=facts               
                                                                                
IORD10   MVI   ORDEL,0                                                          
                                                                                
         USING OAMELD,R3                                                        
IORD12   LA    R3,ORDRFST                                                       
                                                                                
IORD14   CLI   OAMEL,0                                                          
         JE    IORD20                                                           
         CLI   ORDKSEQ,0                                                        
         JE    IORD18                                                           
         CLI   OAMEL,ARTELQ                                                     
         JE    IORD17                                                           
         CLI   OAMEL,ATXELQ                                                     
         JE    IORD17                                                           
         CLI   OAMEL,SCMELQ                                                     
         JE    IORD16                                                           
         CLI   OAMEL,RACELQ                                                     
         JE    IORD18                                                           
         CLI   OAMEL,XDFELQ                                                     
         JE    IORD18                                                           
                                                                                
IORD15   LLC   R1,OAMLN                                                         
         AR    R3,R1                                                            
         J     IORD14                                                           
                                                                                
IORD16   TM    OAMELD+SCMTYPE-SCMELD,SCMTSANP                                   
         JZ    IORD18                                                           
                                                                                
IORD17   DS    0H                                                               
                                                                                
*ORD17   TM    PROCMOD2,PROCMRQ    Detect Facts on update                       
*        JZ    IORD18                                                           
*        OI    PROCMOD2,PROCSFQ                                                 
                                                                                
IORD18   LLC   R1,OAMLN            Save element(s)                              
         SHI   R1,1                                                             
         MVC   ORDEL(0),OAMEL                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         MVI   ORDEL,0                                                          
         L     R1,ABUFFER                                                       
         A     R1,ABUFLEN                                                       
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IORD19                                                           
         L     R1,ABUFOLD                                                       
         A     R1,ABUFOLN                                                       
                                                                                
IORD19   CR    R4,R1                                                            
         JNH   IORD15                                                           
         DC    H'0'                Buffer overrun ... (BUFLENQ)                 
                                                                                
IORD20   DS    0H                                                               
         SAM24 ,                                                                
         CLI   DXMODE,DXUPDTQ      Skip here if update mode                     
         JE    IORD90                                                           
         CLI   ORDKSEQ,0                                                        
         JNE   IORD22                                                           
         TM    ORDRSTA2,ORDSEXEX   all done if not Brandocean/Aura              
         JZ    IORD90                                                           
                                                                                
IORD22   MVC   IOKEY,ORDKEY        use current order key                        
                                                                                
         LA    R2,IOKEY                                                         
         MVI   ORDKSP2,FFQ         read for next                                
                                                                                
         L     R2,AIOGEN                                                        
         GOTO1 AREADHI                                                          
         JNE   *+2                                                              
                                                                                
         CLC   ORDKEY(ORDKSEQ-ORDKEY),IOKEYSAV                                  
         JNE   IORD90                                                           
                                                                                
         MVC   ACCADDR,ORDKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                                                              
         SAM31 ,                                                                
         J     IORD12                                                           
         DROP  R2,R3,R4                                                         
                                                                                
         USING RCVRECD,R2                                                       
IORD30   L     R2,DXARECB                                                       
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IORD31                                                           
         L     R2,DXACPYB                                                       
                                                                                
IORD31   LA    R2,RCVRECRD                                                      
         USING AUDRECD,R2                                                       
                                                                                
         CLI   RECTYPE,ACRTAUDT    Audit?                                       
         JNE   IORD32                                                           
         TM    PROCMOD2,PROCMRQ    ???                                          
         JZ    *+2                                                              
         CLC   SAVOORD,AUDKORDN    If 'correct' audit/order trigger             
         JNE   *+2                 processing                                   
         OI    PROCMOD2,PROCMFQ                                                 
         J     IORD90                                                           
         DROP  R2                                                               
                                                                                
         USING ORDRECD,R2                                                       
IORD32   CLI   ORDKSEQ,0                                                        
         JNE   IORD36                                                           
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IORD32A                                                          
                                                                                
         XC    SAV_ORD(SAV_ORDL),SAV_ORD                                        
         MVI   SAV_OAAS,NOQ                                                     
                                                                                
DAT      USING RCVRECD,RE                                                       
         L     RE,DXACPYB                                                       
         MVC   SAV_OCRD,DAT.RCVDATE                                             
         MVC   SAV_OCRT,DAT.RCVTIME                                             
*&&UK*&& NI    SAV_OCRT,FFQ-RCVHXTNQ                                            
*&&US*&& NI    SAV_OCRT,FFQ-RCVXFIL                                             
         J     IORD33                                                           
         DROP  DAT                                                              
                                                                                
DAT      USING RCVRECD,RE                                                       
IORD32A  L     RE,DXARECB          add a second to time of 'change'?            
         OC    SAV_OCRD,SAV_OCRD                                                
         JZ    IORD33                                                           
         CLC   SAV_OCRD,DAT.RCVDATE                                             
         JNE   IORD33                                                           
         MVC   DUB(L'RCVTIME),DAT.RCVTIME                                       
*&&UK*&& NI    DUB,FFQ-RCVHXTNQ                                                 
*&&US*&& NI    DUB,FFQ-RCVXFIL                                                  
         CLC   SAV_OCRT,DUB                                                     
         JNE   IORD33                                                           
         MVI   SAV_OAAS,YESQ                                                    
         DROP  DAT                                                              
                                                                                
IORD33   NI    PROCMOD2,FFQ-PROCOEQ                                             
         MVC   SAVOORD,ORDKORD                                                  
                                                                                
         TM    ORDRSTA2,ORDSEXEX                                                
         JZ    YES                                                              
                                                                                
         TM    SAVOIND,SAVOISTQ                                                 
         JNZ   IORD34                                                           
         TM    PROCMOD3,PROCMOSQ                                                
         JNZ   IORD34                                                           
                                                                                
         OI    PROCMOD2,PROCMRQ                                                 
         NI    PROCMOD2,FFQ-(PROCMFQ+PROCSFQ)                                   
                                                                                
IORD34   L     R0,ASVOREC          Preserve main order record                   
         TM    PROCMODE,PROCOPQ                                                 
         JNZ   IORD35                                                           
         L     R0,ASVREC                                                        
                                                                                
IORD35   ICM   R1,B'0011',ORDRLEN                                               
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     IORD08                                                           
                                                                                
IORD36   CLC   ORDKORD,SAVOORD     Match on order?|                             
         JNE   *+2                                                              
         CLI   ORDKSEQ,ORDKEXTN                                                 
         JNE   IORD38                                                           
         OI    PROCMOD2,PROCOEQ                                                 
                                                                                
         USING ORDELD,R4                                                        
IORD38   DS    0H                                                               
         SAM31 ,                                                                
         L     R4,ABUFFER          Find next slot                               
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IORD40                                                           
         L     R4,ABUFOLD                                                       
                                                                                
IORD40   CLI   ORDEL,0                                                          
         JE    IORD12                                                           
         LLC   R1,ORDLN                                                         
         AR    R4,R1                                                            
         J     IORD40                                                           
         DROP  R2                                                               
                                                                                
IORD90   DS    0H                                                               
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Order Records for Facts                                  *         
***********************************************************************         
                                                                                
INITORDF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ORDFLEDQ         R1=Max len w/o key:ORDF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ORDFLENQ         R1=Max len of order facts                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Order Records                                                *         
***********************************************************************         
                                                                                
UPDTORD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
                                                                                
         USING RCVRECD,R2                                                       
         L     R2,DXARECB                                                       
         MVC   SVACTION,DXACTION   Save current DXTRACT action                  
                                                                                
         USING AUDRECD,R3                                                       
         LA    R3,RCVRECRD                                                      
         CLI   RECTYPE,ACRTORD     Order or Audit?                              
         JE    UORD02                                                           
         CLI   RECTYPE,ACRTAUDT    (what else?)                                 
         JNE   *+2                                                              
         CLC   SAVOORD,GSPACES     Skip standalone audits                       
         JNH   UORD90                                                           
         CLC   LT_SIN,RCVSEQNO     Skip if other SIN                            
         JNE   UORD90                                                           
         TM    SAVOIND,SAVOISTQ    Skip in status scenario                      
         JNZ   UORD90                                                           
         J     UORD10                                                           
                                                                                
         USING ORDRECD,R3                                                       
UORD02   CLC   ORDKORD,ORDCNT      Skip control                                 
         JE    UORD90                                                           
         CLI   ORDKSEQ,0           Main record?                                 
         JNE   UORD06                                                           
         XC    SAVORD(SAVORDLQ),SAVORD                                          
         MVC   SAVOORD,ORDKORD     Init new order                               
         MVI   SAVOEXT,FALSEQ      BrandOcean/Aura order?                       
                                                                                
         TM    ORDRSTA2,ORDSEXEX                                                
         JZ    UORD04                                                           
         MVI   SAVOEXT,YESQ                                                     
                                                                                
UORD04   TM    ORDRSTA2,ORDSSTAT   Status call?                                 
         JZ    UORD08                                                           
         OI    SAVOIND,SAVOISTQ                                                 
         OI    PROCMOD3,PROCMOSQ                                                
         J     UORD08                                                           
                                                                                
UORD06   CLC   SAVOORD,ORDKORD     (invalid data sequence)                      
         JNE   *+2                                                              
         TM    SAVOIND,SAVOISTQ    (invalid data scenario)                      
         JNZ   *+2                                                              
         CLC   LT_SIN,RCVSEQNO     (invalid data scenario)                      
         JNE   *+2                                                              
                                                                                
UORD08   DS    0H                  (nothing else here)                          
                                                                                
UORD10   TM    SAVOIND,SAVOISTQ    Status update scenario?                      
         JNZ   UORD50                                                           
                                                                                
         CLI   LT_ACT,C'C'         Were we in change mode?                      
         JNE   UORD12                                                           
         CLC   LT_SIN,RCVSEQNO     Same SIN as before?                          
         JNE   UORD12                                                           
         CLI   SVACTION,C'C'       Skip change                                  
         JE    UORD12                                                           
         CLI   SVACTION,C'A'       If add treat as add part to change           
         JNE   UORD11                                                           
***      MVI   DXACTION,C'C'                                                    
         OI    PROCMOD2,PROCACQ    Add is change/add                            
         J     UORD12                                                           
                                                                                
UORD11   CLI   SVACTION,C'D'       If delete treat as delete prt to             
         JNE   *+2                 change ??? what is this ???                  
***      MVI   DXACTION,C'C'                                                    
         OI    PROCMOD2,PROCDCQ    Delete is change/delete                      
                                                                                
UORD12   CLI   DXACTION,C'A'       Adding an order?                             
         JNE   UORD20                                                           
                                                                                
         CLI   RECTYPE,ACRTAUDT                                                 
         JE    UORD14                                                           
                                                                                
         CLI   SAVOEXT,YESQ                                                     
         JE    UORD14                                                           
         J     UORD40                                                           
                                                                                
UORD14   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXORDC,AINITORD,AFILTORD,VAGXCNVX                
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD16                                                           
                                                                                
         TM    PROCMOD2,PROCOEQ    Extension received?                          
         JZ    UORD16                                                           
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCUPDT,DMCB,VAG2ORDC,AINITOR2,0,VAGXCNVX                       
         NI    PROCMODE,FFQ-PROC2DQ                                             
                                                                                
UORD16   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD18                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFORDC,AINIORDF,0,VAGXCNVX                       
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UORD16                                                           
***      MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXOTXC,AINITOTX,0,VAGXCNVX                       
         J     UORD16                                                           
                                                                                
UORD18   TM    PROCMOD2,PROCMRQ+PROCMFQ                                         
         JNO   UORD90                                                           
         NI    PROCMOD2,FFQ-(PROCMRQ+PROCMFQ+PROCSFQ)                           
         XC    SAVOORD,SAVOORD                                                  
         J     UORD90                                                           
                                                                                
UORD20   CLI   DXACTION,C'C'       Change?                                      
         JNE   UORD30                                                           
                                                                                
         J     UORD50                                                           
                                                                                
UORD30   CLI   DXACTION,C'D'       Delete? Anything else - die                  
         JNE   *+2                 Delete seems not to occur (unless            
         TM    PROCMOD2,PROCDCQ    special delete within change from            
         JZ    *+2                 UORD05 scenario)                             
                                                                                
         J     UORD50                                                           
                                                                                
UORD40   DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXORDC,AINITORD,AFILTORD,VAGXCNVX                
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD42                                                           
                                                                                
         TM    PROCMOD2,PROCOEQ    Extension received?                          
         JZ    UORD42                                                           
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCUPDT,DMCB,VAG2ORDC,AINITOR2,0,VAGXCNVX                       
         NI    PROCMODE,FFQ-PROC2DQ                                             
                                                                                
UORD42   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD90                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFORDC,AINIORDF,0,VAGXCNVX                       
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UORD42                                                           
***      MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXOTXC,AINITOTX,0,VAGXCNVX                       
         J     UORD42                                                           
                                                                                
UORD50   NI    PROCMODE,FFQ-PROCOPQ                                             
                                                                                
         TM    PROCMOD2,PROCACQ    Try to process this as 'regular'             
         JNZ   UORD54              add scenario                                 
                                                                                
         TM    PROCMOD2,PROCDCQ    Try to process this as 'regular'             
         JNZ   UORD52              delete scenario                              
                                                                                
         CLI   DXACTION,C'C'                                                    
         JNE   UORD52                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UORD54                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UORD54                                                           
                                                                                
UORD52   CLI   SVACTION,C'D'                                                    
         JNE   UORD54                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UORD54   DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXORDC,AINITORD,AFILTORD,VAGXCNVX                
                                                                                
*        TM    SAVOIND,SAVOISTQ    Skip in status scenario                      
*        JZ    UORD55                                                           
*        NI    RETBYTE,FFQ-RETBFFQ                                              
*                                                                               
*ORD55   DS    0H                                                               
                                                                                
         TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD56                                                           
                                                                                
         TM    PROCMOD2,PROCOEQ    Extension received?                          
         JZ    UORD56                                                           
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCUPDT,DMCB,VAG2ORDC,AINITOR2,0,VAGXCNVX                       
         NI    PROCMODE,FFQ-PROC2DQ                                             
                                                                                
UORD56   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UORD60                                                           
         TM    PROCMOD3,PROCMOSQ                                                
         JNZ   UORD60                                                           
***      CLI   SVSOURCE,RCVPOSTQ   Order update via Postman?                    
***      JE    UORD60                                                           
         TM    PROCMOD2,PROCMRQ+PROCMFQ                                         
         JNO   UORD58                                                           
         TM    PROCMOD2,PROCSFQ    Skip facts if no sequentials ...             
         JZ    UORD60                                                           
                                                                                
UORD58   DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFORDC,AINIORDF,0,VAGXCNVX                       
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UORD56                                                           
***      MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXOTXC,AINITOTX,0,VAGXCNVX                       
         J     UORD56                                                           
                                                                                
UORD60   TM    PROCMODE,PROCOPQ                                                 
         JZ    UORD62                                                           
         CLI   SVACTION,C'D'                                                    
         JE    UORD62                                                           
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UORD54                                                           
         MVI   DXACTION,C'A'                                                    
         CLI   SAV_OAAS,YESQ                                                    
         JNE   UORD54                                                           
         OI    PROCMOD3,PROCAASQ                                                
         J     UORD54                                                           
                                                                                
UORD62   TM    PROCMOD2,PROCMRQ+PROCMFQ                                         
         JNO   UORD90                                                           
         NI    PROCMOD2,FFQ-(PROCMRQ+PROCMFQ+PROCSFQ+PROCOEQ)                   
         XC    SAVOORD,SAVOORD                                                  
                                                                                
UORD90   TM    PROCMOD3,PROCMOSQ                                                
         JZ    UORD92                                                           
         OI    SAVOIND,SAVOISTQ                                                 
                                                                                
UORD92   NI    PROCMOD3,FFQ-(PROCMOEQ+PROCMOSQ+PROCAASQ)                        
         TM    PROCMOD2,PROCACQ+PROCDCQ                                         
         JZ    YES                 If set unset all                             
         NI    PROCMOD2,FFQ-(PROCACQ+PROCDCQ)                                   
         MVI   SVACTION,C'C'       and set for LT_ACT next time                 
         J     YES                                                              
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Estimate Records                                               *         
***********************************************************************         
                                                                                
LOADEST  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ESTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,COMPANY                                                  
         MVC   FULL(3),ESTKEY                                                   
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
         CLC   FULL(3),ESTKEY      Need to check against no EST at all          
         JNE   YES                                                              
                                                                                
LEST02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ESTKEY(ESTKCLI-ESTKEY),IOKEY                                     
         JNE   YES                 All done if not estimates anymore            
                                                                                
         DS    0H                  Dimension:                                   
         MVC   ACCADDR,ESTKDA                                                   
                                                                                
         XC    SAV_EST(SAV_ESTL),SAV_EST                                        
                                                                                
         GOTO1 AACCLOAD,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
         JNE   NO                                                               
                                                                                
         TM    RETBYTE,RETBFFQ     Only do extra dim if fact return             
         JZ    LEST04                                                           
                                                                                
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCLOAD,DMCB,VAG2ESTC,AINITES2,0,VAGXCNVX                       
         IPM   R0                                                               
         NI    PROCMODE,FFQ-PROC2DQ                                             
         SPM   R0                                                               
         JNE   NO                                                               
                                                                                
LEST04   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LEST06                                                           
         DS    0H                  Fact(s):                                     
         GOTO1 AACCLOAD,DMCB,VAGFESTC,AINIESTF,0,VAGXCNVX                       
         JNE   NO                                                               
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    LEST04                                                           
         GOTO1 AACCLOAD,DMCB,VAGXETXC,AINITETX,0,VAGXCNVX                       
         J     LEST04              as long as there is data ...                 
                                                                                
LEST06   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LEST02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Estimate Records at R2                                       *         
***********************************************************************         
                                                                                
FILTEST  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECTYPE,ACRTAUDT    Take Audit records on update                 
         JNE   FEST10                                                           
         CLI   DXMODE,DXUPDTQ                                                   
         JNE   NO                                                               
         J     YES                                                              
                                                                                
         USING ESTRECD,R2                                                       
FEST10   CLI   ESTKTYP,ESTKTYPQ    Estimates?                                   
         JNE   NO                                                               
         CLI   ESTKSUB,ESTKSUBQ                                                 
         JNE   NO                                                               
         CLC   ESTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         CLI   ESTKPRO,ESTKBPQ     Skip ballpark estimates                      
         JE    NO                                                               
         CLI   ESTKJOB,ESTKBPQ                                                  
         JE    NO                                                               
                                                                                
         CLI   DXMODE,DXLOADQ      For load skip sequentials                    
         JNE   FEST12                                                           
         CLI   ESTKSEQ,ESTKSMQ                                                  
         JNE   NO                                                               
                                                                                
FEST12   DS    0H                                                               
                                                                                
         DS    0H                  Any DXF/TDATEP/C filtering?                  
         DS    0H                  (no GETREC done here yet ...)                
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Estimate Records for Dimensions                          *         
***********************************************************************         
                                                                                
INITESTD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ESTDLEDQ         R1=Max len w/o key:ESTD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ESTDLENQ         R1=Max len of estimate dimension             
         GOTO1 AINITALL                                                         
                                                                                
         CLI   DXMODE,DXLOADQ      For load get sequentials                     
         JNE   IEST30                                                           
                                                                                
         USING ESTRECD,R2                                                       
         L     R2,DXARECB          (not AIOAREA)                                
                                                                                
         USING EMDELD,R4                                                        
IEST00   DS    0H                                                               
         SAM31 ,                                                                
         L     R4,ABUFFER          Fill buffer (use TSAR later on)              
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IEST01                                                           
         L     R4,ABUFOLD                                                       
                                                                                
IEST01   MVI   EMDEL,0                                                          
                                                                                
         USING ERDELD,R3                                                        
IEST02   LA    R3,ESTRFST                                                       
                                                                                
IEST04   CLI   ERDEL,0             carry over all elements                      
         JE    IEST12                                                           
         CLI   ERDEL,ERDELQ                                                     
         JNE   IEST08                                                           
         TM    PROCMOD2,PROCMRQ    Detect Facts on update                       
         JZ    IEST08                                                           
         OI    PROCMOD2,PROCSFQ                                                 
         J     IEST08                                                           
                                                                                
IEST06   LLC   R1,ERDLN                                                         
         AR    R3,R1                                                            
         J     IEST04                                                           
                                                                                
IEST08   LLC   R1,ERDLN            Save element(s)                              
         SHI   R1,1                                                             
         MVC   EMDEL(0),ERDEL                                                   
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         MVI   EMDEL,0                                                          
         L     R1,ABUFFER                                                       
         A     R1,ABUFLEN                                                       
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IEST10                                                           
         L     R1,ABUFOLD                                                       
         A     R1,ABUFOLN                                                       
                                                                                
IEST10   CR    R4,R1                                                            
         JNH   IEST06                                                           
         DC    H'0'                Buffer overrun ... (BUFLENQ)                 
                                                                                
IEST12   DS    0H                                                               
         SAM24 ,                                                                
                                                                                
IEST14   CLI   DXMODE,DXUPDTQ      Skip here if update mode                     
         JE    IEST90                                                           
                                                                                
         MVC   IOKEY,ESTKEY        read for next estimate seq record            
                                                                                
         LA    R2,IOKEY                                                         
         LLC   R1,ESTKSEQ                                                       
         CHI   R1,FFQ                                                           
         JE    *+2                                                              
         AHI   R1,1                                                             
         STC   R1,ESTKSEQ                                                       
                                                                                
         L     R2,AIOGEN                                                        
         GOTO1 AREADHI                                                          
         JNE   *+2                                                              
         CLC   ESTKEY,IOKEYSAV                                                  
         JNE   IEST90                                                           
                                                                                
         MVC   ACCADDR,ESTKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                                                              
         SAM31 ,                                                                
         J     IEST02                                                           
         DROP  R2,R3,R4                                                         
                                                                                
         USING RCVRECD,R2                                                       
IEST30   L     R2,DXARECB                                                       
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IEST31                                                           
         L     R2,DXACPYB                                                       
                                                                                
IEST31   LA    R2,RCVRECRD                                                      
         USING AUDRECD,R2                                                       
         CLI   RECTYPE,ACRTAUDT    Audit?                                       
         JNE   IEST32                                                           
         TM    PROCMOD2,PROCMRQ    ???                                          
         JZ    *+2                                                              
         J     IEST90                                                           
                                                                                
         USING ESTRECD,R2                                                       
IEST32   CLI   ESTKSEQ,ESTKSMQ                                                  
         JNE   IEST36                                                           
                                                                                
         L     R0,ASVOREC          Preserve main estimate record                
         TM    PROCMODE,PROCOPQ                                                 
         JNZ   IEST34                                                           
         L     R0,ASVREC                                                        
                                                                                
IEST34   ICM   R1,B'0011',ESTRLEN                                               
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     IEST00                                                           
                                                                                
IEST36   DS    0H                                                               
                                                                                
         USING EMDELD,R4                                                        
IEST38   DS    0H                                                               
         SAM31 ,                                                                
         L     R4,ABUFFER          Find next slot                               
         TM    PROCMODE,PROCOPQ                                                 
         JZ    IEST40                                                           
         L     R4,ABUFOLD                                                       
                                                                                
IEST40   CLI   EMDEL,0                                                          
         JE    IEST02                                                           
         LLC   R1,EMDLN                                                         
         AR    R4,R1                                                            
         J     IEST40                                                           
         DROP  R2                                                               
                                                                                
IEST90   DS    0H                                                               
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Estimate Records for Facts                               *         
***********************************************************************         
                                                                                
INITESTF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ESTFLEDQ         R1=Max len w/o key:ESTF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ESTFLENQ         R1=Max len of estimate facts                 
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Estimate Records                                             *         
*                                                                     *         
* => Brandocean/Aura updates estimates either by status change which  *         
*    gives a COPY/CHANGE pair with old/new values (dimension change   *         
*    only for ESTDAST), followed by an AUDREC change.                 *         
*    Or it updates the overall estimate (genuine estimate change) by  *         
*    first deleting all ESTRECDs (00-nn, copy to change/delete), then *         
*    readding all estimates (change/delete to copy, or adds).         *         
*                                                                     *         
***********************************************************************         
                                                                                
UPDTEST  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         MVC   SVACTION,DXACTION                                                
                                                                                
         USING RCVRECD,R3                                                       
         L     R3,DXARECB          Must be Aura/Prod - skip                     
         CLI   RCVPRGNO,RCVPBRAQ   anything else for now                        
         JE    *+12                                                             
         CLI   RCVPRGNO,RCVPPROQ   Job deleted/restored in =Prod                
         JNE   NO                                                               
                                                                                
         LA    R2,RCVRECRD                                                      
                                                                                
         CLC   LT_SIN,RCVSEQNO     Do we have a new SIN/estimate?               
         JNE   UEST002                                                          
         CLI   LT_RTYP,ACRTESTR                                                 
         JE    UEST100                                                          
         CLI   LT_RTYP,ACRTAUDT    If was audit before skip next audit          
         JNE   UEST002             record                                       
         CLI   RECTYPE,ACRTAUDT                                                 
         JE    UEST900                                                          
                                                                                
         USING ESTRECD,R2                                                       
UEST002  CLI   ESTKSEQ,ESTKSMQ     New SIN must be a main estimate              
         JNE   *+2                                                              
                                                                                
         DS    0H                  Initialise estimate saved data               
         XC    SAV_EST(SAV_ESTL),SAV_EST                                        
         GOTO1 ASETESTK,ESTKEY                                                  
         MVC   SAV_EKEY,SAV_ECKY                                                
                                                                                
         CLI   DXACTION,C'A'       Scenario 'Add'                               
         JE    UEST020                                                          
         CLI   DXACTION,C'C'       Scenario 'Status change=main est'            
         JE    UEST030                                                          
         CLI   DXACTION,C'D'       Scenario 'Genuine change'                    
         JNE   *+2                                                              
                                                                                
UEST010  MVI   SAV_ESCE,SAV_ECHA   * Genuine estimate change *                  
         NI    PROCMODE,FFQ-PROCOPQ                                             
         MVI   PROCMOD2,PROCMRQ+PROCP1Q                                         
DAT      USING RCVRECD,RE                                                       
         L     RE,DXACPYB                                                       
         MVC   SAV_ECRD,DAT.RCVDATE                                             
         MVC   SAV_ECRT,DAT.RCVTIME                                             
*&&UK*&& NI    SAV_ECRT,FFQ-RCVHXTNQ                                            
*&&US*&& NI    SAV_ECRT,FFQ-RCVXFIL                                             
         J     UEST500                                                          
         DROP  DAT                                                              
                                                                                
UEST020  MVI   SAV_ESCE,SAV_EADD   * Estimate add *                             
         NI    PROCMODE,FFQ-PROCOPQ                                             
         MVI   PROCMOD2,PROCMRQ                                                 
         J     UEST200             Add to buffer                                
                                                                                
UEST030  MVI   SAV_ESCE,SAV_ESTA   * Estimate status change (ESTKSMQ) *         
                                                                                
JCS      USING RCVRECD,R1                                                       
         L     R1,DXARECB          Check for 'Job close' scenario               
         LA    RF,JCS.RCVRECRD                                                  
JC2      USING ESTRECD,RF                                                       
         L     R1,DXACPYB                                                       
         LA    RE,JCS.RCVRECRD                                                  
JC1      USING ESTRECD,RE                                                       
                                                                                
* Following 2 lines added, discovered when creating client summary recs         
* as update showed an estimate that has a job reopened and is                   
* then approved would be skipped, so we need to check status 1                  
* is the same                                                                   
                                                                                
         CLC   JC1.ESTRSTA1,JC2.ESTRSTA1 Check whether approval                 
         JNE   UEST032                    status has changed                    
         MVC   HALF+0(1),JC1.ESTRSTA3                                           
         NI    HALF+0,ESTSJBCL                                                  
         MVC   HALF+1(1),JC2.ESTRSTA3                                           
         NI    HALF+1,ESTSJBCL                                                  
         CLC   HALF+0(1),HALF+1    Any change in ESTSJBCL?                      
         JE    UEST032                                                          
         OI    SAV_EIND,SAV_EJCQ   Set to JCS and skip                          
         J     UEST900                                                          
         DROP  JCS,JC1,JC2                                                      
                                                                                
UEST032  NI    PROCMODE,FFQ-PROCOPQ                                             
         MVI   PROCMOD2,0                                                       
         J     UEST400                                                          
                                                                                
UEST100  CLI   SAV_ESCE,0          Another or new Audit record?                 
         JE    UEST102                                                          
         CLI   SAV_ESCE,SAV_ECHA   Genuine change scenario?                     
         JE    UEST110                                                          
         CLI   SAV_ESCE,SAV_EADD   Add scenario?                                
         JE    UEST120                                                          
         CLI   SAV_ESCE,SAV_ESTA   Status change scenario?                      
         JNE   *+2                                                              
         GOTO1 ASETESTK,ESTKEY     Job close in/out scenario won't get          
         CLC   SAV_EKEY,SAV_ECKY   detected ... so try here                     
         JE    UEST130                                                          
         CLC   SAV_EKEY(12),SAV_ECKY                                            
         JNE   UEST130                                                          
         J     UEST002             retry it                                     
                                                                                
UEST102  CLI   RECTYPE,ACRTAUDT                                                 
         JNE   *+2                                                              
         J     UEST900             (skip)                                       
                                                                                
UEST110  DS    0H                  * Genuine estimate change *                  
         USING AUDRECD,R2                                                       
         CLI   RECTYPE,ACRTAUDT    Audit?                                       
         JNE   UEST112                                                          
         OC    SAV_EKEY,SAV_EKEY   Skip if no estimate in progress              
         JZ    UEST900                                                          
         TM    SAV_EIND,SAV_EIBQ   Skip if estimate is ballpark                 
         JNZ   UEST900                                                          
         CLC   SAV_EKEY,AUDKECPJ   Ensure correct internal key                  
         JNE   *+2                                                              
         TM    PROCMOD2,PROCSFQ    Any sequentials?                             
         JZ    *+2                 (???)                                        
         OI    PROCMOD2,PROCMFQ    process estimate                             
         J     UEST600                                                          
                                                                                
         USING ESTRECD,R2                                                       
UEST112  GOTO1 ASETESTK,ESTKEY     Check estimate internal key                  
         CLC   SAV_EKEY,SAV_ECKY                                                
         JNE   *+2                 (what scenario is this?)                     
         CLI   ESTKSEQ,ESTKSMQ     Main record again?                           
         JNE   UEST114                                                          
         CLI   DXACTION,C'A'       (this is add from copy/delete to             
         JNE   *+2                 change)                                      
         OI    PROCMOD2,PROCP2Q    Phase 2 now                                  
         NI    PROCMOD2,FFQ-PROCP1Q                                             
                                                                                
DAT      USING RCVRECD,RE                                                       
         NI    SAV_EIND,FFQ-SAV_EASQ                                            
         L     RE,DXARECB                                                       
         OC    SAV_ECRD,SAV_ECRD                                                
         JZ    UEST500                                                          
         CLC   SAV_ECRD,DAT.RCVDATE                                             
         JNE   UEST500                                                          
         MVC   DUB(L'RCVTIME),DAT.RCVTIME                                       
*&&UK*&& NI    DUB,FFQ-RCVHXTNQ                                                 
*&&US*&& NI    DUB,FFQ-RCVXFIL                                                  
         CLC   SAV_ECRT,DUB                                                     
         JNE   UEST500                                                          
         OI    SAV_EIND,SAV_EASQ   indicate 'add a second' scenario             
         J     UEST500                                                          
         DROP  DAT                                                              
                                                                                
UEST114  TM    PROCMOD2,PROCP1Q    Phase 1 running?                             
         JZ    UEST116                                                          
         CLI   DXACTION,C'C'       Change and delete only (use for old)         
         JE    UEST500                                                          
         CLI   DXACTION,C'D'                                                    
         JE    UEST500                                                          
         J     *+2                 (there can't be an add in this case)         
                                                                                
UEST116  TM    PROCMOD2,PROCP2Q    Phase 2 running?                             
         JZ    *+2                                                              
         CLI   DXACTION,C'A'       Add and change only (use for new)            
         JE    UEST500                                                          
         CLI   DXACTION,C'C'                                                    
         JNE   *+2                 (there can't be a del in this case)          
         J     UEST500                                                          
                                                                                
UEST120  CLI   DXACTION,C'A'       * Estimate add *                             
         JNE   *+2                 (how can this be?)                           
         CLI   RECTYPE,ACRTAUDT    Audit or estimate sequential?                
         JE    UEST122                                                          
         GOTO1 ASETESTK,ESTKEY     Check estimate internal key                  
         CLC   SAV_EKEY,SAV_ECKY                                                
         JNE   *+2                 (what scenario is this?)                     
         OI    PROCMOD2,PROCSFQ                                                 
         J     UEST200             Add to buffer                                
                                                                                
         USING AUDRECD,R2                                                       
UEST122  CLC   SAV_EKEY,AUDKECPJ   Ensure correct internal key                  
         JNE   *+2                                                              
         TM    SAV_EIND,SAV_EIBQ   Skip if estimate is ballpark                 
         JNZ   UEST900                                                          
         TM    PROCMOD2,PROCSFQ    Any sequentials?                             
         JZ    *+2                 (???)                                        
         OI    PROCMOD2,PROCMFQ    Process estimate                             
         J     UEST300                                                          
                                                                                
         USING ESTRECD,R2                                                       
UEST130  DS    0H                  * Estimate status change (ESTKSMQ) *         
         TM    SAV_EIND,SAV_EJCQ   Skip if Job Close scenario                   
         JNZ   UEST900                                                          
         USING AUDRECD,R2                                                       
         CLI   RECTYPE,ACRTAUDT    Audit or estimate sequential?                
         JNE   *+2                 (how can we have sequentials?)               
         OC    SAV_EKEY,SAV_EKEY   Skip if no estimate in progress              
         JZ    UEST900                                                          
         TM    SAV_EIND,SAV_EIBQ   Skip if estimate is ballpark                 
         JNZ   UEST900                                                          
         CLC   SAV_EKEY,AUDKECPJ   Ensure correct internal key                  
         JNE   *+2                                                              
         J     UEST800             Housekeeping via AUDRECD                     
                                                                                
         USING ESTRECD,R2                                                       
UEST200  DS    0H                  INITEST processing only to put data          
         DS    0H                  to buffer                                    
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
                                                                                
         J     UEST900                                                          
                                                                                
UEST300  DS    0H                                                               
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
                                                                                
UEST302  TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UEST800                                                          
                                                                                
         TM    PROCMOD2,PROCSFQ    Extra dim only if sequentials                
         JZ    UEST304                                                          
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCUPDT,DMCB,VAG2ESTC,AINITES2,0,VAGXCNVX                       
         NI    PROCMODE,FFQ-PROC2DQ                                             
                                                                                
UEST304  TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UEST800                                                          
                                                                                
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFESTC,AINIESTF,0,VAGXCNVX                       
                                                                                
UEST306  TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UEST304                                                          
*        MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXETXC,AINITETX,0,VAGXCNVX                       
                                                                                
         J     UEST304                                                          
                                                                                
UEST400  DS    0H                  Dimension only processing                    
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UEST402                                                          
                                                                                
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
                                                                                
UEST402  DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
                                                                                
UEST404  TM    PROCMODE,PROCOPQ                                                 
         JZ    UEST900                                                          
         CLI   SVACTION,C'D'                                                    
         JE    UEST900                                                          
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   *+2                                                              
         MVI   DXACTION,C'A'                                                    
         J     UEST402                                                          
                                                                                
UEST500  DS    0H                  INITEST to put record to buffer              
         TM    PROCMOD2,PROCP2Q    Phase 1 or 2?                                
         JNZ   UEST502                                                          
         OI    PROCMODE,PROCOPQ    Phase 1 use 'old record'                     
                                                                                
UEST502  DS    0H                  INITEST to buffer data only                  
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
         NI    PROCMODE,FFQ-PROCOPQ                                             
                                                                                
         J     UEST900                                                          
                                                                                
UEST600  DS    0H                  Process this estimate                        
                                                                                
         NI    PROCMOD3,FFQ-PROCAASQ                                            
                                                                                
         MVI   DXACTION,C'C'                                                    
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UEST602                                                          
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
                                                                                
UEST602  DS    0H                  Dimension:                                   
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXESTC,AINITEST,AFILTEST,VAGXCNVX                
                                                                                
UEST604  TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UEST608                                                          
         TM    PROCMOD2,PROCMRQ+PROCMFQ                                         
         JNO   *+2                 (wrong logic here)                           
         TM    PROCMOD2,PROCSFQ                                                 
         JZ    *+2                 (wrong logic here)                           
                                                                                
         TM    PROCMOD2,PROCSFQ    Extra dim only if sequentials                
         JZ    UEST606                                                          
         OI    PROCMODE,PROC2DQ                                                 
         GOTO1 AACCUPDT,DMCB,VAG2ESTC,AINITES2,0,VAGXCNVX                       
         NI    PROCMODE,FFQ-PROC2DQ                                             
                                                                                
UEST606  TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UEST608                                                          
                                                                                
         DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFESTC,AINIESTF,0,VAGXCNVX                       
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UEST606                                                          
*        MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXETXC,AINITETX,0,VAGXCNVX                       
         J     UEST606                                                          
                                                                                
UEST608  TM    PROCMODE,PROCOPQ                                                 
         JZ    UEST800                                                          
         TM    ESTRSTA1,ESTKDELT                                                
         JNZ   UEST800             Skip readd if job deleted                    
*                                                                               
         NI    PROCMODE,FFQ-PROCOPQ                                             
         MVI   DXACTION,C'A'                                                    
         TM    SAV_EIND,SAV_EASQ                                                
         JZ    UEST602                                                          
         OI    PROCMOD3,PROCAASQ                                                
         J     UEST602                                                          
                                                                                
UEST800  DS    0H                  End of this estimate                         
         NI    PROCMODE,FFQ-PROCOPQ                                             
         MVI   PROCMOD2,0                                                       
         MVI   PROCMOD3,0                                                       
         XC    SAV_EST(SAV_ESTL),SAV_EST                                        
                                                                                
UEST900  DS    0H                  General exit for UPDTEST                     
         J     YES                                                              
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Expense Claim Record                                           *         
***********************************************************************         
                                                                                
LOADEXP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING EXCRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    EXCKEY,EXCKEY                                                    
         MVI   EXCKTYP,EXCKTYPQ                                                 
         MVI   EXCKSUB,EXCKSUBQ                                                 
         MVC   EXCKCPY,COMPANY                                                  
         MVC   FULL(3),EXCKEY                                                   
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
         CLC   FULL(3),EXCKEY      Need to check against no EXC at all          
         JNE   YES                                                              
                                                                                
LEXP02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   EXCKEY(EXCKPIDB-EXCKEY),IOKEY                                    
         JNE   YES                 All done if not exp claims anymore           
                                                                                
         CLI   EXCKSEQ,0           Dimension: main record only                  
         JNE   LEXP06                                                           
         MVC   ACCADDR,EXCKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXEXPC,AINITEXP,AFILTEXP,VAGXCNVX                
         JNE   NO                                                               
                                                                                
LEXP04   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LEXP14                                                           
         J     LEXP08                                                           
                                                                                
LEXP06   MVC   ACCADDR,EXCKDA                                                   
                                                                                
LEXP08   DS    0H                  Row dimension:                               
         GOTO1 AACCLOAD,DMCB,VAGREXPC,AINIEXPR,0,VAGXCNVX                       
         JNE   NO                                                               
                                                                                
*        TM    PROCMODE,PROCTDQ    Text processing?                             
*        JZ    LEXP10                                                           
*        GOTO1 AACCLOAD,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
LEXP10   DS    0H                  Fact(s): all records                         
         GOTO1 AACCLOAD,DMCB,VAGFEXPC,AINIEXPF,0,VAGXCNVX                       
         JNE   NO                                                               
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    LEXP12                                                           
         J     *+2                 Not supported anymore                        
***      GOTO1 AACCLOAD,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
LEXP12   DS    0H                                                               
         J     LEXP04              as long as there is data ...                 
                                                                                
LEXP14   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LEXP02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Expense Claim Records at R2                                  *         
***********************************************************************         
                                                                                
FILTEXP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING EXCRECD,R2                                                       
         CLI   EXCKTYP,EXCKTYPQ    Expense Claims?                              
         JNE   NO                                                               
         CLI   EXCKSUB,EXCKSUBQ                                                 
         JNE   NO                                                               
         CLC   EXCKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         CLI   DXMODE,DXLOADQ      For load no extra filtering                  
         JNE   FEXP02                                                           
                                                                                
FEXP02   DS    0H                                                               
                                                                                
         DS    0H                  Any DXF/TDATEP/C filtering?                  
         DS    0H                  (no GETREC done here yet ...)                
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Expense Claim Records for diminensions                   *         
***********************************************************************         
                                                                                
INITEXPD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,EXPDLEDQ         R1=Max len w/o key:EXPD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,EXPDLENQ         R1=Max len of exp claim dimension            
         GOTO1 AINITALL                                                         
                                                                                
         CLI   DXMODE,DXUPDTQ      Update mode?                                 
         JNE   IEXP90                                                           
                                                                                
         DS    0H                  (no code yet)                                
                                                                                
IEXP90   DS    0H                                                               
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Expense Claim Records for Row Dimension                  *         
***********************************************************************         
                                                                                
INITEXPR NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,EXPRLEDQ         R1=Max len w/o key:EXPR                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,EXPRLENQ         R1=Max len of exp claim row dim              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Expense Claim Records for Facts                          *         
***********************************************************************         
                                                                                
INITEXPF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,EXPFLEDQ         R1=Max len w/o key:EXPF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,EXPFLENQ         R1=Max len of exp claim facts                
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Expense Claim Records                                        *         
* - As single threaded record processing CHAMODE=CHAMCHAQ may not     *         
*   (resp. has not been given much thought here).                     *         
***********************************************************************         
                                                                                
UPDTEXP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
         NI    PROCMODE,FFQ-PROCOPQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         USING RCVRECD,R3                                                       
         L     R3,DXARECB                                                       
                                                                                
         LA    R2,RCVRECRD                                                      
         USING EXCRECD,R2                                                       
                                                                                
         CLC   LT_SIN,RCVSEQNO     Do we have a new SIN/expense claim?          
         JNE   UEXP02                                                           
         CLC   SAV_XKEY,EXCKTYPE                                                
         JE    UEXP10                                                           
                                                                                
UEXP02   DS    0H                  Reset saved values                           
         XC    SAV_EXP(SAV_EXPL),SAV_EXP                                        
         MVC   SAV_XKEY,EXCKTYPE                                                
         MVI   SAV_XINI,NOQ                                                     
                                                                                
UEXP10   NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UEXP12                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UEXP14                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UEXP14                                                           
                                                                                
UEXP12   CLI   SVACTION,C'D'                                                    
         JNE   UEXP14                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UEXP14   CLI   SAV_XINI,NOQ        Initialised?                                 
         JE    UEXP16                                                           
         CLI   EXCKSEQ,0           Dimension: main record only                  
         JNE   UEXP20                                                           
                                                                                
UEXP16   DS    0H                  Dimension:                                   
         MVI   SAV_XINI,YESQ                                                    
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXEXPC,AINITEXP,AFILTEXP,VAGXCNVX                
                                                                                
UEXP18   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UEXP24                                                           
                                                                                
UEXP20   DS    0H                  Row dimension:                               
         GOTO1 AACCUPDT,DMCB,VAGREXPC,AINIEXPR,0,VAGXCNVX                       
                                                                                
*        TM    PROCMODE,PROCTDQ    Text processing?                             
*        JZ    UEXP22                                                           
*        MVI   IS_DIM,YESQ                                                      
*        GOTO1 AACCUPDT,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
UEXP22   DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFEXPC,AINIEXPF,AFILTEXP,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UEXP23                                                           
         MVI   IS_DIM,YESQ                                                      
         J     *+2                 Not supported anymore                        
***      GOTO1 AACCUPDT,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
UEXP23   DS    0H                                                               
         J     UEXP18                                                           
                                                                                
UEXP24   TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UEXP16                                                           
         MVI   DXACTION,C'A'                                                    
         J     UEXP16                                                           
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load TimeSheet Record                                               *         
***********************************************************************         
                                                                                
LOADTIM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
BUF      USING TMPELD,R4                                                        
         SAM31 ,                                                                
         L     R4,APERBUF          Read in Calendars                            
         MVI   BUF.TMPEL,0         (do via TSAR for Update mode, too?)          
         SAM24 ,                                                                
                                                                                
         USING CASRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,COMPANY                                                  
         MVC   FULL(CASKEMOA-CASKEY),CASKEY                                     
                                                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,ACCDIR,CASKEY,CASKEY,DMWORK                 
         J     LTIM04                                                           
                                                                                
LTIM02   LA    R2,IOKEY                                                         
         GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,CASKEY,CASKEY,DMWORK                 
                                                                                
LTIM04   JNE   *+2                                                              
                                                                                
         CLC   FULL(CASKEMOA-CASKEY),CASKEY                                     
         JNE   LTIM20                                                           
                                                                                
         GOTO1 VDATAMGR,DMCB,GETREC,ACCMST,CASKDA,AIOGEN,DMWORK                 
         JNE   *+2                                                              
                                                                                
         USING TMPELD,R3                                                        
         L     R2,AIOGEN                                                        
         LA    R3,CASRFST                                                       
                                                                                
LTIM06   CLI   TMPEL,0                                                          
         JE    LTIM02                                                           
         CLI   TMPEL,TMPELQ                                                     
         JE    LTIM10                                                           
                                                                                
LTIM08   LLC   R1,TMPLN                                                         
         AR    R3,R1                                                            
         J     LTIM06                                                           
                                                                                
LTIM10   DS    0H                  Save modified element                        
         SAM31 ,                                                                
         MVC   TMPEND+L'TMPEND(2),CASKOFC                                       
         MVC   BUF.TMPEL(TMPLNQ),TMPEL                                          
         AHI   R4,TMPLNQ                                                        
         MVI   BUF.TMPEL,0                                                      
         SAM24 ,                                                                
         J     LTIM08                                                           
         DROP  R2,R3,BUF                                                        
                                                                                
         USING TSWRECD,R2                                                       
LTIM20   LA    R2,IOKEY            For LOAD get records via t/s weekly          
         XC    TSWKEY,TSWKEY       pointers                                     
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,COMPANY                                                  
                                                                                
         XC    TIMTSWP,TIMTSWP     Clear previous & current dir keys            
         XC    TIMTSWC,TIMTSWC                                                  
         MVC   FULL(3),TSWKEY                                                   
                                                                                
         OI    PROCMODE,PROCMPQ    Using passives|                              
                                                                                
         LA    R2,IOKEY                                                         
         GOTO1 AREADHI                                                          
         JNE   LTIMN                                                            
                                                                                
         CLC   FULL(3),TSWKEY      Need to check against no TSW at all          
         JNE   LTIMY                                                            
                                                                                
         MVC   IOKEYPAS,0(R2)      Save passive key                             
                                                                                
LTIM22   TM    DMCB+8,X'80'        All done if EoF                              
         JO    LTIMY                                                            
         CLC   TSWKEY(TSWKPER-TSWKEY),IOKEYPAS                                  
         JNE   LTIMY               All done if not timesheets anymore           
                                                                                
         MVC   TIMTSWC,TSWKEY      Save pointer key                             
         MVC   IOKEYPAS,0(R2)                                                   
                                                                                
         MVC   ACCADDR,TSWKDA      Dimension:                                   
                                                                                
         GOTO1 AACCLOAD,DMCB,VAGXTIMC,AINITTIM,AFILTTIM,VAGXCNVX                
         JNE   LTIMN                                                            
                                                                                
LTIM24   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    LTIM30                                                           
                                                                                
LTIM26   DS    0H                  Row dimension:                               
         GOTO1 AACCLOAD,DMCB,VAGRTIMC,AINITIMR,0,VAGXCNVX                       
         JNE   LTIMN                                                            
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    LTIM28                                                           
         J     *+2                 Not supported anymore                        
***      GOTO1 AACCLOAD,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
LTIM28   DS    0H                  Fact(s):                                     
         GOTO1 AACCLOAD,DMCB,VAGFTIMC,AINITIMF,0,VAGXCNVX                       
         JNE   LTIMN                                                            
         J     LTIM24              as long as there is data ...                 
                                                                                
LTIM30   OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LTIM22                                                           
                                                                                
LTIMY    NI    PROCMODE,FFQ-PROCMPQ                                             
         J     YES                                                              
                                                                                
LTIMN    NI    PROCMODE,FFQ-PROCMPQ                                             
         J     NO                                                               
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter TimeSheet Record at R2                                       *         
***********************************************************************         
                                                                                
FILTTIM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   DXMODE,DXLOADQ                                                   
         JE    FTIMLOAD                                                         
                                                                                
         USING TIMRECD,R2                                                       
         CLC   TIMKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   TIMKUNT(2),ORUL   Unit/Ledger Ok?                                
         JNE   NO                                                               
                                                                                
***      CLI   DXMODE,DXLOADQ                                                   
***      JE    FTIMU02                                                          
                                                                                
         LA    R1,RECTYPE                                                       
***      J     FTIMU04                                                          
***                                                                             
**IMU02  GOTO1 VRECTYP,DMCB,(C'I',TIMRECD)                                      
                                                                                
FTIMU04  CLI   0(R1),ACRTTIM                                                    
         JNE   NO                                                               
                                                                                
         DS    0H                  Any DXF/TDATEP/C filtering?                  
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         USING TSWRECD,R2                                                       
FTIMLOAD DS    0H                  Note: Record not in DXARECB yet              
                                                                                
         OC    DXTDATEC,DXTDATEC                                                
         JZ    FTIML02                                                          
                                                                                
         XR    R1,R1                                                            
         ICM   R1,B'0111',TSWKEND                                               
         LNR   R1,R1                                                            
         STCM  R1,B'0111',FULL                                                  
         CLC   FULL(3),DXTDATEP    Date filtering                               
         JH    NO                                                               
                                                                                
         GOTO1 =V(GETPSD),TSWKEY                                                
         CLC   WORK(3),DXFDATEP                                                 
         JL    NO                                                               
                                                                                
FTIML02  DS    0H                                                               
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise TimeSheet Records for 1st Dimensions                     *         
***********************************************************************         
                                                                                
INITTIMD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TIMDLEDQ         R1=Max len w/o key:TIMD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TIMDLENQ         R1=Max len of timesheet dimension            
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise TimeSheet Records for Row Dimension                      *         
***********************************************************************         
                                                                                
INITTIMR NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TIMRLEDQ         R1=Max len w/o key:TIMR                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TIMRLENQ         R1=Max len of timesheet row dim              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise TimeSheet Records for Facts                              *         
***********************************************************************         
                                                                                
INITTIMF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TIMFLEDQ         R1=Max len w/o key:TIMF                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TIMFLENQ         R1=Max len of timesheet facts                
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update TimeSheet Records                                            *         
* >>> Issue - how to know whether we add a new t/s ???                *         
***********************************************************************         
                                                                                
UPDTTIM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
         NI    PROCMODE,FFQ-PROCOPQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         CLI   SAVICAL,YESQ        Has calendar data been resolved?             
         JE    UTIM12                                                           
                                                                                
         MVI   SAVICAL,YESQ                                                     
                                                                                
BUF      USING TMPELD,R4                                                        
         SAM31 ,                                                                
         L     R4,APERBUF          Read in Calendars                            
         MVI   BUF.TMPEL,0         (do via TSAR for Update mode, too?)          
         SAM24 ,                                                                
                                                                                
         USING CASRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,COMPANY                                                  
         MVC   FULL(CASKEMOA-CASKEY),CASKEY                                     
                                                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,ACCDIR,CASKEY,CASKEY,DMWORK                 
         J     UTIM04                                                           
                                                                                
UTIM02   LA    R2,IOKEY                                                         
         GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,CASKEY,CASKEY,DMWORK                 
                                                                                
UTIM04   JNE   *+2                                                              
                                                                                
         CLC   FULL(CASKEMOA-CASKEY),CASKEY                                     
         JNE   UTIM20                                                           
                                                                                
         GOTO1 VDATAMGR,DMCB,GETREC,ACCMST,CASKDA,AIOGEN,DMWORK                 
         JNE   *+2                                                              
                                                                                
         USING TMPELD,R3                                                        
         L     R2,AIOGEN                                                        
         LA    R3,CASRFST                                                       
                                                                                
UTIM06   CLI   TMPEL,0                                                          
         JE    UTIM02                                                           
         CLI   TMPEL,TMPELQ                                                     
         JE    UTIM10                                                           
                                                                                
UTIM08   LLC   R1,TMPLN                                                         
         AR    R3,R1                                                            
         J     UTIM06                                                           
                                                                                
UTIM10   DS    0H                  Save modified element                        
         SAM31 ,                                                                
         MVC   TMPEND+L'TMPEND(2),CASKOFC                                       
         MVC   BUF.TMPEL(TMPLNQ),TMPEL                                          
         AHI   R4,TMPLNQ                                                        
         MVI   BUF.TMPEL,0                                                      
         SAM24 ,                                                                
         J     UTIM08                                                           
         DROP  R2,R3,BUF                                                        
                                                                                
         USING RCVRECD,R3                                                       
UTIM12   L     R3,DXARECB                                                       
                                                                                
         LA    R2,RCVRECRD                                                      
         USING TIMRECD,R2                                                       
                                                                                
UTIM14   DS    0H                  Reset saved values                           
         XC    SAV_TIM(SAV_TIML),SAV_TIM                                        
         MVI   SAV_TINI,NOQ                                                     
                                                                                
UTIM20   NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UTIM22                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UTIM24                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UTIM24                                                           
                                                                                
UTIM22   CLI   SVACTION,C'D'                                                    
         JNE   UTIM24                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UTIM24   CLI   SAV_XINI,NOQ        Initialised?                                 
         JE    UTIM26                                                           
                                                                                
UTIM26   DS    0H                  Dimension:                                   
         MVI   SAV_XINI,YESQ                                                    
         MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXTIMC,AINITTIM,AFILTTIM,VAGXCNVX                
                                                                                
UTIM28   TM    RETBYTE,RETBFFQ     Fact return?                                 
         JZ    UTIM34                                                           
                                                                                
UTIM30   DS    0H                  Row Dimension:                               
         GOTO1 AACCUPDT,DMCB,VAGRTIMC,AINITIMR,AFILTTIM,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCTDQ    Text processing?                             
         JZ    UTIM32                                                           
         MVI   IS_DIM,YESQ                                                      
         J     *+2                 Not supported anymore                        
***      GOTO1 AACCUPDT,DMCB,VAGXTXTC,AINITTXT,0,VAGXCNVX                       
                                                                                
UTIM32   DS    0H                  Fact(s):                                     
         GOTO1 AACCUPDT,DMCB,VAGFTIMC,AINITIMF,AFILTTIM,VAGXCNVX                
         J     UTIM28                                                           
                                                                                
UTIM34   TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UTIM26                                                           
         MVI   DXACTION,C'A'                                                    
         J     UTIM26                                                           
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Account Records                                                *         
***********************************************************************         
                                                                                
LOADACC  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         LA    R3,ACCLDGS                                                       
                                                                                
LACC02   CLI   0(R3),0             (ACCLDGX=EoT)                                
         JE    YES                                                              
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),0(R3)                                                 
         OI    ACTKACT+L'ACTKACT-1,X'01'                                        
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LACC04   TM    DMCB+8,X'80'        All done if EoF                              
         JO    LACC06                                                           
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JE    LACC10                                                           
                                                                                
LACC06   AHI   R3,L'SEUL           Next ledger                                  
         J     LACC02                                                           
                                                                                
LACC10   MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXACCC,AINITACC,AFILTACC,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LACC04                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* None Account Records at R2                                          *         
***********************************************************************         
                                                                                
FILTACC  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         LA    R3,ACCLDGS                                                       
                                                                                
FACC01   CLI   0(R3),0             (ACCLDGX=EoT)                                
         JE    NO                                                               
         CLC   ACTKUNT(2),0(R3)                                                 
         JE    FACC02                                                           
         AHI   R3,L'SEUL                                                        
         J     FACC01                                                           
                                                                                
FACC02   CLC   ACTKACT,GSPACES     Ensure account code                          
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),GSPACES             
         JNE   NO                  And nothing 'below'                          
                                                                                
         LA    RE,ACTKSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FACC04                                                           
         LA    RE,ACTRSTAT                                                      
                                                                                
FACC04   DS    0H                                                               
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Account Records                                          *         
***********************************************************************         
                                                                                
INITACCD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ACCDLEDQ         R1=Max len w/o key:ACCD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ACCDLENQ         R1=Max len of SE/SI/SQ a/c dimension         
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Account Records - record type ACRTACTL and ACRTACTH          *         
***********************************************************************         
                                                                                
UPDTACC  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         L     R2,DXARECB                                                       
         USING RCVRECD,R2                                                       
         CLC   RCVRECRD+ACTKUNT-ACTKEY(2),PRODUL                                
         JE    NO                  Exit if it's SJ ledger                       
         DROP  R2                                                               
*                                                                               
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UACC02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UACC04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UACC04                                                           
                                                                                
UACC02   CLI   SVACTION,C'D'                                                    
         JNE   UACC04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UACC04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXACCC,AINITACC,AFILTACC,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    UPDTACCX                                                         
         CLI   SVACTION,C'D'                                                    
         JE    UPDTACCX                                                         
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UACC04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UACC04                                                           
                                                                                
UPDTACCX DS    0H                                                               
         MVC   DXACTION,SVACTION   RESTORE ACTION                               
         J     YES                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load EType records                                                  *         
***********************************************************************         
                                                                                
LOADETY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ETYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,COMPANY                                                  
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LETY02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ETYKEY(ETYKCODE-ETYKEY),IOKEY                                    
         JNE   YES                 All done if different record type            
                                                                                
         MVC   ACCADDR,ETYKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXETYC,AINITETY,AFILTETY,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LETY02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter EType records at R2                                          *         
***********************************************************************         
                                                                                
FILTETY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ETYRECD,R2                                                       
         CLI   ETYKTYP,ETYKTYPQ    Still ETypes?                                
         JNE   NO                                                               
         CLI   ETYKSUB,ETYKSUBQ                                                 
         JNE   NO                                                               
         CLC   ETYKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         CLI   ETYKSEQ,0           Main records only                            
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise EType records                                            *         
***********************************************************************         
                                                                                
INITETYD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ETYDLEDQ         R1=Max len w/o key:ETYD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ETYDLENQ         R1=Max len of etype dimension                
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update EType records                                                *         
***********************************************************************         
                                                                                
UPDTETY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UETY02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UETY04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UETY04                                                           
                                                                                
UETY02   CLI   SVACTION,C'D'                                                    
         JNE   UETY04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UETY04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXETYC,AINITETY,AFILTETY,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UETY04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UETY04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Category records                                               *         
***********************************************************************         
                                                                                
LOADCAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING CATRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    CATKEY,CATKEY                                                    
         MVI   CATKTYP,CATKTYPQ                                                 
         MVI   CATKSUB,CATKSUBQ                                                 
         MVC   CATKCPY,COMPANY                                                  
         MVC   CATKUNT(L'CATKUNT+L'CATKLDG),PRODUL                              
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LCAT02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   CATKEY(CATKSCH-CATKEY),IOKEY                                     
         JNE   YES                 All done if different record type            
                                                                                
         MVC   ACCADDR,CATKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXCATC,AINITCAT,AFILTCAT,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LCAT02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Category records at R2                                       *         
***********************************************************************         
                                                                                
FILTCAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING CATRECD,R2                                                       
         CLI   CATKTYP,CATKTYPQ    Still Categories?                            
         JNE   NO                                                               
         CLI   CATKSUB,CATKSUBQ                                                 
         JNE   NO                                                               
         CLC   CATKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   CATKUNT(L'CATKUNT+L'CATKLDG),PRODUL                              
         JNE   NO                                                               
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Category records                                         *         
***********************************************************************         
                                                                                
INITCATD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,CATDLEDQ         R1=Max len w/o key:CATD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CATDLENQ         R1=Max len of category dimension             
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Category records                                             *         
***********************************************************************         
                                                                                
UPDTCAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UCAT02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UCAT04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UCAT04                                                           
                                                                                
UCAT02   CLI   SVACTION,C'D'                                                    
         JNE   UCAT04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UCAT04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXCATC,AINITCAT,AFILTCAT,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UCAT04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UCAT04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Currency Records                                               *         
***********************************************************************         
                                                                                
LOADCUR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING CURTABD,R2                                                       
         L     R2,ABIGAREA                                                      
                                                                                
*&&UK                                                                           
                                                                                
* >>> logic based on ACGETCRL but w/o rates extraction                          
                                                                                
         GOTO1 VBLDCUR,DMCB,(0,A(0)),(X'40',(R2)),(SAVCLOC,ACOMFAC)             
         CLI   0(R1),0                                                          
         JNE   *+2                 Die on any error                             
*&&                                                                             
*&&US                                                                           
         MVC   CURTCUR,USDLIT                                                   
         MVI   CURTDECP,2                                                       
         MVC   CURTLONG,=CL35'United States Dollar'                             
         LA    R1,CURTCUR+CURTABL+L'CURTLONG                                    
         XC    0(L'CURTCUR,R1),0(R1)                                            
*&&                                                                             
                                                                                
                                                                                
         OI    PROCMODE,PROCCLQ                                                 
         XC    SAVCUR(SAVCURLQ),SAVCUR                                          
                                                                                
LCUR02   OC    CURTCUR,CURTCUR     All done?                                    
         JZ    LCUR06                                                           
                                                                                
         ST    R2,SAVCURAD                                                      
         XC    ACCADDR,ACCADDR                                                  
         GOTO1 AACCLOAD,DMCB,VAGXCURC,AINITCUR,AFILTCUR,VAGXCNVX                
         JNE   LCUR04                                                           
                                                                                
         OC    MAXIOS,MAXIOS       Die if max IOs exceeded                      
         JZ    *+2                                                              
                                                                                
         AHI   R2,CURTABL+L'CURTLONG                                            
         J     LCUR02                                                           
         DROP  R2                                                               
                                                                                
LCUR04   NI    PROCMODE,FFQ-PROCCLQ                                             
         J     NO                                                               
                                                                                
LCUR06   NI    PROCMODE,FFQ-PROCCLQ                                             
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Currency records at R2                                       *         
***********************************************************************         
                                                                                
FILTCUR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Currency records                                         *         
***********************************************************************         
                                                                                
INITCURD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,CURDLEDQ         R1=Max len w/o key:CURD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CURDLENQ         R1=Max len of currency dimension             
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Currency records - not applicable                            *         
***********************************************************************         
                                                                                
UPDTCUR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load WorkCode records                                               *         
***********************************************************************         
                                                                                
LOADWCO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
*&&US*&& NI    PROCMOD3,FFQ-PROCW99Q                                            
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING WCORECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    WCOKEY,WCOKEY                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),PRODUL                              
         MVC   WCOKCPY,COMPANY                                                  
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LWCO02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   WCOKEY(WCOKWRK-WCOKEY),IOKEY                                     
*&&UK*&& JNE   YES                 All done if different record type            
*&&US                                                                           
         JE    LWCO04                                                           
                                                                                
         OI    PROCMOD3,PROCW99Q   Now add dummy w/c 99                         
         XC    WCOKEY,WCOKEY       Reread for first w/c                         
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),PRODUL                              
         MVC   WCOKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   *+2                                                              
*&&                                                                             
                                                                                
LWCO04   DS    0H                                                               
         MVC   ACCADDR,WCOKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXWCOC,AINITWCO,AFILTWCO,VAGXCNVX                
*&&US                                                                           
         IPM   R0                                                               
         TM    PROCMOD3,PROCW99Q                                                
         JZ    LWCO06                                                           
         NI    PROCMOD3,FFQ-PROCW99Q                                            
         J     YES                                                              
                                                                                
LWCO06   SPM   R0                                                               
*&&                                                                             
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LWCO02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter WorkCode records at R2                                       *         
***********************************************************************         
                                                                                
FILTWCO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING WCORECD,R2                                                       
         CLI   WCOKTYP,WCOKTYPQ    Still WorkCodes?                             
         JNE   NO                                                               
         CLC   WCOKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         CLC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),PRODUL                              
         JNE   NO                                                               
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise WorkCode records                                         *         
***********************************************************************         
                                                                                
INITWCOD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,WCODLEDQ         R1=Max len w/o key:WCOD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,WCODLENQ         R1=Max len of work code dimension            
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update WorkCode records                                             *         
***********************************************************************         
                                                                                
UPDTWCO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UWCO02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UWCO04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UWCO04                                                           
                                                                                
UWCO02   CLI   SVACTION,C'D'                                                    
         JNE   UWCO04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UWCO04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXWCOC,AINITWCO,AFILTWCO,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UWCO04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UWCO04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Office records                                                 *         
***********************************************************************         
                                                                                
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         CLI   SAVC2CO,YESQ                                                     
         JE    LOFF02                                                           
                                                                                
         USING OGRRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,COMPANY                                                  
         MVC   OGRKUNT(L'OGRKUNT+L'OGRKLDG),PRODUL                              
         J     LOFF04                                                           
                                                                                
         USING OFFRECD,R2                                                       
LOFF02   LA    R2,IOKEY                                                         
         MVC   OFFKEY,GSPACES                                                   
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
                                                                                
LOFF04   L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LOFF10   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLI   SAVC2CO,YESQ                                                     
         JE    LOFF12                                                           
         USING OGRRECD,R2                                                       
         CLC   OGRKEY(OGRKOFC-OGRKEY),IOKEY                                     
         JNE   YES                                                              
         J     LOFF14                                                           
                                                                                
         USING OFFRECD,R2                                                       
LOFF12   CLC   OFFKEY(OFFKOFF-OFFKEY),IOKEY                                     
         JNE   YES                 All done if different record type            
                                                                                
LOFF14   MVC   ACCADDR,OFFKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXOFFC,AINITOFF,AFILTOFF,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LOFF10                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Office records at R2                                         *         
***********************************************************************         
                                                                                
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVC2CO,YESQ                                                     
         JE    FOFF02                                                           
         USING OGRRECD,R2                                                       
         CLI   OGRKTYP,OGRKTYPQ                                                 
         JNE   NO                                                               
         CLI   OGRKSUB,OGRKOFFQ                                                 
         JNE   NO                                                               
         CLC   OGRKCPY,COMPANY                                                  
         JNE   NO                                                               
         CLC   OGRKUNT(L'OGRKUNT+L'OGRKLDG),PRODUL                              
         JNE   NO                                                               
         J     FOFF10                                                           
                                                                                
         USING OFFRECD,R2                                                       
FOFF02   CLI   OFFKTYP,OFFKTYPQ    Still WorkCodes?                             
         JNE   NO                                                               
         CLC   OFFKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
         LA    RE,OFFKSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JE    FOFF04                                                           
         LA    RE,OFFRSTAT                                                      
                                                                                
FOFF04   TM    0(RE),OFFSLIST                                                   
         JNZ   NO                                                               
                                                                                
FOFF10   DS    0H                                                               
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Office records                                           *         
***********************************************************************         
                                                                                
INITOFFD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,OFFDLEDQ         R1=Max len w/o key:OFFD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,OFFDLENQ         R1=Max len of office dimension               
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Office records                                               *         
***********************************************************************         
                                                                                
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UOFF02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UOFF04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UOFF04                                                           
                                                                                
UOFF02   CLI   SVACTION,C'D'                                                    
         JNE   UOFF04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UOFF04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXOFFC,AINITOFF,AFILTOFF,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UOFF04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UOFF04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Item/Article records                                           *         
***********************************************************************         
                                                                                
LOADITM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING ARTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ARTKEY,ARTKEY                                                    
         MVI   ARTKTYP,ARTKTYPQ                                                 
         MVI   ARTKSUB,ARTKAQ                                                   
         MVC   ARTKCPY,COMPANY                                                  
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LITM02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   ARTKEY(ARTKART-ARTKEY),IOKEY                                     
         JNE   YES                 All done if different record type            
                                                                                
         MVC   ACCADDR,ARTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXITMC,AINITITM,AFILTITM,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LITM02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Item/Article records at R2                                   *         
***********************************************************************         
                                                                                
FILTITM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ARTRECD,R2                                                       
         CLI   ARTKTYP,ARTKTYPQ    Still Article records?                       
         JNE   NO                                                               
         CLI   ARTKSUB,ARTKAQ                                                   
         JNE   NO                                                               
         CLC   ARTKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Item/Article records for Dimensions                      *         
***********************************************************************         
                                                                                
INITITMD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ITMDLEDQ         R1=Max len w/o key:ITMD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ITMDLENQ         R1=Max len of item dimension                 
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Item/Article records for Dimensions                          *         
***********************************************************************         
                                                                                
UPDTITM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UITM02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UITM04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UITM04                                                           
                                                                                
UITM02   CLI   SVACTION,C'D'                                                    
         JNE   UITM04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UITM04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXITMC,AINITITM,AFILTITM,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UITM04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UITM04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Person records                                                 *         
***********************************************************************         
                                                                                
LOADPER  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    PERKEY,PERKEY                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,COMPANY                                                  
                                                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LPER02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   PERKEY(PERKCODE-PERKEY),IOKEY                                    
         JNE   YES                 All done if different record type            
                                                                                
         MVC   ACCADDR,PERKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXPERC,AINITPER,AFILTPER,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LPER02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Person records at R2                                         *         
***********************************************************************         
                                                                                
FILTPER  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING PERRECD,R2                                                       
         CLI   PERKTYP,PERKTYPQ    Still Person Records?                        
         JNE   NO                                                               
         CLC   PERKCPY,COMPANY     Company Ok?                                  
         JNE   NO                                                               
                                                                                
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Person records                                           *         
***********************************************************************         
                                                                                
INITPERD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,PERDLEDQ         R1=Max len w/o key:PERD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,PERDLENQ         R1=Max len of person dimension               
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Person records                                               *         
***********************************************************************         
                                                                                
UPDTPER  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         CLI   DXACTION,C'D'       PIDNO checks                                 
         JNE   UPER02                                                           
         L     R2,DXACPYB          For Delete check copy only                   
         JAS   RE,UPIDCHK                                                       
         JE    UPER10                                                           
         J     NO                                                               
                                                                                
UPER02   CLI   DXACTION,C'A'       For Add check change only                    
         JNE   UPER04                                                           
         L     R2,DXARECB                                                       
         JAS   RE,UPIDCHK                                                       
         JE    UPER10                                                           
         J     NO                                                               
                                                                                
UPER04   CLI   DXACTION,C'C'       For Change compare before/after              
         JNE   *+2                                                              
         MVI   HALF+0,NOQ                                                       
         L     R2,DXACPYB                                                       
         JAS   RE,UPIDCHK                                                       
         JNE   UPER06                                                           
         MVI   HALF+0,YESQ                                                      
                                                                                
UPER06   MVI   HALF+1,NOQ                                                       
         L     R2,DXARECB                                                       
         JAS   RE,UPIDCHK                                                       
         JNE   UPER08                                                           
         MVI   HALF+1,YESQ                                                      
                                                                                
UPER08   CLC   HALF,PIDNNQ         (no PID at all so skip)                      
         JE    NO                                                               
         CLC   HALF,PIDYYQ                                                      
         JE    UPER10              (regular change)                             
         CLC   HALF,PIDYNQ                                                      
         JNE   UPER12                                                           
         MVI   DXACTION,C'D'       (make it a delete)                           
         J     UPER10                                                           
                                                                                
UPER12   CLC   HALF,PIDNYQ                                                      
         JNE   *+2                                                              
         MVI   DXACTION,C'A'       (make it an add)                             
                                                                                
UPER10   NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UPER22                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UPER24                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UPER24                                                           
                                                                                
UPER22   CLI   SVACTION,C'D'                                                    
         JNE   UPER24                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UPER24   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXPERC,AINITPER,AFILTPER,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UPER24                                                           
         MVI   DXACTION,C'A'                                                    
         J     UPER24                                                           
                                                                                
         USING RCVRECD,R2                                                       
UPIDCHK  DS    0H                                                               
         LA    R1,RCVRECRD                                                      
         USING PERRECD,R1                                                       
         LA    R1,PERRFST                                                       
         USING PIDELD,R1                                                        
                                                                                
UPIDC2   CLI   PIDEL,0                                                          
         JE    UPIDCN                                                           
         CLI   PIDEL,PIDELQ                                                     
         JE    UPIDC6                                                           
                                                                                
UPIDC4   LLC   RF,PIDLN                                                         
         AR    R1,RF                                                            
         J     UPIDC2                                                           
                                                                                
UPIDC6   OC    PIDNO,PIDNO                                                      
         JZ    UPIDC4                                                           
                                                                                
UPIDCY   LHI   RF,1                                                             
         J     UPIDCX                                                           
                                                                                
UPIDCN   LHI   RF,0                                                             
                                                                                
UPIDCX   CHI   RF,1                                                             
         BR    RE                                                               
         DROP  R2                                                               
                                                                                
PIDNNQ   DC    C'NN'                                                            
PIDNYQ   DC    C'NY'                                                            
PIDYNQ   DC    C'YN'                                                            
PIDYYQ   DC    C'YY'                                                            
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load Timeoff Records                                                *         
***********************************************************************         
                                                                                
LOADTOF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         OI    RUNINDS,RUNTIMOF    Timeoff has been updated                     
         OI    PROCMOD3,PROCTOFQ   Timeoff mode                                 
         LLC   R3,AGYLISN                                                       
         LA    R2,AGYLIST          Filter agencies to update this file          
         USING AGYLSTD,R2                                                       
                                                                                
LTOF02   CLC   AGYLAID,GSPACES                                                  
         JNH   LTOF06              unwanted                                     
                                                                                
         MVC   SV_TOAID,AGYLAID    Pass alpha-id                                
         XC    ACCADDR,ACCADDR                                                  
         GOTO1 AACCLOAD,DMCB,VAGFTOFC,AINITTOF,AFILTTOF,VAGXCNVX                
         JNE   LTOF04                                                           
                                                                                
         OC    MAXIOS,MAXIOS       Die if max IOs exceeded                      
         JZ    *+2                                                              
                                                                                
         AHI   R2,AGYLLNQ                                                       
         JCT   R3,LTOF02                                                        
         J     LTOF06                                                           
         DROP  R2                                                               
                                                                                
LTOF04   NI    PROCMOD3,FFQ-PROCTOFQ                                            
         J     NO                                                               
                                                                                
LTOF06   NI    PROCMOD3,FFQ-PROCTOFQ                                            
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter Timeoff records at R2                                        *         
***********************************************************************         
                                                                                
FILTTOF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise Timeoff records                                          *         
***********************************************************************         
                                                                                
INITTOFF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,TOFFLEDQ         R1=Max len w/o key:TOFD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,TOFFLENQ         R1=Max len of Timeoff dimension              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Update Timeoff records - not applicable                             *         
***********************************************************************         
                                                                                
UPDTTOF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         OI    RUNINDS,RUNTIMOF    Timeoff has been updated                     
         J     YES                                                              
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Load company record                                                 *         
***********************************************************************         
                                                                                
LOADCPY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   MAXIOS,DXMAXREC                                                  
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         USING CPYRECD,R2                                                       
         LA    R2,IOKEY            Set key to read first company record         
         MVC   CPYKEY,GSPACES                                                   
         MVC   CPYKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
                                                                                
LCPY02   TM    DMCB+8,X'80'        All done if EoF                              
         JO    YES                                                              
         CLC   CPYKCPY,COMPANY     All done if company changes                  
         JNE   YES                                                              
         CLC   CPYKCPY+L'CPYKCPY(L'CPYKEY-L'CPYKCPY),GSPACES                    
         JH    YES                                                              
                                                                                
         MVC   ACCADDR,CPYKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAGXCPYC,AINITCPY,AFILTCPY,VAGXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       Exit if max IOs exceeded                     
         JNZ   LCPY02                                                           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Update company record                                               *         
***********************************************************************         
                                                                                
UPDTCPY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   SAVCACT,YESQ        Ensure Bulk API user                         
         JNE   NO                                                               
                                                                                
         NI    PROCMOD2,FFQ-PROCMRQ                                             
                                                                                
         MVC   SVACTION,DXACTION                                                
                                                                                
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   DXACTION,C'C'                                                    
         JNE   UCPY02                                                           
         CLI   CHAMODE,CHAMCHAQ    (so running CHAMDARQ)                        
         JE    UCPY04                                                           
         OI    PROCMODE,PROCOPQ                                                 
         MVI   DXACTION,C'D'                                                    
         J     UCPY04                                                           
                                                                                
UCPY02   CLI   SVACTION,C'D'                                                    
         JNE   UCPY04                                                           
         OI    PROCMODE,PROCOPQ                                                 
                                                                                
UCPY04   MVI   IS_DIM,YESQ                                                      
         GOTO1 AACCUPDT,DMCB,VAGXCPYC,AINITCPY,AFILTCPY,VAGXCNVX                
                                                                                
         TM    PROCMODE,PROCOPQ                                                 
         JZ    YES                                                              
         CLI   SVACTION,C'D'                                                    
         JE    YES                                                              
         NI    PROCMODE,FFQ-PROCOPQ                                             
         CLI   SVACTION,C'C'                                                    
         JNE   UCPY04                                                           
         MVI   DXACTION,C'A'                                                    
         J     UCPY04                                                           
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Filter company record at R2                                         *         
***********************************************************************         
                                                                                
FILTCPY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING CPYRECD,R2                                                       
         CLC   CPYKCPY,COMPANY                                                  
         JNE   NO                                                               
         CLC   CPYKCPY+L'CPYKCPY(L'CPYKEY-L'CPYKCPY),GSPACES                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* Initialise company record                                           *         
***********************************************************************         
                                                                                
INITCPYD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,CPYDLEDQ         R1=Max len w/o key:CPYD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,CPYDLENQ         R1=Max len of company dimension              
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Load all record data for Bulk API                                   *         
***********************************************************************         
                                                                                
LOADALL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TYPTABD,R3                                                       
         L     R3,ATYPTAB                                                       
                                                                                
LOAD02   CLI   TYPNAME,FFQ         EoT                                          
         JE    YES                                                              
         CLC   VERSION,TYPVERS     Required for this version?                   
         JL    LOAD12                                                           
         CLI   TYPRTYP,0           None specific record type?                   
         JNE   LOAD04                                                           
         TM    TYPTYPE2,CLISUMQ    Test client summary                          
         JNZ   LOAD10                                                           
         TM    TYPTYPE,TIMOFFQ     Test Timeoff record?                         
         JZ    LOAD12                                                           
         CLC   TYPECODE,TYPE_ALL   LOAD=ALL excludes Currency                   
         JE    LOAD10              Yes - Load it                                
         J     LOAD12              LOAD=API excludes Timeoff                    
                                                                                
LOAD04   TM    TYPTYPE,IS2COQ      1CO/2CO mutual exclusion                     
         JZ    LOAD06                                                           
         CLI   SAVC2CO,NOQ                                                      
         JE    LOAD12                                                           
         J     LOAD10                                                           
                                                                                
LOAD06   TM    TYPTYPE,IS1COQ                                                   
         JZ    LOAD10                                                           
         CLI   SAVC2CO,YESQ                                                     
         JE    LOAD12                                                           
                                                                                
LOAD10   ICM   RF,B'1111',TYPLOAD  Go to extract routine                        
         BASR  RE,RF               On failure finish else look for              
         JNE   NO                  other extracts                               
                                                                                
LOAD12   AHI   R3,TYPTABLQ         next table entry                             
         J     LOAD02                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Update all record data                                              *         
***********************************************************************         
                                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TYPTABD,R3                                                       
         L     R3,ATYPTAB                                                       
UPDT02   CLI   TYPNAME,FFQ         EoT                                          
         JE    YES                                                              
         CLC   VERSION,TYPVERS     Required for this version?                   
         JL    UPDT06                                                           
         CLI   TYPRTYP,0           None specific record type?                   
         JNE   UPDT03                                                           
         TM    TYPTYPE,TIMOFFQ     Test Timeoff record?                         
         JZ    UPDT06                                                           
         CLC   TYPECODE,TYPE_ALL   LOAD=ALL excludes Currency                   
         JNE   UPDT06              No - LOAD=API excludes Timeoff rec           
         TM    RUNINDS,RUNTIMOF    Timeoff has been updated                     
         JZ    UPDT04              No - call UPDTTOF                            
         J     UPDT06              Skip others                                  
*                                                                               
UPDT03   CLC   RECTYPE,TYPRTYP     Match to table type?                         
         JE    UPDT04                                                           
         CLC   RECTYPE,TYPRTY2                                                  
         JE    UPDT04                                                           
         TM    TYPTYPE,AUDITQ      Audit required?                              
         JZ    UPDT06                                                           
         CLI   RECTYPE,ACRTAUDT                                                 
         JNE   UPDT06                                                           
         CLC   TYPATYP,RECATYP                                                  
         JNE   UPDT06                                                           
                                                                                
UPDT04   ICM   RF,B'1111',TYPUPDT  Go to extract routine                        
         BASR  RE,RF                                                            
                                                                                
UPDT06   AHI   R3,TYPTABLQ         next table entry                             
         J     UPDT02                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Load client summary record date for Aura Warehouse                  *         
***********************************************************************         
                                                                                
LOADCSM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   TYPECODE,TYPE_ALL   If type was ALL or API then data             
         JE    LCSM20               will have already been read so we           
         CLC   TYPECODE,=C'API'      can got to putting out buffered            
         JE    LCSM20                 records                                   
         USING TYPTABD,R3                                                       
         L     R3,ATYPTAB                                                       
                                                                                
LCSM02   CLI   TYPNAME,FFQ         EoT                                          
         JE    LCSM20                                                           
         CLC   VERSION,TYPVERS     Required for this version?                   
         JL    LCSM12                                                           
         TM    TYPTYPE2,CLISUMQ    Test client summary                          
         JZ    LCSM12                                                           
         CLC   TYPNAME,=C'CSM'     If CSM skip otherwise end up in loop         
         JE    LCSM12                                                           
                                                                                
LCSM10   ICM   RF,B'1111',TYPLOAD  Go to extract routine                        
         BASR  RE,RF               On failure finish else look for              
         JNE   NO                  other extracts                               
                                                                                
LCSM12   AHI   R3,TYPTABLQ         next table entry                             
         J     LCSM02                                                           
                                                                                
LCSM20   GOTOR APROCCSM            Process client summary record                
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Update client summary record data                                   *         
***********************************************************************         
                                                                                
UPDTCSM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING TYPTABD,R3                                                       
         L     R3,ATYPTAB                                                       
UCSM02   CLI   TYPNAME,FFQ         EoT                                          
         JE    YES                                                              
         CLC   VERSION,TYPVERS     Required for this version?                   
         JL    UCSM06                                                           
         TM    TYPTYPE2,CLISUMQ    Test client summary record?                  
         JZ    UCSM06                                                           
*                                                                               
         CLC   RECTYPE,TYPRTYP     Match to table type?                         
         JE    UCSM04                                                           
         CLC   RECTYPE,TYPRTY2                                                  
         JE    UCSM04                                                           
         TM    TYPTYPE,AUDITQ      Audit required?                              
         JZ    UCSM06                                                           
         CLI   RECTYPE,ACRTAUDT                                                 
         JNE   UCSM06                                                           
         CLC   TYPATYP,RECATYP                                                  
         JNE   UCSM06                                                           
                                                                                
UCSM04   ICM   RF,B'1111',TYPUPDT  Go to extract routine                        
         BASR  RE,RF                                                            
                                                                                
UCSM06   AHI   R3,TYPTABLQ         next table entry                             
         J     UPDT02                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Initialise Estimates TextRow records                                *         
***********************************************************************         
                                                                                
INITETXD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ETXDLEDQ         R1=Max len w/o key: ETXD                     
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ETXDLENQ         R1=Max len of text row dimension             
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Orders TextRow records                                   *         
***********************************************************************         
                                                                                
INITOTXD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,OTXDLEDQ         R1=Max len w/o key: OTXD                     
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,OTXDLENQ         R1=Max len of text row dimension             
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Bill Number records                                      *         
***********************************************************************         
                                                                                
INITBILD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,BILDLEDQ         R1=Max len w/o key:BILD                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,BILDLENQ         R1=Max len of bill number dimension          
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Estiamte Extra Dimension                                 *         
***********************************************************************         
                                                                                
INITESD2 NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,EST2LEDQ         R1=Max len w/o key:EST2                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,EST2LENQ         R1=Max len of est. extra dimension           
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* Initialise Order Extra Dimension                                    *         
***********************************************************************         
                                                                                
INITORD2 NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R1,ORD2LEDQ         R1=Max len w/o key:ORD2                      
         STCM  R1,B'0011',LEN_DATA Store for compare later                      
         LA    R1,ORD2LENQ         R1=Max len of order extra dimension          
         GOTO1 AINITALL                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* TSAR Routines                                                       *         
* -------------                                                       *         
***********************************************************************         
                                                                                
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIBUF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    TSAROBUF(TSPNEWL),TSAROBUF                                       
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR+TSRVAR                                  
         MVI   T.TSKEYL,L'OB_KEY                                                
         MVI   T.TSIND2,TSI2MANY                                                
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         LHI   R0,10*ONEK                                                       
         OC    T.TSBUFFL,T.TSBUFFL                                              
         JNZ   INIBUF2                                                          
         STCM  R0,3,T.TSBUFFL                                                   
                                                                                
INIBUF2  MVC   T.TSACOM,ACOMFAC                                                 
         GOTOR VTSAR,T.TSARD                                                    
         JE    YES                                                              
         DC    H'0'                                                             
         DROP  T                                                                
                                                                                
***********************************************************************         
* Add a record to optimisation buffer                                 *         
* - Entry: R1 points to caller's OB_D                                 *         
***********************************************************************         
                                                                                
ADDBUF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    YES                                                              
         DC    H'0'                                                             
         DROP  T                                                                
                                                                                
***********************************************************************         
* Get a record from optimisation buffer                               *         
* - Entry: R1 points to caller's OB_D                                 *         
* - Exit:  CC = neq if record not found in buffer                     *         
*          OBTSERR = TSAR error code                                  *         
***********************************************************************         
                                                                                
GETBUF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         MVC   OBTSERR,T.TSERRS                                                 
         JNE   NO                  Not found or EoF                             
         J     YES                                                              
         DROP  T                                                                
                                                                                
***********************************************************************         
* Get next record from optimisation buffer                            *         
* - Entry: R1 points to caller's OB_D                                 *         
* - Exit:  CC = neq if record not found in buffer                     *         
*          OBTSERR = TSAR error code                                  *         
***********************************************************************         
                                                                                
GETNXT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSANXT     Set action to 'Read Next'                    
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         MVC   OBTSERR,T.TSERRS                                                 
         JNE   NO                  Not found or EoF                             
         J     YES                                                              
         DROP  T                                                                
***********************************************************************         
* Put a record to optimisation buffer                                 *         
* - Entry: R1 points to caller's OB_D                                 *         
***********************************************************************         
                                                                                
PUTBUF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAWRT     Set Action to 'Write'                        
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    YES                                                              
         DC    H'0'                                                             
         DROP  T                                                                
***********************************************************************         
* Put a record to optimisation buffer                                 *         
* - Entry: R1 points to caller's OB_D                                 *         
***********************************************************************         
                                                                                
CLIREC   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*CLIREC*'                                                      
                                                                                
         LARL  RA,ADDRESS          RA=A(Common addresses)                       
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
O        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,O.OB_KEY   Set key                                      
                                                                                
         GOTOR AGETBUF,W.OB_D      Read for buffered record                     
         JNE   CLIREC20            Not found so add record                      
                                                                                
         AP    W.OB_BHRS,O.OB_BHRS Billable hours                               
         AP    W.OB_NHRS,O.OB_NHRS Non billable hours including r time          
         AP    W.OB_EHRS,O.OB_EHRS Estimated hours                              
         AP    W.OB_AHRS,O.OB_AHRS Actuals hours at job level                   
         AP    W.OB_NEXA,O.OB_NEXA Non billable expenses                        
         AP    W.OB_ESTA,O.OB_ESTA Estimated amount                             
         AP    W.OB_BILD,O.OB_BILD Billed amounts                               
                                                                                
         GOTOR APUTBUF,W.OB_D                                                   
         J     YES                                                              
                                                                                
CLIREC20 GOTOR AADDBUF,O.OB_D                                                   
         J     YES                                                              
         DROP  O,W                                                              
***********************************************************************         
* Retrieve company details from TSAR - build TSARREC first time       *         
***********************************************************************         
                                                                                
GETCPY   NTR1  BASE=*,LABEL=*                                                   
                                                                                
AGY      USING OB_D,OBTTAREA                                                    
         CLC   AGY.OB_KCPY,COMPANY Same as previous?                            
         JE    GCPYEX                                                           
         LA    R0,OBTTAREA         Read for buffered company                    
         LHI   R1,OB_LNQ-1                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   AGY.OB_KCPY,COMPANY                                              
                                                                                
GCPY02   GOTOR AGETBUF,AGY.OB_D                                                 
         JE    GCPY50                                                           
                                                                                
         USING CPYRECD,R2                                                       
         LA    R2,IOKEY            Read for company                             
         MVC   CPYKEY,GSPACES                                                   
         MVC   CPYKCPY,COMPANY                                                  
         XC    SAVCPY(SAVCPYLQ),SAVCPY                                          
         MVC   SAVCOMP,COMPANY                                                  
         L     R2,AIOAREA                                                       
         GOTO1 AREAD                                                            
         JNE   *+2                 Where is it?                                 
                                                                                
         MVC   ACCADDR,CPYKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                 DMGR issue?                                  
                                                                                
         XC    ACCADDR,ACCADDR                                                  
         AHI   R2,CPYRFST-CPYRECD                                               
         MVI   SAVC2CO,NOQ                                                      
         DROP  R2                                                               
                                                                                
         USING CPYELD,R2                                                        
GCPY03   CLI   CPYEL,0             Bad CPYRECD                                  
         JE    *+2                                                              
         CLI   CPYEL,CPYELQ                                                     
         JE    GCPY04                                                           
         LLC   RF,CPYLN                                                         
         AR    R2,RF                                                            
         J     GCPY03                                                           
                                                                                
GCPY04   MVC   SAVCALP,CPYALPHA                                                 
         XC    SAVCSEC,SAVCSEC                                                  
         MVC   SAVCCUR,CPYCURR                                                  
         MVI   SAVCRQNP,SPACEQ                                                  
         MVI   SAVCRQNS,SPACEQ                                                  
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    GCPY05                                                           
         MVI   SAVC2CO,YESQ                                                     
                                                                                
GCPY05   CLI   CPYLN,CPYLN4Q                                                    
         JL    GCPY06                                                           
         MVC   SAVCRQNP,CPYREQPF                                                
         MVC   SAVCRQNS,CPYREQSF                                                
                                                                                
GCPY06   CLC   CPYCURR,GSPACES                                                  
         JH    GCPY07                                                           
*&&UK*&& MVC   SAVCCUR,GBPLIT                                                   
*&&US*&& MVC   SAVCCUR,USDLIT                                                   
                                                                                
GCPY07   MVI   SAVCACT,YESQ                                                     
         CLI   DXTSTRUN+0,TSTJCLQ  Local override for testing                   
         JE    GCPY08                                                           
         MVI   SAVCACT,NOQ                                                      
         CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    GCPY08                                                           
         TM    CPYSTATD,CPYSGPDX                                                
***      JL    GCPY08                                                           
         JZ    GCPY08                                                           
         MVI   SAVCACT,YESQ                                                     
                                                                                
GCPY08   CLI   CPYLN,CPYSTAT9-CPYELD     For SERELD use company must be         
         JL    *+2                       Accent Cashflow converted|             
         TM    CPYSTAT9,CPYSSRNM                                                
         JZ    *+2                                                              
         DROP  R2                                                               
                                                                                
         USING LDGRECD,R2                                                       
GCPY09   LA    R2,IOKEY                                                         
         MVC   LDGKEY,GSPACES                                                   
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),PRODUL                                                
         L     R2,AIOAREA                                                       
         GOTO1 AREAD                                                            
         JNE   *+2                 Where is it?                                 
                                                                                
         MVC   ACCADDR,LDGKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                 DMGR issue?                                  
                                                                                
         XC    ACCADDR,ACCADDR                                                  
         AHI   R2,LDGRFST-LDGRECD                                               
         DROP  R2                                                               
                                                                                
         USING ACLELD,R2                                                        
GCPY10   CLI   ACLEL,0                                                          
         JE    *+2                                                              
         CLI   ACLEL,ACLELQ                                                     
         JE    GCPY12                                                           
         LLC   RF,ACLLN                                                         
         AR    R2,RF                                                            
         J     GCPY10                                                           
                                                                                
GCPY12   MVC   SAVCSJL,PRODUL                                                   
         MVC   SAVCSJA,ACLELLVA                                                 
         MVC   SAVCSJB,ACLELLVB                                                 
         MVC   SAVCSJC,ACLELLVC                                                 
         MVC   CLILEN,SAVCSJA                                                   
         LLC   RE,SAVCSJB                                                       
         LLC   RF,SAVCSJA                                                       
         SR    RE,RF                                                            
         STC   RE,PROLEN                                                        
         LLC   RE,SAVCSJC                                                       
         LLC   RF,SAVCSJB                                                       
         SR    RE,RF                                                            
         CHI   RE,L'ESTKJOB                                                     
         JNH   GCPY14                                                           
         LHI   RE,L'ESTKJOB                                                     
                                                                                
GCPY14   STC   RE,JOBLEN                                                        
         DROP  R2                                                               
                                                                                
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,GSPACES                                                   
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),ORUL                                                  
         L     R2,AIOAREA                                                       
         GOTO1 AREAD                                                            
         JNE   *+2                 Where is it?                                 
                                                                                
         MVC   ACCADDR,LDGKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                 DMGR issue?                                  
                                                                                
         XC    ACCADDR,ACCADDR                                                  
         AHI   R2,LDGRFST-LDGRECD                                               
         DROP  R2                                                               
                                                                                
         USING ACLELD,R2                                                        
GCPY16   CLI   ACLEL,0                                                          
         JE    *+2                                                              
         CLI   ACLEL,ACLELQ                                                     
         JE    GCPY18                                                           
         LLC   RF,ACLLN                                                         
         AR    R2,RF                                                            
         J     GCPY16                                                           
                                                                                
GCPY18   MVC   SAVC1RL,ORUL                                                     
         MVC   SAVC1RA,ACLELLVA                                                 
         MVC   SAVC1RB,ACLELLVB                                                 
         MVC   SAVC1RC,ACLELLVC                                                 
         MVC   SAVC1RD,ACLELLVD                                                 
         MVC   OFFLEN,SAVC1RA                                                   
         LLC   RE,SAVC1RB                                                       
         LLC   RF,SAVC1RA                                                       
         SR    RE,RF                                                            
         STC   RE,DEPLEN                                                        
         LLC   RE,SAVC1RC                                                       
         LLC   RF,SAVC1RB                                                       
         SR    RE,RF                                                            
         STC   RE,SUBLEN                                                        
         LLC   RE,SAVC1RD                                                       
         LLC   RF,SAVC1RC                                                       
         SR    RE,RF                                                            
         CHI   RE,L'PERKCODE                                                    
         JNH   GCPY20                                                           
         LHI   RE,L'PERKCODE                                                    
                                                                                
GCPY20   STC   RE,PERLEN                                                        
         DROP  R2                                                               
                                                                                
         LA    R3,SAVCLVLS         Retrieve other ledger level values           
         LHI   R4,SAVCLVL#                                                      
         LA    R5,ACCLDGS                                                       
                                                                                
GCPY22   CLI   0(R5),0             End of ledgers?                              
         JE    GCPY30                                                           
         CLC   0(2,R5),PRODUL      SJ and 1R done above                         
         JE    GCPY28                                                           
         CLC   0(2,R5),ORUL                                                     
         JE    GCPY28                                                           
                                                                                
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,GSPACES                                                   
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),0(R5)                                                 
         L     R2,AIOAREA                                                       
         GOTO1 AREAD                                                            
         JNE   GCPY28              If ledger not present skip here              
                                                                                
         MVC   ACCADDR,LDGKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   *+2                 DMGR issue?                                  
                                                                                
         XC    ACCADDR,ACCADDR                                                  
         AHI   R2,LDGRFST-LDGRECD                                               
         DROP  R2                                                               
                                                                                
         USING ACLELD,R2                                                        
GCPY24   CLI   ACLEL,0                                                          
         JE    *+2                                                              
         CLI   ACLEL,ACLELQ                                                     
         JE    GCPY26                                                           
         LLC   RF,ACLLN                                                         
         AR    R2,RF                                                            
         J     GCPY24                                                           
                                                                                
GCPY26   MVC   0(2,R3),0(R5)                                                    
         MVC   2(1,R3),ACLELLVA                                                 
         MVC   3(1,R3),ACLELLVB                                                 
         MVC   4(1,R3),ACLELLVC                                                 
         MVC   5(1,R3),ACLELLVD                                                 
         AHI   R3,6                                                             
         DROP  R2                                                               
                                                                                
GCPY28   AHI   R5,2                Next ledger                                  
         JCT   R4,GCPY22                                                        
         J     *+2                 (increase SAVCLVL#)                          
                                                                                
         USING CT5REC,R2                                                        
GCPY30   LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SAVCALP                                                 
*&&US*&& MVI   SAVCLOC,CTRYUSA                                                  
*&&UK*&& MVI   SAVCLOC,CTRYGBR                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         JNE   GCPY40              (? ought to die ...)                         
                                                                                
         L     R2,AIOAREA                                                       
         LA    R2,CT5DATA                                                       
                                                                                
         USING CTSEAD,R2                                                        
GCPY32   CLI   CTSEAEL,0           EoR?                                         
         JE    GCPY40                                                           
         CLI   CTSEAEL,CTSEAELQ                                                 
         JE    GCPY36                                                           
         CLI   CTSEAEL,CTAGDELQ                                                 
         JE    GCPY38                                                           
                                                                                
GCPY34   LLC   R1,CTSEALEN                                                      
         AR    R2,R1                                                            
         J     GCPY32                                                           
                                                                                
GCPY36   MVC   SAVCSEC,CTSEAAID    Save security alpha                          
         J     GCPY34                                                           
                                                                                
         USING CTAGDD,R2                                                        
GCPY38   CLI   CTAGDLEN,CTAGDL2Q                                                
         JL    GCPY34                                                           
         CLI   CTAGDCTY,0                                                       
         JE    GCPY34                                                           
         MVC   SAVCLOC,CTAGDCTY                                                 
         J     GCPY34                                                           
         DROP  R2                                                               
                                                                                
GCPY40   DS    0H                  Not found so add new                         
         LA    R0,OBTTAREA                                                      
         LHI   R1,OB_LNQ-1                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LHI   RF,OB_LNQ                                                        
         STH   RF,AGY.OB_LEN                                                    
                                                                                
         MVC   AGY.OB_KCPY,COMPANY                                              
                                                                                
         MVC   AGY.OB_CPY,SAVCPY                                                
                                                                                
         ZAP   AGY.OB_RC300,PZERO     DC    CL5'05300'                          
         ZAP   AGY.OB_RC301,PZERO     DC    CL5'05301'                          
         ZAP   AGY.OB_RC302,PZERO     DC    CL5'05302'                          
         ZAP   AGY.OB_RC303,PZERO     DC    CL5'05303'                          
         ZAP   AGY.OB_RC304,PZERO     DC    CL5'05304'                          
         ZAP   AGY.OB_RC305,PZERO     DC    CL5'05305'                          
         ZAP   AGY.OB_RC306,PZERO     DC    CL5'05306'                          
         ZAP   AGY.OB_RC307,PZERO     DC    CL5'05307'                          
         ZAP   AGY.OB_RC308,PZERO     DC    CL5'05308'                          
         ZAP   AGY.OB_RC309,PZERO     DC    CL5'05309'                          
         ZAP   AGY.OB_RC310,PZERO     DC    CL5'05310'                          
         ZAP   AGY.OB_RC311,PZERO     DC    CL5'05311'                          
         ZAP   AGY.OB_RC312,PZERO     DC    CL5'05312'                          
         ZAP   AGY.OB_RC313,PZERO     DC    CL5'05313'                          
         ZAP   AGY.OB_RC314,PZERO     DC    CL5'05314'                          
         ZAP   AGY.OB_RC315,PZERO     DC    CL5'05315'                          
         ZAP   AGY.OB_RC316,PZERO     DC    CL5'05316'                          
         ZAP   AGY.OB_RC317,PZERO     DC    CL5'05317'                          
         ZAP   AGY.OB_RC318,PZERO     DC    CL5'05318'                          
         ZAP   AGY.OB_RC319,PZERO     DC    CL5'05319'                          
         ZAP   AGY.OB_RC320,PZERO     DC    CL5'05320'                          
         ZAP   AGY.OB_RC321,PZERO     DC    CL5'05321'                          
         ZAP   AGY.OB_RC322,PZERO     DC    CL5'05322'                          
         ZAP   AGY.OB_RC323,PZERO     DC    CL5'05323'                          
         ZAP   AGY.OB_RC324,PZERO     DC    CL5'05324'                          
         ZAP   AGY.OB_RC325,PZERO     DC    CL5'05325'                          
         ZAP   AGY.OB_RC326,PZERO     DC    CL5'05326'                          
         ZAP   AGY.OB_RC327,PZERO     DC    CL5'05327'                          
         ZAP   AGY.OB_RC328,PZERO     DC    CL5'05328'                          
         ZAP   AGY.OB_RC329,PZERO     DC    CL5'05329'                          
         ZAP   AGY.OB_RC330,PZERO     DC    CL5'05330'                          
         ZAP   AGY.OB_RC336,PZERO     DC    CL5'05336'                          
         ZAP   AGY.OB_RC337,PZERO     DC    CL5'05337'                          
         ZAP   AGY.OB_RC339,PZERO     DC    CL5'05339'                          
         ZAP   AGY.OB_RC340,PZERO     DC    CL5'05340'                          
                                                                                
         GOTOR AADDBUF,AGY.OB_D    Add it then get it                           
         JE    GCPY02                                                           
         DC    H'0'                                                             
                                                                                
GCPY50   MVC   SAVCPY(SAVCPYLQ),AGY.OB_CPY                                      
                                                                                
         XC    LAST_CLI,LAST_CLI                                                
         XC    LAST_PRO,LAST_PRO                                                
*&&UK                                                                           
         L     R2,ABIGAREA                                                      
         GOTO1 VBLDCUR,DMCB,(0,A(0)),(X'40',(R2)),(SAVCLOC,ACOMFAC)             
         CLI   0(R1),0                                                          
         JNE   *+2                 Die on any error                             
                                                                                
         SAM31 ,                                                                
         L     R0,ACURBUF          Move CURBUF persistent to SVCREC             
         LHI   R1,BIGLENQ          L'SVREC+L'SVOREC                             
         L     RE,ABIGAREA                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
*&&                                                                             
                                                                                
GCPYEX   MVC   TODAYC,DXDATENC                                                  
         PACK  FULL,AGXCPYDQ       SEE AGXRECID                                 
         ZAP   DUB,FULL                                                         
         CVB   R1,DUB                                                           
         ST    R1,AGXCPYOF         STORE OFFSET                                 
         J     YES                                                              
                                                                                
***********************************************************************         
* Literals in LTORG and SSB                                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
                                                                                
         DS    0D                                                               
SSB      DC    X'0000',X'FF',X'00' For Datamgr (copied from DXTRACT)            
         DC    1020X'00'                                                        
SSBLQ    EQU   *-SSB                                                            
                                                                                
ODBUFFER DS    0H                                                               
         DS    (ODBMAXQ*ODBLENQ+1)X                                             
                                                                                
ODBUFOLD DS    0H                                                               
         DS    (ODBMAXQ*ODBLENQ+1)X                                             
                                                                                
***********************************************************************         
* DSECT to cover local working storage                                *         
***********************************************************************         
                                                                                
WORKD    DSECT                                                                  
                                                                                
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
ALPARMS  DS    0XL16                                                            
ALPAEXTR DS    A                   EXTRACT ROUTINE                              
ALPAINIT DS    A                   INITIALISATION ROUTINE                       
ALPAFILT DS    A                   FILTER ROUTINE                               
ALPACNVX DS    A                   CONVERT ROUTINE                              
                                                                                
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPERECT DS    XL1                                                              
TYPEREC2 DS    XL1                                                              
TYPETYPE DS    XL1                                                              
TYPETYP2 DS    XL1                                                              
TYPEVERS DS    XL1                                                              
TYPEATYP DS    XL1                                                              
                                                                                
RETCODE  DS    XL1                                                              
RECTYPE  DS    XL1                                                              
RECATYP  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
RCVDATEC DS    CL6                 CHARACTER RECOVERY DATE                      
LESS1YR  DS    XL2                 COMPRESSED RECOVERY DATE MINUS 1 YR          
NOPRINT  DS    XL1                                                              
ISYESQ   EQU   X'80'                                                            
AUDOPEN  DS    XL1                                                              
XPFM     DS    CL1                 Extract PFm records?                         
                                                                                
ACCSYSQ  EQU   X'06'                                                            
*&&UK                                                                           
BULKAPIQ EQU   X'0F'               RMOR.DXTRACT(B)                              
*&&                                                                             
*&&US                                                                           
BULKAPIQ EQU   X'10'               FOR NA                                       
*&&                                                                             
WAREHSEQ EQU   BULKAPIQ            RMOR.DXTRACT(C) new name                     
                                                                                
COMPANY  DS    XL1                                                              
                                                                                
STSRKEY  DS    XL(L'OB_KEY+L'OB_LEN)                                            
                                                                                
ASUBLAST DS    A                   A(Last sub record) - UNUSED |||              
                                                                                
TREC     DS    0C                                                               
TRECTY   DS    CL1                 RECORD TYPE                                  
         DS    CL1                                                              
TRECID   DS    CL3                 RECORD ID                                    
         DS    CL1                                                              
TALPHA   DS    CL2                 ALPHA ID OF AGENCY                           
         DS    CL1                                                              
TDUMMY   DS    CL4                                                              
         DS    CL1                                                              
TRECL    EQU   *-TREC              RECORD LENGTH                                
TRECACL  EQU   *-(TALPHA+L'TALPHA) LENGTH OF ACCUMULATORS, VERSION 5            
                                                                                
PKWORK2  DS    PL2                                                              
WORKL    EQU   *-WORKD                                                          
                                                                                
***********************************************************************         
* DSECT to cover PRTSUM print line                                    *         
***********************************************************************         
                                                                                
PLINED   DSECT                                                                  
PLINLEF  DS    CL1                                                              
PLINAGY  DS    CL2                                                              
PLINMI1  DS    CL1                                                              
PLINTYP  DS    CL5                                                              
PLINMI2  DS    CL1                                                              
PLINCOD  DS    CL10                                                             
PLINMI3  DS    CL1                                                              
PLINNUM  DS    CL12                                                             
PLINRIG  DS    CL1                                                              
                                                                                
***********************************************************************         
* Other DSECTs required in here                                       *         
***********************************************************************         
                                                                                
APRTABD  DSECT                                                                  
APRSTAT  DS    XL1                 Levels to include                            
APRCLI   EQU   X'80'               Client                                       
APRPRO   EQU   X'40'               Product                                      
APRJOB   EQU   X'20'               Job                                          
APRMED   EQU   X'10'               Media                                        
APROFF   EQU   X'08'               Office                                       
APRLVL   DS    CL1                 Level of approver                            
APRTABL  EQU   *-APRTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* DSECT to cover UIDTAB data                                          *         
***********************************************************************         
                                                                                
UIDTABD  DSECT                                                                  
UIDTHEX  DS    XL2                 User ID or zeroes for EoT                    
UIDTCOD  DS    CL10                User ID code                                 
UIDTSTA  DS    CL1                 Yes/No UID found                             
UIDTLNQ  EQU   *-UIDTABD                                                        
UIDTMAX  EQU   250                                                              
                                                                                
***********************************************************************         
* DSECT to cover AGTLIST data                                         *         
***********************************************************************         
                                                                                
AGYLSTD  DSECT                                                                  
AGYLAID  DS    CL2                 AGENCY ALPHA-ID                              
AGYLBIN  DS    XL1                 BINARY AGENCY/COMPANY                        
AGYLCUT  DS    CL1                 Yes/No CUT output flag                       
AGYLLNQ  EQU   *-AGYLSTD                                                        
AGYLMAXQ EQU   10                  MAXIMUN NUMBER OF AGENCIES                   
AGYLSTLQ EQU   (AGYLMAXQ+1)*AGYLLNQ                                             
                                                                                
                                                                                
* AGXWORKD                                                                      
       ++INCLUDE AGXWORKD                                                       
                                                                                
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
                                                                                
* AGXRECD                                                                       
         PRINT OFF                                                              
       ++INCLUDE AGXRECD                                                        
         PRINT ON                                                               
                                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
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
                                                                                
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
                                                                                
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
                                                                                
* DMRCVRHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVRHDR                                                      
         PRINT ON                                                               
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
* DXSQLD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DXSQLD                                                         
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
                                                                                
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
                                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
                                                                                
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
                                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
* AGXTYPTABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE AGXTYPTABD                                                     
         PRINT ON                                                               
                                                                                
       ++INCLUDE ACJOBBLOCK                                                     
       ++INCLUDE ACGOXBLOCK                                                     
       ++INCLUDE ACGOBBLOCK                                                     
                                                                                
PRORATAD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
                                                                                
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048AGXTRACT  08/24/20'                                      
         END                                                                    
