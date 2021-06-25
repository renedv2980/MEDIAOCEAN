*          DATA SET ACBRA28    AT LEVEL 067 AS OF 08/19/20                      
*PHASE T62428A                                                                  
                                                                                
ACBRA28  TITLE '- BrandOcean Account Download Server'                           
                                                                                
***********************************************************************         
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 29SEP08 New application version                                      
* YNGX 001 11MAR09 <LO01-8680> Person record maintenance                        
* NSHE 003 05AUG10 TSARREC moved to saved storage                               
* NSHE 004-006 ???? replace CHKACS with ADVALL/ACCRUL                           
* NSHE 007 26JAN11 Add Fax, tele, email to high level account return            
* NSHE 008 04FEB11 Add new GAP opt maint settings                               
* NRAK 009 03DEC10 <PR000607> Replace 0D/0E from ACBRA11 with 81                
* SMAN 010 14OCT11 <PR002286> Allow d/l of all Cli/Pro/Jobs                     
* NSHE 011 01NOV11 US Merge and fixes to GETOPT call                            
* NSHE     11NOV11 Change to GAPLST calls                                       
* JFOS     19MAR12 Reduce 'All SJ' req processing to prevent timeouts           
* NSHE 012 31AUG12 <PR003191> Add colour coded WC and exclude memo              
* NSHE     03SEP12 Change IO routine for auto switching system                  
* MPEN     25FEB13 <BR54230L> Check order fully billed first!                   
* NSHE 013 19JUL13 Set SETFAC earlier to capture online process                 
* NRAK 014 20AUG13 <DSRD-14> looking up office code is inefficient              
* YNGX 014         <DSRD-14> fixes to 81 response fields                        
* YNGX 015 11SEP13 <DSRD-13> fixes to 80 response fields                        
* NSHE 016 11OCT13 <DSRD-35> Skip apprver array and ensure level is set         
* NRAK     17OCT13 Also fix limlist+hier building for SJ                        
* NRAK     21OCT13 <DSRD-173> Also make pprel edits match TimeInitial           
* NSHE     21OCT13 QA failure in BRO due to missing approver array              
*                  originally taken out for Rodick - reintroduced               
*                  but only sending 1 row of elements                           
* NRAK     25NOV13 <DSRD-383> fix limlist                                       
* NSHE 017 02APR14 <DSRD-1936> Fix to office download                           
* NSHE 018 20MAY14 <DSRD-2491> Add level to account display response            
* NRAK     01MAY14 <DSBO-819> Pass GOBILCUR on SJ accounts                      
* JFOS 019 03JUN14 <DSRD2790> Test RSTSPRJB in EDTFJOB/don't test               
*                  GOTNOJOB=N in AFLTGOP for Time                               
* NSHE     06JUN14 Ensure office filtering takes place                          
* NSHE 020 24JUN14 Treat exclude and time lock the same for Aura                
* NSHE     25JUN14 Optimise account list output for jobs and time               
* NSHE 021 09JUL14 <DSRD-3453> Return opt maint value for foreign job           
* MPEN     18JUL14 <DSRD-3593> Return all offices if viewall=Y                  
*          22JUL14 <DSRD-3058> Fix issue where closed jobs returned             
*          24JUL14 <DSRD-3708> Show error if displaying deleted job             
* TKLU     29JUL14 <DSRD-3681> AC_ESTIT - use RSTSTAT6 not ESTRECD IOs          
* NSHE     29JUL14 <DSRD-3737> Master job linking                               
* NSHE     15AUG14 <DSRD-2816> Don't allow duplicate person codes               
* TKLU     21Aug14 <DSRD-4006> Account List to filter out none JOBSMCSE         
*                              jobs if application is estimates                 
* NSHE     21AUG14 <DSRD-3495> Fix to ensure correct length of approver         
*                              entry is saved to buffer                         
* NSHE 022 12SEP14 <DSBO-1090> Don't send previous account array for            
*                  resource management                                          
* NSHE 023 17SEP14 <DSRD-4245> Default to added date if no open date            
* NSHE     19SEP14 <DSRD-4422> Ensure name element sent only once               
* NSHE 024 29SEP14 <DSRD-4549> Always respond in English                        
* NSHE 024 01OCT14 <DSRD-4536> Add new request field to account list            
* NSHE             to know if front end is using typeahead or ellipse           
* MPEN 025 28NOV14 <DSRD-5320> Return *** for all currencies                    
* NRAK     23DEC14 fixes for 2D search/list for aura                            
* NSHE     31DEC14 <DSRD-5512> Ensure name is always sent                       
* MPEN 026 22JAN15 <DSRD-5889> Only return previous level array for SJ          
* NRAK     23JAN15 <DSPCA-1237> Fix SETLV1/2/3 skipcols                         
* NSHE     30JAN15 <DSRD-5941> Fix dept and staff look up for Aura              
* TKLU     12Feb15 <RD006127> Add ECE option to account download                
* NRAK     19Feb15 <DSRD-6239> Return 'Force Product for Expense'               
* JSHA     25Feb15 <DSRD-6254> Return 'Force Product for Expense' US            
* JSHA     25Feb15 <DSRD-6314> Return 'ClientAnalysis for Expense' US           
* NSHE 027 14Apr15 <DSRD-4766> Only return relevant info for vendors            
* NSHE 028 07May15 <DSRD-7094> Ensure currency is sent out as * for all         
* NSHE 029 19May15 <DSRD-7221> Return EAM and ERD opt maint settings            
* JFOS 030 18jul15 <PCA-1881> One * currency on Display call for Aura           
* NSHE 031 02Sep15 <DSFTK-190> add new opt maint setting for rates              
*NRAK 032 13Nov15 <DSRD-8878> return default filter names for PDF               
*MPEN 033 11Dec15 <DSRES-22> Fix issue with PIDNO wrong length                  
*JFOS 034 13Oct15 <PCA-1989> Support Limit A/C Access                           
*TKLU 035 27Jan15 <RD010160> Performance improvements (GETOPT)                  
*NSHE     28Jan15 <RD010158> Send correct get opt approved orders               
*GHOA 036 24Mar16 <DSSUP-1732> BIL= in Footer Comments                          
*JSHA 037 09Jun16 <DSRD-11520> Making BIL= in Footer Comments US only           
*NSHE 038 26Jul16 <DSRD-12429> Don't show team in BrandOcean                    
*MPEN 039 03Jan17 <DSRD-14396> Copy US fix for create an estimate               
*GHOA 040 13Jan17 <DSRD-14444> Studio work for Account Search                   
*         17Jan17 <DSRD-14443> Studio work for Account Display                  
*NSHE     03Mar17 Fix bad branch when no entries on limit list                  
*NSHE 041 24Apr17 DSRD-15604 optimise supplier download for orders              
*YNGX     10May17 DSRD-15748 Sent full department code for InvoiceLog           
*YNGX     12May17 DSRD-15789 Fixed VAT account search                           
*MPEN 042 22May17 DSRD-15486 Fix user field exceptions                          
*MPEN 043 14Jul17 DSRD-16317 Loosen inactive date filter to be 5 yrs            
*                 DSRD-16300 Temp disable inactive date filter for US           
*NSHE 044 09Aug17 DSRD-16474 Make change to active date                         
*MPEN     11Aug17 DSRD-16623 Amend MF search to work for suppliers              
*MPEN 045 04Sep17 DSRD-16797 Fix for MF account code search                     
*MPEN 046 08Sep17 DSRD-16864 Fix for 2 char vendor code search                  
*NSHE     11Sep17 DSRD-16858 New application time off                           
*NSHE 047 30Nov17 DSRD-17377 temp fix as US system doesn't update               
*                 passive pointers of accounts correctly                        
*NSHE     11Jan18 DSRD-17088 Return accounts based on back up approval          
*                 for time and expense modules                                  
*ABID 048 12FEB18 DSPCA-2824  EXTEND ACCOUNT DISPLAY X'80' IN =BRA TO           
*                 RETRIEVE EQUIVALENT MAPPINGS                                  
*TKLU 048 14Feb18 DSRD-18081 Return new Opt/Maint GOEPAJ setting                
*MPEN 049 15Feb18 SPEC-20876 Fix for invoicelog vat rate                        
*MPEN 050 19Apr18 <IAPP-184797> Fix bug in resetlvl, not resetting AIO2         
*MPEN 051 01Jun18 <DSRD-18847> Return job locked from t/s for 1N                
*NSHE 052 06Jul18 <DSRD-19544> Allow to return all accounts                     
*NSHE 053 10Oct18 <DSRD-20458> fix word search for account when SJ              
*MPEN 054 10Dec18 <DSRD-20991> Fix for SJ prod level word search                
*MPEN 055 17Dec18 <DSRD-20989> Change acc search sort order                     
*MPEN 056 31Dec18 <DSRD-21048> Ignore no search words                           
*MPEN     03Jan19 <DSRD-21163> Fix for supplier search                          
*NSHE     15Jan19 DSRD-21247 Ensure office is set correct when search           
*MPEN     23Jan19 <DSRD-21414> Fix for DSRD-21163 above                         
*NSHE     31Jan19 <DSRD-21481> Fix to media limit list for search               
*NSHE 057 05Mar19 <DSRD-21898> Fix job list for product offices                 
*NSHE     21Mar19 <DSRD-22039> Fix account code search when code                
*                              contains embedded spaces                         
*ABID 058 05MAY19 <DSRD-22254> INVOICELOG-ACCOUNT DISPLAY CALL TO BRING         
*                              DOWN PREVIOUS HIGHER LEVELS DUE DATE             
*NSHE 059 31MAY19 <DSRD-22083> Fix sending email address and GAP                
*MPEN     07JUN19 <DSRD-22773> Fix for sending GAP in use                       
*MPEN 060 05AUG19 <DSRD-23254> Fix for jobs returned when client locked         
*MPEN 061 02SEP19 <DSRD-23719> Fix for filtering out accounts                   
*NSHE 062 11Oct19 DSRD-24187 Change CHKCPL routine to exit not equal            
*                            if account not found                               
*MPEN 063 01Nov19 <DSRD-23932> Indicator for person terminated in cost          
*MPEN 064 10Feb20 <DSRD-25377> Relink book                                      
*MPEN 065 17Feb20 <DSRD-25557> Fix for GAP service in use flag                  
*MPEN     20Feb20 <DSRD-25583> Fix for filtering locked accounts                
*VGUP     24FEB20 <DSRD-25598> Fix the word search issue                        
*MPEN 066 18MAY20 <DSRD-25987> New opt main setting Est in last fisc yr         
*NSHE     11JUN20 <DSRD-25764> Job/Close returns error Cannot close job         
*MPEN 067 07JUL20 <DSRD-26683> Expense finance search changes                   
*                                                                               
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,WORKERKEY=ACBO,ABENDLIST=FAILS,          +        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#LP_D,LP_D,                                            +        
               B#TWAD,TWAD,                                            +        
               B#ACCREC,ACCRECD,                                       +        
               B#ACNT,ACTRECD,                                         +        
               B#ACEX,AEXRECD,                                         +        
               B#FNVREC,RSFRECD,                                       +        
               B#MJB,MJBPASD,                                          +        
               B#PERREC,PERRECD,B#LLSREC,LLSRECD,B#APPREC,APPRECD,     +        
               B#GLSREC,GLSRECD,B#PRACT,ACTRECD,                       +        
               B#PID,PIDRECD,B#ROLE,ROLRECD,B#1RACT,ACTRECD,           +        
               B#GOBLK,GOBLOCKD,                                       +        
               B#GOXBLK,GOXBLKD,                                       +        
               B#GOBBLK,GOBBLKD,                                       +        
               B#SVRDEF,SVRDEF)                                                 
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO28**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R7=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SETS A(WORKD)          BLOCK #1         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,RSVRSAVE      R8=A(7K SAVE AREA)                           
         B     INIT03                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)              BLOCK #1         
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
INIT03   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
         MVC   ATWA,LP_ATWA                                                     
                                                                                
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         DROP  R6,R7                                                            
                                                                                
         MVI   TWAMODE,0                                                        
         L     R7,ATWA             R7=A(ON/OFFLINE TWA)                         
         USING TWAD,R7                                                          
                                                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         BE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         BE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         BE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   L     R0,ATIA                                                          
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
         L     RF,AMASTC                                                        
         MVC   VACCEMU,MCVACEMU-MASTD(RF)                                       
         MVC   AROUT1,LP_AUIR1     SET LOCAL INDEX ROUTINE ADDRESSES            
         MVC   AROUT2,LP_AUIR2     WHICH WERE LOADED BY MASTER SERVER           
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
         B     RUNSTR04                                                         
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         L     RF,AACCFACS                                                      
         MVC   VACCEMU,X_AACCEMU-X_ACCFACSD(RF)                                 
                                                                                
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#ACNT-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                   
         MVC   LP_BLKS+((B#MJB-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         MVC   LP_BLKS+((B#PERREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                 
         MVC   LP_BLKS+((B#ACEX-1)*L'LP_BLKS)(L'LP_BLKS),AIO8                   
         MVC   LP_BLKS+((B#ACCREC-1)*L'LP_BLKS)(L'LP_BLKS),AGENAREA             
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#GOBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBLOCB              
         MVC   LP_BLKS+((B#GOBBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBBLCK             
         MVC   LP_BLKS+((B#GOXBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOXBLCK             
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    PRCWRK04                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK04 J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2ACCT       set to Account                               
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         MVC   USRID,CUUSER                                                     
         GOTOR VDATCON,DMCB,(15,0),(1,DA_TODP)                                  
         GOTOR (RF),(R1),(15,0),(2,DA_TODC)                                     
         GOTOR (RF),(R1),(15,0),(0,DA_TODF)                                     
         GOTOR VADDAY,DMCB,(C'Y',DA_TODF),WORK,-5                               
         GOTOR VDATCON,DMCB,(0,WORK),(1,DA_MIN5P)                               
         LA    RE,SWSTAB                                                        
         USING SYSSWTAB,RE                                                      
RUNREQ1  CLI   SYSSWSYS,0                                                       
         JE    RUNREQ2                                                          
         CLI   SYSSWSOV,QSMED                                                   
         JNE   *+16                                                             
         MVC   MEDAGYB,SYSSWAGB                                                 
         MVC   MEDSCNUM,SYSSWACS                                                
         LA    RE,SYSSWLEN(RE)                                                  
         J     RUNREQ1                                                          
                                                                                
RUNREQ2  L     RE,LP_AWMP          BUILD DEFAULT RANGE ELEMENTS IN WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    ALL VALUES                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    NON-ZERO VALUES                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
         XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
                                                                                
         MVC   USROFF,SPACES                                                    
         CLI   CUACCS,0                                                         
         JE    RUNREQ04                                                         
         TM    CPYSTATC,CPYSROFF OFFICES FOR RESOURCES                          
         JZ    RUNREQ04                                                         
         MVC   USROFF,CUACCS+2                                                  
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   RUNREQ04                                                         
         MVC   USROFF,OFFAWORK                                                  
         MVI   USROFF+1,C' '                                                    
                                                                                
RUNREQ04 TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNREQX                                                          
                                                                                
         L     R0,AGOBLOCB                                                      
         AHI   R0,GOADM-GOBLOCK                                                 
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SHI   R0,GOADM-GOBLOCK                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,CTFILE,(4,0),0                               
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
**********************************************************************          
* Account download                                                   *          
**********************************************************************          
                                                                                
REQACT   LKREQ H,A#ACTD,OUTACT,NEXTREQ=REQACTS                                  
Unit     LKREQ F,1,(D,B#SAVED,QUNIT),CHAR,OLEN=L'QUNIT,                +        
               MAXLEN=L'QUNIT,TEXT=AC#UNIT,COL=*                                
Ldg      LKREQ F,2,(D,B#SAVED,QLEDG),CHAR,OLEN=L'QLEDG,                +        
               MAXLEN=L'QLEDG,TEXT=AC#LGRC,COL=*                                
Act      LKREQ F,3,(D,B#SAVED,QACT),CHAR,OLEN=L'QACT,                  +        
               MAXLEN=L'QACT,TEXT=AC#ACC,COL=*                                  
LimLis   LKREQ F,4,(D,B#SAVED,QLIML),CHAR,OLEN=L'QLIML,                +        
               MAXLEN=L'QLIML,TEXT=AC#LIMLS,COL=*                               
Applic   LKREQ F,5,(D,B#SAVED,QAPPL),CHAR,OLEN=L'QAPPL,                +        
               MAXLEN=L'QAPPL,TEXT=AC#APPLI,COL=*                               
Media    LKREQ F,6,(D,B#SAVED,QMED),CHAR,OLEN=L'QMED,                  +        
               MAXLEN=L'QMED,TEXT=AC#MEDC,COL=*                                 
InvDte   LKREQ F,7,(D,B#SAVED,QINVDTE),PDAT,OLEN=L'QINVDTE,            +        
               MAXLEN=8,TEXT=AC#INVDT,COL=*                                     
Drft     LKREQ F,9,(D,B#SAVED,QDRAFT),CHAR,OLEN=L'QDRAFT,              +        
               MAXLEN=L'QDRAFT,TEXT=AC#DRAFT,COL=*                              
Workcode LKREQ F,10,(D,B#SAVED,QWORKC),CHAR,OLEN=L'QWORKC,             +        
               MAXLEN=L'QWORKC,TEXT=AC#WC,COL=*                                 
PDF      LKREQ F,11,(D,B#SAVED,QPDF),CHAR,OLEN=L'QPDF,                 +        
               MAXLEN=L'QPDF,TEXT=AC#PDFS,COL=*                                 
Bill     LKREQ F,13,(D,B#SAVED,QBILL),CHAR,OLEN=L'QBILL,               +        
               MAXLEN=L'QBILL,TEXT=AC#BLB,COL=*                                 
FutTim   LKREQ F,14,(D,B#SAVED,QFUTIM),CHAR,OLEN=L'QFUTIM,             +        
               MAXLEN=L'QFUTIM,TEXT=AC#FUTA,COL=*                               
Dept     LKREQ F,15,(D,B#SAVED,QDEPT),CHAR,OLEN=L'QDEPT,               +        
               MAXLEN=L'QDEPT,TEXT=AC#DPT,COL=*                                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTACT   LKOUT H                                                                
                                                                                
ACTDIS   LKOUT R,A#ACTD            account                                      
Array    LKOUT C,1,(A,ARYACT)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYACT   LKOUT A,(R,ACTINI),NROWS=1,ROWNAME=ACTRECD,NEWEL=Y                     
                                                                                
Array    LKOUT C,1,(A,ARYACNM)                                                  
Array    LKOUT C,2,(A,ARYALNM),FILTROUT=TSTSJ                                   
Array    LKOUT C,3,(A,ARYFNAM),FILTROUT=TSTSJCC                                 
Array    LKOUT C,4,(A,ARYLNGN),FILTROUT=TSTSJ                                   
Array    LKOUT C,5,(A,ARYSNM),FILTROUT=TSTSJ                                    
MedCd    LKOUT C,6,ACTKULA,(U,#EDTMED,$EDTMED),FILTROUT=TSTSJ,         +        
               SKIPCOLS=1,ND=Y                                                  
MedNam   LKOUT C,7,ACTKULA,(U,#EDTMDN,$EDTMDN),ND=Y                             
MedInc   LKOUT C,8,ACTKULA,(U,#EDTMIN,$EDTMIN),ND=Y                             
PRout    LKOUT P,,SETLV1                                                        
Array    LKOUT C,1,(A,ARYNAME),FILTROUT=TSTAIOL,SKIPCOLS=6                      
PRout    LKOUT P,,SETLV2                                                        
Array    LKOUT C,1,(A,ARYNAME),FILTROUT=TSTAIOL,SKIPCOLS=4                      
PRout    LKOUT P,,SETLV3                                                        
Array    LKOUT C,1,(A,ARYNAME),FILTROUT=TSTAIOL,SKIPCOLS=2                      
PRout    LKOUT P,,SETLV4                                                        
Array    LKOUT C,1,(A,ARYNAME),FILTROUT=TSTAIOL                                 
PRout    LKOUT P,,RESETLVL                                                      
AppSt    LKOUT C,9,(D,B#SAVED,AC_APST),CHAR                                     
Esti     LKOUT C,10,(D,B#SAVED,AC_ESTI),CHAR,ND=Y                               
ClsDte   LKOUT C,11,(D,B#SAVED,AC_CLSDT),PDAT,ND=Y                              
OpnDte   LKOUT C,12,(D,B#SAVED,AC_OPNDT),PDAT,ND=Y                              
MstJb    LKOUT C,13,(D,B#SAVED,AC_MSTJB),CHAR,ND=Y                              
IDNum    LKOUT C,14,(D,B#SAVED,AC_IDNUM),HEXD                                   
Array    LKOUT C,15,(A,ARYADR)                                                  
Array    LKOUT C,16,(A,ARYFLT)                                                  
Lock     LKOUT C,17,(D,B#SAVED,AC_LOCK),CHAR                                    
Clos     LKOUT C,18,(D,B#SAVED,AC_CLOS),CHAR,ND=Y                               
Dept     LKOUT C,19,(D,B#SAVED,AC_DEPT),CHAR,ND=Y,FILTROUT=TSTSE                
Miles    LKOUT C,20,(D,B#SAVED,AC_MILES),CHAR,ND=Y,FILTROUT=TSTSE               
CostG    LKOUT C,21,(D,B#SAVED,AC_COSTG),CHAR,ND=Y,FILTROUT=TSTCLA              
Staff    LKOUT C,22,(D,B#SAVED,AC_STAF),CHAR,ND=Y,FILTROUT=TSTSE                
BalSht   LKOUT C,23,(D,B#SAVED,AC_BAL),CHAR,ND=Y                                
PnL      LKOUT C,24,(D,B#SAVED,AC_PNL),CHAR,ND=Y                                
JbAna    LKOUT C,25,(D,B#SAVED,AC_JBANA),CHAR,ND=Y,FILTROUT=TSTSE               
WCreqd   LKOUT C,26,(D,B#SAVED,AC_WCRQD),CHAR,ND=Y                              
PrvVnd   LKOUT C,27,(D,B#SAVED,AC_PROVV),CHAR,ND=Y                              
MrgInv   LKOUT C,28,(D,B#SAVED,AC_MRGIN),CHAR,ND=Y                              
PayeLk   LKOUT C,29,(D,B#SAVED,AC_PAYLK),CHAR,ND=Y                              
RCr      LKOUT C,30,(D,B#SAVED,AC_RCR),CHAR,ND=Y                                
RDr      LKOUT C,31,(D,B#SAVED,AC_RDR),CHAR,ND=Y                                
BbfDt    LKOUT C,32,(D,B#SAVED,AC_BBFDT),PDAT,ND=Y                              
TrnDt    LKOUT C,33,(D,B#SAVED,AC_TRNDT),PDAT,ND=Y                              
Sec      LKOUT C,34,(D,B#SAVED,AC_SECY),CHAR,ND=Y                               
CliCst   LKOUT C,35,(D,B#SAVED,AC_CLICS),CHAR,ND=Y                              
EstLk    LKOUT C,36,(D,B#SAVED,AC_LKEST),CHAR,ND=Y                              
OrdLk    LKOUT C,37,(D,B#SAVED,AC_LKORD),CHAR,ND=Y                              
BilLk    LKOUT C,38,(D,B#SAVED,AC_LKBIL),CHAR,ND=Y                              
TimLk    LKOUT C,39,(D,B#SAVED,AC_LKTIM),CHAR,ND=Y                              
AdjLk    LKOUT C,40,(D,B#SAVED,AC_LKADJ),CHAR,ND=Y                              
ExtLk    LKOUT C,41,(D,B#SAVED,AC_LKEXT),CHAR,ND=Y                              
ExcTS    LKOUT C,42,(D,B#SAVED,AC_EXCTS),CHAR,ND=Y                              
Exctv    LKOUT C,43,(D,B#SAVED,AC_PEXEC),CHAR,ND=Y                              
ProjC    LKOUT C,44,(D,B#SAVED,AC_PROCN),CHAR,ND=Y                              
DskPr    LKOUT C,45,(D,B#SAVED,AC_DSKPR),CHAR,ND=Y                              
InVat    LKOUT C,46,(D,B#SAVED,AC_INVAT),CHAR,ND=Y                              
Sundry   LKOUT C,47,(D,B#SAVED,AC_SUNDY),CHAR,ND=Y                              
InvRg    LKOUT C,48,(D,B#SAVED,AC_INVRG),CHAR,ND=Y                              
FutTime  LKOUT C,49,(D,B#SAVED,AC_FUTIM),CHAR,ND=Y                              
DefWC    LKOUT C,50,(D,B#SAVED,AC_DEFWC),CHAR,ND=Y                              
GLoff    LKOUT C,51,(D,B#SAVED,AC_GLOFC),CHAR,ND=Y                              
Array    LKOUT C,60,(A,ARYGLAC)                                                 
Array    LKOUT C,61,(A,ARYOTHEL),FILTROUT=TSTST                                 
Curr     LKOUT C,63,(D,B#SAVED,AC_CUR),CHAR,ND=Y                                
Locl     LKOUT C,64,(D,B#SAVED,AC_LOCL),CHAR,ND=Y                               
ksvRt    LKOUT C,65,(D,B#SAVED,AC_KSVR),CHAR,ND=Y                               
ForUsr   LKOUT C,69,(D,B#SAVED,AC_FUSR),CHAR,ND=Y                               
VATC     LKOUT C,70,(D,B#SAVED,AC_VATC),CHAR,ND=Y                               
Array    LKOUT C,70,(A,ARYFFTL)                                                 
Array    LKOUT C,85,(A,ARYMEMO)                                                 
Array    LKOUT C,86,(A,ARYSCIL)                                                 
Array    LKOUT C,89,(A,ARYDSCR),FILTROUT=TSTCR                                  
Array    LKOUT C,90,(A,ARYVATR),FILTROUT=TSTSG                                  
Array    LKOUT C,91,(A,ARYDEXL)                                                 
DueDat   LKOUT C,91,(D,B#SAVED,AC_DUDT),CHAR,ND=Y                               
BilAl    LKOUT C,92,(D,B#SAVED,AC_BILA),CHAR,ND=Y                               
ChgAl    LKOUT C,93,(D,B#SAVED,AC_CHGA),CHAR,ND=Y                               
NBiAl    LKOUT C,94,(D,B#SAVED,AC_NONA),CHAR,ND=Y                               
TypTim   LKOUT C,95,(D,B#GOXBLK,GOTOT),CHAR,ND=Y                                
FrPrd    LKOUT C,96,(D,B#SAVED,AC_FPT),CHAR,ND=Y                                
FrJob    LKOUT C,97,(D,B#SAVED,AC_FJT),CHAR,ND=Y                                
FNarB    LKOUT C,98,(D,B#SAVED,AC_FNTB),CHAR,ND=Y                               
FNarR    LKOUT C,99,(D,B#SAVED,AC_FNTR),CHAR,ND=Y                               
FNarN    LKOUT C,100,(D,B#SAVED,AC_FNTN),CHAR,ND=Y                              
*&&UK                                                                           
JbAlld   LKOUT C,101,(D,B#GOBLK,GOTNOJOB),CHAR,ND=Y                             
JbNBil   LKOUT C,102,(D,B#GOXBLK,GONJLE),CHAR,ND=Y                              
*&&                                                                             
*&&US                                                                           
JbAlld   LKOUT C,101,(D,B#GOXBLK,GOTNOJOB),CHAR,ND=Y                            
*&&                                                                             
JbBilo   LKOUT C,103,(D,B#GOXBLK,GOBILO),CHAR,ND=Y                              
DrftJbs  LKOUT C,104,(D,B#GOXBLK,GODRFT),CHAR,ND=Y                              
AutoNum  LKOUT C,105,(D,B#GOBLK,GOAUTNUM),CHAR,ND=Y                             
IntEst   LKOUT C,106,(D,B#GOXBLK,GOIAES),CHAR,ND=Y                              
IntDrft  LKOUT C,107,(D,B#GOXBLK,GOIUSO),CHAR,ND=Y                              
DrftEst  LKOUT C,108,(D,B#GOXBLK,GOAEDT),CHAR,ND=Y                              
SupSub   LKOUT C,109,(D,B#GOXBLK,GOSRQ),CHAR,ND=Y                               
ExpSub   LKOUT C,110,(D,B#GOXBLK,GOXRQ),CHAR,ND=Y                               
MdOrWc   LKOUT C,111,(D,B#GOXBLK,GOICRA),LBIN,ND=Y                              
CopUncm  LKOUT C,112,(D,B#GOXBLK,GOACUA),CHAR,ND=Y                              
ExpVisb  LKOUT C,113,(D,B#GOXBLK,GOEVS),CHAR,ND=Y                               
SlfApIn  LKOUT C,114,(D,B#SAVED,AC_SAPIN),CHAR,ND=Y                             
SlfApAr  LKOUT C,115,(D,B#SAVED,AC_SAPAR),CHAR,ND=Y                             
SlfApEx  LKOUT C,116,(D,B#SAVED,AC_SAPEX),CHAR,ND=Y                             
SlfApPr  LKOUT C,117,(D,B#SAVED,AC_SAPPR),CHAR,ND=Y                             
EstInOr  LKOUT C,118,(D,B#SAVED,AC_ESTIN),CHAR,ND=Y                             
EstArOr  LKOUT C,119,(D,B#SAVED,AC_ESTAR),CHAR,ND=Y                             
EstExOr  LKOUT C,120,(D,B#SAVED,AC_ESTEX),CHAR,ND=Y                             
EstPrOr  LKOUT C,121,(D,B#SAVED,AC_ESTPR),CHAR,ND=Y                             
PZeroIn  LKOUT C,122,(D,B#SAVED,AC_ZERIN),CHAR,ND=Y                             
PZeroAr  LKOUT C,123,(D,B#SAVED,AC_ZERAR),CHAR,ND=Y                             
PZeroEx  LKOUT C,124,(D,B#SAVED,AC_ZEREX),CHAR,ND=Y                             
PZeroPr  LKOUT C,125,(D,B#SAVED,AC_ZERPR),CHAR,ND=Y                             
ExpWCRq  LKOUT C,126,(D,B#GOXBLK,GOEWC),CHAR,ND=Y                               
AnalOrd  LKOUT C,127,(D,B#GOXBLK,GOOAN),CHAR,ND=Y                               
AutoInt  LKOUT C,128,(D,B#SAVED,AC_AUTIN),SPAK,ND=Y                             
AutoArt  LKOUT C,129,(D,B#SAVED,AC_AUTAR),SPAK,ND=Y                             
AutoExp  LKOUT C,130,(D,B#SAVED,AC_AUTEX),SPAK,ND=Y                             
AutoPro  LKOUT C,131,(D,B#SAVED,AC_AUTPR),SPAK,ND=Y                             
NAE      LKOUT C,132,(D,B#GOBLK,GONEEDAE),CHAR,ND=Y                             
*&&UK                                                                           
KSVBill  LKOUT C,133,(D,B#GOBLK,GOBILKSV),CHAR,ND=Y                             
*&&                                                                             
GAPyn    LKOUT C,134,(D,B#GOXBLK,GOGAPS),CHAR,ND=Y                              
DefEx    LKOUT C,135,(D,B#GOXBLK,GOGDES),HEXD                                   
DefND    LKOUT C,136,(D,B#GOXBLK,GODNDV),HEXD                                   
GAPEX    LKOUT C,137,(D,B#GOXBLK,GOGEMX),CHAR,ND=Y                              
GAPAR    LKOUT C,138,(D,B#GOXBLK,GOGARA),CHAR,ND=Y                              
WCprnt   LKOUT C,139,(D,B#GOXBLK,GOSWPD),LBIN,ND=Y                              
Bilty    LKOUT C,240,(D,B#GOBLK,GOBILTYP),CHAR,ND=Y                             
GAPED    LKOUT C,241,(D,B#GOXBLK,GOGEMD),CHAR,ND=Y                              
ThrdP    LKOUT C,242,(D,B#SAVED,AC_THRDP),CHAR,ND=Y                             
ExcMem   LKOUT C,243,(D,B#GOXBLK,GOEXM),CHAR,ND=Y                               
ColWC    LKOUT C,244,(D,B#GOXBLK,GOCCW),CHAR,ND=Y                               
WCFlag   LKOUT C,245,(D,B#GOXBLK,GOWCF),CHAR,ND=Y                               
PPORes   LKOUT C,246,(D,B#GOXBLK,GOFPORES),CHAR,ND=Y                            
FPORes   LKOUT C,247,(D,B#GOXBLK,GOPPORES),CHAR,ND=Y                            
MPORes   LKOUT C,248,(D,B#GOXBLK,GOMPORES),CHAR,ND=Y                            
*&&UK                                                                           
Red      LKOUT C,249,(D,B#GOBLK,GOCERED),SPAK,ND=Y                              
Amber    LKOUT C,250,(D,B#GOBLK,GOCEAMBR),SPAK,ND=Y                             
ToBill   LKOUT C,251,(D,B#GOXBLK,GOTBL),LBIN                                    
*&&                                                                             
*&&US                                                                           
Red      LKOUT C,249,(D,B#GOXBLK,GOCERED),SPAK,ND=Y                             
Amber    LKOUT C,250,(D,B#GOXBLK,GOCEAMBR),SPAK,ND=Y                            
         LKOUT P,,GETSTU2                                                       
StuType  LKOUT C,252,(D,B#SAVED,STUTYPE),CHAR,ND=Y                              
StuFlg   LKOUT C,254,(D,B#SAVED,STUFLAG),CHAR,ND=Y                              
*&&                                                                             
MailPAJ  LKOUT C,255,(D,B#GOXBLK,GOEPAJ),CHAR,ND=Y,FILTROUT=TSTRODK             
                                                                                
Array    LKOUT C,140,(A,ARYDESC),FILTROUT=TSTSJ,SKIPCOLS=MGBSKIPS               
MGBSKIP  EQU   *                                                                
Array    LKOUT C,140,(A,ARYDESC1)                                               
Array    LKOUT C,141,(A,ARYCOM)                                                 
Array    LKOUT C,02,(A,ARYUSF),FILTROUT=TSTSJ                                   
Array    LKOUT C,03,(A,ARYTEAM),FILTROUT=TSTRODK                                
Array    LKOUT C,04,(A,ARYCLIPO)                                                
Array    LKOUT C,05,(A,ARYLNKJB)                                                
Array    LKOUT C,142,(A,ARYOTHR)                                                
Offc     LKOUT C,143,(D,B#SAVED,AC_OFF),CHAR,ND=Y                               
OffNam   LKOUT C,144,(D,B#SAVED,AC_OFFN),CHAR,ND=Y                              
BilGrp   LKOUT C,145,(D,B#SAVED,AC_GRUP),CHAR,ND=Y                              
PrnBil   LKOUT C,146,(D,B#SAVED,AC_PRNBL),CHAR,ND=Y                             
DrAcc    LKOUT C,147,(D,B#SAVED,AC_SRULA),CHAR,ND=Y                             
DrNam    LKOUT C,148,(D,B#SAVED,AC_SRULA),(U,#EDTANM,$EDTANM),ND=Y              
DrLvl    LKOUT C,149,(D,B#SAVED,AC_SRLVL),CHAR,ND=Y                             
CsAcc    LKOUT C,150,(D,B#SAVED,AC_1CACT),CHAR,ND=Y                             
CsNam    LKOUT C,151,(D,B#SAVED,AC_1CULA),(U,#EDTANM,$EDTANM),ND=Y              
CsLvl    LKOUT C,152,(D,B#SAVED,AC_1CLVL),CHAR,ND=Y                             
MGBSKIPS EQU   (*-MGBSKIP)/LX_COLSL                                             
Array    LKOUT C,153,(A,ARYSPAEL)                                               
Array    LKOUT C,177,(A,ARYPID)                                                 
Array    LKOUT C,182,(A,ARYAPT)                                                 
Array    LKOUT C,184,(A,ARYBAC)                                                 
Array    LKOUT C,189,(A,ARYABL)                                                 
Array    LKOUT C,192,(A,ARYRAC1)                                                
ActPer   LKOUT C,192,(D,B#SAVED,AC_RPID),(U,#EDTPID,$EDTPID),          +        
               FILTROUT=TSTRCHA,SKIPCOLS=1,ND=Y                                 
ActDt    LKOUT C,193,(D,B#SAVED,AC_RDTE),PDAT,ND=Y                              
                                                                                
Array    LKOUT C,194,(A,ARYSTC),FILTROUT=TSTRODKC                               
Apprver  LKOUT C,202,(D,B#SAVED,AC_APRVR),CHAR,ND=Y                             
CanClse  LKOUT C,203,(D,B#SAVED,AC_CCLSE),CHAR,ND=Y                             
Deletd   LKOUT C,204,(D,B#SAVED,AC_DELT),CHAR,ND=Y                              
Xjob     LKOUT C,205,(D,B#SAVED,AC_XJOB),CHAR,ND=Y                              
CanDelt  LKOUT C,206,(D,B#SAVED,AC_CDELT),CHAR,ND=Y                             
PRout    LKOUT P,,GETEST                                                        
ClApEs   LKOUT C,207,(D,B#SAVED,AC_CAEST),SPAK,ND=Y                             
Array    LKOUT C,208,(A,ARYRAC2)                                                
ClseOff  LKOUT C,212,(D,B#GOBLK,GOAUTADD),LBIN,ND=Y                             
TimeItms LKOUT C,213,(D,B#SAVED,AC_TIMIT),CHAR,ND=Y                             
ExpItms  LKOUT C,214,(D,B#SAVED,AC_EXPIT),CHAR,ND=Y                             
PrdOrd   LKOUT C,215,(D,B#SAVED,AC_PORIT),CHAR,ND=Y                             
ExpOrd   LKOUT C,216,(D,B#SAVED,AC_XORIT),CHAR,ND=Y                             
Estimt   LKOUT C,217,(D,B#SAVED,AC_ESTIT),CHAR,ND=Y                             
Drft     LKOUT C,218,(D,B#SAVED,AC_DRFTX),CHAR,ND=Y                             
CanXp    LKOUT C,219,(D,B#SAVED,AC_CXJOB),CHAR,ND=Y                             
PrAna    LKOUT C,220,(D,B#SAVED,AC_PRANA),CHAR,ND=Y                             
         LKOUT P,,NXTFLV     read 00 RSFREC                                     
Array    LKOUT C,06,(A,ARYFLTN),FILTROUT=TSTPDF,SKIPCOLS=2                      
         LKOUT P,,SETCNT     Reset count                                        
Array    LKOUT C,07,(A,ARYFLVN)                                                 
Array    LKOUT C,08,(A,ARYSLKJB)                                                
Array    LKOUT C,09,(A,ARYFFTEQ)                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYACNM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Actname  LKOUT C,1,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYALNM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(XNMEL,XNMELQ),ROWWIDTH=(V,XNMLN)                          
                                                                                
AltNam   LKOUT C,2,XNMSUBN,CHAR,LEN=V,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
ARYFNAM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(XNMEL,XNMELQ),ROWWIDTH=(V,XNMLN)                          
                                                                                
ForNam   LKOUT C,3,XNMSUBN,CHAR,LEN=V,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
ARYLNGN  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(GPNEL,GPNELQ),ROWWIDTH=(V,GPNLN)                          
                                                                                
PRout    LKOUT P,GPNTYP,SETTTYPE                                                
LngNam   LKOUT C,4,GPNLNAM,CHAR,LEN=V,FILTROUT=TSTGLNG,ND=Y                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYSNM   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SNMEL,SNMELQ),ROWWIDTH=(V,SNMLN)                          
                                                                                
ShrtNam  LKOUT C,5,SNMNAME,CHAR,LEN=V,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
ARYNAME  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,NEWEL=Y,                   +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Prvname  LKOUT C,1,NAMEREC,CHAR,LEN=V,ND=Y                                      
Prvcode  LKOUT C,2,(D,B#SAVED,AC_PRVCD),CHAR,ND=Y                               
SJlvl    LKOUT C,5,(D,B#SAVED,AC_SJLVL),CHAR,ND=Y                               
Array    LKOUT C,15,(A,ARYADR)                                                  
Array    LKOUT C,70,(A,ARYFFTL)                                                 
Array    LKOUT C,91,(A,ARYDEXL)                                                 
                                                                                
         LKOUT E                                                                
* ADREL PROCESSOR                                                               
ARYADR   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(ADREL,ADRELQ),ROWWIDTH=(V,ADRLN)                          
Array    LKOUT C,15,(A,ARYADR1)                                                 
                                                                                
         LKOUT E                                                                
*   ADDRESS LINE ARRAY                                                          
ARYADR1  LKOUT A,(*,ADRADD1),ROWNAME=ADREL,NROWS=*,ROWWIDTH=L'ADRADD1           
Addrss   LKOUT C,15,ADRADD1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYFLT   LKOUT A,(D,B#SAVED,AC_FLT1),NROWS=5,                          +        
               ROWNAME=AC_FLT1,ROWWIDTH=L'AC_FLT1                               
                                                                                
Flter    LKOUT C,16,AC_FLT1,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYGLAC  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(GLPEL,GLPELQ),ROWWIDTH=(V,GLPLN)                          
                                                                                
GLacc    LKOUT C,49,GLPACC1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYOTHEL LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(OTHEL,OTHELQ),ROWWIDTH=(V,OTHLN)                          
                                                                                
VATnum   LKOUT C,61,OTHNUM,CHAR,ND=Y                                            
NICprof  LKOUT C,62,OTHPROF,CHAR,LEN=1,ND=Y                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYFFTL  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN)                          
                                                                                
PRout    LKOUT P,FFTTYPE,SETTTYPE                                               
VATcod13 LKOUT C,70,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFG13B,ND=Y                   
VATcod   LKOUT C,71,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFVATC,ND=Y                   
TaxNum   LKOUT C,72,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFTAXN,ND=Y                   
Email    LKOUT C,73,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPEML,ND=Y                   
CamCod   LKOUT C,74,FFTDATA,LBIN,LEN=4,FILTROUT=TSTFCAMP,SKIPCOLS=1,   +        
               ,ND=Y                                                            
CamNam   LKOUT C,75,FFTDATA,(U,#EDTCMN,$EDTCMN),ND=Y                            
DsExCom  LKOUT C,76,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPCOM,ND=Y                   
Teleph   LKOUT C,77,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPTEL,ND=Y                   
MainCon  LKOUT C,78,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPCON,ND=Y                   
Fax      LKOUT C,79,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPFAX,ND=Y                   
Refer    LKOUT C,80,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFREFN,ND=Y                   
AuthId   LKOUT C,81,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFAUTH,ND=Y                   
Entity   LKOUT C,82,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFBENT,ND=Y                   
BSroll   LKOUT C,83,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFBSRN,ND=Y                   
CredCrd  LKOUT C,84,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFCRCN,ND=Y                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYMEMO  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(OMEEL,OMEELQ),ROWWIDTH=(V,OMELN)                          
                                                                                
OnMemo   LKOUT C,85,OMEMO,CHAR,LEN=V,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYSCIL  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SCIEL,SCIELQ),ROWWIDTH=(V,SCILN)                          
                                                                                
PRout    LKOUT P,SCITYPE,SETTTYPE                                               
CrLim    LKOUT C,86,SCIAMNT,SPAK,FILTROUT=TSTSCLIM,ND=Y                         
ICrLim   LKOUT C,87,SCIAMNT,SPAK,FILTROUT=TSTSICLM,ND=Y                         
Fee      LKOUT C,88,SCIAMNT,SPAK,FILTROUT=TSTSCFEE,ND=Y                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYDSCR  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RATEL,RATEDSCQ),ROWWIDTH=(V,RATLN)                        
                                                                                
DisRat   LKOUT C,89,RATRATE,LBIN,FILTROUT=TSTCR,ND=Y                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYVATR  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RATEL,RATEVATQ),ROWWIDTH=(V,RATLN)                        
                                                                                
VatRat   LKOUT C,90,RATRATE,LBIN,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
                                                                                
ARYDESC  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(JLDEL,JLDELQ),ROWWIDTH=(V,JLDLN)                          
                                                                                
Descrip  LKOUT C,140,JLDDESC,CHAR,LEN=V,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYDESC1 LKOUT A,(R,NXTAEX),NROWS=1,ROWNAME=AEXRECD,NEWEL=N                     
Array    LKOUT C,140,(A,ARYDSC)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYDSC   LKOUT A,(D,B#ACEX,AEXRFST),EOT=EOR,                           +        
               ROWID=(JLDEL,JLDELQ),ROWWIDTH=(V,JLDLN)                          
                                                                                
Descrip  LKOUT C,140,JLDDESC,CHAR,LEN=V,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYCOM   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SCMEL,SCMELQ),ROWWIDTH=(V,SCMLN)                          
*&&UK                                                                           
Comment  LKOUT C,141,SCMNARR,CHAR,LEN=V                                         
*&&                                                                             
*&&US                                                                           
PRout    LKOUT P,SCMEL,SETBILC                                                  
Comment  LKOUT C,141,(D,B#WORKD,TEMP),CHAR,LEN=50,ND=Y                          
*&&                                                                             
         LKOUT E                                                                
                                                                                
ARYUSF   LKOUT A,(D,B#SAVED,USRENT),ROWID=(UFSEL,UFSELQ),              +        
               ROWWIDTH=(V,UFSLN),NEWEL=B,EOT=EOR                               
Code     LKOUT C,1,UFSCODE,CHAR,ND=Y                                            
Data     LKOUT C,2,UFSEL,(R,EDTDATA),ND=Y                                       
Desc     LKOUT C,3,UFSDESC,CHAR,ND=Y                                            
MaxLen   LKOUT C,4,UFSMXLN,LBIN,ND=Y                                            
Edit     LKOUT C,5,UFSEDIT,CHAR,LEN=1,ND=Y                                      
Reqd     LKOUT C,6,UFSSTAT,(R,EDTUFST),ND=Y                                     
         LKOUT E                                                                
                                                                                
ARYTEAM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,NEWEL=B,                   +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,3,(A,ARYTEAM1),FILTROUT=TSTLTEAJ                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYTEAM1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=4,NEWEL=B           
Role     LKOUT C,1,LIDROLN,LBIN,ND=Y                                            
RNam     LKOUT C,2,LIDROLN,(U,#EDTRLN,$EDTRLN),ND=Y                             
PIN      LKOUT C,3,LIDBPID,HEXD,ND=Y                                            
PID      LKOUT C,4,LIDBPID,(U,#EDTPID,$EDTPID),ND=Y                             
SJlvl    LKOUT C,5,(D,B#SAVED,AC_SJLVL),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLIPO LKOUT A,(R,NXTAEX),MULTIROW=Y,ROWNAME=AEXRECD                          
Array    LKOUT C,04,(A,ARYCPO)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYCPO   LKOUT A,(D,B#ACEX,AEXRFST),EOT=EOR,NEWEL=B,                   +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,4,(A,ARYCLPO1),FILTROUT=TSTCLPO                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLPO1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=26,NEWEL=B          
PurOrd   LKOUT C,1,LIDCPO,CHAR,ND=Y                                             
OrdAmt   LKOUT C,2,LIDCPOAM,SPAK,ND=Y                                           
                                                                                
         LKOUT E                                                                
ARYLNKJB LKOUT A,(R,NXTMJB),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
CodCli   LKOUT C,1,(D,B#SAVED,AC_SJULA),(U,#EDTCLI,$EDTCLI),ND=Y                
NamCli   LKOUT C,2,(D,B#SAVED,AC_SJULA),(U,#EDTCLN,$EDTCLN),ND=Y                
CodPrd   LKOUT C,3,(D,B#SAVED,AC_SJULA),(U,#EDTPRD,$EDTPRD),ND=Y                
NamPrd   LKOUT C,4,(D,B#SAVED,AC_SJULA),(U,#EDTPRN,$EDTPRN),ND=Y                
CodJob   LKOUT C,5,(D,B#SAVED,AC_SJULA),(U,#EDTJOB,$EDTJOB),ND=Y                
NamJob   LKOUT C,6,(D,B#SAVED,AC_SJULA),(U,#EDTJBN,$EDTJBN),ND=Y                
                                                                                
         LKOUT E                                                                
*                                                                               
ARYFFTEQ LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,NEWEL=B,                   +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN)                          
*                                                                               
PRout    LKOUT P,FFTTYPE,SETTTYPE                                               
SeqNo    LKOUT C,1,FFTSEQ,HEXD,FILTROUT=TSTFEPTR                                
EquiPtr  LKOUT C,2,FFTDATA,CHAR,ND=Y,LEN=V,FILTROUT=TSTFEPTR                    
*                                                                               
         LKOUT E                                                                
                                                                                
ARYSLKJB LKOUT A,(R,NXTSJB),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
CodCli   LKOUT C,1,(D,B#SAVED,AC_SJULA),(U,#EDTCLI,$EDTCLI),ND=Y                
CodPrd   LKOUT C,2,(D,B#SAVED,AC_SJULA),(U,#EDTPRD,$EDTPRD),ND=Y                
CodJob   LKOUT C,3,(D,B#SAVED,AC_SJULA),(U,#EDTJOB,$EDTJOB),ND=Y                
NamJob   LKOUT C,4,(D,B#SAVED,AC_SJULA),(U,#EDTJBN,$EDTJBN),ND=Y                
StuType  LKOUT C,5,(D,B#SAVED,AC_SJSTT),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYOTHR  LKOUT A,(D,B#SAVED,AC_OTHRI),NROWS=3,                         +        
               ROWNAME=AC_OTHR1,ROWWIDTH=L'AC_OTHR1                             
                                                                                
OInfo    LKOUT C,142,AC_OTHR1,CHAR,ND=Y                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYSPAEL LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SPAEL,SPAELQ),ROWWIDTH=(V,SPALN)                          
                                                                                
PRout    LKOUT P,SPATYPE,SETTTYPE                                               
Charg    LKOUT C,153,SPAAULA,CHAR,FILTROUT=TSTSBCHA,SKIPCOLS=1,ND=Y             
ChrNm    LKOUT C,154,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
ExDif    LKOUT C,155,SPAAULA,CHAR,FILTROUT=TSTSEXDF,SKIPCOLS=1,ND=Y             
ExDNm    LKOUT C,156,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Analy    LKOUT C,157,SPAAULA,CHAR,FILTROUT=TSTSABNK,SKIPCOLS=1,ND=Y             
AnaNm    LKOUT C,158,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Discnt   LKOUT C,159,SPAAULA,CHAR,FILTROUT=TSTSCSHD,SKIPCOLS=1,ND=Y             
DisNm    LKOUT C,160,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
MasCli   LKOUT C,161,SPAAULA,(U,#EDTCLI,$EDTCLI),FILTROUT=TSTSMJOB,    +        
               SKIPCOLS=5,ND=Y                                                  
MCliNm   LKOUT C,162,SPAAULA,(U,#EDTCLN,$EDTCLN),ND=Y                           
MasPro   LKOUT C,163,SPAAULA,(U,#EDTPRD,$EDTPRD),ND=Y                           
MProNm   LKOUT C,164,SPAAULA,(U,#EDTPRN,$EDTPRN),ND=Y                           
MasJob   LKOUT C,165,SPAAULA,(U,#EDTJOB,$EDTJOB),ND=Y                           
MJobNm   LKOUT C,166,SPAAULA,(U,#EDTJBN,$EDTJBN),ND=Y                           
Factor   LKOUT C,167,SPAAULA,CHAR,FILTROUT=TSTSFACC,SKIPCOLS=1,ND=Y             
FacNm    LKOUT C,168,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Person   LKOUT C,169,SPAAULA,CHAR,FILTROUT=TSTSPERS,SKIPCOLS=1,ND=Y             
PsnNm    LKOUT C,170,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
TwoD     LKOUT C,171,SPAAULA,CHAR,FILTROUT=TSTSDEPT,SKIPCOLS=1,ND=Y             
TwoDNm   LKOUT C,172,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
OvInc    LKOUT C,173,SPAAULA,CHAR,FILTROUT=TSTSINCO,SKIPCOLS=1,ND=Y             
OvIncN   LKOUT C,174,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
OvWrt    LKOUT C,175,SPAAULA,CHAR,FILTROUT=TSTSWOFF,SKIPCOLS=1,ND=Y             
OvWrtN   LKOUT C,176,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYPID   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(PIDEL,PIDELQ),ROWWIDTH=(V,PIDLN)                          
                                                                                
PidB     LKOUT C,177,PIDNO,HEXD,ND=Y                                            
PidChr   LKOUT C,178,PIDNO,(U,#EDTPID,$EDTPID),ND=Y                             
PRout    LKOUT P,PIDNO,SETPIDN                                                  
FirstNam LKOUT C,179,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                         
MidNam   LKOUT C,180,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                      
LastNam  LKOUT C,181,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y  Y                      
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPT   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(APTEL,APTELQ),ROWWIDTH=(V,APTLN)                          
                                                                                
PRout    LKOUT P,APTSTAT,SETTTYPE                                               
ExpAc    LKOUT C,182,APTACCU,CHAR,LEN=L'ACTKULA,FILTROUT=TSTAXPNS,     +        
               SKIPCOLS=1,ND=Y                                                  
ExpNm    LKOUT C,183,APTACCU,(U,#EDTANM,$EDTANM),ND=Y                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYBAC   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(BACEL,BACELQ),ROWWIDTH=(V,BACLN)                          
                                                                                
SortCd   LKOUT C,184,BACSORT,CHAR,ND=Y                                          
AccCd    LKOUT C,185,BACCOUNT,CHAR,ND=Y                                         
BnkNm    LKOUT C,186,BACBNAME,CHAR,ND=Y                                         
BnkAcNm  LKOUT C,187,BACACNAM,CHAR,LEN=V,ND=Y                                   
PRout    LKOUT P,BACSTAT,SETPAY                                                 
PayMthd  LKOUT C,188,(D,B#SAVED,AC_PAYMD),CHAR,ND=Y                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYABL   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(ABLEL,ABLELQ),ROWWIDTH=(V,ABLLN)                          
                                                                                
Bbf      LKOUT C,189,ABLFRWD,SPAK,ND=Y                                          
Dr       LKOUT C,190,ABLDR,SPAK,ND=Y                                            
Cr       LKOUT C,191,ABLCR,SPAK,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYRAC1  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RACEL,RACELQ),ROWWIDTH=(V,RACLN)                          
                                                                                
PRout    LKOUT P,RACELD,SETLATST                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYRAC2  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RACEL,RACELQ),ROWWIDTH=(V,RACLN)                          
                                                                                
PRout    LKOUT P,RACTYPE,SETTTYPE                                               
PidChr   LKOUT C,208,RACPERS,(U,#EDTPID,$EDTPID),FILTROUT=TSTRADD,     +        
               SKIPCOLS=4,ND=Y                                                  
PRout    LKOUT P,RACPERS,SETPIDN                                                
FirstNam LKOUT C,209,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                         
MidNam   LKOUT C,210,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                      
LastNam  LKOUT C,211,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTC   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
                                                                                
PRout    LKOUT P,STCIND,SETTTYPE                                                
AprDte   LKOUT C,194,STCDATE,PDAT,FILTROUT=TSTSIJOB,SKIPCOLS=9,ND=Y             
Prout    LKOUT P,STCTIME,SETTIME                                                
AprTim   LKOUT C,195,(D,B#SAVED,AC_TIME),CHAR ,ND=Y                             
AprUsr   LKOUT C,196,STCUSER,(U,#EDTUSR,$EDTUSR),ND=Y                           
AprCom   LKOUT C,197,STCCOMM,CHAR,LEN=V,ND=Y                                    
PidChr   LKOUT C,198,STCPERS,(U,#EDTPID,$EDTPID),ND=Y                           
PRout    LKOUT P,STCPERS,SETPIDN                                                
FirstNam LKOUT C,199,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                         
MidNam   LKOUT C,200,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                      
LastNam  LKOUT C,201,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                         
                                                                                
         LKOUT E                                                                
* 06 array - Filter Number Name                                                 
* loop over filter 1-5                                                          
ARYFLTN  LKOUT A,(R,NXTRSF),NROWS=5,ROWNAME=COUNT,ROWWIDTH=1,NEWEL=B            
                                                                                
Prout    LKOUT P,,SETFLTN                                                       
FltVal   LKOUT C,1,(D,B#WORKD,TEMP2),LEN=1,LBIN                                 
ShrtNm   LKOUT C,2,(D,B#WORKD,TEMP2+1),LEN=L'RSFCODE,CHAR,ND=Y                  
LngNam   LKOUT C,3,(D,B#WORKD,TEMP2+1+L'RSFCODE),LEN=L'RSFLNAM,CHAR             
         LKOUT E                                                                
* 07 array - Filter Value Name                                                  
ARYFLVN  LKOUT A,(R,NXTRSF),NROWS=5,ROWNAME=COUNT,ROWWIDTH=1,NEWEL=B            
                                                                                
Prout    LKOUT P,,NXTFLN                                                        
Prout    LKOUT P,,SETFILT                                                       
Prout    LKOUT P,,SETFLTN                                                       
FltVal   LKOUT C,1,(D,B#WORKD,TEMP2),LEN=1,CHAR,FILTROUT=TSTFLVL,      +        
               SKIPCOLS=3                                                       
ShrtNm   LKOUT C,2,(D,B#WORKD,TEMP2+1),LEN=L'RSFCODE,CHAR,ND=Y                  
LNGNAM   LKOUT C,3,(D,B#WORKD,TEMP2+1+L'RSFCODE),LEN=L'RSFLNAM,CHAR,   +        
               ND=Y                                                             
FiltNum  LKOUT C,4,(D,B#SAVED,MYFLTNUM),LBIN,ND=Y                               
                                                                                
         LKOUT E                                                                
*                                                                               
SETCNT   MVI   COUNT,0                                                          
         J     EXITY                                                            
*                                                                               
* Read for Filter Number names (00 RSFREC)                                      
NXTFLV   MVI   LP_RMODE,LP_RFRST   only called once.                            
         CLI   QPDF,YESQ                                                        
         JE    NXTFLV04            Aura PDF export always wants                 
*&&UK*&& TM    CPYSTAT8,CPYSFNAM                                                
*&&UK*&& JNZ   NXTFLV02                                                         
*                                                                               
*&&UK*&& J     NOMORE                                                           
*                                                                               
NXTFLV02 CLC   QUNIT,SPACES       unit/ledger                                   
         JH    NXTFLV04                                                         
*                                                                               
         MVC   LP_ERROR,=AL2(AE$MISIF)                                          
         J     QERROR                                                           
*                                                                               
                                                                                
NXTFLV04 DS    0H                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,RSFKEYT1,('B#FNVREC',0),         +        
               (0,SAVED),0,0                                                    
         JE    EXITY                                                            
NXTFLV08 L     RF,AIO3                                                          
         MVI   0(RF),0                                                          
         J     EXITY                                                            
*                                                                               
* Set up for reading next Filter Number Name                                    
NXTRSF   DS    0H                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JZ    *+8                                                              
         MVI   COUNT,0                                                          
*                                                                               
         LA    RF,COUNT            DDLINK EXPECTS AN ADDRESS                    
         ST    RF,LP_ADATA                                                      
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
*                                                                               
* Read for Filter Value Names (1-5 RSFRECs)                                     
NXTFLN   CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXTFLN04                                                         
         MVI   COUNT,0                                                          
         CLI   QPDF,YESQ                                                        
         JE    NXTFLN02            Aura PDF export always wants                 
                                                                                
*&&UK*&& TM    CPYSTAT8,CPYSFNAM                                                
*&&UK*&& JNZ   NXTFLN02                                                         
*                                                                               
*&&UK*&& J     NOMORE                                                           
*                                                                               
NXTFLN02 CLC   QUNIT,SPACES       unit/ledger                                   
         JH    NXTFLN04                                                         
                                                                                
         MVC   LP_ERROR,=AL2(AE$MISIF)                                          
         J     QERROR                                                           
                                                                                
                                                                                
NXTFLN04 DS    0H                                                               
         LLC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,MYFLTNUM                                                      
         GOTOR (#NXTREC,ANXTREC),DMCB,RSFKEYT2,('B#FNVREC',0),         +        
               (0,SAVED),0,0                                                    
         JE    EXITY                                                            
         L     RF,AIO3                                                          
         MVI   0(RF),0             prove we didn't find a rec                   
         J     EXITY                                                            
                                                                                
EDTUFST  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),UFSSREQD                                                   
         JZ    EDTUFST2                                                         
         MVI   0(R4),YESQ                                                       
EDTUFST2 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
SETTTYPE L     R1,LP_AINP          Set type of free form element                
         MVC   MYELTYPE,0(R1)                                                   
         J     EXIT                                                             
                                                                                
SETFILT  DS    0H                  Set filter value                             
*        L     R1,LP_AINP          Set filter value                             
*        LLC   RF,0(R1)                                                         
*        STC   RF,MYFLTNUM                                                      
         LLC   RF,COUNT                                                         
         LA    R1,AC_FLT1(RF)                                                   
         MVC   MYFLTVAL,0(R1)                                                   
         J     EXIT                                                             
*                                                                               
SETFLTN  DS    0H                  Set filter name or default                   
         LLC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         L     R4,AIO3                                                          
         CLI   0(R4),0             DID NXTFLV FIND A RECORD?                    
         JE    SFLTN10             NO                                           
         LA    R4,COUNT                                                         
         CLI   MYFLTVAL,0                                                       
         JE    *+8                                                              
         LA    R4,MYFLTVAL                                                      
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSFELQ',AIO3),(1,0(R4))              
         CLI   12(R1),0                                                         
         JNE   SFLTN10                                                          
         USING RSFELD,R1                                                        
         L     R1,12(R1)                                                        
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'RSFTYPE),RSFTYPE                                         
         MVC   TEMP2+L'RSFTYPE(L'RSFCODE),RSFCODE                               
         LLC   RE,RSFLN                                                         
         SHI   RE,RSFLNQ+1                                                      
         BASR  RF,0                                                             
         MVC   TEMP2+L'RSFTYPE+L'RSFCODE(0),RSFLNAM                             
         EX    RE,0(RF)                                                         
         J     EXITY                                                            
* NOT DEFINED, USE DEFAULT                                                      
SFLTN10  DS    0H                                                               
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'MYFLTVAL),MYFLTVAL                                       
         CLI   MYFLTVAL,0                                                       
         JNE   EXITY                                                            
         MVC   TEMP2(1),COUNT                                                   
         MVC   TEMP2+1+L'RSFCODE(L'AC@FLT),AC@FLT                               
         EDIT  COUNT,(1,TEMP2+2+L'RSFCODE+L'AC@FLT)                             
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
SETLATST L     R1,LP_AINP          Set latest change date and time              
         USING RACELD,R1                                                        
         CLI   RACTYPE,RACTCHA     Only interested in changes                   
         JNE   EXIT                                                             
         CLC   RACDATE,AC_RDTE     Only want latest change                      
         JL    EXIT                                                             
         CLC   RACTIME,AC_RTIM                                                  
         JL    EXIT                                                             
         MVC   AC_RPID,RACPERS                                                  
         MVC   AC_RDTE,RACDATE                                                  
         MVC   AC_RTIM,RACTIME                                                  
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
SETTIME  L     R1,LP_AINP          Set type of free form element                
         UNPK  DUB2,0(4,R1)                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   AC_TIME,DUB2+2                                                   
         MVC   WORK(L'FFTTYPE),0(R1)                                            
         J     EXIT                                                             
                                                                                
SETPAY   L     R1,LP_AINP          Set type of free form element                
         CLI   0(R1),BACSTCHQ                                                   
         JNE   SETPAY02                                                         
         MVI   AC_PAYMD,AC_CHQQ                                                 
SETPAY02 CLI   0(R1),BACSTMCH                                                   
         JNE   SETPAY04                                                         
         MVI   AC_PAYMD,AC_MCHQ                                                 
SETPAY04 CLI   0(R1),BACSTPAY                                                   
         JNE   SETPAY06                                                         
         MVI   AC_PAYMD,AC_PAYQ                                                 
SETPAY06 CLI   0(R1),BACSTTRF                                                   
         JNE   SETPAY08                                                         
         MVI   AC_PAYMD,AC_TRFQ                                                 
SETPAY08 CLI   0(R1),BACSTCUR                                                   
         JNE   SETPAY10                                                         
         MVI   AC_PAYMD,AC_INTQ                                                 
SETPAY10 CLI   0(R1),BACSTAFP                                                   
         JNE   SETPAY12                                                         
         MVI   AC_PAYMD,AC_AFPQ                                                 
SETPAY12 CLI   0(R1),BACSTBAC                                                   
         JNE   SETPAY14                                                         
         MVI   AC_PAYMD,AC_BACQ                                                 
SETPAY14 CLI   0(R1),BACSSEPA                                                   
         JNE   EXIT                                                             
         MVI   AC_PAYMD,AC_SEPQ                                                 
         J     EXIT                                                             
                                                                                
TSTFTAXN L     R1,LP_ADATA         Test if ST ledger                            
         USING ACTRECD,R1                                                       
         CLC   =C'ST',ACTKULA                                                   
         BNER  RE                                                               
         CLI   WORK,FFTTAXNO       Test whether type is tax number              
         BR    RE                                                               
                                                                                
TSTRCHA  DS    0H                  Test if PID present                          
         OC    AC_RPID,AC_RPID                                                  
         J     SETCCC                                                           
                                                                                
TSTPDF   CLI   QPDF,YESQ           Test if PDF request                          
         BR    RE                                                               
                                                                                
TSTFLVL  DS    0H                                                               
         CLC   MYFLTVAL,SPACES     Find matching value element                  
         JH    EXITY                                                            
         J     EXITN                                                            
                                                                                
TSTRADD  CLI   MYELTYPE,RACTADD    Test whether type is add                     
         BR    RE                                                               
*&&UK                                                                           
TSTFPEML CLI   MYELTYPE,FFTTPEML   Test whether type is email                   
         BR    RE                                                               
*&&                                                                             
*&&US                                                                           
TSTFPEML CLI   MYELTYPE,FFTTEML    Test whether type is email                   
         BR    RE                                                               
*&&                                                                             
                                                                                
TSTFPCON CLI   MYELTYPE,FFTTPCON   Test whether type is main contact            
         BR    RE                                                               
                                                                                
TSTFREFN CLI   MYELTYPE,FFTTREFN   Test whether type is main contact            
         BR    RE                                                               
                                                                                
TSTFAUTH CLI   MYELTYPE,FFTTAUTH   Test whether type is auth id                 
         BR    RE                                                               
                                                                                
TSTFBENT CLI   MYELTYPE,FFTTBENT   Test whether type is entity                  
         BR    RE                                                               
                                                                                
TSTFCRCN CLI   MYELTYPE,FFTTCRCN   Test whether type is credit card             
         BR    RE                                                               
                                                                                
TSTFBSRN CLI   MYELTYPE,FFTTBSRN   Test whether type is buildg soc roll         
         BR    RE                                                               
                                                                                
TSTFCAMP CLI   MYELTYPE,FFTTCAMP   Test whether type is campaign                
         BR    RE                                                               
                                                                                
TSTFPTEL CLI   MYELTYPE,FFTTPTEL   Test whether type is telephone               
         BR    RE                                                               
                                                                                
TSTFPFAX CLI   MYELTYPE,FFTTPFAX   Test whether type is fax                     
         BR    RE                                                               
                                                                                
TSTFPCOM CLI   MYELTYPE,FFTTPCOM   Test whether type is extra comments          
         BR    RE                                                               
                                                                                
TSTFEPTR DS    0H                                                               
         CLI   MYELTYPE,FFTTEPTR   Test whether type is Equi Acc Pntrs          
         BR    RE                                                               
*                                                                               
TSTFVATC L     R1,LP_ADATA         Test if SG ledger                            
         USING ACTRECD,R1                                                       
*        CLC   =C'SG',ACTKULA                                                   
*        BNER  RE                                                               
         CLI   MYELTYPE,FFTTVATC   Test whether type is vat code                
         BR    RE                                                               
                                                                                
TSTFG13B L     R1,LP_ADATA         Test if SG ledger                            
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         BNER  RE                                                               
         CLI   QAPPL,QAPPINV                                                    
         BNER  RE                                                               
*&&UK*&& CLI   MYELTYPE,FFTTG13B   Test whether type is vat code                
         BR    RE                                                               
                                                                                
TSTGLNG  CLI   MYELTYPE,GPNTLNG    Test whether type is long name               
         BR    RE                                                               
                                                                                
TSTSCLIM CLI   MYELTYPE,SCITCLIM   Test whether type is credit limit            
         BR    RE                                                               
                                                                                
TSTSICLM CLI   MYELTYPE,SCITICLM   Test whether type is int credit lim          
         BR    RE                                                               
                                                                                
TSTSCFEE CLI   MYELTYPE,SCITFEEB   Test whether type is fee                     
         BR    RE                                                               
                                                                                
TSTSBCHA CLI   MYELTYPE,SPATBCHA   Test whether type is bank charges            
         BR    RE                                                               
                                                                                
TSTSEXDF CLI   MYELTYPE,SPATEXDF   Test whether type is exchange diff           
         BR    RE                                                               
                                                                                
TSTSCSHD CLI   MYELTYPE,SPATCSHD   Test whether type is cash discount           
         BR    RE                                                               
                                                                                
TSTSABNK CLI   MYELTYPE,SPATABNK   Test whether type is analysis bank           
         BR    RE                                                               
                                                                                
TSTSFACC CLI   MYELTYPE,SPATFACC   Test whether type is factoring               
         BR    RE                                                               
                                                                                
TSTSPERS CLI   MYELTYPE,SPATPERS   Test whether type is personnel               
         BR    RE                                                               
                                                                                
TSTSDEPT CLI   MYELTYPE,SPATDEPT   Test whether type is department              
         BR    RE                                                               
                                                                                
TSTSINCO CLI   MYELTYPE,SPATINCO   Test whether type is income                  
         BR    RE                                                               
                                                                                
TSTSWOFF CLI   MYELTYPE,SPATWOFF   Test whether type is write off               
         BR    RE                                                               
                                                                                
TSTSMJOB CLI   MYELTYPE,SPATMJOB   Test whether type is master job              
         BR    RE                                                               
                                                                                
TSTAXPNS CLI   MYELTYPE,APTSXPNS   Test whether type is def expense             
         BR    RE                                                               
                                                                                
TSTCLPO  CLI   MYELTYPE,LIDTCPO    Test whether type is client pur ord          
         BR    RE                                                               
                                                                                
TSTLTEAJ CLI   MYELTYPE,LIDTTEAJ   Test whether type is team in jobs            
         BR    RE                                                               
                                                                                
TSTLNKJB CLI   MYELTYPE,LIDTCPJ    Test whether type is client/pro/job          
         BR    RE                                                               
                                                                                
TSTSIJOB CLI   MYELTYPE,STCIJOB    Test whether type is job comments            
         BR    RE                                                               
                                                                                
TSTSJ    CLC   PRODUL,QUNIT         Test if production ledger                   
         BR    RE                                                               
                                                                                
TSTRODKC SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         J     SETCCC                                                           
                                                                                
TSTRODK  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         BR    RE                                                               
                                                                                
TSTJBAPL SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   SETCCC                                                           
         CLI   QAPPL,QAPPJOB       Is it jobs application                       
         J     SETCCC                                                           
                                                                                
TST1RTM  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   SETCCC                                                           
         CLC   =C'1R',QUNIT        For 1R account list skip sending             
         JNE   SETCCC               anything but name code for                  
         CLI   QAPPL,QAPPTIM         time application in Aura                   
         J     SETCCC                                                           
                                                                                
TSTST    L     R1,LP_ADATA         Test if artist ledger                        
         USING ACTRECD,R1                                                       
         CLC   =C'ST',ACTKULA                                                   
         BR    RE                                                               
                                                                                
TSTSG    L     R1,LP_ADATA         Test if VAT ledger                           
         USING ACTRECD,R1                                                       
         CLC   =C'SG',ACTKULA                                                   
         BR    RE                                                               
                                                                                
TSTCR    L     R1,LP_ADATA         Test if creditor ledger                      
         USING ACTRECD,R1                                                       
         CLC   =C'SV',ACTKULA                                                   
         BER   RE                                                               
         CLC   =C'SX',ACTKULA                                                   
         BER   RE                                                               
         CLC   =C'ST',ACTKULA                                                   
         BER   RE                                                               
         CLC   =C'SF',ACTKULA                                                   
         BR    RE                                                               
                                                                                
TSTAIO   L     R1,AIOACC           Test if anything in aioarea                  
         USING ACTRECD,R1                                                       
         OC    ACTKACT,ACTKACT                                                  
         J     SETCCC                                                           
                                                                                
TSTAIOL  L     R1,AIOACC           Test if anything in aioarea                  
         USING ACTRECD,R1          and ensure higher level                      
         OC    ACTKACT,ACTKACT                                                  
         JZ    SETCCC                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   TSTAIOL2                                                         
         CLC   AC_ACLVL,AC_SJLVL                                                
         JH    SETCCC                                                           
         BR    RE                                                               
                                                                                
TSTAIOL2 CLC   AC_ACLVL,AC_SJLVL                                                
         JNL   SETCCC                                                           
         BR    RE                                                               
                                                                                
*                                                                               
TSTSJF   CLI   QFNAME,C'Y'          SKIP UNLESS WE WANT FOREIGN NAMES           
         JNE   SETCCC                                                           
         J     TSTSJCC                                                          
*                                                                               
TSTSJCC  L     R1,LP_ADATA         Test if production ledger                    
         USING ACTRECD,R1                                                       
         CLC   PRODUL,ACTKULA                                                   
         J     SETCCC                                                           
                                                                                
TSTSR    L     R1,LP_ADATA         Test if recievable ledger                    
         USING ACTRECD,R1                                                       
         CLC   =C'SR',ACTKULA                                                   
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
SETLV1   L     R0,AIO2                                                          
         LA    R1,IOLENQ                                                        
         L     RE,AIO4                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   AC_SJLVL,AC_LEVL1                                                
         MVC   AC_PRVCD,SPACES                                                  
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RF,LDGAL1                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   AC_PRVCD(0),ACTKACT                                              
         EX    RF,0(RE)                                                         
         J     EXIT                                                             
                                                                                
SETLV2   L     R0,AIO2                                                          
         LA    R1,IOLENQ                                                        
         L     RE,AIO5                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   AC_SJLVL,AC_LEVL2                                                
         MVC   AC_PRVCD,SPACES                                                  
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RE,LDGAL1                                                        
         LLC   RF,LDGAL2                                                        
         LTR   RF,RF               Do we have level 2 for this ledger           
         JZ    EXITY               No                                           
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ACTKACT(RE)                                                   
         BASR  RE,0                                                             
         MVC   AC_PRVCD(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         J     EXIT                                                             
                                                                                
SETLV3   L     R0,AIO2                                                          
         LA    R1,IOLENQ                                                        
         L     RE,AIO6                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   AC_SJLVL,AC_LEVL3                                                
         MVC   AC_PRVCD,SPACES                                                  
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RE,LDGAL2                                                        
         LLC   RF,LDGAL3                                                        
         LTR   RF,RF                                                            
         JZ    EXITY                                                            
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ACTKACT(RE)                                                   
         BASR  RE,0                                                             
         MVC   AC_PRVCD(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         J     EXIT                                                             
                                                                                
SETLV4   L     R0,AIO2                                                          
         LA    R1,IOLENQ                                                        
         L     RE,AIO7                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   AC_SJLVL,AC_LEVL4                                                
         MVC   AC_PRVCD,SPACES                                                  
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RE,LDGAL3                                                        
         LLC   RF,LDGAL4                                                        
         LTR   RF,RF                                                            
         JZ    EXITY                                                            
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ACTKACT(RE)                                                   
         BASR  RE,0                                                             
         MVC   AC_PRVCD(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         J     EXIT                                                             
         DROP  R2                                                               
RESETLVL L     R0,AIO2                                                          
         LA    R1,IOLENQ                                                        
         L     RE,AIO4                                                          
         CLI   AC_ACLVL,AC_LEVL1                                                
         JE    RESETLV2                                                         
         L     RE,AIO5                                                          
         CLI   AC_ACLVL,AC_LEVL2                                                
         JE    RESETLV2                                                         
         L     RE,AIO6                                                          
         CLI   AC_ACLVL,AC_LEVL3                                                
         JE    RESETLV2                                                         
         L     RE,AIO7                                                          
*                                                                               
RESETLV2 LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   AC_SJLVL,AC_ACLVL   Reset level                                  
         J     EXIT                                                             
                                                                                
                                                                                
SETPIDN  L     R1,LP_AINP          Binary PID                                   
         MVC   TEMP2(L'QPIDB),0(R1)                                             
         GOTOR (#GETPID,AGETPID)                                                
         GOTOR (#GETPIN,AGETPIN)                                                
         J     EXIT                                                             
*&&US                                                                           
         USING SCMELD,R1                                                        
SETBILC  L     R1,LP_AINP          SCMTYPE field                                
         MVC   TEMP,SPACES                                                      
         LA    R2,TEMP                                                          
         LLC   RF,SCMLN            Get length                                   
         SHI   RF,SCMLN1Q                                                       
         LA    RE,SCMCODE                                                       
                                                                                
         TM    SCMTYPE,SCMTPRBI    Print on Bills comment                       
         JZ    SETBILC7            no, regular comment                          
         MVC   0(4,R2),=C'BIL='                                                 
         AHI   R2,4                                                             
         LHI   RF,L'SCMCODE                                                     
                                                                                
SETBILC5 CLI   0(RE),C' '          parse thru right justified comment           
         JH    SETBILC7                                                         
         AHI   RE,1                                                             
         AHI   RF,-1                                                            
         J     SETBILC5                                                         
                                                                                
SETBILC7 AHI   RF,-1               FOR EXECUTED MOVE                            
         BASR  R1,0                                                             
         MVC   0(0,R2),0(RE)                                                    
         EX    RF,0(R1)                                                         
         J     EXIT                                                             
*&&                                                                             
***********************************************************************         
* GET ACCOUNT RECORDS                                                 *         
***********************************************************************         
                                                                                
ACTINI   OC    QULA,SPACES                                                      
         CLC   QUNIT,SPACES        Test unit given                              
         JH    ACTI002                                                          
         MVC   LP_ERROR,=AL2(AE$INUNT)                                          
         J     QERROR                                                           
*                                                                               
ACTI002  CLC   QLEDG,SPACES        Test ledger given                            
         JH    ACTI004                                                          
         MVC   LP_ERROR,=AL2(AE$INLDG)                                          
         J     QERROR                                                           
*                                                                               
ACTI004  XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,QUNIT        Read the ledger                              
         GOTOR (#SETLDG,ASETLDG)                                                
         JE    ACTI006                                                          
         MVC   LP_ERROR,=AL2(AE$INLDG)                                          
         J     QERROR                                                           
*                                                                               
ACTI006  CLC   QACT,SPACES         Test ledger given                            
         JH    ACTI008                                                          
         MVC   LP_ERROR,=AL2(AE$INACC)                                          
         J     QERROR                                                           
*                                                                               
         OC    QACT,SPACES                                                      
         CLI   QMED,C' '                                                        
         JNH   ACTI008                                                          
         LLC   RE,PPROLEN                                                       
         LA    RE,QACT(RE)                                                      
         CLI   0(RE),C' '                                                       
         JNH   ACTI008                                                          
         CLC   QMED,0(RE)                                                       
         JE    ACTI008                                                          
         MVC   LP_ERROR,=AL2(AE$ECSCP)                                          
         J     QERROR                                                           
*                                                                               
ACTI008  MVI   AC#IND1,0           Clear indicator                              
         GOTOR AVALACT             Read account                                 
         JNE   QERROR                                                           
*                                                                               
         CLI   QLIML,YESQ                                                       
         JE    ACTI010                                                          
         GOTOR AADVALL             Check if connected user has access           
         JE    ACTI010                                                          
         MVC   LP_ERROR,=AL2(AE$NOSTJ)                                          
         J     QERROR                                                           
ACTI010  MVC   SJOFFC,AC_OFF       Set office for global work                   
         MVI   AC_APST,AC_APP      Set default as approved                      
         GOTOR ACHKAPP             Check if connected user is approver          
         L     R2,AIOACC                                                        
         L     R0,AIO2             Copy record to IO2                           
         LA    R1,IOLENQ                                                        
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         USING ACTRECD,R2                                                       
         GOTOR AGETOPT             Get option maintain values                   
         GOTOR ACHKACT             Check if job can be closed                   
         CLI   QDRAFT,YESQ         Do we want draft accounts                    
         JE    ACTI012             Yes                                          
         TM    ACTRSTAT,ACTSDRFT   No - check if account is draft               
         JZ    ACTI012             No                                           
         MVC   LP_ERROR,=AL2(AE$ACCNA) Yes - error                              
         J     QERROR                                                           
ACTI012  LA    R4,ACTRFST                                                       
         USING ASTELD,R4                                                        
ACTI014  CLI   ASTEL,0                                                          
         JE    ACTI044                                                          
         CLI   ASTEL,ASTELQ                                                     
         JE    ACTI017                                                          
         CLI   ASTEL,RSTELQ                                                     
         JE    ACTI022                                                          
         CLI   ASTEL,JOBELQ                                                     
         JE    ACTI034                                                          
ACTI016  LLC   R0,ASTLN                                                         
         AR    R4,R0                                                            
         J     ACTI014                                                          
*                                                                               
ACTI017  MVI   AC_LOCL,NOQ                                                      
         TM    ASTSTAT1,ASTSLOCL   Account in local currency                    
         JZ    *+8                                                              
         MVI   AC_LOCL,YESQ                                                     
         CLI   AC_ACLVL,AC_LEVL3   Is it a job                                  
         JNE   ACTI018             No                                           
         MVI   AC_FUSR,NOQ         Don't overwrite option maintain val          
         TM    ASTSTAT1,ASTISFOR   Account is foreign language user             
         JZ    *+8                                                              
         MVI   AC_FUSR,YESQ                                                     
ACTI018  MVC   AC_CUR,ASTCUR                                                    
         CLI   ASTCUR,ASTCANY                                                   
         JNE   ACTI019                                                          
         MVC   AC_CUR,=C'***'                                                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Test Aura connected                          
         JNE   ACTI019             No                                           
         MVC   AC_CUR,=C'*  '      Expects 1 asterisk only                      
ACTI019  DS    0H                                                               
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   ACTI016                                                          
         CLI   AST13VAT,C' '       TEST VAT CODE SET                            
         JNH   *+10                                                             
         MVC   AC_VATC,AST13VAT                                                 
         CLI   ASTKSVTY,0          Test KSV type set                            
         JNH   ACTI016                                                          
         CLC   QINVDTE,SPACES      Test invoice date passed                     
         JNH   ACTI020                                                          
         LA    RF,WORK             Get rate for KSV type                        
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV call                                     
         MVI   CONACTN,CONAGETQ    Get KSV details                              
         MVI   CONILEN,L'ASTKSVTY  Length of code                               
         LA    RE,ASTKSVTY                                                      
         STCM  RE,15,CONIADD       input address                                
         LA    RE,TEMP                                                          
         STCM  RE,15,CONOADD       Output address                               
         MVC   CONCOMF,ACOMFACS    A(comfacs)                                   
         MVC   CONKSVDT,QINVDTE    Effective KSV date                           
         GOTO1 VCONVERT,CONBLK                                                  
         JE    *+14                                                             
ACTI020  MVC   LP_ERROR,=AL2(AE$NOKSV)                                          
         J     QERROR                                                           
         DROP  RF                                                               
         ZAP   DUB1,TEMP(3)                                                     
         CURED (P8,DUB1),(L'AC_KSVR,AC_KSVR),0,ZERO=YES,ALIGN=LEFT              
*&&                                                                             
         J     ACTI016                                                          
*                                                                               
         USING RSTELD,R4                                                        
ACTI022  MVC   AC_SECY,RSTSECY                                                  
         MVC   AC_COSTG,RSTCOSTG   Costing analysis                             
         MVC   AC_GLOFC,RSTOFFC    General ledger office                        
         MVC   AC_BBFDT,RSTBDATE   Balance brought forward date                 
         MVC   AC_TRNDT,RSTTDATE   Last transaction date                        
         MVI   AC_PNL,NOQ                                                       
         MVI   AC_ESTIT,NOQ                                                     
         TM    RSTSTAT3,RSTSLAPL                                                
         JZ    *+8                                                              
         MVI   AC_PNL,YESQ                                                      
         MVI   AC_BAL,NOQ                                                       
         TM    RSTSTAT3,RSTSLABS                                                
         JZ    *+8                                                              
         MVI   AC_BAL,YESQ                                                      
*                                                                               
         MVI   AC_RCR,NOQ                                                       
         TM    RSTSTAT3,RSTSPRCR                                                
         JZ    *+8                                                              
         MVI   AC_RCR,YESQ                                                      
         MVI   AC_RDR,NOQ                                                       
         TM    RSTSTAT3,RSTSPRDR                                                
         JZ    *+8                                                              
         MVI   AC_RDR,YESQ                                                      
         MVI   AC_PROCN,NOQ                                                     
         TM    RSTSTAT3,RSTSPRTS                                                
         JZ    *+8                                                              
         MVI   AC_PROCN,YESQ       Project control 1R accounts                  
*                                                                               
         MVI   AC_STAF,NOQ                                                      
         TM    RSTSTAT1,RSTSGPEI                                                
         JZ    *+8                                                              
         MVI   AC_STAF,YESQ        Staff analysis                               
         MVI   AC_PEXEC,NOQ                                                     
         TM    RSTSTAT1,RSTSPIAE                                                
         JZ    *+8                                                              
         MVI   AC_PEXEC,YESQ       Person is executive                          
         MVI   AC_DEPT,NOQ                                                      
         TM    RSTSTAT1,RSTSEADD                                                
         JZ    *+8                                                              
         MVI   AC_DEPT,YESQ        Department analysis                          
         MVI   AC_LOCK,NOQ                                                      
         TM    RSTSTAT1,RSTSACIL                                                
         JZ    *+8                                                              
         MVI   AC_LOCK,YESQ        Locked account                               
         MVI   AC_CLOS,NOQ                                                      
         TM    RSTSTAT1,RSTSACIC                                                
         JZ    *+8                                                              
         MVI   AC_CLOS,YESQ        Closed account                               
         MVI   AC_INVAT,NOQ                                                     
         TM    RSTSTAT1,RSTSIVAT                                                
         JZ    *+8                                                              
         MVI   AC_INVAT,YESQ       Input VAT                                    
         MVI   AC_PAYLK,NOQ                                                     
         TM    RSTSTAT2,RSTSPLOK                                                
         JZ    *+8                                                              
         MVI   AC_PAYLK,YESQ       Lock from payment                            
         MVI   AC_MILES,NOQ                                                     
         TM    RSTSTAT2,RSTSMILE                                                
         JZ    *+8                                                              
         MVI   AC_MILES,YESQ       Miles analysis required                      
         MVI   AC_CLICS,AC_NONE    None                                         
         TM    RSTSTAT4,RSTSCSTH                                                
         JZ    *+8                                                              
         MVI   AC_CLICS,AC_HOUSE   House client                                 
         TM    RSTSTAT4,RSTSCSTN                                                
         JZ    *+8                                                              
         MVI   AC_CLICS,AC_NEWBS   New business client                          
         TM    RSTSTAT4,RSTSCSTC                                                
         JZ    *+8                                                              
         MVI   AC_CLICS,AC_CLINT   Regular client                               
         MVI   AC_JBANA,NOQ                                                     
         MVI   AC_WCRQD,NOQ                                                     
         TM    RSTSTAT4,RSTSJREA                                                
         JZ    *+12                                                             
         MVI   AC_JBANA,YESQ       Job analysis                                 
         MVI   AC_WCRQD,YESQ       WC required                                  
         MVI   AC_MRGIN,NOQ                                                     
         TM    RSTSTAT4,RSTSMSDR                                                
         JZ    *+8                                                              
         MVI   AC_MRGIN,YESQ       Merge invs with same date and ref            
*                                                                               
         CLI   RSTLN,RSTLN2Q                                                    
         JNH   ACTI016                                                          
         MVC   AC_DEFWC,RSTDFTSK   Default work code                            
                                                                                
         CLI   AC_ACLVL,AC_LEVL3   Is it a job                                  
         JNE   *+8                 No                                           
         MVI   AC_LKTIM,NOQ        Don't overwrite option maintain val          
         MVI   AC_EXCTS,NOQ           unless it's a real job                    
         TM    RSTSTAT5,RSTSNOTS                                                
         JZ    ACTI024                                                          
         MVI   AC_EXCTS,YESQ       Exclude from timesheets - cli/pro/jb         
         CLI   AC_ACLVL,AC_LEVL3   Is it a job                                  
         JNE   ACTI024             No                                           
         SR    RF,RF               Yes - check whether BrandOcean or            
         ICM   RF,3,CUXPNUM          or Aura                                    
         CHI   RF,XPRODIKQ                                                      
         JNE   ACTI024                                                          
         MVI   AC_LKTIM,YESQ       If Aura set lock as well                     
                                                                                
                                                                                
ACTI024  MVI   AC_PRANA,NOQ                                                     
*&&UK*&& TM    RSTSTAT5,RSTSPREA                                                
*&&UK*&& JZ    *+8                                                              
*&&US*&& TM    CPYSTAT5,CPYSEXPP                                                
*&&US*&& JZ    *+8                                                              
         MVI   AC_PRANA,YESQ       Product analysis                             
         MVI   AC_FPT,NOQ                                                       
         TM    RSTSTAT5,RSTSPROD                                                
         JZ    *+8                                                              
         MVI   AC_FPT,YESQ         Force product                                
         MVI   AC_FJT,NOQ                                                       
         TM    RSTSTAT5,RSTSPRJB                                                
         JZ    *+8                                                              
         MVI   AC_FJT,YESQ         Force Job                                    
         MVI   AC_SUNDY,NOQ                                                     
*&&UK                                                                           
         TM    RSTSTAT5,RSTSSUND                                                
         JZ    *+8                                                              
         MVI   AC_SUNDY,YESQ       Sundry creditor                              
*&&                                                                             
         TM    RSTSTAT6,RSTSMCSE   Brandocean estimates on job?                 
         JZ    *+8                                                              
         MVI   AC_ESTIT,YESQ                                                    
                                                                                
         MVI   AC_PROVV,NOQ                                                     
         TM    RSTSTAT5,RSTSPROV                                                
         JZ    *+8                                                              
         MVI   AC_PROVV,YESQ       Provisional vendor                           
         MVI   AC_INVRG,YESQ                                                    
*&&UK                                                                           
         TM    RSTSTAT6,RSTSNINV                                                
         JZ    *+8                                                              
         MVI   AC_INVRG,NOQ        Invoice register off                         
*&&                                                                             
         MVI   AC_DSKPR,NOQ                                                     
         TM    RSTSTAT6,RSTSDTOP                                                
         JZ    *+8                                                              
         MVI   AC_DSKPR,YESQ       Desktop priority                             
                                                                                
         CLI   AC_ACLVL,AC_LEVL3                                                
         JNE   ACTI026                                                          
         MVI   AC_THRDP,NOQ                                                     
         TM    RSTSTAT7,RSTSTHRP                                                
         JZ    *+8                                                              
         MVI   AC_THRDP,YESQ       Third party upload flag                      
                                                                                
ACTI026  MVI   AC_FUTIM,NOQ                                                     
         TM    RSTSTAT7,RSTSFUTM                                                
         JZ    *+12                                                             
         MVI   AC_FUTIM,YESQ       Future entry allowed for timesheets          
         J     ACTI028                                                          
                                                                                
         CLI   QAPPL,QAPPTIM       Are we displaying an acc in time             
         JNE   ACTI028             No                                           
         CLI   QFUTIM,YESQ         Do we want only future time accounts         
         JNE   ACTI028             No                                           
         MVC   LP_ERROR,=AL2(AE$NOFUT) Yes - error                              
         J     QERROR                                                           
                                                                                
ACTI028  CLI   AC_ACLVL,AC_LEVL3                                                
         JNE   ACTI016                                                          
         MVI   AC_LKEST,NOQ                                                     
         TM    RSTLSTAT,RSTLSESQ                                                
         JZ    *+8                                                              
         MVI   AC_LKEST,YESQ       Locked from estimates                        
         MVI   AC_LKORD,NOQ                                                     
         TM    RSTLSTAT,RSTLSORQ                                                
         JZ    *+8                                                              
         MVI   AC_LKORD,YESQ       Locked from orders                           
         MVI   AC_LKBIL,NOQ                                                     
         TM    RSTLSTAT,RSTLSBIQ                                                
         JZ    *+8                                                              
         MVI   AC_LKBIL,YESQ       Locked from billing                          
         SR    RF,RF               Check whether BrandOcean or                  
         ICM   RF,3,CUXPNUM          or Aura                                    
         CHI   RF,XPRODIKQ                                                      
         JNE   ACTI030             BrandOcean                                   
         CLI   AC_EXCTS,YESQ       Exclude from timesheets is on, then          
         JE    ACTI032               for Aura lock is also on                   
ACTI030  MVI   AC_LKTIM,NOQ        If not on check lock status                  
         TM    RSTLSTAT,RSTLSTIQ                                                
         JZ    *+8                                                              
         MVI   AC_LKTIM,YESQ       Locked from time                             
         CHI   RF,XPRODIKQ         If Aura set exclude from timesheets          
         JNE   ACTI032                                                          
         MVI   AC_EXCTS,YESQ                                                    
ACTI032  MVI   AC_LKADJ,NOQ                                                     
         TM    RSTLSTAT,RSTLSADQ                                                
         JZ    *+8                                                              
         MVI   AC_LKADJ,YESQ       Locked from adjustments                      
         MVI   AC_LKEXT,NOQ                                                     
         TM    RSTLSTAT,RSTLSEXQ                                                
         JZ    *+8                                                              
         MVI   AC_LKEXT,YESQ       Locked from external postings                
         J     ACTI016                                                          
*                                                                               
         USING JOBELD,R4                                                        
ACTI034  TM    ACTRSTAT,ACTSDRFT                                                
         JZ    ACTI036                                                          
         MVI   AC_APST,AC_AWAIT    Set awaiting approval                        
         TM    JOBSTA2,JOBSREJ                                                  
         JZ    ACTI036                                                          
         MVI   AC_APST,AC_REJ      Set as rejected                              
ACTI036  MVI   AC_ESTI,NOQ                                                      
         MVI   AC_XJOB,NOQ                                                      
         MVC   AC_CLSDT,JOBCDATE                                                
         MVC   AC_OPNDT,JOBADATE   Default to added date                        
         CLI   JOBLN,JOBLN2Q                                                    
         JL    ACTI038                                                          
         OC    JOBODATE,JOBODATE                                                
         JZ    ACTI038                                                          
         MVC   AC_OPNDT,JOBODATE                                                
ACTI038  MVC   AC_IDNUM,JOBREVNO                                                
         MVI   AC_MSTJB,NOQ                                                     
         TM    JOBSTA2,JOBSMST     Is job marked as master job                  
         JZ    ACTI040                                                          
         MVI   AC_MSTJB,YESQ                                                    
ACTI040  TM    JOBSTA1,JOBSMCSE                                                 
         JZ    ACTI042                                                          
         MVI   AC_ESTI,YESQ                                                     
ACTI042  TM    JOBSTA1,JOBSXJOB    Is it expense job                            
         JZ    ACTI016                                                          
         MVI   AC_XJOB,YESQ                                                     
         J     ACTI016                                                          
*                                                                               
ACTI044  MVI   AC_DELT,NOQ                                                      
         TM    ACTRSTAT,ACTSDELT                                                
         JZ    ACTI046                                                          
         MVI   AC_DELT,YESQ                                                     
ACTI046  MVC   LP_ADATA,AIOACC                                                  
         CLC   PRODUL,QUNIT        Only for SJ                                  
         JNE   EXITY                                                            
         GOTOR GETUSF                                                           
         J     EXITY                                                            
         EJECT                                                                  
         DROP  R4                                                               
                                                                                
ARYDEXL  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(DEXEL,DEXELQ),ROWWIDTH=(V,DEXLN)                          
                                                                                
PRout    LKOUT P,DEXVAL,CONVERT                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get account extension records                                      *          
***********************************************************************         
                                                                                
NXTAEX   GOTOR (#NXTREC,ANXTREC),DMCB,AEXKEYT,('B#ACEX',0),            +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Get sub/linked jobs for master job                                  *         
***********************************************************************         
         SPACE 1                                                                
NXTMJB   ST    R8,LP_ADATA                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JE    *+10                                                             
         MVC   IOKEY,SVMJBKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,MJBKEYT,('B#MJB',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         LA    R2,IOKEY                                                         
         MVC   SVMJBKEY,IOKEY                                                   
         USING MJBPASD,R2                                                       
         MVC   AC_SJUNT(L'PRODUL),PRODUL                                        
         MVC   AC_SJULA+L'PRODUL(L'ACTKACT),MJBPSUBJ                            
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Get Studio linked jobs for Agency job                               *         
***********************************************************************         
         SPACE 1                                                                
NXTSJB   ST    R8,LP_ADATA                                                      
                                                                                
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         CLI   GOSTUDIO,C'Y'       STUDIO JOB?                                  
         JNE   NXTSJB50            NO, AGENCY JOB IS AN ARRAY                   
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   EXITN               1 TIME ARRAY                                 
         GOTOR GETELA,DMCB,('LNKELQ',AIO2)                                      
         JNE   EXITN                                                            
         USING LNKELD,R1                                                        
         MVC   AC_SJUNT(L'PRODUL),PRODUL                                        
         MVC   AC_SJULA+L'PRODUL(L'ACTKACT),LNKAGJB    Studio Job Link          
         J     EXITY                                                            
                                                                                
NXTSJB50 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JE    *+10                                                             
         MVC   IOKEY,SVSJBKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,SJBKEYT,('B#SJB',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         LA    R2,IOKEY                                                         
         MVC   SVSJBKEY,IOKEY                                                   
         USING SACRECD,R2                                                       
         MVC   AC_SJUNT(L'PRODUL),PRODUL                                        
         MVC   AC_SJULA+L'PRODUL(L'ACTKACT),SACKSJB                             
         MVC   AC_SJSTT,SACKSTY                                                 
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
CONVERT  NTR1  ,                                                                
*&&UK                                                                           
         L     RF,LP_AINP                                                       
         LA    R1,WORK                                                          
         USING CONBLKD,R1                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONATRAQ                                                 
         MVI   CONFLD,CONFIDUE     Due date expression                          
         MVI   CONILEN,L'DEXVAL                                                 
         STCM  RF,15,CONIADD                                                    
         LA    RF,AC_DUDT                                                       
         STCM  RF,15,CONOADD                                                    
         MVC   CONCOMF,ACOMFACS                                                 
         GOTO1 VCONVERT            Translated by convert                        
*&&                                                                             
         J     EXITY                                                            
                                                                                
GETOPT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETOPT*'                                                      
                                                                                
         LA    R2,SVSJACT                                                       
         CLC   PRODUL,SVSJACT      Only get options for prod ledger             
         JE    *+14                or for office ledger                         
         CLC   =C'2D',SVSJACT                                                   
         JNE   EXITY                                                            
                                                                                
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOCTRY,CUCTRY                                                    
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
                                                                                
         CLC   =C'2D',SVSJACT                                                   
         JNE   GETOPT02                                                         
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    GETOPT06                                                         
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELOFC(0),L'ACTKUNT+L'ACTKLDG(R2)                              
         EX    RE,0(R1)                                                         
         J     GETOPT06                                                         
                                                                                
GETOPT02 MVC   GOSELCLI,SPACES                                                  
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   GOSELCLI(0),L'ACTKUNT+L'ACTKLDG(R2)                              
                                                                                
         LLC   RE,PCLILEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNH   GETOPT04            NO                                           
         CLC   0(0,R1),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         MVC   GOSELPRO,SPACES                                                  
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   GOSELPRO(0),0(R1)                                                
         LLC   RE,PPROLEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,6                                                             
         JNH   *+8                                                              
         LHI   RF,6                                                             
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNH   GETOPT04            NO                                           
         CLC   0(0,R1),SPACES      DO WE HAVE A JOB CODE                        
         MVC   GOSELJOB,SPACES                                                  
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   GOSELJOB(0),0(R1)                                                
         MVC   GOSELMED,0(R1)                                                   
         CLI   QWORKC,C' '                                                      
         JNH   GETOPT06                                                         
         MVC   GOSELWC,QWORKC                                                   
         MVI   GOANYWC,YESQ                                                     
         J     GETOPT06                                                         
                                                                                
GETOPT04 CLI   QMED,C' '                                                        
         JNH   GETOPT06                                                         
         MVC   GOSELMED,QMED                                                    
                                                                                
GETOPT06 GOTOR VGETOPT,DMCB,GOBLOCKD                                            
*&&UK                                                                           
         MVC   AC_TTALL,GOTTALLW                                                
         MVC   AC_TFNAR,GOTFNARR                                                
         MVC   AC_FUSR,GOJULDEF                                                 
*&&                                                                             
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
*&&US*&& MVC   AC_TTALL,GOTTALLW                                                
*&&US*&& MVC   AC_TFNAR,GOTFNARR                                                
         MVC   AC_FPT,GOFPT                                                     
         MVC   AC_THRDP,GOGJ3D                                                  
         MVC   AC_FJT,GOFJT                                                     
         CLI   QBILL,NOQ           Is it non billable                           
         JNE   GETOPT10            No                                           
         CLI   GOBILO,YESQ         Yes - check job isn't billable only          
         JNE   GETOPT10                                                         
         MVC   LP_ERROR,=AL2(AE$NALNB) not allowed for non billable             
         J     QERROR                                                           
                                                                                
GETOPT10 LA    R2,GOSLA                                                         
         LA    R1,4                                                             
         MVC   AC_SAPIN(4*L'AC_SAPIN),=C'NNNN'                                  
GETOPT12 CLI   0(R2),C'I'          Internal order                               
         JNE   *+8                                                              
         MVI   AC_SAPIN,YESQ                                                    
         CLI   0(R2),C'A'          Artist order                                 
         JNE   *+8                                                              
         MVI   AC_SAPAR,YESQ                                                    
         CLI   0(R2),C'E'          Expense order                                
         JNE   *+8                                                              
         MVI   AC_SAPEX,YESQ                                                    
         CLI   0(R2),C'P'          Production order                             
         JNE   *+8                                                              
         MVI   AC_SAPPR,YESQ                                                    
         LA    R2,1(R2)                                                         
         JCT   R1,GETOPT12                                                      
                                                                                
         LA    R2,GOENC                                                         
         LA    R1,4                                                             
         MVC   AC_ESTIN(4*L'AC_SAPIN),=C'NNNN'                                  
GETOPT14 CLI   0(R2),C'I'          Internal order                               
         JNE   GETOPT16                                                         
         MVI   AC_ESTIN,YESQ                                                    
         CLI   1(R2),COMPQ         Compulsory                                   
         JNE   GETOPT16                                                         
         LA    R2,1(R2)                                                         
         MVI   AC_ESTIN,COMPQ                                                   
GETOPT16 CLI   0(R2),C'A'          Artist order                                 
         JNE   GETOPT18                                                         
         MVI   AC_ESTAR,YESQ                                                    
         CLI   1(R2),COMPQ         Compulsory                                   
         JNE   GETOPT18                                                         
         LA    R2,1(R2)                                                         
         MVI   AC_ESTAR,COMPQ                                                   
GETOPT18 CLI   0(R2),C'E'          Expense order                                
         JNE   GETOPT20                                                         
         MVI   AC_ESTEX,YESQ                                                    
         CLI   1(R2),COMPQ         Compulsory                                   
         JNE   GETOPT20                                                         
         LA    R2,1(R2)                                                         
         MVI   AC_ESTEX,COMPQ                                                   
GETOPT20 CLI   0(R2),C'P'          Production order                             
         JNE   GETOPT22                                                         
         MVI   AC_ESTPR,YESQ                                                    
         CLI   1(R2),COMPQ         Compulsory                                   
         JNE   GETOPT22                                                         
         LA    R2,1(R2)                                                         
         MVI   AC_ESTPR,COMPQ                                                   
GETOPT22 LA    R2,1(R2)                                                         
         JCT   R1,GETOPT14                                                      
                                                                                
         LA    R2,GOPZO                                                         
         LA    R1,4                                                             
         MVC   AC_ZERIN(4*L'AC_ZERIN),=C'NNNN'                                  
GETOPT24 CLI   0(R2),C'I'          Internal order                               
         JNE   *+8                                                              
         MVI   AC_ZERIN,YESQ                                                    
         CLI   0(R2),C'A'          Artist order                                 
         JNE   *+8                                                              
         MVI   AC_ZERAR,YESQ                                                    
         CLI   0(R2),C'E'          Expense order                                
         JNE   *+8                                                              
         MVI   AC_ZEREX,YESQ                                                    
         CLI   0(R2),C'P'          Production order                             
         JNE   *+8                                                              
         MVI   AC_ZERPR,YESQ                                                    
         LA    R2,1(R2)                                                         
         JCT   R1,GETOPT24                                                      
                                                                                
                                                                                
         ZAP   AC_AUTIN,PZERO                                                   
         XR    RF,RF                                                            
         ICM   RF,7,GOIAU                                                       
         JZ    GETOPT2A                                                         
         CVD   RF,DUB                                                           
         AP    AC_AUTIN,DUB                                                     
                                                                                
GETOPT2A ZAP   AC_AUTAR,PZERO                                                   
         XR    RF,RF                                                            
         ICM   RF,7,GOAAU                                                       
         JZ    GETOPT2B                                                         
         CVD   RF,DUB                                                           
         AP    AC_AUTAR,DUB                                                     
                                                                                
GETOPT2B ZAP   AC_AUTEX,PZERO                                                   
         XR    RF,RF                                                            
         ICM   RF,7,GOEAU                                                       
         JZ    GETOPT2C                                                         
         CVD   RF,DUB                                                           
         AP    AC_AUTEX,DUB                                                     
                                                                                
GETOPT2C ZAP   AC_AUTPR,PZERO                                                   
         XR    RF,RF                                                            
         ICM   RF,7,GOPAU                                                       
         JZ    GETOPT2D                                                         
         CVD   RF,DUB                                                           
         AP    AC_AUTPR,DUB                                                     
                                                                                
GETOPT2D DS    0H                                                               
*&&US                                                                           
         CLI   AC_ACLVL,AC_LEVL3                                                
         JE    GETOPT26                                                         
         MVC   AC_XJOB,GOAWOO                                                   
*&&                                                                             
         DROP  R3                                                               
                                                                                
GETOPT26 LA    R3,AC_TTALL                                                      
         LA    R1,3                                                             
         MVC   AC_BILA(L'AC_BILA+L'AC_CHGA+L'AC_NONA),=C'NNN'                   
GETOPT28 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   AC_BILA,YESQ                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   AC_CHGA,YESQ                                                     
         CLI   0(R3),NOQ                                                        
         JNE   *+8                                                              
         MVI   AC_NONA,YESQ                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,GETOPT28                                                      
                                                                                
         LA    R3,AC_TFNAR                                                      
         LA    R1,3                                                             
         MVC   AC_FNTB(L'AC_FNTB+L'AC_FNTR+L'AC_FNTN),=C'NNN'                   
GETOPT30 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   AC_FNTB,YESQ                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   AC_FNTR,YESQ                                                     
         CLI   0(R3),NOQ                                                        
         JNE   *+8                                                              
         MVI   AC_FNTN,YESQ                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,GETOPT30                                                      
                                                                                
         L     R3,AGOBBLCK                                                      
         USING GOBBLKD,R3                                                       
         MVC   AC_LKEST,GOJLDEST                                                
         MVC   AC_LKORD,GOJLDORD                                                
         MVC   AC_LKBIL,GOJLDBIL                                                
         MVC   AC_LKTIM,GOJLDTSI                                                
         MVC   AC_LKADJ,GOJLDADJ                                                
         MVC   AC_LKEXT,GOJLDEXT                                                
GETOPT32 J     EXITY                                                            
                                                                                
GETEST   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETEST*'                                                      
                                                                                
         CLC   PRODUL,SVSJACT      client approved estimate total               
         JNE   EXITY               on SJ only                                   
                                                                                
         ZAP   AC_CAEST,PZERO                                                   
         LA    R2,SVSJACT                                                       
                                                                                
         USING ESTRECD,R3                                                       
         LA    R3,IOKEY            read estimates                               
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,CUXCPY                                                   
         MVC   ESTKCLI,SPACES                                                   
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   ESTKCLI(0),L'ACTKUNT+L'ACTKLDG(R2)                               
                                                                                
         LLC   RE,PCLILEN          set client/product/job                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNH   GETESTX                                                          
         CLC   0(0,R1),SPACES      do we have a product?                        
         MVC   ESTKPRO,SPACES                                                   
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   ESTKPRO(0),0(R1)                                                 
         LLC   RE,PPROLEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,L'ESTKJOB                                                     
         JNH   GETEST02                                                         
         LHI   RF,L'ESTKJOB                                                     
                                                                                
GETEST02 SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNH   GETESTX                                                          
         CLC   0(0,R1),SPACES      do we have a job?                            
         MVC   ESTKJOB,SPACES                                                   
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   ESTKJOB(0),0(R1)                                                 
                                                                                
         MVC   CSVKEY1,ESTKEY                                                   
                                                                                
GETEST04 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   GETESTX                                                          
                                                                                
         CLC   CSVKEY1(ESTKLNO-ESTRECD),ESTKEY                                  
         JNE   GETESTX                                                          
                                                                                
         CLI   ESTKSEQ,0           main record only                             
         JE    GETEST06                                                         
         MVI   ESTKSEQ,FF                                                       
         J     GETEST04                                                         
                                                                                
GETEST06 TM    ESTKSTA1,ESTKCAPP   client approved only                         
         JNZ   GETEST08                                                         
         MVI   ESTKSEQ,FF                                                       
         J     GETEST04                                                         
                                                                                
GETEST08 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    GETEST10                                                         
         DC    H'0'                                                             
                                                                                
         USING EMDELD,R1                                                        
GETEST10 L     R1,AIO1                                                          
         AHI   R1,ESTRFST-ESTRECD                                               
                                                                                
GETEST12 CLI   EMDEL,0                                                          
         JE    GETEST16                                                         
         CLI   EMDEL,EMDELQ                                                     
         JE    GETEST14                                                         
         LLC   R0,EMDLN                                                         
         AR    R1,R0                                                            
         J     GETEST12                                                         
                                                                                
GETEST14 AP    AC_CAEST,EMDAMT     add to total                                 
                                                                                
GETEST16 MVI   ESTKSEQ,FF          go for next                                  
         J     GETEST04                                                         
                                                                                
GETESTX  DS    0H                                                               
         J     EXITY                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
                                                                                
**********************************************************************          
* ACCOUNT LIST/SEARCH                                                *          
**********************************************************************          
*                                                                               
REQACTS  LKREQ H,A#ACTS,OUTACTS,NEXTREQ=REQPERS                                 
*                                                                               
Unit     LKREQ F,1,(D,B#SAVED,QUNIT),CHAR,OLEN=L'QUNIT,                +        
               MAXLEN=L'QUNIT,TEXT=AC#UNIT,COL=*                                
Ldg      LKREQ F,2,(D,B#SAVED,QLEDG),CHAR,OLEN=L'QLEDG,                +        
               MAXLEN=L'QLEDG,TEXT=AC#LGRC,COL=*                                
Act      LKREQ F,3,(I,B#SAVED,QACTIND),CHAR,LIST=F,DEFAULT=NOT,        +        
               SORT=NO,OLEN=L'ACTKACT,TEXT=AC#ACC,COL=*                         
Locked   LKREQ F,4,(D,B#SAVED,QLOCK),CHAR,OLEN=L'QLOCK,                +        
               MAXLEN=L'QLOCK,TEXT=AC#LCKED,COL=*                               
Applic   LKREQ F,5,(D,B#SAVED,QAPPL),CHAR,OLEN=L'QAPPL,                +        
               MAXLEN=L'QAPPL,TEXT=AC#APPLI,COL=*                               
Drft     LKREQ F,6,(D,B#SAVED,QDRAFT),CHAR,OLEN=L'QDRAFT,              +        
               MAXLEN=L'QDRAFT,TEXT=AC#DRAFT,COL=*                              
VAll     LKREQ F,7,(D,B#SAVED,QVWALL),CHAR,OLEN=L'QVWALL,              +        
               MAXLEN=L'QVWALL,TEXT=AC#VWALL,COL=*                              
VATTyp   LKREQ F,8,(D,B#SAVED,QVTYPE),CHAR,OLEN=L'QVTYPE,              +        
               MAXLEN=L'QVTYPE,TEXT=(*,VATTLIT),COL=*                           
Media    LKREQ F,9,(D,B#SAVED,QMED),CHAR,OLEN=L'QMED,                  +        
               MAXLEN=L'QMED,TEXT=AC#MEDC,COL=*                                 
DepNOff  LKREQ F,10,(D,B#SAVED,QDNOFF),CHAR,OLEN=L'QDNOFF,             +        
               MAXLEN=L'QDNOFF,TEXT=AC#NOFF,COL=*                               
ForName  LKREQ F,11,(D,B#SAVED,QFNAME),CHAR,OLEN=L'QFNAME,             +        
               MAXLEN=L'QFNAME,TEXT=AC#FDESC,COL=*                              
Active   LKREQ F,12,(D,B#SAVED,QACTV),CHAR,OLEN=L'QACTV,               +        
               MAXLEN=L'QACTV,TEXT=(*,ACTVLIT),COL=*                            
Finance  LKREQ F,13,(D,B#SAVED,QFIN),CHAR,OLEN=L'QFIN,                 +        
               MAXLEN=L'QFIN,TEXT=AC#FINCE,COL=*                                
Master   LKREQ F,14,(D,B#SAVED,QMASTJ),CHAR,OLEN=L'QMASTJ,             +        
               MAXLEN=L'QMASTJ,TEXT=AC#MJO,COL=*                                
Closed   LKREQ F,15,(D,B#SAVED,QCLOSED),CHAR,OLEN=L'QCLOSED,           +        
               MAXLEN=L'QCLOSED,TEXT=AC#CLO,COL=*                               
Bill     LKREQ F,16,(D,B#SAVED,QBILL),CHAR,OLEN=L'QBILL,               +        
               MAXLEN=L'QBILL,TEXT=AC#BLB,COL=*                                 
FutTim   LKREQ F,17,(D,B#SAVED,QFUTIM),CHAR,OLEN=L'QFUTIM,             +        
               MAXLEN=L'QFUTIM,TEXT=AC#FUTA,COL=*                               
InvDte   LKREQ F,18,(D,B#SAVED,QINVDTE),PDAT,OLEN=L'QINVDTE,           +        
               MAXLEN=8,TEXT=AC#INVDT,COL=*                                     
Hier     LKREQ F,19,(D,B#SAVED,QHIER),CHAR,OLEN=L'QHIER,               +        
               MAXLEN=L'QHIER,TEXT=AC#HIERY,COL=*                               
Office   LKREQ F,20,(D,B#SAVED,QOFFC),CHAR,OLEN=L'QOFFC,               +        
               MAXLEN=L'QOFFC,TEXT=AC#OFFC,COL=*                                
Search   LKREQ F,21,(D,B#SAVED,QSCHTYP),CHAR,OLEN=L'QSCHTYP,           +        
               MAXLEN=L'QSCHTYP,TEXT=AC#SRCH,COL=*                              
*ords    LKREQ F,22,(I,B#SAVED,QWORDIND),CHAR,LIST=F,DEFAULT=NOT,      +        
               SORT=NO,OLEN=10,TEXT=AC#WORD,COL=*                               
WordPre  LKREQ F,22,(I,B#SAVED,QWORDIND),CHAR,TEXT=AC#PREFX,           +        
               LIST=NOSORT,OLEN=1,ARRAY=S                                       
Word     LKREQ F,23,,CHAR,TEXT=AC#WORD,OLEN=L'ACTKACT,ARRAY=E                   
*                                                                               
Dept     LKREQ F,24,(D,B#SAVED,QDEPT),CHAR,OLEN=L'QDEPT,               +        
               MAXLEN=L'QDEPT,TEXT=AC#DPT,COL=*                                 
Workcode LKREQ F,25,(D,B#SAVED,QWORKC),CHAR,OLEN=L'QWORKC,             +        
               MAXLEN=L'QWORKC,TEXT=AC#WC,COL=*                                 
VAll     LKREQ F,26,(D,B#SAVED,QSJALL),CHAR,OLEN=L'QSJALL,             +        
               MAXLEN=L'QSJALL,TEXT=AC#SJALL,COL=*                              
Elip     LKREQ F,27,(D,B#SAVED,QTYVEL),LBIN,OLEN=L'QTYVEL,             +        
               MAXLEN=L'QTYVEL,TEXT=(*,TYVELIT),COL=*                           
*                                                                               
         LKREQ E                                                                
*                                                                               
OUTACTS  LKOUT H                                                                
*    ACCOUNT LIST/SEARCH RETURN VALUES                                          
ACTSRCH  LKOUT R,A#ACTS                                                         
Array    LKOUT C,1,(A,ARYACTS)                                                  
*                                                                               
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
* ACCOUNT LIST/SEARCH PROCESSOR ARRAY                                           
ARYACTS  LKOUT A,(R,NXTACTS),MULTIROW=Y,ROWNAME=ACTRECD                         
*                                                                               
         LKOUT P,ACTKACT,SETACT                                                 
ActCode  LKOUT C,1,(D,B#WORKD,TEMP2),CHAR,LEN=L'ACTKACT,ND=Y                    
Array    LKOUT C,2,(A,ARYLACN)                                                  
Array    LKOUT C,3,(A,ARYLFNM),FILTROUT=TSTSJCC                                 
Array    LKOUT C,4,(A,ARYLANM),FILTROUT=TSTSJ                                   
Array    LKOUT C,5,(A,ARYSNM),FILTROUT=TST1RTM,SKIPCOLS=AL1RSKPS                
AL1RSKP  EQU   *                                                                
Draft    LKOUT C,6,ACTRSTAT,(R,EDTDRFT),ND=Y                                    
         LKOUT P,,GETMED                                                        
IncSusAC LKOUT C,7,(D,B#SAVED,INCSUSAC),CHAR,ND=Y,FILTROUT=TSTSJ                
*        LKOUT P,,GETBAG       *** getbag not fully tested ***                  
*uyAgy   LKOUT C,8,(D,B#WORKD,TEMP2),LBIN,ND=Y,LEN=L'CLIDBAGY          +        
               FILTROUT=TSTSJ                                                   
Array    LKOUT C,9,(A,ARYJOB),FILTROUT=TSTSJ                                    
Array    LKOUT C,11,(A,ARYLOAT)                                                 
Array    LKOUT C,10,(A,ARYLADR),FILTROUT=TSTOAT                                 
Array    LKOUT C,12,(A,ARYRST)                                                  
Array    LKOUT C,34,(A,ARYGLP),FILTROUT=TSTASJ,SKIPCOLS=ALSJSKPS                
ALSJSKP  EQU   *                                                                
CurrSJ   LKOUT C,35,(D,B#GOBBLK,GOBILCUR),CHAR,ND=Y,FILTROUT=TSTSJ              
Array    LKOUT C,35,(A,ARYAST)                                                  
Array    LKOUT C,40,(A,ARYFFT)                                                  
Array    LKOUT C,44,(A,ARYSCI),FILTROUT=TSTVND,SKIPCOLS=ALVNSKPS                
ALVNSKP  EQU   *                                                                
ALSJSKPS EQU   (*-ALSJSKP)/LX_COLSL                                             
Array    LKOUT C,46,(A,ARYVRAT)                                                 
Array    LKOUT C,47,(A,ARYSDSC),FILTROUT=TSTASJ,SKIPCOLS=ALSJSKP2               
ALSJSK2  EQU   *                                                                
Array    LKOUT C,48,(A,ARYDUED)                                                 
*  getopt                                                                       
         LKOUT P,,SETTTALL                                                      
BillAll  LKOUT C,49,(D,B#WORKD,TEMP2),CHAR,LEN=1,ND=Y                           
CharAll  LKOUT C,50,(D,B#WORKD,TEMP2+1),CHAR,LEN=1,ND=Y                         
NonNAll  LKOUT C,51,(D,B#WORKD,TEMP2+2),CHAR,LEN=1,ND=Y                         
OptECE   LKOUT C,125,(D,B#WORKD,TEMP2+3),CHAR,LEN=1,ND=Y                        
DefToTm  LKOUT C,52,(D,B#GOXBLK,GOTOT),CHAR,ND=Y                                
         LKOUT P,,SETTFNAR                                                      
BillNarr LKOUT C,53,(D,B#WORKD,TEMP2),CHAR,LEN=1,ND=Y                           
CharNarr LKOUT C,54,(D,B#WORKD,TEMP2+1),CHAR,LEN=1,ND=Y                         
NonNNarr LKOUT C,55,(D,B#WORKD,TEMP2+2),CHAR,LEN=1,ND=Y                         
*&&UK                                                                           
JobAllT  LKOUT C,56,(D,B#GOBLK,GOTNOJOB),CHAR,ND=Y                              
*&&                                                                             
*&&US                                                                           
JobAllT  LKOUT C,56,(D,B#GOXBLK,GOTNOJOB),CHAR,ND=Y                             
*&&                                                                             
JobAll   LKOUT C,57,(D,B#GOXBLK,GONJLE),CHAR,ND=Y                               
BillOnly LKOUT C,58,(D,B#GOXBLK,GOBILO),CHAR,ND=Y                               
MWCSus   LKOUT C,59,(D,B#GOXBLK,GOICRA),LBIN,ND=Y                               
AppEst   LKOUT C,60,(D,B#GOBLK,GONEEDAE),CHAR,ND=Y                              
*&&UK                                                                           
BillKSV  LKOUT C,61,(D,B#GOBLK,GOBILKSV),CHAR,ND=Y                              
*&&                                                                             
EstGAP   LKOUT C,62,(D,B#GOXBLK,GOGAPS),CHAR,ND=Y                               
DefExpir LKOUT C,63,(D,B#GOXBLK,GOGDES),LBIN,ND=Y                               
DefView  LKOUT C,64,(D,B#GOXBLK,GODNDV),LBIN,ND=Y                               
GAPEExt  LKOUT C,65,(D,B#GOXBLK,GOGEMX),CHAR,ND=Y                               
GAPAppR  LKOUT C,66,(D,B#GOXBLK,GOGARA),CHAR,ND=Y                               
BilType  LKOUT C,67,(D,B#GOBLK,GOBILTYP),CHAR,ND=Y                              
SuppWC   LKOUT C,68,(D,B#GOXBLK,GOSWPD),LBIN,ND=Y                               
ExMemo   LKOUT C,116,(D,B#GOXBLK,GOEXM),CHAR,ND=Y                               
CCWc     LKOUT C,117,(D,B#GOXBLK,GOCCW),CHAR,ND=Y                               
WCFlag   LKOUT C,118,(D,B#GOXBLK,GOWCF),CHAR,ND=Y                               
PORes    LKOUT C,119,(D,B#GOXBLK,GOPPORES),CHAR,ND=Y                            
FORes    LKOUT C,120,(D,B#GOXBLK,GOFPORES),CHAR,ND=Y                            
MORes    LKOUT C,121,(D,B#GOXBLK,GOMPORES),CHAR,ND=Y                            
*&&US                                                                           
USKof    LKOUT C,127,(D,B#GOXBLK,GOSJOF),CHAR,ND=Y                              
*&&                                                                             
ALSJSKP2 EQU   (*-ALSJSK2)/LX_COLSL                                             
Array    LKOUT C,69,(A,ARYPPR)                                                  
Array    LKOUT C,72,(A,ARYSPA)                                                  
Array    LKOUT C,85,(A,ARYLPID),FILTROUT=TSTJBAPL,SKIPCOLS=ALJBSKPS             
ALJBSKP  EQU   *                                                                
Array    LKOUT C,86,(A,ARYLAPT)                                                 
Array    LKOUT C,88,(A,ARYLBAC)                                                 
Array    LKOUT C,93,(A,ARYLABL)                                                 
*       READ ALL RACELS FOR LAST UPDATE INFO, THEN EDIT                         
Array    LKOUT C,96,(A,ARYRAC1)                                                 
ActPer   LKOUT C,96,(D,B#SAVED,AC_RPID),(U,#EDTPID,$EDTPID),           +        
               FILTROUT=TSTRCHA,SKIPCOLS=1,ND=Y                                 
ActDt    LKOUT C,97,(D,B#SAVED,AC_RDTE),PDAT,ND=Y                               
*       'ADDED RACEL' NOW                                                       
Array    LKOUT C,98,(A,ARYRACA)                                                 
Array    LKOUT C,99,(A,ARYLSTC),FILTROUT=TSTRODKC                               
ALJBSKPS EQU   (*-ALJBSKP)/LX_COLSL                                             
AL1RSKPS EQU   (*-AL1RSKP)/LX_COLSL                                             
ALVNSKPS EQU   (*-ALVNSKP)/LX_COLSL                                             
Prout    LKOUT P,ACTKACT,SETLVL                                                 
ACLevel  LKOUT C,107,(D,B#SAVED,ACLEVEL),CHAR,LEN=1,ND=Y                        
Array    LKOUT C,108,(A,ARYDESC2),FILTROUT=TSTSJ,SKIPCOLS=AJBSKPS               
AJBSKP   EQU   *                                                                
Array    LKOUT C,109,(A,ARYACLT),FILTROUT=TSTISJOB                              
AJBSKPS  EQU   (*-AJBSKP)/LX_COLSL                                              
Array    LKOUT C,112,(A,ARYACLK),FILTROUT=TSTISJOB                              
Unit     LKOUT C,122,ACTKUNT,CHAR,ND=Y                                          
Ledger   LKOUT C,123,ACTKLDG,CHAR,ND=Y                                          
*&&US                                                                           
         LKOUT P,,GETSTU                                                        
StuFlg   LKOUT C,128,(D,B#SAVED,STUFLAG),CHAR,ND=Y                              
StuType  LKOUT C,129,(D,B#SAVED,STUTYPE),CHAR,ND=Y                              
*&&                                                                             
Array    LKOUT C,130,(A,ARYLOC)                                                 
Edtlf    LKOUT C,131,(D,B#GOXBLK,GOEEDT),CHAR,ND=Y                              
Array    LKOUT C,1,(A,ARYPLEV),FILTROUT=TST1FLD                                 
         LKOUT E                                                                
*                                                                               
ARYLOC   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(EMPEL,EMPELQ),ROWWIDTH=(V,EMPLN)                          
                                                                                
Term1r   LKOUT C,130,EMPCSTAT,(R,EDTTERM),ND=Y                                  
         LKOUT E                                                                
*                                                                               
ARYLACN  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Actname  LKOUT C,2,NAMEREC,CHAR,LEN=V,FILTROUT=TSTBYTE1                         
         LKOUT P,,SETBYTE1                                                      
                                                                                
         LKOUT E                                                                
*                                                                               
*  FOREIGN NAME                                                                 
ARYLFNM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(XNMEL,XNMELQ),ROWWIDTH=(V,XNMLN)                          
                                                                                
ForNam   LKOUT C,3,XNMSUBN,CHAR,LEN=V,ND=Y,FILTROUT=TSTFNAM                     
                                                                                
         LKOUT E                                                                
*                                                                               
*  ALTERNATIVE NAME                                                             
ARYLANM  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(XNMEL,XNMELQ),ROWWIDTH=(V,XNMLN)                          
                                                                                
AltNam   LKOUT C,4,XNMSUBN,CHAR,LEN=V,ND=Y,FILTROUT=TSTNGER                     
                                                                                
         LKOUT E                                                                
*                                                                               
* ACCOUNT PREVIOUS LEVELS PROCESSOR ARRAY                                       
ARYPLEV  LKOUT A,(R,NXTPACT),MULTIROW=Y,ROWNAME=ACTRECD                         
*                                                                               
PrevName LKOUT C,1,(A,ARYPACN)                                                  
PrevCode LKOUT C,2,ACTKACT,CHAR                                                 
Array    LKOUT C,3,(A,ARYPADR)                                                  
Array    LKOUT C,69,(A,ARYPPPR)                                                 
Prout    LKOUT P,ACTKACT,SETLVL                                                 
ACLevel  LKOUT C,107,(D,B#SAVED,ACLEVEL),CHAR,LEN=1,ND=Y                        
PrevAc   LKOUT C,124,(D,B#SAVED,PREVAC2),CHAR                                   
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYPACN  LKOUT A,(D,B#PRACT,ACTRFST),EOT=EOR,                          +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
*                                                                               
Actname  LKOUT C,1,NAMEREC,CHAR,LEN=V                                           
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYPADR  LKOUT A,(D,B#PRACT,ACTRFST),EOT=EOR,                          +        
               ROWID=(ADREL,ADRELQ),ROWWIDTH=(V,ADRLN)                          
Array    LKOUT C,3,(A,ARYPADR1)                                                 
*                                                                               
         LKOUT E                                                                
*   PREVIOUS ADDRESS LINE ARRAY                                                 
ARYPADR1 LKOUT A,(*,ADRADD1),ROWNAME=ADREL,NROWS=*,ROWWIDTH=L'ADRADD1           
Addrss   LKOUT C,3,ADRADD1,CHAR,ND=Y                                            
                                                                                
         LKOUT E                                                                
*                                                                               
ARYPPPR  LKOUT A,(D,B#PRACT,ACTRFST),EOT=EOR,ROWID=(PPREL,PPRELQ),     +        
               ROWWIDTH=(V,PPRLN)                                               
         LKOUT P,PPREL,SETOFFC                                                  
OfficeC  LKOUT C,69,(D,B#SAVED,AC_OFF),CHAR,ND=Y                                
OfficeN  LKOUT C,70,(D,B#SAVED,AC_OFFN),CHAR,ND=Y                               
BillGrp  LKOUT C,71,PPRGRUP,CHAR,ND=Y                                           
         LKOUT E                                                                
*                                                                               
* OATEL PROCESSOR                                                               
ARYLOAT  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(OATEL,OATELQ),      +        
               ROWWIDTH=(V,OATLN)                                               
         LKOUT P,OATEL,SETOATT                                                  
Addrss1  LKOUT C,11,OATLINE1,CHAR,ND=Y,FILTROUT=TSTOATT,SKIPCOLS=3              
Addrss2  LKOUT C,11,OATLINE2,CHAR,ND=Y                                          
AddCsz   LKOUT C,11,(D,B#WORKD,WORK),CHAR,LEN=50,ND=Y                           
         LKOUT P,,SETOAT                                                        
                                                                                
         LKOUT E                                                                
*                                                                               
* ADREL PROCESSOR                                                               
ARYLADR  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(ADREL,ADRELQ),ROWWIDTH=(V,ADRLN)                          
Array    LKOUT C,11,(A,ARYLADR1)                                                
                                                                                
         LKOUT E                                                                
*   ADDRESS LINE ARRAY                                                          
ARYLADR1 LKOUT A,(*,ADRADD1),ROWNAME=ADREL,NROWS=*,ROWWIDTH=L'ADRADD1           
Addrss   LKOUT C,11,ADRADD1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYJOB   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(JOBELD,JOBELQ),     +        
               ROWWIDTH=(V,JOBLN)                                               
JobAppS  LKOUT C,9,JOBSTA2,(R,EDTJAPP),ND=Y                                     
JobMast  LKOUT C,10,JOBSTA2,(R,EDTJMST),ND=Y                                    
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYRST   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(RSTELD,RSTELQ),     +        
               ROWWIDTH=(V,RSTLN)                                               
         LKOUT P,RSTELD,SETFLT                                                  
         LKOUT P,RSTEL,SETOFFC                                                  
Array    LKOUT C,12,(A,ARYLFLT)                                                 
Locked   LKOUT C,13,RSTSTAT1,(R,EDTRSTL),ND=Y                                   
Closed   LKOUT C,14,RSTSTAT1,(R,EDTRSTC),ND=Y                                   
DeptAna  LKOUT C,15,RSTSTAT1,(R,EDTRDPT),ND=Y,FILTROUT=TSTSE                    
MilesAna LKOUT C,16,RSTSTAT2,(R,EDTRMIL),ND=Y,FILTROUT=TSTSE                    
*&&UK                                                                           
CliAna   LKOUT C,17,RSTCOSTG,CHAR,ND=Y,FILTROUT=TSTCLA                          
*&&                                                                             
*&&US                                                                           
PRout    LKOUT P,RSTELD,SETCSTB                                                 
CliAna   LKOUT C,17,(D,B#WORKD,BYTE3),CHAR,LEN=1,ND=Y                           
*&&                                                                             
StaffAna LKOUT C,18,RSTSTAT1,(R,EDTRSTF),ND=Y,FILTROUT=TSTSE                    
BalSheet LKOUT C,19,RSTSTAT3,(R,EDTBSHE),ND=Y                                   
ProfLoss LKOUT C,20,RSTSTAT3,(R,EDTPNL),ND=Y                                    
JobAna   LKOUT C,21,RSTSTAT4,(R,EDTJANA),ND=Y,FILTROUT=TSTSE                    
BBFDte   LKOUT C,22,RSTBDATE,PDAT,ND=Y                                          
SecLvl   LKOUT C,23,RSTSECY,LBIN,ND=Y                                           
SundCred LKOUT C,24,RSTSTAT5,(R,EDTSCRD),ND=Y,FILTROUT=TSTLCR                   
GLOff    LKOUT C,25,RSTOFFC,CHAR,ND=Y                                           
PeelCR   LKOUT C,26,RSTSTAT3,(R,EDTPCR),ND=Y                                    
PeelDR   LKOUT C,27,RSTSTAT3,(R,EDTPDR),ND=Y                                    
LockBil  LKOUT C,28,RSTLSTAT,(R,EDTLBIL),ND=Y                                   
1NFut    LKOUT C,29,RSTSTAT7,(R,EDT1NFT),ND=Y                                   
*                                                                               
         LKOUT P,RSTELD,GETGAP                                                  
UseGAP   LKOUT C,30,(D,B#WORKD,TEMP2),CHAR,LEN=1,ND=Y                           
AckQue   LKOUT C,31,(D,B#WORKD,TEMP2+1),CHAR,LEN=1,ND=Y                         
*                                                                               
FProdTm  LKOUT C,32,RSTSTAT5,(R,EDTFPRO),ND=Y                                   
FJob     LKOUT C,33,RSTSTAT5,(R,EDTFJOB),ND=Y                                   
TPrdExp  LKOUT C,126,RSTSTAT5,(R,EDTFPRE),ND=Y                                  
         LKOUT E                                                                
*                                                                               
ARYLFLT  LKOUT A,(D,B#SAVED,AC_FLT1),NROWS=5,                          +        
               ROWNAME=AC_FLT1,ROWWIDTH=L'AC_FLT1                               
                                                                                
Flter    LKOUT C,12,AC_FLT1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYGLP   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(GLPELD,GLPELQ),     +        
               ROWWIDTH=(V,GLPLN)                                               
*                                                                               
GLAcc    LKOUT C,34,GLPACC1,CHAR,ND=Y                                           
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYAST   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(ASTELD,ASTELQ),     +        
               ROWWIDTH=(V,ASTLN)                                               
*                                                                               
PROUT    LKOUT P,,SETCUR                                                        
Curr     LKOUT C,35,(D,B#SAVED,AC_CUR),CHAR,FILTROUT=TSTSJCC,ND=Y               
Local    LKOUT C,36,ASTSTAT1,(R,EDTASTLC),ND=Y                                  
FLang    LKOUT C,37,ASTSTAT1,(R,EDTASTFL),ND=Y                                  
*&&UK                                                                           
VAT13B   LKOUT C,38,AST13VAT,CHAR,ND=Y                                          
KSVRate  LKOUT C,39,ASTKSVTY,(R,EDTKSVTY),ND=Y                                  
*&&                                                                             
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYFFT   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN)                          
                                                                                
PRout    LKOUT P,FFTTYPE,SETTTYPE                                               
TaxNum   LKOUT C,40,FFTDATA,CHAR,LEN=V,FILTROUT=TSTLTAXN,ND=Y                   
BSroll   LKOUT C,41,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFBSRN,ND=Y                   
VATcod13 LKOUT C,42,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFG13B,ND=Y                   
VATcod   LKOUT C,42,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFVATC,ND=Y                   
Email    LKOUT C,43,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPEML,ND=Y                   
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYSCI   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SCIEL,SCIELQ),ROWWIDTH=(V,SCILN)                          
*                                                                               
PRout    LKOUT P,SCITYPE,SETTTYPE                                               
CrLim    LKOUT C,44,SCIAMNT,SPAK,FILTROUT=TSTSCLIM,ND=Y                         
ICrLim   LKOUT C,45,SCIAMNT,SPAK,FILTROUT=TSTSICLM,ND=Y                         
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYVRAT  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RATEL,RATEVATQ),ROWWIDTH=(V,RATLN)                        
*                                                                               
VatRat   LKOUT C,46,RATRATE,LBIN,ND=Y                                           
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYSDSC  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RATEL,RATEDSCQ),ROWWIDTH=(V,RATLN)                        
                                                                                
DisRat   LKOUT C,47,RATRATE,LBIN,FILTROUT=TSTLCR,ND=Y                           
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYDUED  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(DEXEL,DEXELQ),ROWWIDTH=(V,DEXLN)                          
                                                                                
PRout    LKOUT P,DEXVAL,CONVERT                                                 
DueDat   LKOUT C,48,(D,B#SAVED,AC_DUDT),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYPPR   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,ROWID=(PPREL,PPRELQ),      +        
               ROWWIDTH=(V,PPRLN)                                               
         LKOUT P,PPREL,SETOFFC                                                  
OfficeC  LKOUT C,69,(D,B#SAVED,AC_OFF),CHAR,ND=Y                                
OfficeN  LKOUT C,70,(D,B#SAVED,AC_OFFN),CHAR,ND=Y                               
BillGrp  LKOUT C,71,PPRGRUP,CHAR,ND=Y                                           
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYSPA   LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(SPAEL,SPAELQ),ROWWIDTH=(V,SPALN)                          
*                                                                               
PRout    LKOUT P,SPATYPE,SETTTYPE                                               
         LKOUT P,,SETNOSEQ                                                      
TwoD     LKOUT C,072,SPAAULA,CHAR,FILTROUT=TSTSDEPT,SKIPCOLS=1,ND=Y             
TwoDNm   LKOUT C,073,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
TE2PAcc  LKOUT C,074,SPAAULA,CHAR,FILTROUT=TSTSPERS,SKIPCOLS=1,ND=Y             
TE2PAccN LKOUT C,075,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Charg    LKOUT C,076,SPAAULA,CHAR,FILTROUT=TSTSBCHA,SKIPCOLS=1,ND=Y             
ChrNm    LKOUT C,077,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
ExDif    LKOUT C,078,SPAAULA,CHAR,FILTROUT=TSTSEXDF,SKIPCOLS=1,ND=Y             
ExDNm    LKOUT C,079,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Analy    LKOUT C,080,SPAAULA,CHAR,FILTROUT=TSTSABNK,SKIPCOLS=1,ND=Y             
AnaNm    LKOUT C,081,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
Discnt   LKOUT C,082,SPAAULA,CHAR,FILTROUT=TSTSCSHD,SKIPCOLS=1,ND=Y             
DisNm    LKOUT C,083,SPAAULA,(U,#EDTANM,$EDTANM),ND=Y                           
MasJob   LKOUT C,084,SPAAULA,(U,#EDTJOB,$EDTJOB),FILTROUT=TSTSMJOB,ND=Y         
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYLPID  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(PIDEL,PIDELQ),ROWWIDTH=(V,PIDLN)                          
*                                                                               
         LKOUT P,,SETNOSEQ                                                      
*PidB     LKOUT C,217,PIDNO,HEXD,LEN=V,ND=Y                                     
PidChr   LKOUT C,085,PIDNO,(U,#EDTPID,$EDTPID),ND=Y                             
*PRout    LKOUT P,PIDNO,SETPIDN                                                 
*FirstNam LKOUT C,219,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                        
*MidNam   LKOUT C,220,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                     
*LastNam  LKOUT C,221,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y  Y                     
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYLAPT  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(APTEL,APTELQ),ROWWIDTH=(V,APTLN)                          
                                                                                
PRout    LKOUT P,APTSTAT,SETTTYPE                                               
ExpAc    LKOUT C,086,APTACCU,CHAR,LEN=L'ACTKULA,FILTROUT=TSTAXPNS,     +        
               SKIPCOLS=1,ND=Y                                                  
ExpNm    LKOUT C,087,APTACCU,(U,#EDTANM,$EDTANM),ND=Y                           
         LKOUT P,,SETNOSEQ                                                      
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYLBAC  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(BACEL,BACELQ),ROWWIDTH=(V,BACLN)                          
*                                                                               
PRout    LKOUT P,BACSTAT,SETPAY                                                 
PayMthd  LKOUT C,088,(D,B#SAVED,AC_PAYMD),CHAR,ND=Y                             
SortCd   LKOUT C,089,BACSORT,CHAR,ND=Y                                          
AccCd    LKOUT C,090,BACCOUNT,CHAR,ND=Y                                         
BnkAcNm  LKOUT C,091,BACACNAM,CHAR,LEN=V,ND=Y                                   
BnkNm    LKOUT C,092,BACBNAME,CHAR,ND=Y                                         
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYLABL  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(ABLEL,ABLELQ),ROWWIDTH=(V,ABLLN)                          
*                                                                               
Bbf      LKOUT C,093,ABLFRWD,SPAK,ND=Y                                          
Dr       LKOUT C,094,ABLDR,SPAK,ND=Y                                            
Cr       LKOUT C,095,ABLCR,SPAK,ND=Y                                            
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYRACA  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RACEL,RACELQ),ROWWIDTH=(V,RACLN)                          
*                                                                               
PRout    LKOUT P,RACTYPE,SETTTYPE                                               
AddPID   LKOUT C,098,RACPERS,(U,#EDTPID,$EDTPID),FILTROUT=TSTRADD,     +        
               SKIPCOLS=1,ND=Y                                                  
AddDt    LKOUT C,099,RACDATE,PDAT,ND=Y                                          
*PRout    LKOUT P,RACPERS,SETPIDN                                               
*FirstNam LKOUT C,249,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                        
*MidNam   LKOUT C,250,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                     
*LastNam  LKOUT C,251,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                        
*                                                                               
         LKOUT E                                                                
*                                                                               
*                                                                               
ARYLSTC  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
*                                                                               
PRout    LKOUT P,STCIND,SETTTYPE                                                
AprDte   LKOUT C,100,STCDATE,PDAT,FILTROUT=TSTSIJOB,SKIPCOLS=8,ND=Y             
Prout    LKOUT P,STCTIME,SETTIME                                                
AprTim   LKOUT C,101,(D,B#SAVED,AC_TIME),CHAR,ND=Y                              
PidChr   LKOUT C,102,STCPERS,(U,#EDTPID,$EDTPID),ND=Y                           
PRout    LKOUT P,STCPERS,SETPIDN                                                
FirstNam LKOUT C,103,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                         
MidNam   LKOUT C,104,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                      
LastNam  LKOUT C,105,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                         
AprCom   LKOUT C,106,STCCOMM,CHAR,LEN=V,ND=Y                                    
*                                                                               
         LKOUT E                                                                
*                                                                               
ARYDESC2 LKOUT A,(R,NXTAEX),NROWS=1,ROWNAME=AEXRECD,NEWEL=N                     
Array    LKOUT C,108,(A,ARYDSC2)                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYDSC2  LKOUT A,(D,B#ACEX,AEXRFST),EOT=EOR,                           +        
               ROWID=(JLDEL,JLDELQ),ROWWIDTH=(V,JLDLN)                          
                                                                                
Descrip  LKOUT C,108,JLDDESC,CHAR,LEN=V,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYACLT  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(JOBEL,JOBELQ),ROWWIDTH=(V,JOBLN)                          
*                                                                               
JOpDte   LKOUT C,109,JOBODATE,PDAT,ND=Y                                         
JClDte   LKOUT C,110,JOBCDATE,PDAT,ND=Y                                         
ExpJob   LKOUT C,111,JOBSTA1,(R,EDTAXJOB)                                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYACLK  LKOUT A,(D,B#ACNT,ACTRFST),EOT=EOR,                           +        
               ROWID=(RSTEL,RSTELQ),ROWWIDTH=(V,RSTLN)                          
                                                                                
JLkTim   LKOUT C,112,RSTLSTAT,(R,EDTLKTIM)                                      
JLkEst   LKOUT C,113,RSTLSTAT,(R,EDTLKEST)                                      
JExTim   LKOUT C,114,RSTSTAT5,(R,EDTEXTIM)                                      
ThrdPty  LKOUT C,115,RSTSTAT7,(R,EDTTHRPY)                                      
                                                                                
         LKOUT E                                                                
                                                                                
*                                                                               
*                                                                               
*        LKOUT P,,DOOMED                                                        
DOOMED   DC    H'0'                                                             
         DC    C'DOOMED'                                                        
*                                                                               
TSTSE    DS    0H                                                               
         CLC   =C'SE',QUNIT                                                     
         BR    RE                                                               
*                                                                               
                                                                                
TSTCLA   CLC   =C'SE',QUNIT       Only these ledgers have client                
         BER   RE                                           analysis            
         CLC   =C'SI',QUNIT                                                     
         BER   RE                                                               
         CLC   =C'SQ',QUNIT                                                     
         BR    RE                                                               
*                                                                               
                                                                                
TSTLCR   CLC   =C'SV',QUNIT       Test for creditor ledgers                     
         BER   RE                                                               
         CLC   =C'SX',QUNIT                                                     
         BER   RE                                                               
         CLC   =C'ST',QUNIT                                                     
         BER   RE                                                               
         CLC   =C'SF',QUNIT                                                     
         BR    RE                                                               
*                                                                               
TST1FLD  DS    0H                  FILTER OUT LEDGERS WHERE WEB APP             
*                                   DOESN'T USE FIELD PER HIER LVL              
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether AURA                           
         CHI   RF,XPRODIKQ          is connected                                
         BNER  RE                                                               
         CLC   =C'SJ',QUNIT        Only return for SJ                           
         BR    RE                                                               
*                                                                               
TSTLTAXN DS    0H                  Test if ST ledger                            
*        CLC   =C'ST',QUNIT                                                     
*        BNER  RE                                                               
         CLI   MYELTYPE,FFTTAXNO   Test whether type is tax number              
         BR    RE                                                               
*                                                                               
TSTFNAM  DS    0H                  TEST 'FOREIGN NAME' OPTION SET               
         CLI   QFNAME,C'Y'                                                      
         BR    RE                                                               
*                                                                               
TSTNGER  DS    0H                                                               
         CLI   CUCTRY,CTRYGER                                                   
         J     SETCCC                                                           
*                                                                               
TSTDRFT  DS    0H                                                               
         L     R1,LP_AINP                                                       
         TM    ACTRSTAT-ACTRECD(R1),ACTSDRFT                                    
         J     SETCCC              RETURN 'OK' IF ON                            
*                                                                               
TSTOATT  DS    0H                                                               
         CLI   DOATTYP,OATSUB5Q    ORDER DELIVERY ADDRESS                       
         BR    RE                                                               
*                                                                               
TSTOAT   DS    0H                  SKIP IF HAVE OATEL FOR ACCOUNT               
         L     R1,LP_AINP                                                       
         CLC   DOATACT,ACTKACT-ACTRECD(R1)                                      
         J     SETCCC                                                           
*                                                                               
TSTISJOB CLC   QUNIT(2),=C'1N'     FOR 1N RETURN JOB LOCKED FROM T/S            
         JNE   TSTISJO2                                                         
         BR    RE                                                               
TSTISJO2 CLC   QUNIT(2),=C'SJ'     CHECK SJ JOB LEVEL                           
         BNER  RE                                                               
         CLI   ACLEVEL,C'3'                                                     
         BR    RE                                                               
*                                                                               
TSTASJ   CLI   QAPPL,QAPPJOB       If jobs module skip a lot of output          
         J     SETCCC                                                           
                                                                                
TSTVND   SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether AURA                           
         CHI   RF,XPRODIKQ          is connected                                
         JE    *+10                                                             
         CLI   QAPPL,QAPPINV      Or application invoices                       
         BNER  RE                                                               
         CLC   =C'SV',QUNIT       Test for creditor ledgers                     
         JE    SETCCC                                                           
         CLC   =C'SX',QUNIT                                                     
         JE    SETCCC                                                           
*&&US                                                                           
         CLC   =C'SY',QUNIT       Test for US creditor ledgers                  
         JE    SETCCC                                                           
         CLC   =C'SW',QUNIT                                                     
*&&                                                                             
         J     SETCCC                                                           
                                                                                
TSTVND1  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM       Check whether AURA                            
         CHI   RF,XPRODIKQ          is connected                                
         JNE   SETCCC                                                           
         CLI   QAPPL,QAPPORD      Are we in orders?                             
         JNE   SETCCC                                                           
         CLI   QLOCK,C'Y'         Include locked or only locked                 
         BER   RE                  means we need to show locked status          
         CLI   QLOCK,C'O'                                                       
         BER   RE                                                               
         CLC   =C'SV',QUNIT       Test for creditor ledgers                     
         JE    SETCCC                                                           
         CLC   =C'SX',QUNIT                                                     
         JE    SETCCC                                                           
*&&US                                                                           
         CLC   =C'SY',QUNIT       Test for US creditor ledgers                  
         JE    SETCCC                                                           
         CLC   =C'SW',QUNIT                                                     
*&&                                                                             
         J     SETCCC                                                           
                                                                                
*                                                                               
* edit routines                                                                 
*                                                                               
EDTJMST  LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),JOBSMST                                                    
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTJAPP  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),C'3'          DEFAULT IS APPROVED                          
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         L     R1,AIO2                                                          
         TM    ACTRSTAT-ACTRECD(R1),ACTSDRFT                                    
         JZ    EXITY                                                            
         MVI   0(R4),C'1'          Set awaiting approval                        
         TM    0(R2),JOBSREJ                                                    
         JZ    EXITY                                                            
         MVI   0(R4),C'2'          REJECTED                                     
         J     EXITY                                                            
*                                                                               
EDTDRFT  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),C'N'          Draft is no                                  
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         L     R1,AIO2                                                          
         TM    ACTRSTAT-ACTRECD(R1),ACTSDRFT                                    
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'          Draft is yes                                 
         J     EXITY                                                            
*                                                                               
EDTRSTL  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSACIL                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTRSTC  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSACIC                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTRDPT  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSEADD                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTRMIL  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSMILE                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTRSTF  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSGPEI                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTBSHE  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSLABS                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTPNL   DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSLAPL                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTJANA  DS    0H                  ALSO FOR W/C REQ                             
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSJREA                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTSCRD  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
*&&US*&& MVI   0(R4),C'N'                                                       
*&&UK                                                                           
         TM    0(R2),RSTSSUND                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
*&&                                                                             
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTPCR   DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSPRCR                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTPDR   DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSPRDR                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTLBIL  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTLSBIQ                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDT1NFT  DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),RSTSFUTM                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTASTLC DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),ASTSLOCL                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTASTFL DS    0H                                                               
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         TM    0(R2),ASTISFOR                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTKSVTY DS    0H                                                               
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   EXITY                                                            
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         CLI   0(R2),0             Test KSV type set                            
         JNH   EXITY                                                            
         CLC   QINVDTE,SPACES      Test invoice date passed                     
         JNH   EXITY                                                            
*                                                                               
         ZAP   DUB,PZERO                                                        
         LA    RF,WORK             Get rate for KSV type                        
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV call                                     
         MVI   CONACTN,CONAGETQ    Get KSV details                              
         MVI   CONILEN,L'ASTKSVTY  Length of code                               
         STCM  R2,15,CONIADD       input address                                
         LA    RE,TEMP2                                                         
         STCM  RE,15,CONOADD       Output address                               
         MVC   CONCOMF,ACOMFACS    A(comfacs)                                   
         MVC   CONKSVDT,QINVDTE    Effective KSV date                           
         GOTO1 VCONVERT,CONBLK                                                  
         JNE   EXITY                                                            
         DROP  RF                                                               
         ZAP   DUB1,TEMP2(3)                                                    
         CURED (P8,DUB1),(4,(R4)),0,ZERO=YES,ALIGN=LEFT                         
         LHI   R3,4                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTFPRO  L     R1,AGOXBLCK                                                      
         USING GOXBLKD,R1                                                       
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
*                                                                               
         CLC   PRODUL,QUNIT                                                     
         JE    EDTFPRO2                                                         
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTSPROD                                                   
         JZ    EDTFPRO4                                                         
         MVI   0(R4),YESQ                                                       
         J     EDTFPRO4                                                         
*                                                                               
EDTFPRO2 MVC   0(1,R4),GOFPT       SJ USE GETOPT                                
*                                                                               
EDTFPRO4 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
EDTFPRE  LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
*&&UK*&& TM    0(R2),RSTSPREA      product forced for exp analysis?             
*&&UK*&& JZ    *+8                                                              
*&&US*&& TM    CPYSTAT5,CPYSEXPP                                                
*&&US*&& JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTFJOB  L     R1,AGOXBLCK                                                      
         USING GOXBLKD,R1                                                       
         LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
*                                                                               
         CLC   PRODUL,QUNIT                                                     
         JE    EDTFJOB2                                                         
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTSPRJB                                                   
         JZ    EDTFJOB4                                                         
         MVI   0(R4),YESQ                                                       
         J     EDTFJOB4                                                         
*                                                                               
EDTFJOB2 MVC   0(1,R4),GOFJT       SJ USE GETOPT                                
*                                                                               
EDTFJOB4 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
EDTAXJOB LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
         TM    0(R2),JOBSXJOB      Expense job?                                 
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTLKTIM LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTLSTIQ      Locked from Time?                            
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTLKEST LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTLSESQ      Locked from Estimates?                       
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTEXTIM LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTSNOTS      Excluded from Time?                          
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTTHRPY LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         MVI   0(R4),NOQ                                                        
         TM    0(R2),RSTSTHRP      Third party upload required                  
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
         USING UFSELD,R2                                                        
DIS      USING UFSELD,R3                                                        
EDTDATA  LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         L     R3,AIO2             Check if user field exists on acc            
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XC    LP_OLEN,LP_OLEN                                                  
*                                                                               
EDTDAT02 CLI   DIS.UFSEL,0         Locate user field                            
         JE    EXITY                                                            
         CLI   DIS.UFSEL,UFSELQ                                                 
         JE    EDTDAT06                                                         
EDTDAT04 LLC   R0,DIS.UFSLN                                                     
         AR    R3,R0                                                            
         J     EDTDAT02                                                         
*                                                                               
EDTDAT06 CLC   DIS.UFSCODE,UFSCODE Match user fields                            
         JNE   EDTDAT04                                                         
         CLC   DIS.UFSDESC,UFSDESC                                              
         JNE   EDTDAT04                                                         
         CLC   DIS.UFSEDIT,UFSEDIT                                              
         JNE   EDTDAT04                                                         
         LLC   RF,DIS.UFSLN                                                     
         SHI   RF,UFSLN1Q+1                                                     
         LTR   RF,RF               Any value?                                   
         JM    EXITY                                                            
         BASR  RE,0                                                             
         MVC   0(0,R4),DIS.UFSDATA Output data                                  
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STCM  RF,15,LP_OLEN                                                    
         J     EXITY                                                            
*                                                                               
EDTTERM  LM    R2,R4,LP_AINP       A(INPUT,?,OUTPUT)                            
         LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         MVI   0(R4),NOQ                                                        
         CLI   0(R2),EMPCTRM       User is terminated?                          
         JNE   EXITY                                                            
         MVI   0(R4),YESQ                                                       
         J     EXITY                                                            
*                                                                               
SETNOSEQ DS    0H                                                               
         OI    RUNFLAGS,RFNOSEQ                                                 
         J     EXITY                                                            
*                                                                               
         USING ASTELD,R1                                                        
SETCUR   DS    0H                                                               
         L     R1,LP_AINP                                                       
         CLI   ASTCUR,ASTCANY      All currencies?                              
         JNE   *+14                                                             
         MVC   AC_CUR,=C'*  '                                                   
         J     EXITY                                                            
         MVC   AC_CUR,ASTCUR                                                    
         J     EXITY                                                            
*                                                                               
SETFLT   DS    0H                                                               
         L     R1,LP_AINP                                                       
         USING RSTELD,R1                                                        
         MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
SETOFFC  DS    0H                                                               
         MVC   AC_OFF,SPACES                                                    
         MVC   AC_OFFN,SPACES                                                   
         L     R1,LP_AINP                                                       
         USING RSTELD,R1                                                        
         CLI   RSTEL,RSTELQ                                                     
         JNE   SOFFC050                                                         
         CLI   LDGAOP,LDGOKEY                                                   
         JH    SOFFC040                                                         
         CLI   LDGAOP,0                                                         
         JE    EXITY                                                            
         MVC   AC_OFF,SPACES                                                    
         L     R1,AIO2                                                          
         LLC   RE,LDGAOP                                                        
         SHI   RE,1                                                             
         LA    RF,ACTKACT-ACTRECD(R1)                                           
         AR    RE,RF               A(OFFICE CODE)                               
         SR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    SOFFC020                                                         
         AHI   RF,1                L'OFFICE CODE                                
SOFFC020 BASR  R2,0                                                             
         MVC   AC_OFF(0),0(RE)                                                  
         EX    RF,0(R2)                                                         
         J     SOFFC080                                                         
*                                                                               
SOFFC040 DS    0H                                                               
         CLI   LDGAOP,LDGOFLT1     OFFICE IN FILTER 1-5                         
         JL    EXITY                                                            
         MVC   AC_OFF,SPACES                                                    
         PACK  DUB,LDGAOP          Office in filters                            
         CVB   R3,DUB              Value is of form X'F1'-X'F4'                 
         MVC   TEMP2(1),RSTFILT1   FILTERS SCATTERED AROUND                     
         MVC   TEMP2+1(1),RSTFILT2                                              
         MVC   TEMP2+2(1),RSTFILT3                                              
         MVC   TEMP2+3(1),RSTFILT4                                              
         MVC   TEMP2+4(1),RSTFILT5                                              
         LA    R2,TEMP2-1(R1)                                                   
         AR    R3,R2               A(OFFICE FILTER)                             
         CLI   0(R1),X'40'                                                      
         JNH   VALAC12                                                          
         MVC   AC_OFF(1),0(R1)                                                  
         J     SOFFC080                                                         
*                                                                               
         USING PPREL,R1                                                         
SOFFC050 CLC   PPRGAOFF,SPACES                                                  
         JNH   EXITY                                                            
         MVC   AC_OFF,PPRGAOFF                                                  
*                                                                               
SOFFC080 MVC   TEMP2,SPACES        GET OFFICE NAME                              
         MVC   TEMP2(L'AC_OFF),AC_OFF                                           
         OI    RUNFLAGS,RFNOSEQ                                                 
         GOTOR (#GETOFN,AGETOFN)                                                
         MVC   AC_OFFN,TEMP2                                                    
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
SETBYTE1 MVI   BYTE1,X'FF'                                                      
         J     EXITY                                                            
                                                                                
*&&US                                                                           
SETCSTB  L     R1,LP_AINP                                                       
         USING RSTELD,R1                                                        
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         MVI   BYTE3,0                                                          
         CLC   =C'SI',ACTKUNT                                                   
         JNE   *+14                                                             
         MVC   BYTE3,RSTCOSTG                                                   
         J     EXITY                                                            
         CLC   =C'SE',ACTKUNT                                                   
         JNE   EXITY                                                            
         USING CATD,R1                                                          
         LA    R1,CATBLK           Get cost setting from CATCALL                
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR                                                 
         MVC   CATSEAC(1),CUXCPY   Company code                                 
         MVC   CATSEAC+1(L'ACTKULA),ACTKULA SE account from above               
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'Y'         Do they require cost postings                
         JNE   EXITY                                                            
         MVI   BYTE3,C'Y'                                                       
         J     EXITY                                                            
         DROP  R1,R2                                                            
*&&                                                                             
*                                                                               
SETACT   L     R1,LP_AINP                                                       
         MVC   TEMP2(L'ACTKACT),0(R1)                                           
         XC    BYTE1,BYTE1                                                      
         MVC   QACT,0(R1)                                                       
         CLC   QUL,=C'1R'                                                       
         JE    SETACT20                                                         
         CLC   QUL,=C'2P'                                                       
         JE    SETACT04                                                         
         CLC   QUL,=C'2D'                                                       
         JNE   EXITY                                                            
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether InvoiceLog                     
         CHI   RF,XPPCUDFQ          is connected                                
         JE    SETACT02            Yes                                          
         OC    DDPTPOS,DDPTPOS                                                  
         JZ    SETACT02                                                         
         MVC   TEMP2(L'ACTKACT),SPACES                                          
         LLC   RE,DDPTPOS                                                       
         SHI   RE,1                                                             
         AR    R1,RE            A(DEPT CODE)                                    
         LLC   RF,DDPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP2(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
                                                                                
SETACT02 CLI   LDGAOP,1            Is office in position 1                      
         JNE   EXITY                                                            
         CLC   QOFFC,SPACES                                                     
         JNH   EXITY                                                            
         MVC   TEMP2(L'ACTKACT),SPACES                                          
         LLC   RE,LDGAOP                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         AR    R1,RE            A(DEPT CODE)                                    
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP2(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
                                                                                
SETACT04 CLI   LDGAOP,1            Is office in position 1                      
         JNE   SETACT10                                                         
         CLC   QOFFC,SPACES                                                     
         JNH   EXITY                                                            
         MVC   TEMP2(L'ACTKACT),SPACES                                          
         LLC   RE,LDGAOP                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         OC    DDPTPOS,DDPTPOS                                                  
         JZ    SETACT06                                                         
         CLC   QDEPT,SPACES                                                     
         JNH   SETACT06                                                         
         LLC   RF,DDPTPOS                                                       
         SHI   RF,1                                                             
         CR    RF,RE             Does dept start where office ends              
         JNE   SETACT06                                                         
         LLC   RF,DDPTLEN                                                       
         AR    RE,RF             Displacement to staff code                     
SETACT06 AR    R1,RE             R1=A(staff code)                               
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP2(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
                                                                                
SETACT10 OC    DDPTPOS,DDPTPOS                                                  
         JZ    EXITY                                                            
         CLI   DDPTPOS,1                                                        
         JNE   EXITY                                                            
         CLC   QDEPT,SPACES                                                     
         JNH   EXITY                                                            
         MVC   TEMP2(L'ACTKACT),SPACES                                          
         LLC   RE,DDPTPOS                                                       
         SHI   RE,1                                                             
         LLC   RF,DDPTLEN                                                       
         AR    RE,RF            RE=Displacement to staff code                   
         AR    R1,RE            R1=A(staff code)                                
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE            RF=length of staff                              
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP2(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
                                                                                
                                                                                
SETACT20 CLI   QHIER,C'4'          Person level only send out per code          
         JNE   EXITY                                                            
         MVC   TEMP2,SPACES        Clear temp2                                  
         LA    RF,L'ACTKACT        RF=total account length                      
         LLC   RE,LDGAL3           RE=sub-dept account length                   
         LA    R1,0(RE,R1)         R1=A(person code)                            
         SR    RF,RE               RF=length of person code                     
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP2(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
*                                                                               
SETLVL   DS    0H                                                               
         L     R1,LP_AINP                                                       
         MVC   TEMP2(L'ACTKACT),0(R1)                                           
         LA    RE,L'ACTKACT                                                     
         LA    RF,TEMP2+L'ACTKACT-1                                             
SETLVL02 CLI   0(RF),C' '                                                       
         JH    *+14                                                             
         BCTR  RF,0                                                             
         JCT   RE,SETLVL02                                                      
         DC    H'0'                                                             
*                                                                               
         XR    R2,R2                                                            
         MVI   ACLEVEL,C'1'                                                     
         IC    R2,LDGAL1                                                        
         CR    RE,R2                                                            
         JE    EXIT                                                             
         JH    *+12                                                             
         MVI   ACLEVEL,C'1'                                                     
         J     EXIT                                                             
                                                                                
         MVI   ACLEVEL,C'2'                                                     
         IC    R2,LDGAL2                                                        
         CR    RE,R2                                                            
         JNH   EXIT                                                             
                                                                                
         MVI   ACLEVEL,C'3'                                                     
         IC    R2,LDGAL3                                                        
         CR    RE,R2                                                            
         JNH   EXIT                                                             
                                                                                
         MVI   ACLEVEL,C'4'                                                     
         J     EXIT                                                             
*                                                                               
                                                                                
         USING OATELD,R1                                                        
SETOATT  L     R1,LP_AINP                                                       
         MVC   DOATTYP,OATSUB                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'OATCSZZR),OATCSZZR       Set up default output            
         TM    OATSTAT,OATCSZ                                                   
         JNO   EXITY                                                            
         MVC   WORK,SPACES                                                      
         LA    RE,WORK                                                          
         MVC   0(L'OATCITY,RE),OATCITY                                          
         LA    RE,L'OATCITY-1(RE)                                               
         CLI   0(RE),X'40'         Are we at a significant character?           
         JH    *+12                                                             
         AHI   RE,-1                                                            
         J     *-12                                                             
         MVI   1(RE),C','          put in comma                                 
         LA    RE,3(RE)            Bump passed comma and add space              
         MVC   0(L'OATSTATE,RE),OATSTATE                                        
         LA    RE,L'OATSTATE(RE)                                                
         MVC   1(L'OATZIP,RE),OATZIP                                            
         CLC   OATZIPRN,SPACES     Any zip extension?                           
         JNH   EXITY                                                            
         LA    RE,L'OATZIP(RE)                                                  
         MVI   1(RE),C'-'                                                       
         MVC   2(L'OATZIPRN,RE),OATZIPRN                                        
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
SETOAT   DS    0H                  SET OATEL FOUND, FOR TSTOAT                  
         XC    DOATACT,DOATACT                                                  
         CLI   DOATTYP,OATSUB5Q                                                 
         JNE   EXITY                                                            
         L     R1,AIO2                                                          
         MVC   DOATACT,ACTKACT-ACTRECD(R1)                                      
         J     EXITY                                                            
*                                                                               
SETTTALL DS    0H                                                               
*&&UK                                                                           
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
*&&                                                                             
*&&US                                                                           
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
*&&                                                                             
         MVC   TEMP2(3),=C'NNN'                                                 
         LA    R1,GOTTALLW                                                      
         LA    R2,3                                                             
STTALL05 DS    0H                                                               
         CLI   0(R1),C'B'                                                       
         JNE   *+8                                                              
         MVI   TEMP2,YESQ                                                       
         CLI   0(R1),C'R'                                                       
         JNE   *+8                                                              
         MVI   TEMP2+1,YESQ                                                     
         CLI   0(R1),NOQ                                                        
         JNE   *+8                                                              
         MVI   TEMP2+2,YESQ                                                     
         LA    R1,1(R1)                                                         
         JCT   R2,STTALL05                                                      
                                                                                
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   TEMP2+3(1),GOECE                                                 
         CLI   GOECE,X'40'                                                      
         JH    EXITY                                                            
         CLI   GOECE,0                                                          
         JH    STTALL10                                                         
         MVI   TEMP2+3,C'N'                                                     
         J     EXITY                                                            
STTALL10 OI    TEMP2+3,X'F0'                                                    
         J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
SETTFNAR DS    0H                                                               
*&&UK                                                                           
         L     R3,AGOBLOCB                                                      
         USING GOBLOCKD,R3                                                      
*&&                                                                             
*&&US                                                                           
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
*&&                                                                             
         MVC   TEMP2(3),=C'NNN'                                                 
         LA    R1,GOTFNARR                                                      
         LA    R2,3                                                             
STNARR05 DS    0H                                                               
         CLI   0(R1),C'B'                                                       
         JNE   *+8                                                              
         MVI   TEMP2,YESQ                                                       
         CLI   0(R1),C'R'                                                       
         JNE   *+8                                                              
         MVI   TEMP2+1,YESQ                                                     
         CLI   0(R1),NOQ                                                        
         JNE   *+8                                                              
         MVI   TEMP2+2,YESQ                                                     
         LA    R1,1(R1)                                                         
         JCT   R2,STNARR05                                                      
         J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* Get account records for search/list                                           
***********************************************************************         
*                                                                               
NXTACTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    NACTS080                                                         
* INITIAL FOR LIST/search                                                       
         MVI   LP_RMODE,LP_RFRST                                                
         MVI   RUNFLAGS,0                                                       
         MVI   RUNFLAG2,0                                                       
         XC    CLIOFF,CLIOFF                                                    
         XC    THISOFF,THISOFF                                                  
         LA    R2,DMCB                                                          
         GOTO1 VACCEMU,DMCB,=C'NEWO',,AIO8,AIO8  Convert company to old         
         LA    R2,DMCB                                     file format          
         GOTO1 VACCEMU,DMCB,=C'NEWO',,AIO7,AIO7  Convert ledger to old          
                                                                                
         USING GOBLOCKD,RF                                                      
         L     RF,AGOBLOCB                                                      
         L     RE,ATWA                                                          
         LA    RE,2304+FALINKDL(RE)                                             
         ST    RE,GOABUFF          A(AFTER FALINK BLOCK)                        
         L     RE,=AL4(TWAMAXRL-(FALINKDL+2304))   MAX L'TWA                    
         ST    RE,GOLBUFF                                                       
         TM    LP_FLAG,LP_FOFFL                                                 
         JNZ   NACTS005             OFFLINE                                     
         MVC   GOABUFF,ATIA         USE TIA ONLINE                              
         MVC   GOLBUFF,=AL4(TWAMAXRL)   SHOULD BE OK, TIA IS 14K?               
         DROP  RF                                                               
                                                                                
*                                                                               
NACTS005 GOTOR INILDG                                                           
         JNE   NACTSNOX            ERROR, RETURN NO DATA                        
*                                                                               
*        GOTOR ACCRUL              initialise default limlist access            
*                                  rules                                        
         MVC   BINHIER,QHIER                                                    
         NI    BINHIER,X'0F'                                                    
*  CHOOSE WHICH ROUTINE TO CALL, BASED ON LEDGER                                
NACTS010 DS    0H                                                               
         LAY   RE,NXTACCT          GENERIC ACT READER ROUTINE                   
         CLC   QUNIT(2),=C'1R'                                                  
         JNE   NACTS022                                                         
         LAY   RE,NXT1RAC                                                       
*                                                                               
NACTS022 DS    0H                                                               
         CLC   QUNIT(2),=C'SJ'                                                  
         JNE   NACTS025                                                         
         LAY   RE,NXTSJAC                                                       
* ??? nxt2p2d                                                                   
* ??? nxt1Nac                                                                   
* suppliers                                                                     
NACTS025 DS    0H                                                               
         CLC   QUNIT(2),=C'SV'                                                  
         JNE   NACTS028                                                         
         LAY   RE,NXTSVAC                                                       
*                                                                               
NACTS028 DS    0H                                                               
         CLC   QUNIT(2),=C'SX'                                                  
         JNE   NACTS031                                                         
         LAY   RE,NXTSVAC                                                       
*                                                                               
NACTS031 DS    0H                                                               
         CLC   QUNIT(2),=C'ST'                                                  
         JNE   *+10                                                             
         LAY   RE,NXTSVAC                                                       
*                                                                               
NACTS034 DS    0H                                                               
         CLC   QUNIT(2),=C'SF'                                                  
         JNE   NACTS037                                                         
         LAY   RE,NXTSVAC                                                       
*                                                                               
NACTS037 DS    0H                                                               
         CLC   QUNIT(2),=C'1N'                                                  
         JNE   NACTS040                                                         
         LAY   RE,NXT1NAC                                                       
*                                                                               
NACTS040 ST    RE,AACTROUT                                                      
         GOTOR SETOPOS                                                          
*                                                                               
NACTS080 CLI   QSCHTYP,C' '    Is it search request                             
         JH    NACTS090                                                         
*                                                                               
*  LIST REQUEST - READ FROM QAACTS ARRAY                                        
*                                                                               
         GOTOR NXTLIST                                                          
         JNE   NACTSNOX            NO MORE                                      
         J     NACTSX                                                           
*                                                                               
* SEARCH REQUEST - USE SEARCH FIELDS                                            
*                                                                               
NACTS090 DS    0H                                                               
         GOTOR NXTSRCH                                                          
         JNE   NACTSNOX            NO MORE                                      
*                                                                               
NACTSX   L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         MVC   PREVACT,ACTKACT     TELL NXTPACT ITS A NEW ACCOUNT               
         J     EXITY                                                            
*                                                                               
NACTSNOX MVI   LP_RMODE,LP_RLAST                                                
         J     EXITNX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ ONE LEVEL BACK FROM WHATEVER'S IN PREVACT (RETURNED IN AIO4)             
***********************************************************************         
*                                                                               
NXTPACT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PREVACT+L'PREVACT-1                                           
         LA    R6,LDGAL4                                                        
         LA    R3,4                                                             
*                                                                               
NPACT010 CLI   0(R6),0                                                          
         JE    NPACT015                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JNE   NPACT020                                                         
*                                                                               
NPACT015 SHI   R6,1                                                             
         JCT   R3,NPACT010                                                      
         MVI   LP_RMODE,LP_RLAST   NO MORE                                      
         J     EXITNX                                                           
*                                                                               
NPACT020 LLC   RF,0(R6)                                                         
         LA    RE,PREVACT(RF)                                                   
         CLI   0(RE),C' '                                                       
         JNH   NPACT015            ALREADY ABOVE THIS LEVEL                     
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),QUNIT                                                 
         SHI   RF,1                                                             
         MVC   ACTKACT(0),PREVACT                                               
         EX    RF,*-6                                                           
         MVC   PREVAC2,PREVACT                                                  
         MVC   PREVACT,ACTKACT                                                  
*                                                                               
         L     RE,AIO4                                                          
         MVC   0(L'ACTKEY,RE),ACTKEY                                            
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    NPACT040                                                         
*                                                                               
*                                                                               
         OI    RUNFLAGS,RFNOSEQ    GETOPT BREAKS READ SEQ                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SAVE SO WE DON'T READ AGAIN                                                   
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)                                  
*                                                                               
NPACT040 DS    0H                                                               
         L     RF,AIO4             B#PRACT                                      
         ST    RF,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Get account records for List                                                  
***********************************************************************         
*                                                                               
NXTLIST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    NXLST030                                                         
* INITIAL STUFF                                                                 
*                                                                               
*  Build keyfragments                                                           
*                                                                               
         GOTOR AACTROUT                                                         
         JNE   EXITNX              Nothing to do                                
         MVI   LP_RMODE,LP_RFRST                                                
         XC    LASTMEDC,LASTMEDC                                                
*                                                                               
NXLST030 GOTOR GETACT              Read account rec from keyfragments           
         JNE   EXITNX              Found all accounts                           
*                                                                               
* Filter key and record                                                         
*                                                                               
NXLST050 GOTOR ACTFILT             Filter rountine                              
         JNE   NXLST030            Don't want record - get next key             
*                                                                               
*                                                                               
NXLST060 L     RF,AIO2             B#PRACT                                      
         ST    RF,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* Get account records for search                                                
***********************************************************************         
*                                                                               
NXTSRCH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    NXSCH058                                                         
         LA    R3,L'THISWORD                                                    
         GOTOR GOTSAR,DMCB,('TSAINI',TSAROBUF),('WRD_KEYL',TSARREC),   X        
               ('TSRTSAB2',WRD_RECL)                                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  build keyfragments                                                           
         GOTOR AACTROUT                                                         
         JNE   EXITNX              NOTHING TO DO                                
         MVI   LP_RMODE,LP_RFRST                                                
         XC    LASTMEDC,LASTMEDC                                                
         XC    IGNWCNT,IGNWCNT                                                  
*                                                                               
*  read 1st word in array                                                       
         XR    R3,R3                                                            
         ICM   R3,7,QAWORDS        R3=A(Uploaded filters)                       
         JZ    EXITNX              No words then exit                           
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN-LW_D(R3)  Number of filters lines                   
         AHI   R3,LW_LN2Q          R3=A(first address line data)                
*&&UK                                                                           
         CHI   R4,1                Only one word?                               
         JH    NXSCH010             then could be account code search           
*&&                                                                             
*&&US                                                                           
         CHI   R4,3                In NA they have embedded spaces in           
         JH    NXSCH010             code so need to check 2 words               
*&&                                                                             
         XC    TEMP,TEMP                                                        
         MVC   TEMP(L'THISACC),1(R3) Acc code search                            
         OC    TEMP,SPACES         Convert to upper case                        
         OI    RUNFLAG2,R2ACCWOR   Set flag to call ACCCSRCH                    
         CHI   R4,1                Only one word?                               
         JE    NXSCH010                                                         
         NI    RUNFLAG2,X'FF'-R2ACCWOR                                          
         LA    RE,L'THISACC                                                     
         LA    RF,TEMP+L'THISACC-1                                              
NXSCH002 CLI   0(RF),C' '                                                       
         JH    NXSCH004                                                         
         AHI   RF,-1                                                            
         JCT   RE,NXSCH002                                                      
         DC    H'0'                                                             
                                                                                
NXSCH004 AHI   RE,1                  Add one for the space                      
         AHI   RF,2                                                             
         CLM   RE,1,PCLILEN          Does it match client length                
         JE    NXSCH006                                                         
         CLM   RE,1,PPROLEN          Does it match product length               
         JNE   NXSCH010              No - can't be code search                  
NXSCH006 MVC   0(L'THISACC,RF),L'THISACC+(2*L'THISPRFX)(R3)                     
         CHI   R4,2                                                             
         JE    NXSCH009                                                         
         LA    RF,L'THISACC-1(RF)                                               
         LA    RE,L'THISACC(RE)                                                 
NXSCH007 CLI   0(RF),C' '                                                       
         JH    NXSCH004                                                         
         AHI   RF,-1                                                            
         JCT   RE,NXSCH007                                                      
         DC    H'0'                                                             
                                                                                
NXSCH008 AHI   RE,1                  Add one for the space                      
         AHI   RF,2                                                             
         CLM   RE,1,PPROLEN          Does it match product length               
         JNE   NXSCH010              No - can't be code search                  
         MVC   0(L'THISACC,RF),(2*L'THISACC)+(3*L'THISPRFX)(R3)                 
                                                                                
NXSCH009 OC    TEMP,SPACES         Convert to upper case                        
         OI    RUNFLAG2,R2ACCWOR   Set flag to call ACCCSRCH                    
*                                                                               
NXSCH010 DS    0H                                                               
         MVI   LP_RMODE,LP_RFRST                                                
         MVC   THISPRFX,0(R3)      Prefix Y/N                                   
         MVC   THISWORD,1(R3)      Search word                                  
         OC    THISWORD,SPACES     Convert to upper case                        
                                                                                
         LA    RF,STOPWENG         List of English words not supported          
         CLI   CUCTRY,CTRYGER      Test Germany                                 
         JNE   NXSCH012                                                         
         LA    RF,STOPWGER         List of German words not supported           
NXSCH012 CLI   0(RF),0             Did we find any words not supported          
         JE    NXSCH016            No                                           
         CLC   THISWORD,0(RF)                                                   
         JE    NXSCH014            Yes                                          
         LA    RF,L'THISWORD(RF)                                                
         J     NXSCH012                                                         
                                                                                
NXSCH014 LH    R1,IGNWCNT          Count number of words to ignore              
         AHI   R1,1                                                             
         STH   R1,IGNWCNT                                                       
                                                                                
NXSCH016 DS    0H                                                               
*&&UK*&& GOTOR UMLAUT              replace umlauts                              
*                                                                               
*  call whole-word rec-reading routine                                          
NXSCH020 TM    RUNFLAG2,R2ACCWOR   Acc + word search                            
         JZ    NXSCH024                                                         
         TM    RUNFLAG2,R2ACCSRC   Acc search complete                          
         JNZ   NXSCH024                                                         
         GOTOR ACCCSRCH            Acc search                                   
         JE    NXSCH030                                                         
         OI    RUNFLAG2,R2ACCSRC   Set finished now do word search              
         MVI   LP_RMODE,LP_RFRST                                                
*                                                                               
NXSCH024 GOTOR WORDSRCH                                                         
         JNE   NXSCH045                                                         
*        J     NXSCH030                                                         
*                                                                               
* KEY FILTERS                                                                   
NXSCH030 DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING SRCRECD,R2                                                       
*  CHECK AGAINST HIER VALUE                                                     
         CLI   QHIER,C' '                                                       
         JNH   NXSCH037                                                         
         LLC   RE,BINHIER          GET L'HIER LVL                               
         SHI   RE,1                                                             
         LA    RE,LDGAL1(RE)       ??? ADD 2 FOR SUPPLIERS?                     
         CLI   0(RE),L'SRCKACT     LOW LEVEL A/C ARE DIFFERENT                  
         JE    NXSCH037                                                         
         LLC   RF,0(RE)                                                         
         LA    RF,SRCKACT(RF)                                                   
         CLI   0(RF),C' '          NEXT LVL MUST BE BLANK                       
         JH    NXSCH020                                                         
*        BCTR  RF,0                                                             
*        CLI   0(RF),C' '          THIS LVL MUST BE SET                         
*        JNH   NXSCH020                                                         
         J     NXSCH037            OK                                           
*                                                                               
NXSCH035 DS    0H                                                               
         BCTR  RE,0                GET PREVIOUS LVL                             
         LA    RF,LDGAL1                                                        
         CR    RE,RF                                                            
         JNL   *+6                                                              
         DC    H'0'                SINGLE-LVL LEDGER, HIER UNNECESSARY          
         LLC   RF,0(RE)            L'PREVIOUS LVL                               
         LA    RF,SRCKACT(RF)                                                   
         CLI   0(RF),C' '          CHECK 1ST CHAR OF LOWEST LVL                 
         JNH   NXSCH020                                                         
*                                                                               
* add to/UPDATE sort buffer/table                                               
NXSCH037 DS    0H                                                               
WRD      USING WRD_D,TSARREC                                                    
         XC    TSARREC,TSARREC                                                  
         MVC   WRD.WRD_DA,SRCKDA                                                
         GOTOR GOTSAR,DMCB,('TSARDH',TSAROBUF)                                  
         TM    TSARERRS,TSEEOF                                                  
         JNZ   NXSCH040                                                         
         CLC   WRD.WRD_DA,SRCKDA                                                
         JE    NXSCH042            ALREADY ADDED ONE FOR THIS                   
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   WRD.WRD_DA,SRCKDA                                                
         MVI   WRD.WRD_SRT,WRD_WRD  Serch for word                              
         GOTOR GOTSAR,DMCB,('TSARDH',TSAROBUF)                                  
         TM    TSARERRS,TSEEOF                                                  
         JNZ   NXSCH040                                                         
         CLC   WRD.WRD_DA,SRCKDA                                                
         JE    NXSCH042            ALREADY ADDED ONE FOR THIS                   
* ADD NEW ENTRY                                                                 
NXSCH040 DS    0H                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   WRD.WRD_DA,SRCKDA                                                
         MVC   WRD.WRD_ACT,SRCKACT                                              
         MVC   WRD.WRD_IND,=H'1'   ONE SEARCH TERM FOUND ON THIS A/C            
         STC   R4,WRD.WRD_LAST                                                  
         MVI   WRD.WRD_SRT,WRD_ACC                                              
         TM    RUNFLAG2,R2ACCSRC   Acc search?                                  
         JZ    *+8                                                              
         MVI   WRD.WRD_SRT,WRD_WRD                                              
         GOTOR GOTSAR,DMCB,('TSAADD',TSAROBUF)                                  
         J     NXSCH020            any more matches for word?                   
* UPDATE ENTRY                                                                  
NXSCH042 DS    0H                                                               
         CLI   QSCHTYP,C'A'                                                     
         JNE   NXSCH020           WORD COUNTING ONLY FOR 'AND' SEARCH           
         CLM   R4,1,WRD.WRD_LAST                                                
         JE    NXSCH020            COUNT ONLY 1 MATCH PER WORD/DA               
         LH    R1,WRD.WRD_IND                                                   
         AHI   R1,1                                                             
         STH   R1,WRD.WRD_IND                                                   
         GOTOR GOTSAR,DMCB,('TSAPUT',TSAROBUF)                                  
         J     NXSCH020            any more matches for word?                   
*                                                                               
*        NEXT WMP ENTRY                                                         
NXSCH045 DS    0H                                                               
         LA    R3,L'THISACC+L'THISPRFX(R3)                                      
         JCT   R4,NXSCH010                                                      
         J     NXSCH055            NO MORE                                      
*                                                                               
*  ALL RECS FOUND, NOW READ FIRST REC                                           
NXSCH055 DS    0H                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         XC    TSARREC,TSARREC                                                  
         GOTOR GOTSAR,DMCB,('TSARDH',TSAROBUF)                                  
         J     NXSCH060                                                         
*                                                                               
*  READ NEXT BUFFER REC                                                         
*                                                                               
NXSCH058 DS    0H                                                               
         GOTOR GOTSAR,DMCB,('TSANXT',TSAROBUF)                                  
* FILTER                                                                        
                                                                                
NXSCH060 DS    0H                                                               
         TM    TSARERRS,TSEEOF                                                  
         JNZ   EXITNX              DONE                                         
                                                                                
* IF 'AND' SEARCH, CHECK ALL WORDS FOUND IN NAME                                
                                                                                
         CLI   QSCHTYP,C'A'                                                     
         JNE   NXSCH070                                                         
         ICM   R3,7,QAWORDS        R3=A(Uploaded filters)                       
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN-LW_D(R3) Number of words                            
         LH    R1,IGNWCNT             and subtract ignored words                
         SR    R4,R1                                                            
         CLM   R4,3,WRD.WRD_IND                                                 
         JNE   NXSCH058            Ignore if not all words matched              
                                                                                
* READ ACT REC                                                                  
                                                                                
NXSCH070 DS    0H                                                               
         GOTOR VDATAMGR,DMCB,=C'GETREC',ACCMST,WRD.WRD_DA,AIO2,IOWORK           
         JE    *+6                                                              
         DC    H'0'                BAD DA?                                      
         L     R1,AIO2                                                          
         LA    R2,IOKEY            DUMMY UP IS KEY FOR FILTERING                
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,0(R1)                                                     
         MVC   ACTKSTA,ACTRSTA-ACTRECD(R1)                                      
         GOTOR ACTFILT                                                          
         JNE   NXSCH058            RSEQ                                         
*                                                                               
         L     RF,AIO2             B#PRACT                                      
         ST    RF,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  WRD,R2                                                           
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* 1R account fragment builder - calls GAPLST and merges with QAACCS             
***********************************************************************         
*                                                                               
NXT1RAC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,L'TD_AACC                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARRBLK),((R2),TSARREC),         X        
               ('TSRTSAB1',TD_ACCLQ)      For keyfragments buffer               
         OC    CCTPID,CCTPID                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF           Initialise buffer                             
         LA    R2,L'SV1RACT                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARSBUF),((R2),SV1RACT),         X        
               ('TSRMINB1',L'SV1RACT)                                           
                                                                                
*                                                                               
* Try limit list first                                                          
*                                                                               
NX1R002  CLI   QAPPL,QAPPPTO      Time off system doesn't have limit            
         JE    NX1R075                                           list           
         CLI   BINHIER,4          If person level search/list                   
         JNE   NX1R005                                                          
         CLI   QVWALL,C'A'         and person search = Y                        
         JNE   NX1R005                                                          
         CLI   QTYVEL,QELIPA        and ellipse show all values                 
         JE    NX1R070            Show all persons                              
                                                                                
NX1R005  LLC   R2,QAPPL                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            X        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),((R2),0)               
         JNE   NX1R020                                                          
         OI    RUNFLAGS,RFGAPLQ     Set we have a limit list                    
*                                                                               
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         CLC   GAPTCODE-GAPTABD+GAPAREA,SPACES   All-access entry only?         
         JNE   NX1R080              No - use this                               
         CLI   QAPPL,QAPPTIM        Yes, remove it for time and expense         
         JE    NX1R010                                                          
         CLI   QAPPL,QAPPEXP                                                    
         JNE   NX1R080              Otherwise use it                            
*                                                                               
NX1R010  MVI   BYTE1,GAPTT1Q                                                    
         GOTOR DTALLACC             Routine to remove all access                
         J     NX1R040                                                          
*                                                                               
NX1R020  CLI   QAPPL,QAPPTIM       If not check approver record                 
         JE    NX1R040                                                          
         CLI   QAPPL,QAPPEXP                                                    
         JNE   NX1R070                                                          
*                                                                               
* Read for approver entires if no person search=Y and no limit list             
*                                                                               
NX1R040  CLI   QVWALL,C'A'        Do we have per search=Y                       
         JNE   NX1R050            No - check approver level                     
         CLI   BINHIER,4          Yes - if office dept or sub assume            
         JNE   NX1R070            access to all                                 
         CLI   QTYVEL,QELIPA      If person level check ellipse for all         
         JE    NX1R070            has been chosen to                            
                                                                                
NX1R050  LLC   R2,QAPPL                                                         
         LHI   RF,GAPTT1Q                                                       
         CLI   QFIN,YESQ          Are we doing finance view?                    
         JNE   *+8                                                              
         LHI   RF,GAPTT9Q         Set finance 1R                                
         GOTOR (#GAPLST,AGAPLST),DMCB,((RF),TSARABUF),                 +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),((R2),0)               
         JNE   NX1R060                                                          
         OI    RUNFLAGS,RFGAPLQ+RFAPPRQ  Have some approver data                
         J     NX1R080                                                          
*                                                                               
NX1R060  DS    0H                                                               
*&&US                                                                           
         CLC   CUAALF,=C'H7'                                                    
         JNE   NX1R070                                                          
         LHI   RF,GAPTT1Q                                                       
         CLI   QFIN,YESQ          Are we doing finance view?                    
         JNE   *+8                                                              
         LHI   RF,GAPTT9Q         Set finance 1R                                
         GOTOR (#GAPLST,AGAPLST),DMCB,((RF),TSARABUF),                 +        
               ('GAPLBKAP',SPACES),('GAPLTDTE',GAPLPARM),((R2),0)               
         JNE   NX1R070                                                          
         OI    RUNFLAGS,RFGAPLQ+RFAPPRQ  Have some approver data                
         J     NX1R080                                                          
*&&                                                                             
NX1R070  CLI   QVWALL,C'A'    No approvers or limit list                        
         JNE   NX1R080                                                          
NX1R075  MVI   BYTE1,GAPTT1Q                                                    
         GOTOR TALLACC        Add 'all access' to TSARABUF manually             
*                                                                               
NX1R080  GOTOR ACBUILD        Build key from filters/gaplist                    
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              No data to return                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 2P account fragment builder                                                   
***********************************************************************         
*                                                                               
NXT2PAC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SJ account fragment builder                                                   
***********************************************************************         
*                                                                               
NXTSJAC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,L'TD_AACC                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARRBLK),((R2),TSARREC),         X        
               ('TSRTSAB1',TD_ACCLQ)      FOR KEYFRAGMENTS                      
         OC    CCTPID,CCTPID                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF           FOR GAPLST                                    
         LA    R2,L'ACTKEY                                                      
         GOTOR GOTSAR,DMCB,('TSAINI',TSARSBUF),((R2),AIO4),            X        
               ('TSRMINB1',2048)        For previous levels                     
                                                                                
* Skip GAPLST if viewall overrides RSTEL profiles/gaplst                        
                                                                                
         CLI   QSJALL,C'Y'    Return all CLI/PRO/JOBS regardless                
         JNE   NXSJ020                                                          
         MVI   BYTE1,GAPTT2Q                                                    
         GOTOR TALLACC                                                          
         J     NXSJ080                                                          
                                                                                
* Get SJ accounts according to Limit list records                               
                                                                                
NXSJ020  MVI   GAPLPARM,GAPLACLS                                                
         LLC   R2,QAPPL                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT2Q',TSARABUF),            X        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),((R2),0)               
         JNE   NXSJ040                                                          
         OI    RUNFLAGS,RFGAPLQ     Set have SJ limit list                      
         MVI   GAPLPARM,GAPLPADD+GAPLACLS Concatenate media list                
         XC    GAPAREA,GAPAREA                                                  
         MVI   GAPAREA+GAPTDAT1-GAPTABD,GAPTT2Q                                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         CLC   GAPAREA+GAPTCODE-GAPTABD(L'GAPTCODE),SPACES                      
         JH    NXSJ040                                                          
                                                                                
* may need code here to deal with all                                           
                                                                                
* Get media list according to Limit list records                                
                                                                                
NXSJ040  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT5Q',TSARABUF),            X        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),((R2),0)               
         JNE   NXSJ080             No media list found                          
         XC    GAPAREA,GAPAREA     Is it all codes?                             
         MVI   GAPAREA+GAPTDAT1-GAPTABD,GAPTT5Q                                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         CLC   GAPAREA+GAPTCODE-GAPTABD(L'GAPTCODE),SPACES                      
         JH    NXSJ080                                                          
         OI    RUNFLAGS,RFMEDLQ    Set all media codes allowed                  
                                                                                
NXSJ080  MVI   GAPLPARM,GAPLACLS                                                
         GOTOR ACBUILD             Build key from filters/GAPLST                
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              No data to return                            
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 1N account fragment builder                                                   
***********************************************************************         
*                                                                               
NXT1NAC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,L'TD_AACC                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARRBLK),((R2),TSARREC),         X        
               ('TSRTSAB1',TD_ACCLQ)      FOR KEYFRAGMENTS                      
         OC    CCTPID,CCTPID                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF           FOR GAPLST                                    
                                                                                
* Skip GAPLST if viewall overrides RSTEL profiles/gaplst                        
                                                                                
         CLI   QSJALL,C'Y'    Return all 1N account regardless                  
         JNE   NX1N020                                                          
         MVI   BYTE1,GAPTT3Q                                                    
         GOTOR TALLACC                                                          
         J     NX1N080                                                          
                                                                                
* Get 1N accounts according to Limit list records                               
                                                                                
NX1N020  MVI   BYTE1,0                                                          
         MVI   GAPLPARM,GAPLACLS                                                
         LLC   R2,QAPPL                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT3Q',TSARABUF),            X        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),((R2),0)               
         JNE   NX1N080                                                          
         OI    RUNFLAGS,RFGAPLQ     HAVE SOME LIMLIST                           
*                                                                               
NX1N080  GOTOR ACBUILD        BUILD KEY FROM FILTERS/GAPLST                     
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              NO DATA TO RETURN                            
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* supplier fragment builder (SV/SX/SF/ST)                                       
***********************************************************************         
*                                                                               
NXTSVAC  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         LA    R2,L'TD_AACC                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARRBLK),((R2),TSARREC),         X        
               ('TSRTSAB1',TD_ACCLQ)      FOR KEYFRAGMENTS                      
         LA    R2,L'ACTKEY                                                      
         GOTOR GOTSAR,DMCB,('TSAINI',TSARSBUF),((R2),AIO4),            X        
               ('TSRMINB1',2048)        FOR supplier gap optim                  
         OC    CCTPID,CCTPID                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF           FOR GAPLST                                    
*                                                                               
*  if not invoices skip gaplst(not supported)                                   
         CLI   QAPPL,QAPPINV                                                    
         JNE   NXSV075                                                          
*                                                                               
* GET LIMLIST                                                                   
NXSV020  DS    0H                                                               
         LLC   R2,QAPPL                                                         
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT8Q',TSARABUF),            X        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),((R2),0)               
         JNE   NXSV070                                                          
         OI    RUNFLAGS,RFGAPLQ     HAVE SOME LIMLIST                           
         J     NXSV080                                                          
*                                                                               
* NO GAPLST, EVERYTHING BY DERFAULT?                                            
NXSV070  DS    0H                                                               
         CLI   LL_DSUPA,LL_ALLQ                                                 
         JNE   NXSV080                                                          
NXSV075  DS    0H                                                               
         MVI   BYTE1,GAPTT2Q                                                    
         GOTOR TALLACC        ADD 'ALL ACCESS' TO TSARABUF MANUALLY             
*                                                                               
NXSV080  DS    0H                                                               
         GOTOR ACBUILD        BUILD KEY FROM FILTERS/GAPLST                     
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              NO DATA TO RETURN                            
*                                                                               
         J     EXITY                                                            
***********************************************************************         
* GENERIC fragment builder - no gaplst                                          
***********************************************************************         
*                                                                               
NXTACCT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,L'TD_AACC                                                     
         GOTOR GOTSAR,DMCB,('TSAINI',TSARRBLK),((R2),TSARREC),         X        
               ('TSRTSAB1',TD_ACCLQ)      FOR KEYFRAGMENTS                      
*                                                                               
         CLC   =C'SE',QUNIT               SE NEEDS OPTIM BUFFER FOR             
         JNE   NXACC10                       PREVIOUS LEVELS                    
NXACC05  DS    0H                         SPLIT TO NEW ROUT IF ANY MORE         
         LA    R2,L'ACTKEY                   DEVIATIONS                         
         GOTOR GOTSAR,DMCB,('TSAINI',TSARSBUF),((R2),AIO4),            X        
               ('TSRMINB1',2048)        FOR supplier gap optim                  
*                                                                               
NXACC10  DS    0H                                                               
         OC    CCTPID,CCTPID                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF           FOR GAPLST                                    
         MVI   BYTE1,GAPTT3Q  treat as 1N (simplest)                            
         GOTOR TALLACC        ADD 'ALL ACCESS' TO TSARABUF MANUALLY             
*                                                                               
         GOTOR ACBUILD        BUILD KEY FROM FILTERS/GAPLST                     
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              NO DATA TO RETURN                            
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD 'ALL ACCESS' ENTRY TO TSARABUF MANUALLY (FOR WHEN GAPLST WON'T            
* DO IT FOR US.                                                                 
* BYTE1 HOLDS GAPTDAT1 VALUE TO USE                                             
* TWINNED WITH DTALLACC                                                         
***********************************************************************         
*                                                                               
TALLACC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    RUNFLAGS,RFGAPLQ        Set GAPLST found                         
         XC    GAPAREA,GAPAREA                                                  
GAP      USING GAPTABD,GAPAREA                                                  
         MVC   GAP.GAPTDAT1,BYTE1                                               
         MVC   GAP.GAPTCODE,SPACES     SPACE-FILLED MEANS EVERYTHING            
         MVI   GAP.GAPTSTA,GAPTSIQ+GAPTSMQ                                      
         MVI   GAP.GAPTLVL,0                                                    
         MVI   GAP.GAPTLEN,L'GAPTCODE  NOTE THIS IS NOT ZERO!                   
*                                                                               
         LA    R3,TSARABUF                                                      
         USING TSARD,R3            R3=A(TSAR block)                             
         TM    TSINDS,TSIINIOK     Test initialised                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSAADD       Set action                                   
         LA    R0,GAPAREA                                                       
         ST    R0,TSAREC                                                        
         GOTOR VTSAR,TSARD         Call TSAR                                    
         CLI   TSERRS,0          Set condition code for caller                  
         JE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R3,GAP                                                           
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* REMOVE 'ALL-ACCESS' ENTRY FROM GAPLST TABLE                                   
* BYTE1 HOLDS GAPTDAT1 VALUE TO USE                                             
* TWINNED WITH TALLACC                                                          
***********************************************************************         
*                                                                               
DTALLACC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    GAPAREA,GAPAREA                                                  
GAP      USING GAPTABD,GAPAREA                                                  
         MVC   GAP.GAPTDAT1,BYTE1                                               
         MVC   GAP.GAPTCODE,SPACES     SPACE-FILLED MEANS EVERYTHING            
         MVI   GAP.GAPTSTA,GAPTSIQ+GAPTSMQ                                      
         MVI   GAP.GAPTLVL,0                                                    
         MVI   GAP.GAPTLEN,L'GAPTCODE  NOTE THIS IS NOT ZERO!                   
         OI    RUNFLAGS,RFGAPLQ                                                 
*                                                                               
         LA    R3,TSARABUF                                                      
         USING TSARD,R3            R3=A(TSAR block)                             
         TM    TSINDS,TSIINIOK     Test initialised                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSADEL       Set action                                   
         LA    R0,GAPAREA                                                       
         ST    R0,TSAREC                                                        
         GOTOR VTSAR,TSARD         Call TSAR                                    
         CLI   TSERRS,0          Set condition code for caller                  
         JE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R3,GAP                                                           
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD ACCOUNT CODES FROM QAACTS AND GAPLST, save to tsar (TSARRBLK)           
* assumes GAPLST entries in TSARABUF                                            
***********************************************************************         
*                                                                               
ACBUILD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
TSR      USING TD_ACC,TSARREC                                                   
         MVC   TSR.TD_AACC,SPACES                                               
         NI    RUNFLAGS,FF-RFLIMEOF                                             
         OI    RUNFLAGS,RFLIMFST                                                
         SR    RE,RE                                                            
         ICM   RE,7,QAACTS                                                      
         LTR   RE,RE                                                            
         JZ    ACBL050             No filter array                              
         USING LW_D,RE                                                          
         CLC   LW_DATA2(L'ACTKACT),SPACES                                       
         JNH   ACBL050             No filter value                              
                                                                                
* if filters, build keyfragment and refine using limlist/appr (if any)          
                                                                                
         CLI   LW_TYPE,LW_TLSTQ                                                 
         JE    *+6                 List of values                               
         DC    H'0'                                                             
         LH    RF,LW_NUMN          Test for single entry                        
         CHI   RF,1                                                             
         JH    ACBL020                                                          
         LLC   RF,LW_LN            Single entry                                 
         SHI   RF,LW_LN1Q+1                                                     
         MVC   TSR.TD_AACC(0),LW_DATA2                                          
         EX    RF,*-6                                                           
         XC    ATHISACC,ATHISACC   Don't look at array again                    
         J     ACBL030                                                          
                                                                                
ACBL020  LA    RE,LW_DATA2                                                      
         CLC   0(L'ACTKACT,RE),SPACES                                           
         JNH   EXITNX                                                           
         MVC   TSR.TD_AACC,0(RE)                                                
         ST    RE,ATHISACC         A(A/C FILTER IN ARRAY)                       
                                                                                
* Refine using GAPLST                                                           
                                                                                
ACBL030  TM    RUNFLAGS,RFLIMEOF   Already found EOF?                           
         JNZ   ACBL040                                                          
         GOTOR GPLMOD                                                           
         JE    ACBL030             Next filter entry                            
                                                                                
* No more GAPLST for filter, get next filter                                    
                                                                                
ACBL040  OC    ATHISACC,ATHISACC   Is there only one filter                     
         JZ    EXITNX              Yes - end of process                         
         SR    RE,RE               No get next entry                            
         ICM   RE,7,QAACTS                                                      
         CLI   LW_TYPE,LW_TLSTQ                                                 
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         LH    RF,LW_LN                                                         
         AR    RF,RE               A(END OF ENTRY/SUBENTRIES)                   
         L     RE,ATHISACC         LAST SUBENTRY PROCESSED                      
         LA    RE,L'ACTKACT(RE)                                                 
         CR    RF,RE                                                            
         JNH   EXITNX              END OF ENTRY, ASSUME NO MORE                 
         MVC   TSR.TD_AACC,0(RE)                                                
         ST    RE,ATHISACC         A(A/C FILTER IN ARRAY)                       
         NI    RUNFLAGS,FF-RFLIMEOF                                             
         OI    RUNFLAGS,RFLIMFST                                                
         J     ACBL030                                                          
                                                                                
* if no filters, build keyfragment from gaplst                                  
                                                                                
ACBL050  DS    0H                                                               
         GOTOR GPLCPY                                                           
         J     EXITNX              NO MORE                                      
*                                                                               
         DROP  TSR,RE                                                           
         EJECT                                                                  
**********************************************************************          
* Special code for German special characters (Umlaute)               *          
**********************************************************************          
UMLAUT   NTR1  LABEL=*,BASE=*                                                   
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      (from GESRCHPARS)                            
         JNE   UMLAUTX                                                          
*                                                                               
         MVC   CSVKEY2,SPACES      SUBSTITUTION USES UP TO 19 CHARS..           
         MVC   CSVKEY2(L'THISWORD),THISWORD                                     
*                                                                               
UMLAUT05 LA    R2,CSVKEY2                                                       
         LA    R3,L'THISWORD                                                    
*                                                                               
UMLAUT10 LA    RE,=C'AE'           RE IS SUBSTITUTION STRING                    
         CLI   0(R2),X'4A'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'OE'                                                        
         CLI   0(R2),X'E0'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'UE'                                                        
         CLI   0(R2),X'5A'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'SS'                                                        
         CLI   0(R2),X'A1'                                                      
         JNE   UMLAUT20                                                         
*                                                                               
UMLAUT15 MVC   TEMP2(L'THISWORD),1(R2)                                          
         MVC   0(2,R2),0(RE)                                                    
         MVC   2(L'THISWORD,R2),TEMP2                                           
         J     UMLAUT05            start again after substitution               
*                                                                               
UMLAUT20 AHI   R2,1                CHECK NEXT CHAR                              
         JCT   R3,UMLAUT10                                                      
*                                                                               
         MVC   THISWORD,CSVKEY2    NOTE: MAY TRUNCATE WORD!                     
*&&                                                                             
UMLAUTX  J     EXIT                                                             
                                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* LOOK FOR 1ST/NEXT ACCOUNT NAME PASSIVE                                        
* ENTRY: THISWORD IS SEARCH TERM                                                
*        THISPRFX Y TREATS AS PREFIX, IE RETURNS 'TENTPEG' FOR 'TENT'           
*        LP_RMODE CONTROLS 1ST/NEXT                                             
*        CALLS GOTSAR TO FILTER BY KEYFRAGMENT                                  
* RETURN: MATCHING PASSIVE IN IO1                                               
* USES CSVKEY1, IOKEY                                                           
***********************************************************************         
*                                                                               
WORDSRCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    WSRCH035                                                         
*                                                                               
TSR      USING TD_ACC,TSARREC                                                   
         MVI   LP_RMODE,LP_RNEXT                                                
*                                                                               
         LA    R1,SRCKSEQ1-SRCKEY-1     EXECUTE LENGTH!                         
         CLI   THISPRFX,C'Y'                                                    
         JNE   WSRCH020                                                         
*  GET PARTIAL WORD LENGTH, DERIVE COMPARE LENGTH.                              
         LA    RE,THISWORD+L'THISWORD-1                                         
         LA    R1,L'THISWORD-1          EXECUTE LENGTH!                         
WSRCH010 DS    0H                                                               
         CLI   0(RE),C' '                                                       
         JH    WSRCH015                                                         
         SHI   RE,1                                                             
         JCT   R1,WSRCH010                                                      
WSRCH015 DS    0H                                                               
         LA    R1,SRCKWRD1-SRCKEY(R1)                                           
*  READ FOR MATCHES                                                             
WSRCH020 DS    0H                                                               
         STC   R1,THISWRDL                                                      
         XC    IOKEY,IOKEY                                                      
         USING SRCRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVI   SRCKTYP,SRCKTYPQ                                                 
         MVC   SRCKCPY,CUXCPY                                                   
         MVC   SRCKUL,QUNIT                                                     
         MVI   SRCKSUB,SRCKWDSQ                                                 
         MVC   SRCKWRD1,THISWORD                                                
         MVC   CSVKEY1,SRCKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   EXITNX              NONE FOUND                                   
         J     WSRCH040                                                         
*                                                                               
WSRCH035 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   EXITN               NO MORE                                      
*                                                                               
* CHECK MATCH                                                                   
WSRCH040 DS    0H                                                               
         LLC   R1,THISWRDL                                                      
         CLC   SRCKEY(0),CSVKEY1                                                
         EX    R1,*-6                                                           
         JNE   EXITNX              NO MORE WORD MATCHES, DONE                   
*                                                                               
* PREFILTER AGAINST KEYFRAG, APPROVER EXCEPTIONS                                
         GOTOR WSRCHFLT                                                         
         JNE   WSRCH035                                                         
         GOTOR APPFLT,SRCKACT                                                   
         JE    EXITY               MATCH                                        
         J     WSRCH035                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LOOK FOR 1ST/NEXT ACCOUNT NAME CODE PASSIVE                                   
* ENTRY: THISWORD IS SEARCH TERM                                                
*        THISPRFX Y TREATS AS PREFIX, IE RETURNS 'TENTPEG' FOR 'TENT'           
*        LP_RMODE CONTROLS 1ST/NEXT                                             
*        CALLS GOTSAR TO FILTER BY KEYFRAGMENT                                  
* RETURN: MATCHING PASSIVE IN IO1                                               
* USES CSVKEY1, IOKEY                                                           
***********************************************************************         
         SPACE 1                                                                
TSR      USING TD_ACC,TSARREC                                                   
ACCCSRCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    ACCCS050                                                         
         MVI   LP_RMODE,LP_RNEXT                                                
         MVC   THISACC,SPACES      Initialise account search                    
         LA    RE,TEMP+L'THISACC-1                                              
         LA    R1,L'THISACC-1                                                   
*                                                                               
ACCCS010 CLI   0(RE),C' '          Work out length of account input             
         JH    ACCCS015                                                         
         SHI   RE,1                                                             
         JCT   R1,ACCCS010                                                      
*                                                                               
ACCCS015 STC   R1,THISACCL         THISACCL = length of account input           
         AHI   R1,1                                                             
         LA    RF,LDGAL4           Find match account level                     
*                                                                               
ACCCS020 LLC   R5,0(RF)                                                         
         LTR   R5,R5               Find level with a length                     
         JZ    ACCCS025                                                         
         CLM   R5,1,LDGAL1         If highest account level                     
         JE    ACCCS035             then don't manipulate account code          
         CR    R5,R1               Otherwise find matching level                
         JNH   ACCCS030                                                         
ACCCS025 JCT   RF,ACCCS020                                                      
         DC    H'0'                Should find some match                       
*                                                                               
ACCCS030 LLC   R4,0(RF)                                                         
         SHI   RF,1                Get the previous ledger level                
         LLC   R5,0(RF)                                                         
         J     ACCCS040                                                         
*                                                                               
ACCCS035 LLC   R4,0(RF)                                                         
         XR    R5,R5               Reset R5 to get entire code                  
*                                                                               
ACCCS040 SR    R4,R5                                                            
         LA    RF,TEMP(R5)                                                      
         BCTR  R4,0                                                             
         BASR  RE,0                                                             
         MVC   THISACC(0),0(RF)    Extract lowest level part of account         
         EX    R4,0(RE)                                                         
         CLC   PRODUL,QUNIT        or client rec?                               
         JNE   *+8                                                              
         STC   R4,THISACCL                                                      
*                                                                               
*  READ FOR MATCHES                                                             
*                                                                               
         LHI   R1,L'SRCKWRD1-1                                                  
         LA    R1,SRCKWRD1-SRCKEY(R1)                                           
         STC   R1,THISWRDL                                                      
         XC    IOKEY,IOKEY                                                      
         USING SRCRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVI   SRCKTYP,SRCKTYPQ                                                 
         MVC   SRCKCPY,CUXCPY                                                   
         MVC   SRCKUL,QUNIT                                                     
         MVI   SRCKSUB,SRCKWDSQ                                                 
         MVC   SRCKWRD1,THISACC                                                 
         MVC   CSVKEY1,SRCKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   EXITNX              NONE FOUND                                   
         J     ACCCS055                                                         
*                                                                               
ACCCS050 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   EXITN               NO MORE                                      
*                                                                               
* CHECK MATCH                                                                   
ACCCS055 DS    0H                                                               
         LLC   R1,THISWRDL                                                      
         BASR  RF,0                                                             
         CLC   SRCKEY(0),CSVKEY1                                                
         EX    R1,0(RF)                                                         
         JNE   EXITNX              NO MORE WORD MATCHES, DONE                   
*                                                                               
* PREFILTER AGAINST KEYFRAG, APPROVER EXCEPTIONS                                
         GOTOR WSRCHFLT                                                         
         JNE   ACCCS050                                                         
*                                                                               
         LLC   RF,THISACCL         THISACCL = ORIGINAL ACCOUNT LENGTH           
         BASR  R1,0                                                             
         CLC   SRCKACT(0),TEMP     CHECK ACCOUNT CODE MATCHES WHAT WAS          
         EX    RF,0(R1)            ENTERED                                      
         JNE   ACCCS050                                                         
*                                                                               
         GOTOR APPFLT,SRCKACT                                                   
         JE    EXITY               MATCH                                        
         J     ACCCS050                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* POST-FILTER A SRCKEY AGAINST KEYFRAGMENTS                                     
***********************************************************************         
WSRCHFLT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    RUNFLAGS,RFFRAGQ                                                 
         JZ    EXITNX              NO FRAGMENTS, DON'T RETURN ANYTHING          
         XC    TSARREC,TSARREC                                                  
         GOTOR GOTSAR,DMCB,('TSARDH',TSARRBLK)                                  
         CLC   TSR.TD_AACC,SPACES                                               
         JE    EXITY               ALL ACCESS ENTRY, NO FILTERING               
*                                                                               
* FIND NEXT KEYFRAG MATCHING SRCKACT, MAY BE HIGHER LVL                         
         XC    TSARREC,TSARREC                                                  
         MVC   TSR.TD_AACC,SRCKACT                                              
         GOTOR GOTSAR,DMCB,('TSARDH',TSARRBLK)                                  
         TM    TSARERRS,TSEEOF                                                  
         JZ    WSRCHF20            FOUND SOMETHING                              
* NOTHING FOUND, USE SRCKACT TO FIND L'PREV LEVEL                               
         LA    RE,SRCKACT+L'SRCKACT-1                                           
         LA    RF,L'SRCKACT                                                     
WSRCHF10 DS    0H                                                               
         CLI   0(RE),C' '                                                       
         JH    WSRCHF25                                                         
         BCTR  RE,0                                                             
         JCT   RF,WSRCHF10                                                      
         DC    H'0'                SRCKACT EMPTY!                               
*                                                                               
WSRCHF20 DS    0H                                                               
* GOT A TSAR REC, DOES IT MATCH?                                                
         LLC   RF,TSR.TD_ACLN                                                   
         SHI   RF,1                                                             
         CLC   SRCKACT(0),TSR.TD_AACC                                           
         EX    RF,*-6                                                           
         JE    EXITY               EXACT MATCH!                                 
* TRY HIGHER LEVEL MATCH                                                        
         LLC   RF,TSR.TD_ACLN                                                   
WSRCHF25 DS    0H                                                               
         LA    R3,1                                                             
         LA    R4,LDGAL1                                                        
WSRCHF30 DS    0H                                                               
         CLM   RF,1,0(R4)                                                       
         JNH   WSRCHF40                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)            NUMBER OF HIGHER LEVELS                      
         J     WSRCHF30                                                         
*                                                                               
WSRCHF40 DS    0H                                                               
         LA    RF,LDGAL1                                                        
         CR    R4,RF               If highest account level                     
         JNH   WSRCHF42            then don't get previous level                
         BCTR  R4,0                A(L'PREVIOUS LEVEL)                          
WSRCHF42 LLC   RF,0(R4)                                                         
         SHI   RF,1                                                             
         XC    TSARREC,TSARREC                                                  
         MVC   TSR.TD_AACC(0),SRCKACT                                           
         EX    RF,*-6                                                           
         GOTOR GOTSAR,DMCB,('TSARDH',TSARRBLK)                                  
         TM    TSARERRS,TSEEOF                                                  
         JZ    WSRCHF60                                                         
         JCT   R3,WSRCHF40         EOF, BACK A LEVEL                            
         J     EXITNX              NO MORE HIGHER LEVELS                        
*                                                                               
WSRCHF60 DS    0H                                                               
         LLC   RF,TSR.TD_ACLN                                                   
         SHI   RF,1                                                             
         CLC   SRCKACT(0),TSR.TD_AACC                                           
         EX    RF,*-6                                                           
         JE    EXITY               MATCH ON HIGHER LEVEL                        
         JCT   R3,WSRCHF40         NO MATCH, CHECK PREV LEVEL                   
         J     EXITNX              NO MORE HIGHER LEVELS                        
         DROP  R2,TSR                                                           
         EJECT                                                                  
***********************************************************************         
* READ ACCOUNT DIR FROM KEYFRAGMENTS (SAPREC IF QHIER SET)                      
***********************************************************************         
*                                                                               
GETACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ FIRST/NEXT KEYFRAGMENT                                                   
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         TM    RUNFLAGS,RFNOSEQ                                                 
         JZ    GTAC010             IOSEQ BROKEN, RESTORE                        
         MVC   IOKEY,CSVKEY1                                                    
         J     GTAC030                                                          
*                                                                               
GTAC010  DS    0H                                                               
         CLI   LP_RMODE,LP_RNEXT                                                
         JE    GTAC040                                                          
         XC    TSARREC,TSARREC                                                  
         GOTOR GOTSAR,DMCB,('TSARDH',TSARRBLK)                                  
         TM    TSARERRS,TSEEOF                                                  
         JNZ   EXITNX              NO MORE KEYFRAGMENTS, ALL DONE               
         MVI   LP_RMODE,LP_RNEXT                                                
         J     GTAC025                                                          
*                                                                               
GTAC020  DS    0H                                                               
         GOTOR GOTSAR,DMCB,('TSANXT',TSARRBLK)                                  
         TM    TSARERRS,TSEEOF                                                  
         JNZ   EXITNX              NO MORE KEYFRAGMENTS, ALL DONE               
*                                                                               
* read FIRST ACCOUNT FOR KEYFRAGMENT                                            
GTAC025  DS    0H                                                               
TSR      USING TD_ACC,TSARREC                                                   
         CLI   QHIER,C' '                                                       
         JH    GTAC028                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QUNIT                                                    
         MVC   ACTKLDG,QLEDG                                                    
         MVC   ACTKACT,TSR.TD_AACC                                              
         J     GTAC030                                                          
         DROP  R2                                                               
*                                                                               
* hier filter                                                                   
GTAC028  DS    0H                                                               
         USING SAPRECD,R2                                                       
         XC    SAPKEY,SAPKEY                                                    
         MVI   SAPKTYP,SAPKTYPQ                                                 
         MVI   SAPKSUB,SAPKSUBQ                                                 
         MVC   SAPKCPY,CUXCPY                                                   
         MVC   SAPKUNT,QUNIT                                                    
         MVC   SAPKLDG,QLEDG                                                    
         MVC   SAPKLVL,BINHIER                                                  
         MVC   SAPKACT,TSR.TD_AACC                                              
         DROP  R2                                                               
*                                                                               
GTAC030  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLI   IOERR,0                                                          
         JE    GTAC035                                                          
         CLI   IOERR,IOEEOF                                                     
         JE    GTAC035                                                          
         DC    H'0'                                                             
GTAC035  DS    0H                                                               
         TM    RUNFLAGS,RFNOSEQ         IF WE'RE HERE TO RESTORE SEQ            
         JZ    GTAC060                      WANT TO RSEQ AFTER HI               
         NI    RUNFLAGS,X'FF'-RFNOSEQ                                           
*                                                                               
GTAC040  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         CLI   IOERR,0                                                          
         JE    GTAC060                                                          
         CLI   IOERR,IOEEOF                                                     
         JE    GTAC060                                                          
         DC    H'0'                                                             
*                                                                               
* BASIC A/C CHECKS - RETURN KEY MATCHES WHAT WE ASKED FOR?                      
         USING ACTRECD,R2                                                       
GTAC060  DS    0H                                                               
         MVC   CSVKEY1,ACTKEY                                                   
         CLI   QHIER,C' '                                                       
         JH    GTAC070                                                          
         CLC   QUNIT,ACTKUNT                                                    
         JNE   EXITNX              DONE                                         
         CLC   QLEDG,ACTKLDG                                                    
         JNE   EXITNX              DONE                                         
         CLC   ACTKACT,SPACES                                                   
         JNH   GTAC040             LEDGER REC, CARRY ON                         
*                                                                               
         CLC   TSR.TD_AACC,SPACES                                               
         JE    GTAC065             'ALL ACCESS' ENTRY, NO CODE FILTER           
         LLC   RF,TSR.TD_ACLN                                                   
         SHI   RF,1                                                             
         CLC   TSR.TD_AACC(0),ACTKACT                                           
         EX    RF,*-6                                                           
         JNE   GTAC020             NOT EVEN CLOSE, GET NEXT FILTER              
*                                                                               
GTAC065  DS    0H                                                               
         LA    RE,ACTKACT+L'ACTKACT   CHECK SPACE-FILLED KEY                    
         LA    RF,L'ACTKEY-ACTKEND                                              
         SHI   RF,1                                                             
         CLC   0(0,RE),SPACES                                                   
         EX    RF,*-6                                                           
         JE    EXITY               YES, WANT THIS ONE                           
         LH    RF,0(RE)            NOT AN ACCOUNT REC, SKIP TO NEXT             
         AHI   RF,1                                                             
         STH   RF,0(RE)                                                         
         J     GTAC030                                                          
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
* BASIC A/C CHECKS - HIERARCHY VERSION                                          
         USING SAPRECD,R2                                                       
GTAC070  DS    0H                                                               
         CLC   QUNIT,SAPKUNT                                                    
         JNE   EXITNX              DONE                                         
         CLC   QLEDG,SAPKLDG                                                    
         JNE   EXITNX              DONE                                         
         CLC   SAPKLVL,BINHIER                                                  
         JNE   EXITNX                                                           
*                                                                               
         CLC   TSR.TD_AACC,SPACES                                               
         JE    EXITY               'ALL ACCESS' ENTRY, RETURN ANYTHING          
         LLC   RF,TSR.TD_ACLN                                                   
         SHI   RF,1                                                             
         CLC   TSR.TD_AACC(0),SAPKACT                                           
         EX    RF,*-6                                                           
         JNE   GTAC020             NOT EVEN CLOSE, GET NEXT FILTER              
         J     EXITY               WANT THIS ONE                                
         DROP  R2,TSR                                                           
         EJECT                                                                  
***********************************************************************         
* Build keyfragment(s) from an account, incorporating gaplist                   
*                                                                               
* IF MATCHING GAPLST ENTRY IS AT HIGHER LEVEL, A/C UNCHANGED                    
* IF MATCHING GAPLST AT LOWER LEVEL, REPLACE.                                   
* IF MULTIPLE (LOWER LEVEL) MATCHES, WILL BUILD MULTIPLE KEYFRAGMENTS           
***********************************************************************         
*                                                                               
GPLMOD   NTR1  BASE=*,LABEL=*                                                   
         TM    RUNFLAGS,RFGAPLQ                                                 
         JNZ   GPLMD002                                                         
         OI    RUNFLAGS,RFLIMEOF   PRETEND EOF SO WE DON'T COME BACK            
         OI    RUNFLAGS,RFLIMFST   EXCEPT WITH A NEW ARRAY VALUE                
         J     GPLMODX                                                          
*                                                                               
GAP      USING GAPTABD,GAPAREA                                                  
TSR      USING TD_ACC,TSARREC                                                   
GPLMD002 TM    RUNFLAGS,RFLIMFST   First time in?                               
         JZ    GPLMD020            No                                           
         LA    RE,TSR.TD_AACC+L'TD_AACC-1                                       
         LA    RF,L'ACTKACT                                                     
*                                                                               
GPLMD005 CLI   0(RE),C' '                                                       
         JH    GPLMD010            RF=L'key passed                              
         SHI   RE,1                                                             
         JCT   RF,GPLMD005                                                      
         DC    H'0'                No data - error                              
GPLMD010 LA    RE,LDGAL1                                                        
GPLMD012 CLM   RF,1,0(RE)          Is ledger level higher than filter           
         JNH   GPLMD014                                                         
         LA    RE,1(RE)                                                         
         J     GPLMD012                                                         
                                                                                
GPLMD014 LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         STC   R1,GPLMDLEN         Length of filter account according           
*                                           ledger lengths                      
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JZ    GPLMD025                                                         
         DC    H'0'                This shouldn't happen - RFGAPLQ              
*                                         says we have a limit list             
GPLMD020 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JZ    GPLMD025                                                         
         OI    RUNFLAGS,RFLIMEOF                                                
         OI    RUNFLAGS,RFLIMFST                                                
         J     GPLMODX             No more entries in buffer                    
*                                                                               
GPLMD025 CLI   GAP.GAPTDAT1,GAPTT5Q Ignore media codes                          
         JE    GPLMD020                                                         
         TM    GAP.GAPTSTA,GAPTSMQ                                              
         JZ    GPLMD020            And approver sub-entries                     
         NI    RUNFLAGS,FF-RFLIMFST                                             
         CLC   GAP.GAPTCODE,SPACES If all access entry use filter               
         JNH   GPLMD044                                                         
         LA    R3,GAP.GAPTACT      Set R3 to A(GAPLST A/C)                      
         CLI   GAP.GAPTDAT1,GAPTT1Q                                             
         JE    GPLMD026                                                         
         CLI   GAP.GAPTDAT1,GAPTT3Q                                             
         JE    GPLMD026                                                         
         LA    R3,GAP.GAPTACC                                                   
         CLI   GAP.GAPTDAT1,GAPTT2Q                                             
         JE    GPLMD026                                                         
         CLI   GAP.GAPTDAT1,GAPTT8Q                                             
         JE    GPLMD026                                                         
         DC    H'0'                GAPLST type not tested                       
                                                                                
GPLMD026 CLI   GAP.GAPTDAT1,GAPTT2Q Check we are dealing with SJ ledger         
         JNE   GPLMD028            Not SJ skip office check                     
         CLC   GAP.GAPTCOFF,SPACES Check we have an office                      
         JNH   GPLMD028            No office present can skip check             
         MVC   THISOFF,GAP.GAPTCOFF                                             
         GOTOR OFFFLT,0            Check office is valid                        
         JNE   GPLMD020            Not valid get next entry                     
*                                                                               
GPLMD028 LLC   RE,GPLMDLEN         RE=Filter account length                     
         LLC   R0,GAP.GAPTLEN      R0=GAPLST code length                        
         SHI   R0,1                Remove 1 for ex instr                        
         CLI   GAP.GAPTDAT1,GAPTT1Q 1R or 1N accounts                           
         JE    GPLMD030                                                         
         CLI   GAP.GAPTDAT1,GAPTT3Q                                             
         JE    GPLMD030                                                         
         SHI   R0,L'GAP.GAPTCOFF   Remove office or unit and ledger             
GPLMD030 STC   R0,BYTE2                                                         
         CLC   GPLMDLEN,BYTE2      Is filter not higher than GAPLST             
         JNH   GPLMD035            Yes                                          
         LLC   RE,BYTE2            Use the smaller length for compare           
*                                                                               
GPLMD035 CLC   TSR.TD_AACC(0),0(R3) Compare GAPLST a/c to filter a/c            
         EX    RE,*-6                                                           
         JNE   GPLMD020                                                         
*                                                                               
* can't do this below as the tsar entries are by office so the client           
* codes are random                                                              
*        JH    GPLMD020            GAPLST a/c lower - get next entry            
*        JL    GPLMODX             GAPLST a/c higher - end                      
                                                                                
         CLC   GPLMDLEN,BYTE2      GAPLST entry at higher level                 
         JNL   GPLMD044            No - use filter                              
*                                                                               
GPLMD040 LA    RF,L'ACTKACT-1      Use GAPLST entry for the hierachy            
         CLI   QHIER,C' '                       requested                       
         JNH   GPLMD042                                                         
         LLC   RE,BINHIER                                                       
         SHI   RE,1                                                             
         LLC   RF,LDGAL1(RE)                                                    
         SHI   RF,1                                                             
GPLMD042 MVC   TSR.TD_AACC(0),0(R3)  Make key fragment based on GAPLST          
         EX    RF,*-6                                                           
*                                                                               
GPLMD044 LA    RF,L'TD_AACC                                                     
         CLI   QHIER,C' '                                                       
         JNH   GPLMD080                                                         
         LA    RE,TSR.TD_AACC+L'TD_AACC-1                                       
GPLMD046 CLI   0(RE),C' '                                                       
         JH    GPLMD048                                                         
         SHI   RE,1                                                             
         BCT   RF,GPLMD046                                                      
         J     GPLMD080                                                         
                                                                                
GPLMD048 LA    RE,LDGAL1                                                        
GPLMD050 CLM   RF,1,0(RE)                                                       
         JNH   GPLMD052                                                         
         LA    RE,1(RE)                                                         
         J     GPLMD050                                                         
                                                                                
GPLMD052 LLC   R1,0(RE)                                                         
         LR    RF,R1                                                            
                                                                                
GPLMD080 STC   RF,TSR.TD_ACLN                                                   
         OI    RUNFLAGS,RFFRAGQ                                                 
         GOTOR GOTSAR,DMCB,('TSAADD',TSARRBLK)                                  
         JE    GPLMD020                                                         
         TM    TSARERRS,TSEDUP     Ignore duplicate errors                      
         JNZ   GPLMD020               limit list allows this                    
         DC    H'0'                                                             
*                                                                               
GPLMODX  TM    RUNFLAGS,RFFRAGQ    Did we add any key fragments                 
         JNZ   EXITY               Yes - good                                   
         J     EXITNX              No - no matches with limit list              
         DROP  GAP,TSR                                                          
         EJECT                                                                  
***********************************************************************         
* COPY GAPLST ENTRIES TO KEYFRAGMENTS (IF ANY)                                  
*                                                                               
***********************************************************************         
*                                                                               
GPLCPY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GAP      USING GAPTABD,GAPAREA                                                  
TSR      USING TD_ACC,TSARREC                                                   
*                                                                               
         TM    RUNFLAGS,RFGAPLQ     NO GAPLST ENTRIES                           
         JZ    EXITY                                                            
*                                                                               
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JZ    *+6                                                              
         DC    H'0'                WHY IS RFGAPLQ SET? EMPTY BUFFER!            
* Set R3 to location of account code                                            
         LA    R3,GAP.GAPTACT                                                   
         CLI   GAP.GAPTDAT1,GAPTT1Q  1R acc                                     
         JE    GPLCPY20                                                         
         CLI   GAP.GAPTDAT1,GAPTT3Q  1N acc                                     
         JE    GPLCPY20                                                         
         CLI   GAP.GAPTDAT1,GAPTT9Q  1R finance acc                             
         JE    GPLCPY20                                                         
         LA    R3,GAP.GAPTACC                                                   
         CLI   GAP.GAPTDAT1,GAPTT2Q                                             
         JE    GPLCPY20                                                         
         CLI   GAP.GAPTDAT1,GAPTT8Q                                             
         JE    GPLCPY20                                                         
         DC    H'0'                GAPLST TYPE NOT TESTED                       
*                                                                               
GPLCPY10 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   EXITY                                                            
*                                                                               
GPLCPY20 LA    RF,L'ACTKACT-1                                                   
         CLI   GAP.GAPTDAT1,GAPTT5Q IGNORE MEDIA CODES                          
         JE    GPLCPY10                                                         
         TM    GAP.GAPTSTA,GAPTSMQ                                              
         JZ    GPLCPY10           AND APPROVER SUB-ENTRIES                      
         CLC   GAP.GAPTCODE,SPACES     IF ALL-ACCESS ENTRY SKIP HIER,           
         JNH   GPLCPY30                              JUST ADD IT                
*                                                                               
         CLI   GAP.GAPTDAT1,GAPTT2Q SJ?                                         
         JNE   GPLCPY25                                                         
         CLC   GAP.GAPTCOFF,SPACES                                              
         JNH   GPLCPY25                                                         
         MVC   THISOFF,GAP.GAPTCOFF                                             
         GOTOR OFFFLT,0                                                         
         JNE   GPLCPY10                                                         
*                                                                               
GPLCPY25 LA    RF,L'ACTKACT-1                                                   
         CLI   QHIER,C' '                                                       
         JNH   GPLCPY30                                                         
         LLC   RE,BINHIER                                                       
         SHI   RE,1                                                             
         LLC   RF,LDGAL1(RE)                                                    
         SHI   RF,1                                                             
GPLCPY30 MVC   TSR.TD_AACC(0),0(R3)  Make key fragment based on GAPLST          
         EX    RF,*-6                                                           
                                                                                
         LA    RF,L'ACTKACT                                                     
         CLI   QHIER,C' '                                                       
         JNH   GPLCPY50                                                         
         LA    RE,TSR.TD_AACC+L'TD_AACC-1                                       
GPLCPY35 CLI   0(RE),C' '                                                       
         JH    GPLCPY40                                                         
         SHI   RE,1                                                             
         BCT   RF,GPLCPY35                                                      
         J     GPLCPY50                                                         
                                                                                
GPLCPY40 LA    RE,LDGAL1                                                        
GPLCPY42 CLM   RF,1,0(RE)                                                       
         JNH   GPLCPY44                                                         
         LA    RE,1(RE)                                                         
         J     GPLCPY42                                                         
                                                                                
GPLCPY44 LLC   R1,0(RE)                                                         
         LR    RF,R1                                                            
                                                                                
GPLCPY50 STC   RF,TSR.TD_ACLN                                                   
         OI    RUNFLAGS,RFFRAGQ                                                 
         GOTOR GOTSAR,DMCB,('TSAADD',TSARRBLK)                                  
         JE    GPLCPY10            Look for more GAPLST entries                 
         TM    TSARERRS,TSEDUP     Ignore duplicate errors                      
         JNZ   GPLCPY10                                                         
         DC    H'0'                                                             
*                                                                               
         DROP  GAP                                                              
         EJECT                                                                  
***********************************************************************         
* FILTER ACCOUNT RECORDS FOR LIST/SEARCH                                        
***********************************************************************         
*                                                                               
ACTFILT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOKEY            Note this is SAPRECD if list call            
*                                   and QHIER is set otherwise ACTRECD          
         USING ACTRECD,R2                                                       
         MVC   THISOFF,SPACES                                                   
                                                                                
         CLC   =C'1R',QUNIT        Is it 1R ledger                              
         JNE   AFLT002                                                          
         CLI   BINHIER,4           If person search/list ensure we              
         JNE   AFLT002             only send the person code once               
         MVC   SV1RACT,SPACES      Clear buffer area                            
         LA    R1,SAPKACT-SAPKEY(R2) R1=A(1R account code)                      
         CLI   QSCHTYP,C' '          Search use ACTKEY layout                   
         JNH   *+8                                                              
         LA    R1,ACTKACT-ACTKEY(R2)                                            
         LA    RF,L'ACTKACT        RF=total account length                      
         LLC   RE,LDGAL3           RE=sub-dept account length                   
         LA    R1,0(RE,R1)         R1=A(person code)                            
         SR    RF,RE               RF=length of person code                     
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SV1RACT(0),0(R1)                                                 
         EX    RF,0(RE)                                                         
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    EXITNX              Found record - exit                          
         J     AFLT010                                                          
*                                                                               
* Directory filters                                                             
*                                                                               
AFLT002  CLC   =C'SJ',QUNIT        Is it SJ ledger                              
         JNE   AFLT010                                                          
         MVC   CSVKEY3,IOKEY                                                    
         CLI   QLOCK,NOQ           Only filter if we don't want                 
         JNE   AFLT010             locked accounts                              
         GOTOR BUFCPL                                                           
         GOTOR CHKCPL              Check client or product is locked            
         JNE   EXITNX              and this acc should be excluded              
         MVC   IOKEY,CSVKEY3                                                    
*                                                                               
AFLT010  TM    ACTKSTAT,ACTSDELT                                                
         JNZ   EXITNX                                                           
* CLOSED FILTER                                                                 
         CLI   QCLOSED,C'O'        only?                                        
         JNE   AFLT011                                                          
         TM    ACTKSTAT,ACTSCLOS   ONLY CLOSED ACCOUNTS                         
         JZ    EXITNX                                                           
*                                                                               
AFLT011  DS    0H                                                               
         CLI   QCLOSED,YESQ                                                     
         JE    AFLT014                                                          
         TM    ACTKSTAT,ACTSCLOS   DON'T WAN'T CLOSED THEN                      
         JNZ   EXITNX                                                           
                                                                                
* LOCKED FILTER - DIRECTORY SECTION                                             
AFLT014  DS    0H                                                               
*&&UK                                                                           
         CLI   QLOCK,C'O'          ONLY                                         
         JNE   AFLT016                                                          
         TM    ACTKSTAT,ACTSLOCK   ONLY LOCKED ACCOUNTS                         
         JZ    EXITNX                                                           
         J     AFLT018                                                          
*                                                                               
AFLT016  CLI   QLOCK,NOQ           BLANK IS NO                                  
         JE    *+12                                                             
         CLI   QLOCK,C' '          BLANK MEANS NO                               
         JH    AFLT018                                                          
         TM    ACTKSTAT,ACTSLOCK   IF NO OR BLANK THEN SKIP LOCKED              
         JNZ   EXITNX                                                           
*&&                                                                             
* DRAFT FILTER                                                                  
AFLT018  CLI   QDRAFT,C'O'                                                      
         JNE   AFLT020                                                          
         TM    ACTKSTAT,ACTSDRFT                                                
         JZ    EXITNX                                                           
         J     AFLT022                                                          
*                                                                               
AFLT020  CLI   QDRAFT,C'N'                                                      
         JE    *+12                                                             
         CLI   QDRAFT,C' '                                                      
         JH    *+12                                                             
         TM    ACTKSTAT,ACTSDRFT                                                
         JNZ   EXITNX                                                           
                                                                                
* Office code filter                                                            
                                                                                
AFLT022  GOTOR GETOFFD             Check where office is in directory           
         JNE   AFLT023                                                          
         CLI   CUACCS,0            Limit access logon                           
         JE    AFLT023             No - skip office filtering                   
         GOTOR OFFFLT,0            Yes then filter office here                  
         JNE   EXITNX                                                           
                                                                                
* DEPARTMENT FILTER (2P)                                                        
                                                                                
AFLT023  CLC   QDEPT,SPACES                                                     
         JNH   AFLT025                                                          
         OC    DDPTPOS,DDPTPOS                                                  
         JZ    AFLT025                                                          
         LLC   RF,DDPTPOS                                                       
         SHI   RF,1                                                             
         LA    R1,ACTKACT                                                       
         CLI   QHIER,C' '                                                       
         JNH   *+8                                                              
         LA    R1,IOKEY+SAPKACT-SAPRECD                                         
         AR    RF,R1                                                            
*        CLC   =C'2P',QUNIT                                                     
*        JE    AFLT024                                                          
*                                                                               
*        LA    RE,QDEPT            ==2D ACCOUNT                                 
*        CLI   DOFFPOS,1                                                        
*        JNE   AFLT024                                                          
*        AHI   RE,1                                                             
*        TM    CPYSTAT4,CPYSOFF2                                                
*        JZ    AFLT024                                                          
*        AHI   RE,1                A(DEPT) IN 2D                                
*                                                                               
AFLT024  LLC   R3,DDPTLEN                                                       
         SHI   R3,1                                                             
         CLC   0(0,RF),QDEPT                                                    
         EX    R3,*-6                                                           
         JNE   EXITNX                                                           
*                                                                               
* Approver exclusions post-filtering                                            
*                                                                               
AFLT025  CLI   QSCHTYP,C' '          If list call do approver                   
         JH    AFLT027                 filtering here if GAPLST                 
         TM    RUNFLAGS,RFAPPRQ         included approver entries               
         JZ    AFLT027                                                          
         LA    R1,ACTKACT            Point to correct part of key               
         CLI   QHIER,C' '             for account                               
         JNH   *+8                                                              
         LA    R1,IOKEY+SAPKACT-SAPRECD                                         
         GOTOR APPFLT                                                           
         JNE   AFLTNO                                                           
*                                                                               
AFLT027  CLI   QMED,C' '           Apply media filter                           
         JNH   AFLT028                                                          
         LLC   RF,LDGAL2                                                        
         JZ    AFLT028                                                          
         LA    RE,ACTKACT                                                       
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JNH   AFLT028                                                          
         CLC   QMED,0(RE)                                                       
         JNE   EXITNX                                                           
*                                                                               
AFLT028  CLI   QSJALL,C'Y'         test 'All SJ'                                
         JE    AFLT030                                                          
         GOTOR GPLMEDF             FILTER BY MEDIA (GAPLST)                     
         JNE   AFLTNO                                                           
*                                                                               
* Read account record                                                           
*                                                                               
AFLT030  CLI   QSCHTYP,C' '          Search already has record                  
         JH    AFLT040                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
AFLT040  L     R2,AIO2                                                          
                                                                                
*&&US                                                                           
         CLC   =C'1N',QUNIT        is it non client account ?                   
         JE    AFLT042                                                          
         CLC   PRODUL,QUNIT        or client rec?                               
         JNE   AFLT044                                                          
         LLC   RF,LDGAL1                                                        
         LA    RE,ACTKACT                                                       
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JH    AFLT044             prod/job                                     
AFLT042  TM    SCPYEL+CPYSTATB-CPYELD,CPYXLC1N                                  
         JZ    AFLT044                                                          
         TM    ACTKSTAT,ACTSLOCK   Exclude Locked Accounts                      
         JNZ   EXITNX                                                           
                                                                                
AFLT044  CLI   QLOCK,C'O'          ONLY                                         
         JNE   AFLT046                                                          
         TM    ACTRSTAT,ACTSLOCK   ONLY LOCKED ACCOUNTS                         
         JZ    EXITNX                                                           
         J     AFLT050                                                          
*                                                                               
AFLT046  CLI   QLOCK,NOQ           BLANK IS NO                                  
         JE    *+12                                                             
         CLI   QLOCK,C' '          BLANK MEANS NO                               
         JH    AFLT050                                                          
         TM    ACTKSTAT,ACTSLOCK   IF NO OR BLANK THEN SKIP LOCKED              
         JNZ   EXITNX                                                           
*&&                                                                             
* File filters                                                                  
                                                                                
* Office code filter - 2nd attempt if we need to get it from master rec         
                                                                                
AFLT050  CLC   THISOFF,SPACES                                                   
         JH    AFLT060             ALREADY FOUND AND FILTERED                   
         GOTOR GETOFFM             CHECK IF OFFICE IN MASTER REC                
         JNE   AFLT060                                                          
         GOTOR OFFFLT,ACTKEY       YES, FILTER HERE                             
         JNE   EXITNX                                                           
         J     AFLT060                                                          
                                                                                
* Filter on elements                                                            
                                                                                
AFLT060  MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
         LA    R3,ACTRFST                                                       
         USING EMPELD,R3                                                        
AFLT065  DS    0H                                                               
         CLI   EMPEL,0                                                          
         JE    AFLT130                                                          
*        CLI   EMPEL,EMPELQ                                                     
*        JE    AFLT080                                                          
         CLI   EMPEL,RSTELQ                                                     
         JE    AFLT085                                                          
         CLI   EMPEL,JOBELQ                                                     
         JE    AFLT090                                                          
         CLI   EMPEL,RATEVATQ                                                   
         JE    AFLT110                                                          
         CLI   EMPEL,SPAELQ                                                     
         JE    AFLT120                                                          
AFLT070  LLC   RE,EMPLN                                                         
         AR    R3,RE                                                            
         J     AFLT065                                                          
                                                                                
*  If empel, check staff employment status                                      
                                                                                
AFLT080  CLC   =C'1R',QUNIT       For staff, only want active                   
         JNE   AFLT070                                                          
         LLC   RF,LDGAL3                                                        
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '         Person account?                               
         JE    AFLT070                                                          
         CLI   EMPCSTAT,0                                                       
         JE    AFLT070                                                          
         CLI   QSCHTYP,C' '                                                     
         JNH   EXITNX                                                           
         CLI   EMPCSTAT,EMPCTRM   But search allows terminated too              
         JE    AFLT070                                                          
         J     EXITNX                                                           
*                                                                               
AFLT085  GOTOR AFLTRST                                                          
         JNE   EXITNX                                                           
         J     AFLT070                                                          
*                                                                               
         USING JOBELD,R3                                                        
AFLT090  DS    0H                                                               
*&&UK                                                                           
         CLI   QAPPL,QAPPEST                                                    
         JE    AFLT097                                                          
         CLI   QAPPL,QAPPEJB                                                    
         JNE   AFLT100                                                          
*&&                                                                             
*&&US                                                                           
         TM    JOBSTA2,JOBSREJ                                                  
         JO    EXITNX                                                           
         CLI   QAPPL,QAPPEJB                                                    
         JE    AFLT095                                                          
         CLI   QAPPL,QAPPEST                                                    
         JNE   AFLT100                                                          
AFLT095  DS    0H                                                               
*&&                                                                             
AFLT097  CLI   JOBLN,JOBLN3Q       FOR ESTIMATES RETURN ONLY JOBS               
         JL    EXITN                       SET UP FOR BRO EST                   
         TM    JOBSTA1,JOBSMCSE                                                 
         JZ    EXITN                                                            
*                                                                               
AFLT100  CLI   QMASTJ,NOQ          Don't want master jobs?                      
         JNE   AFLT110                                                          
         TM    JOBSTA2,JOBSMST     No master then skip if master job            
         JNZ   EXITNX                                                           
         J     AFLT070                                                          
*                                                                               
         USING RATELD,R3                                                        
AFLT110  CLC   =C'SG',QUNIT        Pass VAT rate on SG                          
         JNE   AFLT070                                                          
         MVI   BYTE1,YESQ                                                       
         J     AFLT070                                                          
*                                                                               
         USING SPAELD,R3                                                        
AFLT120  CLI   SPATYPE,SPATMJOB    Does it have a master job                    
         JNE   AFLT070                                                          
         CLI   QMASTJ,YESQ         Want master jobs and those unlinked          
         JNE   AFLT070                                                          
         J     EXITNX              Not unlinked so remove                       
         DROP  R3                                                               
                                                                                
* End of element loop                                                           
                                                                                
AFLT130  CLI   QVTYPE,C' '                                                      
         JNH   AFLT140                                                          
         CLI   BYTE1,YESQ                                                       
         JNE   EXITNX              IF VAT FILTER MUST HAVE RATEL                
*                                                                               
AFLT140  CLC   =C'1R',QUNIT        Is it 1R ledger                              
         JNE   AFLT150                                                          
         CLI   BINHIER,4           If person search/list ensure we              
         JNE   AFLT150             only send the person code once               
         MVC   SV1RACT,SPACES      Clear buffer area                            
         L     RF,AIO2                                                          
         LA    R1,ACTKACT-ACTKEY(RF) R1=A(1R account code)                      
         LA    RF,L'ACTKACT        RF=total account length                      
         LLC   RE,LDGAL3           RE=sub-dept account length                   
         LA    R1,0(RE,R1)         R1=A(person code)                            
         SR    RF,RE               RF=length of person code                     
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SV1RACT(0),0(R1)                                                 
         EX    RF,0(RE)                                                         
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)  Add to buffer                   
                                                                                
AFLT150  CLC   PRODUL,QUNIT                                                     
         JNE   AFLTYES                                                          
*                                                                               
         OI    RUNFLAGS,RFNOSEQ    Getopt breaks read sequence                  
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    AFLT160                                                          
         CLI   QSJALL,C'Y'         Test 'All SJ'                                
         JE    AFLTYES             Reduce processing,else req times out         
                                                                                
AFLT160  GOTOR AFLTGOP                                                          
         JNE   EXITNX                                                           
*                                                                               
AFLTYES  J     EXITY                                                            
*                                                                               
AFLTNO   DS    0H                                                               
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER RSTEL SETTINGS ON AN ACCOUNT REC                                       
* ENTRY: R3=A(RSTEL)                                                            
***********************************************************************         
         USING RSTELD,R3                                                        
AFLTRST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QSCHTYP,C' '                                                     
         JNH   AFRST10                                                          
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JH    EXITNX                                                           
*                                                                               
AFRST10  CLI   QLOCK,C'Y'         INCLUDE LOCKED                                
         JE    AFRST14                                                          
         CLI   QLOCK,C'A'         Show all accounts                             
         JE    AFRST14                                                          
         CLI   QLOCK,C'O'                                                       
         JE    AFRST12                                                          
         TM    RSTSTAT1,RSTSACIL  EXCLUDE LOCKED                                
         JZ    AFRST14                                                          
         J     EXITNX                                                           
*                                                                               
AFRST12  TM    RSTSTAT1,RSTSACIL  LOCKED AND INACTIVE ONLY                      
         JZ    EXITNX                                                           
*                                                                               
AFRST14  CLI   QACTV,YESQ         Do they only want active accounts             
         JNE   AFRST16            No                                            
         CLC   RSTTDATE,DA_MIN5P  Yes - check there has been activity           
         JL    EXITNX                    in the last 5 years                    
         J     AFRST17                                                          
*                                                                               
AFRST16  CLI   QACTV,NOQ          Do they only want inactive accounts           
         JNE   AFRST17            No - view all                                 
         CLC   RSTTDATE,DA_MIN5P  Yes - check there has been no                 
         JNL   EXITNX               activity in the last 5 years                
                                                                                
AFRST17  CLI   QCLOSED,C'Y'       INCLUDE CLOSED                                
         JE    AFRST20                                                          
         CLI   QCLOSED,C'O'                                                     
         JE    AFRST18                                                          
         TM    RSTSTAT1,RSTSACIC  EXCLUDE CLOSED                                
         JNZ   EXITNX                                                           
         J     AFRST20                                                          
*                                                                               
AFRST18  TM    RSTSTAT1,RSTSACIC   LOCKED ONLY                                  
         JZ    EXITNX                                                           
*                                                                               
AFRST20  DS    0H                                                               
         CLI   RSTLN,RSTLN3Q       CHECK ELEMENT RIGHT LENGTH                   
         JL    AFRST55                                                          
         CLI   QAPPL,QAPPORD       ARE WE IN ORDERS?                            
         JNE   AFRST30                                                          
         TM    RSTLSTAT,RSTLSORQ                                                
         JZ    AFRST55             NOT LOCKED                                   
         J     EXITNX              YES THEN SKIP                                
*                                  CHECK WHETEHR JOB LOCKED FROM EST            
AFRST30  CLI   QAPPL,QAPPEST       ARE WE IN ESTIMATES?                         
         JE    *+12                                                             
         CLI   QAPPL,QAPPEJB       ARE WE IN ESTIMATES JOB BILLING              
         JNE   AFRST40                                                          
         TM    RSTLSTAT,RSTLSESQ                                                
         JNZ   EXITNX                                                           
*                                                                               
AFRST40  CLI   QAPPL,QAPPTIM       ARE WE IN TIMESHEETS?                        
         JNE   AFRST50                                                          
         CLI   QLOCK,C'A'          show all accounts                            
         JE    AFRST42                                                          
         CLI   QLOCK,C'T'          Include Locked from T/S ?                    
         JE    *+12                                                             
         TM    RSTLSTAT,RSTLSTIQ                                                
         JNZ   EXITNX              LOCKED FROM T/S                              
         CLC   PRODUL,QUNIT                                                     
         JNE   AFRST42                                                          
         TM    RSTSTAT5,RSTSNOTS                                                
         JNZ   EXITNX              LOCKED FROM T/S                              
*                                                                               
AFRST42  DS    0H                                                               
         CLI   QFUTIM,YESQ       DO WE ONLY WANT FUTURE TIME ACCOUNTS           
         JNE   AFRST55                                                          
         TM    RSTSTAT7,RSTSFUTM                                                
         JZ    EXITNX                                                           
*                                                                               
AFRST50  DS    0H                                                               
         CLI   QAPPL,QAPPEXP       ARE WE IN EXPENSES?                          
         JNE   AFRST55                                                          
         TM    RSTLSTAT,RSTLSEXQ   LOCKED FROM EXTERNAL POSTINGS?               
         JZ    AFRST55                                                          
         J     EXITNX                                                           
*                                                                               
AFRST55  CLI   QVTYPE,C' '                                                      
         JNH   AFRST60                                                          
         CLI   QVTYPE,C'I'                                                      
         JNE   AFRST56                                                          
         TM    RSTSTAT1,RSTSIVAT                                                
         JZ    EXITNX              Skip account                                 
         J     AFRST60                                                          
                                                                                
AFRST56  TM    RSTSTAT1,RSTSIVAT                                                
         JNZ   EXITNX              skip account                                 
         J     AFRST60                                                          
*                                                                               
AFRST60  DS    0H                                                               
*                                                                               
AFRST80  DS    0H                                                               
*                                                                               
AFRST90  DS    0H                                                               
         J     EXITY               A/C STILL WANTED                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER ACCOUNT BY GETOPT SETTINGS                                             
* ENTRY: ACCOUNT KEY IN IOKEY                                                   
***********************************************************************         
         USING RSTELD,R3                                                        
AFLTGOP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GOBLOCK,R3                                                       
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US USES 1ST EXTENSION BLOCK                  
         MVC   GOABEXT,AGOBBLCK    UK USES 2ND EXTENSION BLOCK                  
*                                                                               
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOACOMP,AIO8                                                     
         MVC   GOALEDG,AIO7                                                     
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+10                                                             
         MVC   GOACOVL,VCOVAIL                                                  
         L     RF,ACOMFACS                                                      
         MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
*                                                                               
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LA    R4,ACTKACT                                                       
*                                                                               
AFGOP10  LLC   RF,LDGAL1                                                        
         SHI   RF,1                                                             
         MVC   GOSELCLI(0),ACTKACT                                              
         EX    RF,*-6                                                           
         LA    RE,1(R4)                                                         
         AR    RE,RF                                                            
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         LLC   R0,LDGAL1                                                        
         SR    RF,R0                                                            
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(0),0(RE)                                                
         EX    RF,*-6                                                           
         AR    RE,RF                                                            
         AHI   RE,1                                                             
         LLC   RF,LDGAL3                                                        
         SHI   RF,1                                                             
         LLC   R0,LDGAL2                                                        
         SR    RF,R0                                                            
         MVC   GOSELJOB(0),0(RE)                                                
         EX    RF,*-6                                                           
         CLI   QMED,C' '                                                        
         JNH   *+10                                                             
         MVC   GOSELMED,QMED                                                    
         CLI   QWORKC,C' '                                                      
         JNH   *+14                                                             
         MVC   GOSELWC,QWORKC                                                   
         MVI   GOANYWC,YESQ                                                     
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
*                                                                               
*&&US                                                                           
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
*&&                                                                             
         USING ACTRECD,R2                                                       
         CLI   QAPPL,QAPPTIM       CHECK TIME APPLICATION                       
         J     AFGOP30          Don't exclude jobs if JBT=N, as 1R PER2         
*                               may have Force Job=Y. Aura will handle.         
*        JNE   AFGOP30                                                          
*        LLC   RF,LDGAL2           CHECK JOB LEVEL REQUEST                      
*        LA    RE,0(R4)                                                         
*        AR    RE,RF                                                            
*        CLI   0(RE),C' '                                                       
*        JNH   AFGOP30                                                          
*        CLI   GOTNOJOB,NOQ        JOB INPUT ALLOWED ON TIME?                   
*        JE    EXITNX              NO                                           
*                                                                               
AFGOP30  DS    0H                                                               
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         CLI   QAPPL,QAPPORD       ORDERS                                       
         JE    AFGOP40                                                          
         CLI   QAPPL,QAPPEXP       Expenses                                     
         JE    AFGOP40                                                          
         CLI   QAPPL,QAPPINV       Invoices                                     
         JNE   AFGOP50                                                          
AFGOP40  CLI   QBILL,NOQ           Is the entry billable within module          
         JNE   AFGOP50             Yes                                          
         CLI   GOBILO,YESQ         No - is it billable only                     
         JE    EXITNX              Yes - skip this account                      
*                                                                               
AFGOP50  CLI   QAPPL,QAPPEST       Estimates                                    
         JE    *+12                                                             
         CLI   QAPPL,QAPPEJB       Estimates for job list                       
         JNE   EXITY                                                            
         TM    ACTRSTAT,ACTSDRFT   Is account draft                             
         JZ    EXITY                                                            
         CLI   GOAEDT,NOQ                                                       
         JE    EXITNX                                                           
         J     EXITY               Account still wanted                         
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* MEDIA CODE FILTER - ONLY IF GAPLST FINDS SOME.                                
***********************************************************************         
*                                                                               
GPLMEDF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         CLC   PRODUL,QUNIT                                                     
         JNE   GMEDFYES                                                         
         TM    RUNFLAGS,RFMEDLQ                                                 
         JNZ   GMEDFYES            NO FILTERS                                   
*                                                                               
         LLC   RE,LDGAL2                                                        
         LA    RF,ACTKACT          RF=A(Account code)                           
         CLI   QSCHTYP,C' '        Account code in different position           
         JH    GMEDF010               when list call with QHIER set             
         CLI   QHIER,C' '                                                       
         JNH   GMEDF010                                                         
         LA    RF,SAPKACT-SAPKEY(R2)                                            
                                                                                
GMEDF010 AR    RF,RE               A(JOB CODE)                                  
         CLI   0(RF),C' '                                                       
         JNH   GMEDFYES            CLI/PRODUCT REC - N/A                        
         CLC   LASTMEDC,0(RF)                                                   
         JNE   GMEDF030                                                         
         CLI   LASTMEDI,C'Y'       LOOKED FOR THIS LAST TIME - FIND IT?         
         JE    GMEDFYES            YES                                          
         J     EXITN                                                            
*                                                                               
GMEDF030 DS    0H                                                               
         MVC   LASTMEDC,0(RF)                                                   
         MVI   LASTMEDI,C'N'                                                    
         XC    GAPAREA,GAPAREA                                                  
GAP      USING GAPTABD,GAPAREA                                                  
         MVI   GAP.GAPTDAT1,GAPTT5Q                                             
         MVC   GAP.GAPTCODE(L'ACTKACT),SPACES                                   
         MVC   GAP.GAPTCODE(1),0(RF)                                            
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
GMEDF050 TM    ATSRERRS,TSEEOF                                                  
         JNZ   EXITN                                                            
         CLI   GAP.GAPTDAT1,GAPTT5Q                                             
         JNE   EXITN                                                            
         CLC   GAP.GAPTCODE(1),LASTMEDC                                         
         JNE   EXITN                                                            
         MVI   LASTMEDI,C'Y'                                                    
         DROP  GAP,R2                                                           
*                                                                               
GMEDFYES J     EXITY                                                            
***********************************************************************         
* APPROVER - CHECK INCLUDE/EXCLUDE FOR IOKEY                                    
* ENTRY R1 =A(ACCONT TO FILTER ON)                                              
***********************************************************************         
*                                                                               
APPFLT   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         TM    RUNFLAGS,RFAPPRQ    APPROVER ENTRIES IN GAPLST?                  
         JZ    APPFYES             NOTHING TO FILTER                            
*                                                                               
         LR    R2,R1                                                            
*        LA    R2,IOKEY                                                         
*        USING ACTRECD,R2                                                       
         XC    GAPAREA,GAPAREA                                                  
GAP      USING GAPTABD,GAPAREA                                                  
         MVI   GAP.GAPTDAT1,GAPTT1Q     START AT THE BEGINNING                  
         MVC   GAP.GAPTACT,SPACES                                               
*                                                                               
APPF005  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JZ    *+6                                                              
         DC    H'0'                SHOULD BE SOME RECORDS!                      
         CLI   GAP.GAPTDAT1,GAPTT5Q    LOOK FOR ANYTHING BUT MEDIA              
         JNE   APPF006                                                          
         XC    GAPAREA,GAPAREA                                                  
         MVI   GAP.GAPTDAT1,GAPTT6Q                                             
         MVC   GAP.GAPTACT,SPACES                                               
         J     APPF005                                                          
*                                                                               
APPF006  CLC   GAP.GAPTCODE,SPACES                                              
         JNH   APPFYES             ALL-ACCESS ENTRY                             
         MVC   BYTE1,GAP.GAPTDAT1                                               
*                                                                               
* NOW LOOK FOR INCLUDE/EXCLUDES                                                 
         SR    R3,R3               find lvl of account we're filtering          
         LLC   RF,LDGAL1                                                        
         LA    RE,0(R2)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JNH   APPFYES             LVL1 a/c - no filtering to apply             
*                                                                               
         LLC   RF,LDGAL2                                                        
         JZ    APPFYES             ONE LEVEL, NO EXCLUSIONS POSSIBLE            
         LA    R3,1(R3)                                                         
         LA    RE,0(R2)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JNH   APPF010             LVL2 a/c                                     
*                                                                               
         LLC   RF,LDGAL3                                                        
         JZ    APPF010             NO MORE LEVELS                               
         LA    R3,1(R3)                                                         
         LA    RE,0(R2)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JNH   APPF010             LVL3 a/c                                     
*                                                                               
         CLI   LDGAL4,0                                                         
         JE    APPF010             No more levels...?                           
         LA    R3,1(R3)            LVL4 a/c                                     
*                                                                               
APPF010  XC    GAPAREA,GAPAREA                                                  
         MVC   GAP.GAPTDAT1,BYTE1                                               
         MVC   GAP.GAPTACT,0(R2)                                                
                                                                                
APPF015  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     APPF025                                                          
APPF020  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
                                                                                
APPF025  TM    ATSRERRS,TSEEOF     Have we reached end of buffer                
         JNZ   APPFYES             Yes - no exceptions found                    
         CLC   GAP.GAPTDAT1,BYTE1                                               
         JNE   APPFYES                                                          
*                                                                               
APPF030  LA    RE,LDGAL1                                                        
         AR    RE,R3                                                            
         CLC   GAP.GAPTLEN,0(RE)                                                
         JL    APPF040                                                          
         JH    APPFYES                                                          
         LLC   RF,0(RE)                                                         
         SHI   RF,1                                                             
         CLC   GAP.GAPTACT(0),0(R2)                                             
         EX    RE,*-6                                                           
         JE    APPF070             MATCH, CHECK STATUS                          
*                                                                               
* BULD ENTRY FOR PREVIOUS LEVEL                                                 
*                                                                               
APPF040  LTR   R3,R3                                                            
         JZ    EXITN               NO HIGHER LEVEL FOUND, NO MATCH              
         SHI   R3,1                                                             
         XC    GAPAREA,GAPAREA                                                  
         MVC   GAP.GAPTDAT1,BYTE1                                               
         MVC   GAP.GAPTACT,SPACES                                               
         LA    RE,LDGAL1                                                        
         AR    RE,R3               A(L'PREVIOUS LEVEL)                          
         LLC   RF,0(RE)                                                         
         SHI   RF,1                                                             
         MVC   GAP.GAPTACT(0),0(R2)                                             
         EX    RF,*-6                                                           
         J     APPF015                                                          
*                                                                               
* FOUND A MATCH, CHECK STATUS                                                   
*                                                                               
APPF070  TM    GAP.GAPTSTA,GAPTSEQ                                              
         JO    EXITN                                                            
*                                                                               
APPFYES  J     EXITY                                                            
         DROP  GAP                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK IF OFFICE CODE CAN BE FOUND IN ACCDIR REC                               
* SET THISOFF IF SO                                                             
* ASSUMES ACTKEY, SRCKEY OR SAPKEY                                              
***********************************************************************         
*                                                                               
GETOFFD  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         MVC   THISOFF,SPACES                                                   
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         LA    R3,ACTKACT                                                       
         CLI   SAPKSUB-SAPKEY(R2),SAPKSUBQ                                      
         JNE   *+8                                                              
         LA    R3,SAPKACT-SAPKEY(R2)                                            
         CLI   SRCKSUB-SRCKEY(R2),SRCKWDSQ                                      
         JNE   *+8                                                              
         LA    R3,SRCKACT-SRCKEY(R2)                                            
         CLI   SRCKTYP-SRCKEY(R2),SRCKSQWD                                      
         JNE   *+8                                                              
         LA    R3,SRCKACT-SRCKEY(R2)                                            
*                                                                               
GOFFDOP  DS    0H                                                               
         CLI   LDGAOP,LDGONONE                                                  
         JE    GOFFD1R                                                          
         CLI   LDGAOP,LDGOKEY                                                   
         JH    GOFFDOL                                                          
         LLC   RF,LDGAOP           OFFPOS=1-12                                  
         SHI   RF,1                                                             
         AR    R3,RF                                                            
         J     GOFFDYES                                                         
*                                                                               
GOFFDOL  DS    0H                                                               
         CLI   LDGAOP,LDGOOFLS                                                  
         JE    GOFFDYES            Office list Ledgers have OFFPOS=1            
*                                                                               
GOFFD1R  DS    0H                                                               
         CLC   =C'1R',QUNIT        1R OFFICE ALWAYS IN POS 1                    
         JE    GOFFDYES                                                         
*                                                                               
*                                                                               
GOFFDNO  DS    0H                  OFFICE NOT IN DIRECTORY                      
         J     EXITNX                                                           
*                                                                               
GOFFDYES DS    0H                                                               
         MVC   THISOFF(1),0(R3)    FOUND OFFICE AT R3                           
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    EXITY                                                            
         MVC   THISOFF(2),0(R3)                                                 
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF OFFICE CODE CAN BE FOUND IN ACCMST REC                               
* SET THISOFF IF SO                                                             
* ENTRY: AIO2 HOLDS ACTREC TO PARSE                                             
***********************************************************************         
*                                                                               
GETOFFM  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
LOW      USING ACTRECD,R2                                                       
         USING ACTRECD,R3                                                       
         L     R2,AIO2                                                          
         LR    R3,R2                                                            
         CLC   =C'SJ',QUNIT                                                     
         JNE   GOFFM060                                                         
         USING PPRELD,R1                                                        
         LA    R1,ACTRFST                                                       
         SR    RE,RE                                                            
         MVC   THISOFF,SPACES                                                   
*                                                                               
GOFFM010 DS    0H                                                               
         CLI   PPREL,0                                                          
         JE    GOFFM017                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    GOFFM015                                                         
         IC    RE,PPRLN                                                         
         AR    R1,RE                                                            
         J     GOFFM010                                                         
*                                                                               
GOFFM015 DS    0H                                                               
         MVC   THISOFF,PPRGAOFF                                                 
*        CLC   PPRGAOFF,SPACES                                                  
*        JH    GOFFM025                                                         
***  UPDATE SAVED VALUES                                                        
GOFFM017 DS    0H                                                               
         IC    RE,LDGAL1                                                        
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         CLI   0(RF),C' '          CLIENT REC?                                  
         JH    GOFFM020                                                         
         MVC   CLIOFF,THISOFF      YES, SAVE OFFICE FOR PRODUCTS/JOBS           
         SHI   RE,1                                                             
         MVC   CLIOFCD(0),ACTKACT                                               
         EX    RE,*-6                                                           
         J     EXITY               ALL DONE                                     
*                                                                               
GOFFM020 DS    0H                                                               
         IC    RE,LDGAL2                                                        
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         CLI   0(RF),C' '          PROD REC?                                    
         JH    GOFFM024                                                         
         MVC   PROOFF,THISOFF      YES, SAVE OFFICE FOR JOBS                    
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         MVC   PROOFCD(0),ACTKACT                                               
         EX    RF,*-6                                                           
GOFFM024 CLC   THISOFF,SPACES                                                   
         JH    EXITY                                                            
* NO PPREL/pprgaoff FOUND                                                       
GOFFM025 DS    0H                                                               
         IC    RE,LDGAL1                                                        
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         CLI   0(RF),C' '          PROD REC?                                    
         JNH   EXITY               CLIENT (SHOULD HAVE PPREL)                   
         IC    RE,LDGAL2                                                        
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         CLI   0(RF),C' '                                                       
         JH    GOFFM035            JOB                                          
*                                                                               
GOFFM030 IC    RE,LDGAL1           PRODUCT                                      
         SHI   RE,1                                                             
         CLC   CLIOFCD(0),ACTKACT                                               
         EX    RE,*-6                                                           
         JNE   GOFFM045            CLIENT OFFICE NOT SAVED, READ IT             
         MVC   THISOFF,CLIOFF                                                   
         J     EXITY                                                            
*                                                                               
GOFFM035 DS    0H                  JOB REC, NO OFFICE HERE                      
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         CLC   PROOFCD(0),ACTKACT                                               
         EX    RE,*-6                                                           
         JNE   GOFFM040            PROD OFFICE NOT SAVED, GO READ               
         MVC   THISOFF,PROOFF                                                   
         CLC   THISOFF,SPACES      PRODUCT READ BEFORE, BUT HAS NO OFF          
         JNH   GOFFM030            CHECK CLIENT                                 
         J     EXITY                                                            
*                                                                               
GOFFM040 J     GOFFM045                                                         
         CLI   QSJALL,C'Y'         Test 'All SJ' request                        
         JNE   GOFFM045            No - so assume higher lvls not read          
         CLI   QSCHTYP,C' '                                                     
         JH    GOFFM045                                                         
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         CLC   CLIOFCD(0),ACTKACT  Else should have client offic saved          
         JNE   GOFFM045            ?                                            
         MVC   THISOFF,CLIOFF      Yes - set office from client                 
         J     EXITY                                                            
*                                                                               
* READ A HIGHER LEVEL (RE=MVC LENGTH)                                           
*                                                                               
GOFFM045 MVC   IOKEY,SPACES                                                     
         LA    R3,IOKEY                                                         
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QUNIT                                                    
         MVC   ACTKLDG,QLEDG                                                    
         MVC   ACTKACT(0),LOW.ACTKACT                                           
         EX    RE,*-6                                                           
         MVC   CSVKEY2(L'ACTKEY),ACTKEY                                         
*                                                                               
         L     RE,AIO4                                                          
         MVC   0(L'ACTKEY,RE),ACTKEY                                            
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    GOFFM050                                                         
*                                                                               
         OI    RUNFLAGS,RFNOSEQ    GETOPT BREAKS READ SEQ                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SAVE SO WE DON'T READ AGAIN                                                   
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)                                  
*                                                                               
GOFFM050 L     R3,AIO4                                                          
         LA    R1,ACTRFST                                                       
         SR    RE,RE                                                            
         J     GOFFM010            FIND PPREL AGAIN                             
*                                                                               
*  NOT SJ, NO FILTER                                                            
*                                                                               
GOFFM060 DS    0H                                                               
         J     EXITNX                                                           
         DROP  R3,LOW                                                           
         EJECT                                                                  
***********************************************************************         
* OFFICE FILTERING (OFFAL AND REQUEST FILTER)                                   
* ENTRY: THISOFF IS OFFICE TO CHECK                                             
* R1 = A(ACTREC) CHECK LIDTPOFC                                                 
***********************************************************************         
*                                                                               
OFFFLT   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         LR    R2,R1                                                            
         CLC   QOFFC,SPACES                                                     
         JNH   OFFFLT20                                                         
         CLC   QOFFC,THISOFF                                                    
         JE    EXITY                                                            
         J     OFFFLT30                                                         
*                                                                               
OFFFLT20 DS    0H                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,THISOFF    validate current office                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    EXITY                                                            
*  NO MATCH ON THIS RECORD - LOOK FOR LIDTPOFC?                                 
OFFFLT30 DS    0H                                                               
         LTR   R2,R2                                                            
         JZ    EXITNX              NO RECORD TO CHECK                           
         USING ACTRECD,R2                                                       
         CLC   PRODUL,ACTKUNT                                                   
         JNE   EXITNX              NOT SJ                                       
         GOTOR ACHKOFF,ACTKEY                                                   
         JNE   EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* IF OFFPOS=1, WE SHOULD APPLY ANY OFFICE FILTER TO KEYFRAGS BEFORE             
* WE READ THE DIRECTORY. IF ACT ARRAY EMPTY, WE CAN PUT THE OFFICE              
* IN THAT SO ACBUILD DOES THE HARD WORK.                                        
* FOR 2P WE CAN ALSO USE THE DEPARTMENT CODE IN THIS WAY, IF IT                 
* FOLLOWS THE OFFICE CODE IMMEDIATELY OR IS 1ST IN KEY                          
*                                                                               
* IF THERE IS AN ARRAY, WE'LL ASSUME THE CALLER KNOWS WHAT IT'S DOING           
*                                                                               
***********************************************************************         
SETOPOS  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         CLI   QACTIND,0           anything in array?                           
         JE    SETOP010            no                                           
         ICM   RE,7,QAACTS         Indicator not always reliable?               
         LTR   RE,RE                  check array content explicitly.           
         JZ    SETOP010            empty array                                  
         USING LW_D,RE                                                          
         CLC   LW_DATA2(L'ACTKACT),SPACES                                       
         JH    EXITY               Have filter value, trust the caller.         
         MVI   QACTIND,0           Tidy up.                                     
         XC    QAACTS,QAACTS                                                    
         DROP  RE                                                               
SETOP010 MVC   TEMP2,SPACES                                                     
         LA    RF,TEMP2                                                         
         CLI   LDGAOP,1                                                         
         JE    SETOP030                                                         
*                                                                               
         CLC   QDEPT,SPACES        OFFPOS =/= 1, DEPT POS =1? (2P)              
         JNH   EXITY               NO DEPT FILTER                               
         CLI   DDPTPOS,1                                                        
         JNE   EXITY               DEPT POS =/= 1                               
         LA    R3,QDEPT            QDEPT IS 2D ACC, FIND DEPT CODE              
*        CLI   DOFFPOS,1                                                        
*        JNE   SETOP020                                                         
*        AHI   R3,1                                                             
*        TM    CPYSTAT4,CPYSOFF2                                                
*        JZ    SETOP020                                                         
*        AHI   R3,1                                                             
         LLC   RE,DDPTLEN                                                       
         SHI   RE,1                                                             
         MVC   TEMP2(0),0(R3)                                                   
         EX    RE,*-6                                                           
SETOP020 DS    0H                                                               
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,TEMP2),QACTIND,2,LP_D                   
         J     EXITY               STRUCTURE IS U/L/DEPT                        
*                                                                               
* OFFPOS=1                                                                      
*                                                                               
SETOP030 DS    0H                                                               
         CLC   QOFFC,SPACES                                                     
         JNH   EXITY                                                            
*                                                                               
         MVC   TEMP2(L'QOFFC),QOFFC  BUILD KEY FROM OFFICE                      
*                                                                               
         CLC   QDEPT,SPACES        IF WE HAVE A DEPT                            
         JNH   SETOP050                                                         
         CLI   DDPTPOS,1                                                        
         JNE   *+6                                                              
         DC    H'0'                BAD SETUP, OFFPOS=DEPT POS                   
*                                                                               
         LA    RE,2                If  1 char office position 2                 
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LA    RE,3                If 2 char office position 3                  
         CLM   RE,1,DDPTPOS        Dept immediately follows office              
         JNE   SETOP050            No add office only                           
         SHI   RE,1                                                             
         LA    R3,QDEPT            QDEPT is dept code                           
*        CLI   DOFFPOS,1                                                        
*        JNE   SETOP040                                                         
*        AR    R3,RE                                                            
*                                                                               
SETOP040 DS    0H                  MOVE DEPT INTO TEMP2 AFTER OFFICE            
         LA    RE,TEMP2(RE)                                                     
         LLC   RF,DDPTLEN                                                       
         SHI   RF,1                                                             
         MVC   0(0,RE),0(R3)                                                    
         EX    RF,*-6                                                           
*                                                                               
SETOP050 DS    0H                  STRUCTURE IS U/L/OFF(/DEPT)/???              
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,TEMP2),QACTIND,1,LP_D                   
*        MVC   QOFFC,SPACES                                                     
         CLI   QACTIND,0                                                        
         JNE   EXITY                                                            
         MVI   QACTIND,2                                                        
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET GAP SETTINGS FROM RSTEL OR HIGHER LEVELS                                  
* ENTRY - LP_AINP IS A(RSTEL)                                                   
*         AIO2 HOLDS SUPPLIER RECORD                                            
* EXIT  - TEMP2/B1 = GAP IN USE Y/N                                             
*         TEMP2/B2 = ACKNOWLEDGE/QUERY Y/N                                      
***********************************************************************         
* NOTE - In UK GAP in use setting can be switched on at high level              
*        and switched off at a lower level, e.g.                                
*        SVA - GAP set in use                                                   
*        SVABB - GAP set not in use                                             
*        This allows suppliers to individually switched off for GAP             
***********************************************************************         
         USING RSTELD,R1                                                        
GETGAP   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R1,LP_AINP                                                       
         MVC   TEMP2(2),SPACES                                                  
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    GETGAP20                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   TEMP2,YESQ                                                       
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   TEMP2+1,YESQ                                                     
*                                                                               
         CLI   TEMP2,C'Y'                                                       
         JE    EXITY               ALL SET AT THIS LEVEL                        
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQN                                                
         JZ    *+8                                                              
         MVI   TEMP2,NOQ                                                        
         TM    RSTSTAT7,RSTGAPQY   QUERY                                        
         JZ    *+8                                                              
         MVI   TEMP2,YESQ                                                       
                                                                                
         TM    RSTSTAT7,RSTGAPAN                                                
         JZ    *+8                                                              
         MVI   TEMP2+1,NOQ                                                      
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE                                  
         JZ    *+8                                                              
         MVI   TEMP2+1,YESQ                                                     
         CLI   TEMP2,C' '                                                       
         JH    EXITY                                                            
*&&                                                                             
*                                                                               
*                                                                               
GETGAP20 DS    0H                  CHECK FOR SETTINGS AT HIGHER LVL             
         GOTOR SUPGAP,DMCB,AIO2,TEMP2,TEMP2+1                                   
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Read down supplier levels to get settings                           *         
* ON NTRY P1=A(supplier account)                                      *         
*         P2,P3=A(GAP/ACKNOWLEDGE SETTINGS)                           *         
* USES AIO4                                                                     
***********************************************************************         
OPT      USING OB_D,ELEMENT                                                     
         USING ACTRECD,R2                                                       
SUPGAP   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         XC    ELEMENT,ELEMENT     Clear work area                              
         USING OB_D,ELEMENT                                                     
         LM    R2,R4,0(R1)                                                      
*        CLI   0(R3),YESQ          Is the GAP set at the lowest level           
*        JNE   SUPGAP10            No - read higher levels                      
*        CLI   0(R4),YESQ                                                       
*        JE    EXITY               Yes - nothing to do                          
*                                                                               
SUPGAP10 DS    0H                                                               
         CLC   =C'SX',ACTKULA      Only needed for suppliers                    
         JE    SUPGAP15                                                         
         CLC   =C'SV',ACTKULA                                                   
         JE    SUPGAP15                                                         
         CLC   =C'ST',ACTKULA                                                   
         JE    SUPGAP15                                                         
         CLC   =C'SF',ACTKULA                                                   
         JNE   EXITY                                                            
*                                                                               
SUPGAP15 DS    0H                                                               
         CLI   LDGAL1,L'ACTKACT                                                 
         JE    EXITY                                                            
         LA    R0,4                                                             
         LA    R6,LDGAL4                                                        
*                                                                               
SUPGAP20 DS    0H                                                               
         CLI   0(R6),0                                                          
         JE    SUPGAP25                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JE    SUPGAP25                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LA    RF,ACTKULA(RF)      FIND LENGTH OF ACCOUNT                       
         CLI   0(RF),C' '                                                       
         JH    SUPGAP30                                                         
SUPGAP25 SHI   R6,1                                                             
         JCT   R0,SUPGAP20                                                      
         J     EXITY                                                            
*                                                                               
K        USING ACTRECD,IOKEY                                                    
SUPGAP30 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         LLC   RF,0(R6)                                                         
         AHI   RF,1                +2 FOR U/L, -1 FOR EX                        
         BASR  RE,0                                                             
         MVC   K.ACTKULA(0),ACTKULA                                             
         EX    RF,0(RE)                                                         
         L     RF,AIO4                                                          
         MVC   0(L'ACTKEY,RF),K.ACTKEY                                          
* CHECK WE ALREADY HAVE THIS REC IN BUFFER                                      
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    SUPGAP50                                                         
*                                                                               
* NOT IN BUFFER, GO GET                                                         
SUPGAP40 DS    0H                                                               
         OI    RUNFLAGS,RFNOSEQ    GETOPT BREAKS READ SEQ                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SAVE SO WE DON'T READ AGAIN                                                   
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)                                  
*                                                                               
*                                                                               
SUPGAP50 DS    0H                                                               
         GOTOR GETELA,DMCB,('RSTELQ',AIO4)                                      
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    SUPGAP25                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   0(R3),YESQ                                                       
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
*&&                                                                             
*                                                                               
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQN                                                
         JZ    *+8                                                              
         MVI   0(R3),NOQ                                                        
         TM    RSTSTAT7,RSTGAPQY   QUERY                                        
         JZ    *+8                                                              
         MVI   0(R3),YESQ                                                       
                                                                                
         TM    RSTSTAT7,RSTGAPAN                                                
         JZ    *+8                                                              
         MVI   0(R4),NOQ                                                        
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE                                  
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
*&&                                                                             
*&&US                                                                           
         CLI   0(R3),YESQ          GO BACK ANOTHER LVL IF STILL SOME            
         JNE   *+12                                         N'S                 
         CLI   0(R4),YESQ                                                       
         JE    EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   0(R3),C' '          GO BACK ANOTHER LVL IF STILL SOME            
         JNH   *+12                                         N'S                 
         CLI   0(R4),C' '                                                       
         JH    EXITY                                                            
*&&                                                                             
         J     SUPGAP25                                                         
*                                                                               
         DROP  K,R1,R2,OPT                                                      
*                                                                               
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Read for production media record                                    *         
***********************************************************************         
*                                                                               
GETMED   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         CLI   QAPPL,QAPPORD       IS IT ORDERS MODULE                          
         JNE   EXITY               NO - NOT INTERESTED                          
         CLC   PRODUL,QUNIT                                                     
         JNE   EXITY                                                            
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
*                                                                               
         LA    RF,ACTKACT                                                       
         LLC   RE,LDGAL2                                                        
         AR    RF,RE                                                            
         LLC   R1,LDGAL3                                                        
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         JM    EXITY                                                            
         BASR  RE,0                                                             
         CLC   0(0,RF),SPACES      HAVE WE GOT A JOB                            
         EX    R1,0(RE)                                                         
         JNH   EXITY               NO                                           
                                                                                
         USING PMDRECD,R3                                                       
         LA    R3,IOKEY            READ 1ST CHAR FOR MEDIA CODE                 
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(RF)                                                    
         CLC   PMDKMED,DSAVPMD     HAVE WE READ MEDIA BEFORE                    
         JE    EXITY               YES                                          
         MVC   DSAVPMD,PMDKMED    NO - SAVE NEW MEDIA                           
         OI    RUNFLAGS,RFNOSEQ    GETOPT BREAKS READ SEQ                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO4                                                          
         LA    R3,PMDRFST                                                       
         USING PMDELD,R3                                                        
                                                                                
GETMED02 CLI   PMDEL,PMDELQ                                                     
         JE    GETMED04                                                         
         CLI   PMDEL,0                                                          
         JE    GETMED08                                                         
         LLC   R0,PMDLN                                                         
         AR    R3,R0                                                            
         J     GETMED02                                                         
                                                                                
GETMED04 CLI   PMDLN,PMDLN2Q                                                    
         JL    GETMED06                                                         
         MVC   INCSUSAC,PMDCOM2+1   Set account                                 
                                                                                
GETMED06 CLC   INCSUSAC,SPACES                                                  
         JH    EXITY                                                            
         MVC   INCSUSAC,PMDCOMU1                                                
                                                                                
GETMED08 DS    0H                                                               
         CLC   INCSUSAC,SPACES                                                  
         JH    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$MIACC)                                           
         J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Read for Studio settings                                            *         
***********************************************************************         
*                                                                               
GETSTU   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   STUFLAG,GOSTUDIO   Studio? Y/N                                   
         MVC   STUTYPE,GOTYPE     Studio Type                                   
         J     EXITY                                                            
         DROP  R3                                                               
*----------------------------------------------------------------------         
GETSTU2  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RE,LDGAL2                                                        
         LA    RF,ACTKACT(RE)                                                   
         CLI   0(RF),C' '         Job level account?                            
         JNH   GETSTU2A           No, use Opt Maint settings                    
         GOTOR GETELA,DMCB,('LNKELQ',AIO2)                                      
         JNE   GETSTU2B                                                         
         USING LNKELD,R1                                                        
         MVC   STUTYPE,LNKSTUD    Studio Type                                   
         J     GETSTU2B                                                         
                                                                                
GETSTU2A MVC   STUTYPE,GOTYPE     Studio Type                                   
GETSTU2B MVC   STUFLAG,GOSTUDIO   Studio? Y/N                                   
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET BUYING AGENCY FROM MEDIA CLIENT RECORD                          *         
***********************************************************************         
********not fully tested! seems to switch OK, but then dies in ********         
**************dmgr. Test thoroughly if you resurrect.******************         
***********************************************************************         
*                                                                               
GETBAG   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         CLC   PRODUL,QUNIT                                                     
         JNE   EXITY                                                            
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         LLC   RE,LDGAL1                                                        
         LA    RF,ACTKACT(RE)                                                   
         CLI   0(RF),C' '                                                       
         JH    GETBAG40            CLI REC ONLY                                 
CLI      USING DCLI,TEMP2                                                       
         XC    CLI.CLIKEY,CLI.CLIKEY                                            
         MVI   CLI.CLIKTYP,CLIKTYPQ                                             
         MVC   CLI.CLIKCLI,SPACES                                               
         SHI   RE,1                                                             
         MVC   CLI.CLIKCLI(0),ACTKACT                                           
         EX    RE,*-6                                                           
         CLI   LDGAL1,3                                                         
         JNH   GETBAG10            SIMPLE 2-3 CHAR CLIENT                       
         CLI   LDGAL1,5                                                         
         JNE   GETBAG40            NOT MEDIA-COMPATIBLE                         
         XC    CLI.CLIKCLI+1(4),CLI.CLIKCLI+1                                   
         MVZ   CLI.CLIKCLI+1(4),ACTKACT+1                                       
         CLC   CLI.CLIKCLI+1(4),=C'0000'                                        
         JNE   GETBAG40            not media compatible                         
         PACK  DUB1,ACTKACT+1(11)  4 LAST CHARS PACKED INTO 2 BYTES             
         CVB   R1,DUB1                                                          
         STCM  R1,3,CLI.CLIKCLI+1                                               
*                                                                               
GETBAG10 OI    RUNFLAGS,RFNOSEQ    BREAKING READ SEQ                            
         MVC   CLI.CLIKAM,MEDAGYB ???                                           
*                                                                               
GETBAG20 MVC   IOKEY,CLI.CLIKEY                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOMEDDIR+IO4'                            
         JE    GETBAG30                                                         
         TM    CLI.CLIKAM,X'0F'                                                 
         JO    GETBAG40            NO MATCH FOUND                               
         LLC   RF,CLI.CLIKAM                                                    
         AHI   RF,1                                                             
         STC   RF,CLI.CLIKAM                                                    
         J     GETBAG20                                                         
*                                                                               
GETBAG30 MVC   TEMP2(L'CLIDBAGY),CLI.CLIDBAGY                                   
         J     GETBAG42                                                         
*                                                                               
GETBAG40 XC    TEMP2(L'CLIDBAGY),TEMP2    ZERO IF NO MATCH                      
*                                                                               
GETBAG42 J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER INITIALISATION - ALL LEDGERS                                           
***********************************************************************         
*                                                                               
INILDG   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,QUNIT        Read the ledger                              
         CLC   =C'2P',QUNIT                                                     
         JNE   *+10                                                             
         MVC   LDGAUL,=C'2D'       FOR 2P READ 2D FIRST                         
         GOTOR (#SETLDG,ASETLDG)                                                
         JE    INILDG10                                                         
         MVC   LP_ERROR,=AL2(AE$INLDG)                                          
         J     QERROR                                                           
*                                                                               
INILDG10 DS    0H                   FOR ESTIMATES/TIME CAN SUBSTITUTE           
         CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
*                                                                               
         CLC   LDGAUL,=C'2D'                                                    
         JNE   INILDG30           IF DONE 2D LEDGER SAVE POS AND LEN            
         MVC   DOFFPOS,LDGAOP     OF OFFICE (2P OVERWRITES)                     
*                                                                               
INILDG30 DS    0H                                                               
         CLC   QUNIT(2),=C'2P'    IF 2P REQUESTED WE'VE JUST DONE 2D            
         JNE   INILDG35                                                         
         XC    LDGAREA(LDGALNQ),LDGAREA     DO 2P setldg NOW                    
         MVC   LDGAUL,QUNIT                                                     
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   EXITNX                                                           
INILDG35 CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
*                                                                               
         L     R1,AIO1                                                          
         XR    R0,R0                                                            
         USING LDGRECD,R1                                                       
         LA    R1,LDGRFST                                                       
*                                                                               
         USING LDGELD,R1                                                        
INILDG40 CLI   LDGEL,0             GET DEP. POS/LEN                             
         JNE   *+6                                                              
         DC    H'0'                ERROR NO LEDGER ELEMENT                      
         CLI   LDGEL,LDGELQ                                                     
         JE    INILDG50                                                         
         IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         J     INILDG40            Bump to next elem                            
*                                                                               
INILDG50 MVC   DDPTLEN,LDGDLEN     Save department length                       
         MVC   DDPTPOS,LDGDPOS     Save department position                     
*                                                                               
INILDG80 CLC   QUNIT(2),=C'2P'     If 2D or 2P set to lowest lvl                
         JE    *+14                                                             
         CLC   QUNIT(2),=C'2D'                                                  
         JNE   INILDG90                                                         
         CLI   QHIER,C' '                                                       
         JH    INILDG90                                                         
         MVI   QHIER,C'4'          Force dept or staff                          
*                                                                               
INILDG90 CLI   QHIER,C'4'          Always means lowest level wanted             
         JNE   EXITY                                                            
         CLI   LDGAL4,0            Adjust if no actual level 4                  
         JNE   EXITY                                                            
         MVI   QHIER,C'3'                                                       
         CLI   LDGAL3,0                                                         
         JNE   EXITY                                                            
         MVI   QHIER,C'2'                                                       
         CLI   LDGAL2,0                                                         
         JNE   EXITY                                                            
         MVI   QHIER,C'1'                                                       
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* Person download                                                    *          
**********************************************************************          
                                                                                
REQGRPL  LKREQ H,A#GRPL,OUTGRPL,NEXTREQ=REQPERS                                 
                                                                                
Ind      LKREQ F,1,(D,B#SAVED,QINITIAL),CHAR,OLEN=L'QINITIAL,          +        
               MAXLEN=L'QINITIAL,TEXT=(*,INITLIT),COL=*                         
                                                                                
         LKREQ E                                                                
                                                                                
                                                                                
OUTGRPL  LKOUT H                                                                
                                                                                
GRPLIS   LKOUT R,A#GRPL            Group list record                            
Array    LKOUT C,1,(A,ARYGRPL)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYGRPL  LKOUT A,(R,NXTGRP),MULTIROW=Y,ROWNAME=GLSRECD                          
                                                                                
OffCod   LKOUT C,1,GLSKOFF,CHAR,FILTROUT=TSTOFF,SKIPCOLS=2                      
PRout    LKOUT P,GLSKOFF,SETOFFN                                                
OffNam   LKOUT C,2,(D,B#WORKD,TEMP2),CHAR,LEN=36,ND=Y                           
GrpCod   LKOUT C,3,GLSKGRP,CHAR                                                 
Array    LKOUT C,4,(A,ARYGRNM)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYGRNM  LKOUT A,(D,B#GLSREC,GLSRFST),EOT=EOR,                         +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
GrpNam   LKOUT C,4,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
SETOFFN  L     R1,LP_AINP          Binary PID                                   
         CLC   0(L'GLSKOFF,R1),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2(L'GLSKOFF),0(R1)                                           
         GOTOR (#GETOFN,AGETOFN)                                                
         J     EXITY                                                            
                                                                                
                                                                                
TSTOFF   L     R1,LP_ADATA         Test if office present                       
         USING GLSRECD,R1                                                       
         OC    GLSKOFF,GLSKOFF                                                  
         J     SETCCC                                                           
                                                                                
NXTGRP   GOTOR (#NXTREC,ANXTREC),DMCB,GLSKEYT,('B#GLSREC',0),          +        
               (0,SAVED),AFLTGRP,0                                              
         J     EXITY                                                            
                                                                                
                                                                                
**********************************************************************          
* Person download                                                    *          
**********************************************************************          
                                                                                
REQPERS  LKREQ H,A#PSEC,OUTPER,NEXTREQ=REQPLST                                  
Pin      LKREQ F,1,(D,B#SAVED,QPIDB),HEXD,TEXT=AC#RSPID,COL=*                   
                                                                                
         LKREQ E                                                                
                                                                                
OUTPER   LKOUT H                                                                
                                                                                
PERSEC   LKOUT R,A#PSEC            Person security record                       
Array    LKOUT C,1,(A,ARYSEC)                                                   
         LKOUT E                                                                
                                                                                
PERTIM   LKOUT R,A#PTIM            Person cost record                           
Array    LKOUT C,2,(A,ARYCST)                                                   
         LKOUT E                                                                
                                                                                
PERPER   LKOUT R,A#PCRD            Account person record                        
PRout    LKOUT P,,SET1RACT                                                      
Array    LKOUT C,3,(A,ARYACT)                                                   
         LKOUT E                                                                
                                                                                
PERCRD   LKOUT R,A#PCRD            Account creditor record                      
Array    LKOUT C,4,(A,ARYCRD)                                                   
         LKOUT E                                                                
                                                                                
PERACCS  LKOUT R,A#PACCS           Person access record                         
Array    LKOUT C,5,(A,ARYACS)                                                   
         LKOUT E                                                                
                                                                                
PERACS2  LKOUT R,A#PACCS           Person access record                         
Array    LKOUT C,5,(A,ARYACS2)                                                  
         LKOUT E                                                                
                                                                                
PERAPR   LKOUT R,A#PAPPR           Person approver record                       
Array    LKOUT C,6,(A,ARYAPR)                                                   
         LKOUT E                                                                
                                                                                
PERCLRL  LKOUT R,A#PCLRL           Person client role record                    
Array    LKOUT C,7,(A,ARYROL)                                                   
Array    LKOUT C,7,(A,ARYCRO)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSEC   LKOUT A,(R,BLDSEC),NROWS=1,ROWNAME=SAVED,NEWEL=Y                       
                                                                                
PID      LKOUT C,01,QPID,CHAR                                                   
FirstNam LKOUT C,02,SE_FSTNM,CHAR                                               
MidNam   LKOUT C,03,SE_MIDNM,CHAR,ND=Y                                          
LastNam  LKOUT C,04,SE_LSTNM,CHAR                                               
Title    LKOUT C,05,SE_TITLE,CHAR,ND=Y                                          
WorkExt  LKOUT C,06,SE_WKEXT,CHAR,ND=Y                                          
Phone    LKOUT C,07,SE_PHONE,CHAR,ND=Y                                          
Array    LKOUT C,08,(A,ARYHADR)                                                 
City     LKOUT C,09,SE_CITY,CHAR,ND=Y                                           
PostCode LKOUT C,10,SE_PSTCD,CHAR,ND=Y                                          
Country  LKOUT C,11,SE_CTRY,CHAR,ND=Y                                           
OffCode  LKOUT C,12,SE_OFCOD,CHAR,ND=Y                                          
OffName  LKOUT C,13,SE_OFCOD,(R,EDTONM),ND=Y                                    
DepCode  LKOUT C,14,SE_DEPCD,CHAR,ND=Y                                          
DepName  LKOUT C,15,SE_ODCOD,(R,EDTDNM),ND=Y                                    
SecGroup LKOUT C,16,SE_SECGR,CHAR,ND=Y                                          
TSAppGrp LKOUT C,17,SE_TSAGR,CHAR,ND=Y                                          
NICode   LKOUT C,18,SE_NICOD,CHAR,ND=Y                                          
Array    LKOUT C,19,(A,ARYCUID)                                                 
EffDate  LKOUT C,20,SE_EFFDT,CDAT,ND=Y                                          
HireDate LKOUT C,21,SE_HIRDT,CDAT,ND=Y                                          
IValFrom LKOUT C,22,SE_IVFRM,CDAT,ND=Y                                          
Email    LKOUT C,23,SE_EMAIL,CHAR,ND=Y                                          
StfCode  LKOUT C,24,SE_STFCD,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYHADR  LKOUT A,(D,B#SAVED,SE_HADR),NROWS=(B#SAVED,SE_NHADR),         +        
               ROWNAME=SE_HADR,ROWWIDTH=L'SE_HADR                               
                                                                                
HomeAdr  LKOUT C,8,SE_HADR,CHAR,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYCUID  LKOUT A,(D,B#SAVED,SE_CUID),NROWS=(B#SAVED,SE_NCUID),         +        
               ROWNAME=SE_CUID,ROWWIDTH=L'SE_CUID                               
                                                                                
CpyUID   LKOUT C,19,SE_CUID,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
SET1RACT MVC  QULA,SV1RACT                                                      
         J    EXIT                                                              
                                                                                
***********************************************************************         
* Edit Office name                                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTONM   LM    R2,R4,LP_AINP                                                    
         OC    0(L'SE_OFCOD,R2),0(R2)  Any office code?                         
         JZ    EXITY                   No - done                                
*                                                                               
ONM      USING SAOFREC,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   ONM.SAOFTYP,SAOFTYPQ                                             
         MVI   ONM.SAOFSUB,SAOFSUBQ                                             
         MVC   ONM.SAOFAGY,QALPHA                                               
         MVC   ONM.SAOFOID,0(R2)                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         JNE   EXITY                                                            
         DROP  ONM                                                              
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,SAOFDATA-SAOFREC(R2)                                          
         XR    R0,R0                                                            
         USING SAOFFD,R2                                                        
EDTONM10 CLI   SAOFFEL,0                                                        
         JE    EXITY                                                            
         CLI   SAOFFEL,SAOFFELQ                                                 
         JE    *+14                                                             
         IC    R0,SAOFFLN                                                       
         AR    R2,R0                                                            
         J     EDTONM10                                                         
                                                                                
         MVC   TEMP2,SPACES                                                     
         LLC   RF,SAOFFLN                                                       
         SHI   RF,SAOFFLNQ+1                                                    
         JM    EXITY                                                            
*                                                                               
         BASR  RE,0                                                             
         MVC   TEMP2(0),SAOFFNAM                                                
         EX    RF,0(RE)                                                         
         MVC   0(L'SAOFFNAM,R4),TEMP2                                           
         LHI   RE,L'SAOFFNAM                                                    
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Edit Department name                                                *         
***********************************************************************         
         SPACE 1                                                                
EDTDNM   LM    R2,R4,LP_AINP                                                    
         OC    2(L'SE_DEPCD,R2),2(R2)  Any Department code                      
         JZ    EXITY                   No - done                                
*                                                                               
DNM      USING SADPREC,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   DNM.SADPTYP,SADPTYPQ                                             
         MVI   DNM.SADPSUB,SADPSUBQ                                             
         MVC   DNM.SADPAGY,QALPHA                                               
         MVC   DNM.SADPOID(L'SE_ODCOD),0(R2)                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         JNE   EXITY                                                            
         DROP  DNM                                                              
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,SADPDATA-SADPREC(R2)                                          
         XR    R0,R0                                                            
         USING SADPTD,R2                                                        
EDTDNM10 CLI   SADPTEL,0                                                        
         JE    EXITY                                                            
         CLI   SADPTEL,SADPTELQ                                                 
         JE    *+14                                                             
         IC    R0,SADPTLN                                                       
         AR    R2,R0                                                            
         J     EDTDNM10                                                         
                                                                                
         MVC   TEMP2,SPACES                                                     
         LLC   RF,SADPTLN                                                       
         SHI   RF,SADPTLNQ+1                                                    
         JM    EXITY                                                            
*                                                                               
         BASR  RE,0                                                             
         MVC   TEMP2(0),SADPTNAM                                                
         EX    RF,0(RE)                                                         
         MVC   0(L'SADPTNAM,R4),TEMP2                                           
         LHI   RE,L'SADPTNAM                                                    
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUILD PERSON SECURITY RECORD                                        *         
***********************************************************************         
                                                                                
BLDSEC   OC    QPIDB,QPIDB         Test PIN given                               
         JNZ   BLDSEC04                                                         
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
                                                                                
BLDSEC04 OC    QALPHA,CUSALF       USE SECURITY AGENCY IF PRESENT               
         JNZ   *+10                                                             
         MVC   QALPHA,CUAALF       ELSE NATIVE AGENCY                           
         MVC   TEMP2(L'QPIDB),QPIDB                                             
         GOTOR (#GETPID,AGETPID)   GET 8 CHARACTER PID                          
         MVC   QPID,TEMP2          PERSON ID                                    
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     BUILD KEY TO READ                            
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,QALPHA                                                   
         MVC   SAPEPID,QPID                                                     
         MVC   CSVKEY1,SAPEKEY                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO1'                               
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
         L     R2,AIO1             MATCH KEY ON ALL BUT EFFECTIVE DATE          
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),CSVKEY1                             
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
                                                                                
         LA    R0,SE_VALS                                                       
         LHI   R1,SE_VLNQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SE_EFFDT,EFFS                                                    
         XC    SE_EFFDT,SAPEDEF    EFFECTIVE DATE 1'S COMPLEMENT                
         LA    R4,SAPEDATA                                                      
BLDSEC06 CLI   0(R4),0                                                          
         JE    BLDSEC40                                                         
         CLI   0(R4),SANAMELQ      PERSON NAME ELEMENT                          
         JE    BLDSEC10                                                         
         CLI   0(R4),SAPERELQ      PERSONNEL DETAILS ELEMENT                    
         JE    BLDSEC20                                                         
         CLI   0(R4),SAADRELQ      PERSON ADDRESS ELEMENTS                      
         JE    BLDSEC24                                                         
         CLI   0(R4),SAAGCELQ      ACCESS GROUP CODE ELEMENT                    
         JE    BLDSEC28                                                         
         CLI   0(R4),SAAPCELQ      TS APPROVER GROUP CODE ELEMENT               
         JE    BLDSEC32                                                         
         CLI   0(R4),SAPEEELQ      PERSON EMAIL ID ELEMENT                      
         JE    BLDSEC36                                                         
                                                                                
BLDSEC08 LLC   RF,1(,R4)           L'ELEMENT                                    
         AR    R4,RF                                                            
         J     BLDSEC06                                                         
                                                                                
         USING SANAMD,R4                                                        
BLDSEC10 LA    R1,SANAMELN         L'NAME                                       
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    BLDSEC12                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,L'SE_FSTNM       TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LHI   RF,L'SE_FSTNM       SET IT IF GREATER                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SE_FSTNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         LLC   RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
BLDSEC12 TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT TOO                 
         JZ    BLDSEC14                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,L'SE_MIDNM       TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LHI   RF,L'SE_MIDNM       SET IT IF GREATER                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SE_MIDNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         LLC   RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
BLDSEC14 TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    BLDSEC08                                                         
         LLC   RF,SANAMELN                                                      
         CHI   RF,L'SE_LSTNM       TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LHI   RF,L'SE_LSTNM       SET IT IF GREATER                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SE_LSTNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         J     BLDSEC08                                                         
         DROP  R1                                                               
                                                                                
         USING SAPERD,R4                                                        
BLDSEC20 MVC   SE_WKEXT,SAPEREXT                                                
         MVC   SE_OFCOD,SAPEROFF                                                
         MVC   SE_DEPCD,SAPERDID                                                
         MVC   SE_NICOD,SAPERINC                                                
         MVC   SE_HIRDT,SAPERDHI                                                
         MVC   SE_IVFRM,SAPERDTE                                                
         MVC   SE_STFCD,SAPERSTA                                                
         CLI   SAPERLN,SAPERLNQ                                                 
         JNH   BLDSEC08                                                         
         LLC   RF,SAPERLN                                                       
         SHI   RF,SAPERLNQ+1                                                    
         JM    BLDSEC08                                                         
         BASR  RE,0                                                             
         MVC   SE_TITLE(0),SAPERTIT                                             
         EX    RF,0(RE)                                                         
         J     BLDSEC08                                                         
                                                                                
         USING SAADRD,R4                                                        
BLDSEC24 CLI   SAADRTYP,SAADLINQ                                                
         JNE   *+18                                                             
         MVC   SE_NHADR,=H'1'                                                   
         LA    R1,SE_HADR          ADDRESS FIRST LINE                           
         J     BLDSEC26                                                         
         CLI   SAADRTYP,SAADLINQ+1                                              
         JNE   *+18                                                             
         MVC   SE_NHADR,=H'2'                                                   
         LA    R1,SE_HADR+L'SE_HADR ADDRESS SECOND LINE                         
         J     BLDSEC26                                                         
         CLI   SAADRTYP,SAADCITQ                                                
         JNE   *+12                                                             
         LA    R1,SE_CITY          ADDRESS CITY                                 
         J     BLDSEC26                                                         
         CLI   SAADRTYP,SAADCODQ                                                
         JNE   *+12                                                             
         LA    R1,SE_PSTCD         ADDRESS CODE                                 
         J     BLDSEC26                                                         
         CLI   SAADRTYP,SAADCTRQ                                                
         JNE   *+12                                                             
         LA    R1,SE_CTRY          ADDRESS COUNTRY                              
         J     BLDSEC26                                                         
         CLI   SAADRTYP,SAADPHOQ                                                
         JNE   BLDSEC26                                                         
         LA    R1,SE_PHONE         HOME TELEPHONE                               
                                                                                
BLDSEC26 LLC   RF,SAADRDLN                                                      
         SHI   RF,1                                                             
         JM    BLDSEC08                                                         
         BASR  RE,0                                                             
         MVC   0(0,R1),SAADRDAT                                                 
         EX    RF,0(RE)                                                         
         J     BLDSEC08                                                         
                                                                                
         USING SAAGCD,R4                                                        
BLDSEC28 OC    SAAGCCOD,SAAGCCOD   ACCESS GROUP                                 
         JZ    BLDSEC08                                                         
         MVC   SE_SECGR,SAAGCCOD                                                
         J     BLDSEC08                                                         
                                                                                
         USING SAAPCD,R4                                                        
BLDSEC32 OC    SAAPCCOD,SAAPCCOD   APPROVER GROUP CODE                          
         JZ    BLDSEC08                                                         
         MVC   SE_TSAGR,SAAPCCOD                                                
         J     BLDSEC08                                                         
                                                                                
         USING SAPEED,R4                                                        
BLDSEC36 LLC   RF,SAPEELN                                                       
         SHI   RF,3                                                             
         JM    BLDSEC08                                                         
         BASR  RE,0                                                             
         MVC   SE_EMAIL(0),SAPEEID                                              
         EX    RF,0(RE)                                                         
         J     BLDSEC08                                                         
         DROP  R2,R4                                                            
                                                                                
         USING SA0REC,R2                                                        
BLDSEC40 LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,QALPHA                                                   
         MVC   SA0KNUM,QPIDB                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         JNE   BLDSECX                                                          
                                                                                
         LA    RF,SE_CUID                                                       
         L     R2,AIO1                                                          
         LA    R4,SA0DATA                                                       
         USING SAIDD,R4                                                         
BLDSEC44 CLI   SAIDEL,0            TEST END-OF-RECORD                           
         JE    BLDSECX                                                          
         CLI   SAIDEL,SAIDELQ      TEST ID ELEMENT                              
         JNE   BLDSEC48                                                         
         TM    SAIDTYP,SAIDNEQ     NEW TYPE?                                    
         JNZ   BLDSEC52            NO                                           
         TM    SAIDTYP,SAIDROQ     READ-ONLY USERID?                            
         JNO   BLDSEC52            NO - GET IT                                  
                                                                                
BLDSEC48 LLC   R1,SAIDLEN          BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         J     BLDSEC44                                                         
                                                                                
BLDSEC52 MVC   0(L'SAID,RF),SAID                                                
         OC    0(2,RF),0(RF)       TEST ID LIST                                 
         JNZ   *+10                                                             
         MVC   0(2,RF),=C'L='                                                   
         CLC   0(2,RF),=X'0001'    TEST AGY=                                    
         JNE   *+16                                                             
         MVC   0(2,RF),=C'A='                                                   
         MVC   4(6,RF),SPACES                                                   
                                                                                
         CLC   0(4,RF),=C'ALL '    CHECK FOR ALL AND TRANSLATE                  
         JNE   *+10                                                             
         MVC   0(L'AC@ALL,RF),AC@ALL                                            
                                                                                
         LA    RF,L'SAID(,RF)                                                   
         LH    R1,SE_NCUID                                                      
         AHI   R1,1                BUMP COUNTER                                 
         STH   R1,SE_NCUID                                                      
         J     BLDSEC48                                                         
                                                                                
BLDSECX  ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  R2,R4                                                            
                                                                                
         EJECT                                                                  
                                                                                
ARYCST   LKOUT A,(R,BLDCST),NROWS=1,ROWNAME=SAVED                               
                                                                                
HirDte   LKOUT C,01,CS_HIRDT,PDAT                                               
TerDte   LKOUT C,02,CS_TERDT,PDAT                                               
TimLck   LKOUT C,03,CS_TSLCK,PDAT,ND=Y                                          
ActHr    LKOUT C,04,CS_ACTHR,CHAR                                               
                                                                                
Array    LKOUT C,01,(A,ARYLOCI)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYLOCI  LKOUT A,(D,B#PERREC,PERRFST),NEWEL=B,EOT=EOR,                 +        
               ROWID=(LOCEL,LOCELQ),ROWWIDTH=(V,LOCLN)                          
                                                                                
PRout    LKOUT P,LOCELD,LOCINI                                                  
LocOfC   LKOUT C,1,(D,B#SAVED,CS_1RULA),(U,#EDTOFF,$EDTOFF),           +        
               FILTROUT=TSTACT,SKIPCOLS=LOCSKIPS                                
                                                                                
LOCSKIP  EQU   *                                                                
LocOfN   LKOUT C,2,(D,B#SAVED,CS_1RULA),(U,#EDTOFN,$EDTOFN)                     
LocDptC  LKOUT C,3,LOCDEPT,CHAR                                                 
LocDptN  LKOUT C,4,(D,B#SAVED,CS_1RULA),(U,#EDTDPN,$EDTDPN)                     
LocSubC  LKOUT C,5,LOCSUB,CHAR                                                  
LocSubN  LKOUT C,6,(D,B#SAVED,CS_1RULA),(U,#EDTSUN,$EDTSUN)                     
LocStrDt LKOUT C,7,LOCSTART,PDAT,ND=Y                                           
LocEndDt LKOUT C,8,LOCEND,PDAT,ND=Y                                             
LocSalDt LKOUT C,9,LOCSALKD,PDAT,ND=Y                                           
LocPerSt LKOUT C,10,(D,B#SAVED,CS_STAT),CHAR                                    
LOCSKIPS EQU   (*-LOCSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Initialize location values from element                             *         
***********************************************************************         
         SPACE 1                                                                
LOCINI   L     R2,LP_AINP                                                       
         USING LOCELD,R2                                                        
                                                                                
         MVC   CS_1RULA,SPACES                                                  
         MVC   CS_1RUNT(2),COSTUL                                               
                                                                                
         LLC   R1,ONERL1L          For each location build 1R account           
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   CS_1RACT(0),LOCOFF                                               
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         LA    R4,CS_1RACT                                                      
         AR    R4,R1                                                            
         LLC   RF,ONERL2L                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),LOCDEPT                                                  
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R4,RF               R4=A(end of dept code on 1R account)         
         LLC   RF,ONERL3L                                                       
         LLC   R1,ONERL2L                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),LOCSUB                                                   
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   R1,ONERL3L                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),CS_PER                                                   
         EX    RF,0(RE)                                                         
                                                                                
         CLI   LOCSTAT,LOCSACT     Test active location                         
         JNE   *+10                                                             
         MVC   SV1RACT,CS_1RULA    Display active 1R account only               
                                                                                
         MVC   QULA,CS_1RULA       Set QULA for TSTACT routine                  
         SR    RF,RF                                                            
         IC    RF,LOCSTAT                                                       
         AHI   RF,1                                                             
         STC   RF,CS_STAT                                                       
         OI    CS_STAT,X'F0'       Convert number to character                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUILD PERSON COST RECORD                                            *         
***********************************************************************         
                                                                                
BLDCST   OC    QPIDB,QPIDB         Test PIN given                               
         JNZ   *+14                                                             
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
                                                                                
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
         XC    PIDKEY,PIDKEY       BUILD KEY TO READ                            
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,QPIDB                                                    
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   CSVKEY1,PIDKEY                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
         CLC   PIDKEY(PIDKPER-PIDRECD),CSVKEY1                                  
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR              PERSON NOT ON FILE                           
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SV1RACT,SPACES                                                   
         XC    CS_VALS(CS_VLNQ),CS_VALS                                         
         L     R4,AIO4                                                          
         MVC   CS_PER,PERKCODE-PERRECD(R4)                                      
*                                                                               
         LA    R4,PERRFST-PERRECD(R4)                                           
BLDCST04 CLI   0(R4),0                                                          
         JE    BLDCSTX                                                          
         CLI   0(R4),EMPELQ                                                     
         JE    BLDCST08                                                         
         CLI   0(R4),LOCELQ                                                     
         JE    BLDCST12                                                         
BLDCST08 LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         J     BLDCST04                                                         
                                                                                
         USING EMPELD,R4                                                        
BLDCST12 MVC   CS_HIRDT,EMPHIR                                                  
         MVC   CS_TERDT,EMPTRM                                                  
         MVC   CS_TSLCK,EMPLOCK                                                 
         MVI   CS_ACTHR,NOQ                                                     
         TM    EMPSTAT,EMPSACT     Use Actual hours                             
         JZ    *+8                                                              
         MVI   CS_ACTHR,YESQ                                                    
         J     BLDCST08                                                         
                                                                                
         USING OFFALD,R1                                                        
         USING LOCELD,R4                                                        
BLDCST16 CLI   LOCSTAT,LOCSACT     Test active location                         
         JNE   BLDCST08                                                         
         MVC   SVOFF,LOCOFF        Saved office code                            
*                                                                               
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,LOCOFF     Move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    BLDCST08                                                         
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     QERROR              Security lock                                
                                                                                
BLDCSTX  ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  R1,R4                                                            
         EJECT                                                                  
                                                                                
ARYCRD   LKOUT A,(R,BLDCRD),NROWS=1,ROWNAME=ACTRECD                             
                                                                                
Array    LKOUT C,01,(A,ARYACT)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Build creditor record                                               *         
***********************************************************************         
                                                                                
BLDCRD   OC    QPIDB,QPIDB         Test PIN given                               
         JNZ   *+14                                                             
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
                                                                                
         MVC   QULA,SPACES                                                      
                                                                                
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
         XC    PIDKEY,PIDKEY       BUILD KEY TO READ                            
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,QPIDB                                                    
         MVI   PIDKSTYP,PIDKCRDQ                                                
         MVC   CSVKEY1,PIDKEY                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
         CLC   PIDKEY(PIDKPER-PIDRECD),CSVKEY1                                  
         JNE   EXITY                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         CLC   =C'SX',ACTKUNT-ACTRECD(RF)                                       
         JNE   BLDCRDX                                                          
         MVC   QULA,ACTKULA-ACTRECD(RF)                                         
                                                                                
BLDCRDX  J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
ARYACS   LKOUT A,(R,NXTACS),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
Array    LKOUT C,01,(A,ARYCPJL)                                                 
Array    LKOUT C,02,(A,ARYMEDL)                                                 
Array    LKOUT C,03,(A,ARYETYL)                                                 
Array    LKOUT C,04,(A,ARYNCLL)                                                 
Array    LKOUT C,05,(A,ARYSTFL)                                                 
Array    LKOUT C,06,(A,ARYWCL)                                                  
Array    LKOUT C,07,(A,ARYSCRL)                                                 
Array    LKOUT C,08,(A,ARYESCL)                                                 
Array    LKOUT C,09,(A,ARYSUPL)                                                 
Array    LKOUT C,10,(A,ARYGRP),FILTROUT=TSTGRLS                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCPJL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,1,(A,ARYCPJL1),FILTROUT=TSTLLCPJ                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYCPJL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN1Q            
                                                                                
PRout    LKOUT P,LIDLACT,SETJOB                                                 
CPJCcd   LKOUT C,1,(D,B#SAVED,LL_ULA),(U,#EDTCLI,$EDTCLI),             +        
               FILTROUT=TSTACT,SKIPCOLS=CPJSKIPS                                
                                                                                
CPJSKIP  EQU   *                                                                
CPJCnm   LKOUT C,2,(D,B#SAVED,LL_ULA),(U,#EDTCLN,$EDTCLN)                       
CPJPcd   LKOUT C,3,(D,B#SAVED,LL_ULA),(U,#EDTPRD,$EDTPRD)                       
CPJPnm   LKOUT C,4,(D,B#SAVED,LL_ULA),(U,#EDTPRN,$EDTPRN)                       
CPJJcd   LKOUT C,5,(D,B#SAVED,LL_ULA),(U,#EDTJOB,$EDTJOB)                       
CPJJnm   LKOUT C,6,(D,B#SAVED,LL_ULA),(U,#EDTJBN,$EDTJBN)                       
CPJEst   LKOUT C,7,LIDLAPPL,(R,EDTAPEST)                                        
CPJExp   LKOUT C,8,LIDLAPPL,(R,EDTAPEXP)                                        
CPJInv   LKOUT C,9,LIDLAPPL,(R,EDTAPINV)                                        
CPJJob   LKOUT C,10,LIDLAPPL,(R,EDTAPJOB)                                       
CPJOrd   LKOUT C,11,LIDLAPPL,(R,EDTAPORD)                                       
CPJRes   LKOUT C,12,LIDLAPPL,(R,EDTAPRES)                                       
CPJTim   LKOUT C,13,LIDLAPPL,(R,EDTAPTIM)                                       
CPJSKIPS EQU   (*-CPJSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYMEDL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,2,(A,ARYMEDL1),FILTROUT=TSTLLMED                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYMEDL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN3Q            
                                                                                
MedCod   LKOUT C,1,LIDLMED,CHAR                                                 
MedNam   LKOUT C,2,LIDLMED,(U,#EDTMCN,$EDTMCN)                                  
MedEst   LKOUT C,3,LIDLAPPL,(R,EDTAPEST)                                        
MedExp   LKOUT C,4,LIDLAPPL,(R,EDTAPEXP)                                        
MedInv   LKOUT C,5,LIDLAPPL,(R,EDTAPINV)                                        
MedJob   LKOUT C,6,LIDLAPPL,(R,EDTAPJOB)                                        
MedOrd   LKOUT C,7,LIDLAPPL,(R,EDTAPORD)                                        
MedRes   LKOUT C,8,LIDLAPPL,(R,EDTAPRES)                                        
MedTim   LKOUT C,9,LIDLAPPL,(R,EDTAPTIM)                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYETYL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,3,(A,ARYETYL1),FILTROUT=TSTLLETY                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYETYL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN4Q            
                                                                                
EtyCod   LKOUT C,1,LIDLETY,CHAR                                                 
PRout    LKOUT P,LIDLETY,SETETYN                                                
EtyNam   LKOUT C,2,(D,B#WORKD,TEMP2),CHAR,LEN=36,ND=Y                           
EtyExp   LKOUT C,3,LIDLAPPL,(R,EDTAPEXP)                                        
EtyInv   LKOUT C,4,LIDLAPPL,(R,EDTAPINV)                                        
EtyOrd   LKOUT C,7,LIDLAPPL,(R,EDTAPORD)                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYNCLL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,4,(A,ARYNCLL1),FILTROUT=TSTLLNCC                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYNCLL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN1Q            
                                                                                
PRout    LKOUT P,LIDLACT,SETNCA                                                 
NccCod   LKOUT C,1,LIDLACT,CHAR,FILTROUT=TSTACT,SKIPCOLS=NCCSKIPS               
NCCSKIP  EQU   *                                                                
NccNam   LKOUT C,2,(D,B#SAVED,LL_ULA),(U,#EDTANM,$EDTANM)                       
NccTim   LKOUT C,3,LIDLAPPL,(R,EDTAPTIM)                                        
NCCSKIPS EQU   (*-NCCSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTFL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,5,(A,ARYSTFL1),FILTROUT=TSTLLSTF                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTFL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN1Q            
                                                                                
PRout    LKOUT P,LIDLACT,SETSTF                                                 
StfOcd   LKOUT C,1,(D,B#SAVED,LL_ULA),(U,#EDTOFF,$EDTOFF),             +        
               FILTROUT=TSTACT,SKIPCOLS=STFSKIPS                                
                                                                                
STFSKIP  EQU   *                                                                
StfOnm   LKOUT C,2,(D,B#SAVED,LL_ULA),(U,#EDTOFN,$EDTOFN)                       
StfDcd   LKOUT C,3,(D,B#SAVED,LL_ULA),(U,#EDTDPT,$EDTDPT)                       
StfDnm   LKOUT C,4,(D,B#SAVED,LL_ULA),(U,#EDTDPN,$EDTDPN)                       
StfScd   LKOUT C,5,(D,B#SAVED,LL_ULA),(U,#EDTSUB,$EDTSUB)                       
StfSnm   LKOUT C,6,(D,B#SAVED,LL_ULA),(U,#EDTSUN,$EDTSUN)                       
StfPcd   LKOUT C,7,(D,B#SAVED,LL_ULA),(U,#EDTPER,$EDTPER)                       
StfPnm   LKOUT C,8,(D,B#SAVED,LL_ULA),(U,#EDTPEN,$EDTPEN)                       
StfExp   LKOUT C,9,LIDLAPPL,(R,EDTAPEXP)                                        
StfTim   LKOUT C,10,LIDLAPPL,(R,EDTAPTIM)                                       
STFSKIPS EQU   (*-STFSKIP)/LX_COLSL                                             
         LKOUT E                                                                
                                                                                
ARYWCL   LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,6,(A,ARYWCL1),FILTROUT=TSTLLWCL                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYWCL1  LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN2Q            
                                                                                
WclCod   LKOUT C,1,LIDLWC,CHAR                                                  
WclNam   LKOUT C,2,LIDLWC,(U,#EDTWCD,$EDTWCD)                                   
WclTim   LKOUT C,3,LIDLAPPL,(R,EDTAPTIM)                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYSCRL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,7,(A,ARYSCRL1),FILTROUT=TSTLLSCR                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYSCRL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN5Q            
                                                                                
ScrFcd   LKOUT C,1,LIDLREP,CHAR                                                 
PRout    LKOUT P,LIDLREP,SETRFM                                                 
ScrFnm   LKOUT C,2,(D,B#SAVED,LL_FRMNM),CHAR                                    
ScrRty   LKOUT C,3,(D,B#SAVED,LL_FRMTY),CHAR                                    
ScrRep   LKOUT C,4,LIDLAPPL,(R,EDTAPREP)                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYESCL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,8,(A,ARYESCL1),FILTROUT=TSTLLESC                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYESCL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN5Q            
                                                                                
SchCod   LKOUT C,1,LIDLREP,CHAR                                                 
SchNam   LKOUT C,2,LIDLREP,(U,#EDTSCN,$EDTSCN)                                  
SchEst   LKOUT C,3,LIDLAPPL,(R,EDTAPEST)                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYSUPL  LKOUT A,(D,B#LLSREC,LLSRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,9,(A,ARYSUPL1),FILTROUT=TSTLSUPL                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYSUPL1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN8Q            
                                                                                
PRout    LKOUT P,LIDLSULA,SETQULA                                               
SupLcd   LKOUT C,1,LIDLSUNT,CHAR,FILTROUT=TSTACT,SKIPCOLS=SUPSKIPS              
SUPSKIP  EQU   *                                                                
SupLnm   LKOUT C,2,LIDLSULA,(R,EDTLDGNM)                                        
SupCod   LKOUT C,3,LIDLSACT,CHAR                                                
SupNam   LKOUT C,4,LIDLSULA,(U,#EDTANM,$EDTANM)                                 
SupInv   LKOUT C,5,LIDLAPPL,(R,EDTAPINV)                                        
SUPSKIPS EQU   (*-SUPSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYGRP   LKOUT A,(D,B#SAVED,LL_GRCOD),NROWS=1,ROWNAME=LL_GRCOD                  
                                                                                
GrpCod   LKOUT C,1,(D,B#SAVED,LL_GRCOD),CHAR                                    
GrpNam   LKOUT C,2,LL_GRCOD,(U,#EDTGRN,$EDTGRN)                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYACS2  LKOUT A,(D,B#SAVED,LL_DCLIA),NROWS=1,ROWNAME=LL_DCLIA                  
                                                                                
DefCliA  LKOUT C,01,LL_DCLIA,CHAR                                               
DefMedA  LKOUT C,02,LL_DMEDA,CHAR                                               
DefExpA  LKOUT C,03,LL_DEXPA,CHAR                                               
DefNClA  LKOUT C,04,LL_DNCLA,CHAR                                               
DefStfA  LKOUT C,05,LL_DSTFA,CHAR                                               
DefWCA   LKOUT C,06,LL_DWCA,CHAR                                                
DefRepA  LKOUT C,07,LL_DREPA,CHAR                                               
DefEstA  LKOUT C,08,LL_DESTA,CHAR                                               
DefSupA  LKOUT C,09,LL_DSUPA,CHAR                                               
GrpLst   LKOUT C,10,LL_GRLST,CHAR                                               
                                                                                
         LKOUT E                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
                                                                                
TSTGRLS  CLI   LL_GRLST,YESQ                                                    
         BR    RE                                                               
                                                                                
TSTLLCPJ CLI   MYELTYPE,LIDTCPJL   Test client/product/job limit list           
         BR    RE                                                               
                                                                                
TSTLLMED CLI   MYELTYPE,LIDTMEDL   Test media code limit list                   
         BR    RE                                                               
                                                                                
TSTLLETY CLI   MYELTYPE,LIDTEXPL   Test expenditure type limit list             
         BR    RE                                                               
                                                                                
TSTLLNCC CLI   MYELTYPE,LIDTNCLL   Test non client code limit list              
         BR    RE                                                               
                                                                                
TSTLLSTF CLI   MYELTYPE,LIDT1RAC   Test 1R costing account list                 
         BR    RE                                                               
                                                                                
TSTLLWCL CLI   MYELTYPE,LIDTWCL    Test MYELTYPE code limit list                
         BR    RE                                                               
                                                                                
TSTLLSCR CLI   MYELTYPE,LIDTSCRB   Test scribe format code list                 
         BR    RE                                                               
                                                                                
TSTLLESC CLI   MYELTYPE,LIDTESCH   Test estimate scheme code list               
         BR    RE                                                               
                                                                                
TSTLSUPL CLI   MYELTYPE,LIDTSUPP   Test supplier account list                   
         BR    RE                                                               
                                                                                
TSTBYTE1 CLI   BYTE1,0             Test whether name has been put out           
         BR    RE                                                               
                                                                                
TSTACT   MVI   AC#IND1,0           Unit/ledger account set in QULA              
         CLC   PRODUL,QUNIT                                                     
         JE    *+8                                                              
         OI    AC#IND1,AC#IRDLO    Read lowest level account only               
         GOTOR AVALACT             Read account and test security               
         BR    RE                                                               
                                                                                
SETQULA  L     R1,LP_AINP          Set unit/ledger account in QULA              
         MVC   QULA,0(R1)                                                       
         J     EXIT                                                             
                                                                                
SETJOB   L     R1,LP_AINP          Set job code                                 
         MVC   LL_UNT(2),PRODUL                                                 
         MVC   LL_ACT,0(R1)                                                     
         MVC   QULA,LL_ULA         Set QULA for TSTACT routine                  
         J     EXIT                                                             
                                                                                
SETNCA   L     R1,LP_AINP          Set non client code account                  
         MVC   LL_UNT(2),NCLIUL                                                 
         MVC   LL_ACT,0(R1)                                                     
         MVC   QULA,LL_ULA                                                      
         J     EXIT                                                             
                                                                                
SETSTF   L     R1,LP_AINP          Set 1R staff account                         
         MVC   LL_UNT(2),COSTUL                                                 
         MVC   LL_ACT,0(R1)                                                     
         MVC   QULA,LL_ULA         Set QULA for TSTACT routine                  
         J     EXIT                                                             
                                                                                
SETETYN  L     R1,LP_AINP                                                       
         MVC   TEMP2(L'ETYKCODE),0(R1)                                          
         GOTOR (#GETETN,AGETETN)                                                
         J     EXIT                                                             
                                                                                
EDTAPTIM LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLTIME                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPEXP LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLEXPN                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPEST LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLESTM                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPJOB LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLJOBS                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPINV LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLINVC                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPORD LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLORDS                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPRES LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLRESC                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTAPREP LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDLREPT                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTLDGNM LM    R2,R4,LP_AINP                                                    
                                                                                
         USING LDGRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(2),0(R2)                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     RF,AIO1                                                          
         LA    RF,LDGRFST                                                       
         XR    R0,R0                                                            
         USING NAMELD,RF                                                        
EDTLN04  CLI   NAMEL,0                                                          
         JE    EXITY                                                            
         CLI   NAMEL,NAMELQ                                                     
         JE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    RF,R0                                                            
         J     EDTLN04                                                          
                                                                                
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         JM    EXITY                                                            
         BASR  RE,0                                                             
         MVC   0(0,R4),NAMEREC                                                  
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         STCM  R1,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R6,RF                                                            
                                                                                
SETRFM   L     R1,LP_AINP          Set Scribe report format                     
         MVI   LL_FRMTY,C' '                                                    
         MVC   LL_FRMNM,SPACES                                                  
                                                                                
         USING RESRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ                                                 
         MVI   RESKSUB,RESKSUBQ                                                 
         MVC   RESKCPY,CUXCPY                                                   
         MVC   RESKFORM,0(R1)                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     R4,AIO1                                                          
         LA    R4,PMDRFST-PMDRECD(R4)                                           
SETRFM04 CLI   0(R4),0                                                          
         JE    EXITY                                                            
         CLI   0(R4),STYELQ                                                     
         JE    SETRFM08                                                         
         CLI   0(R4),NAMELQ                                                     
         JE    SETRFM20                                                         
         LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         J     SETRFM04                                                         
                                                                                
         USING STYELD,R4                                                        
         USING REPTYPD,RF                                                       
SETRFM08 LA    RF,REPTBL                                                        
                                                                                
SETRFM12 CLI   REPTYLLN,0          EOT                                          
         JE    SETRFM04            No report type                               
         LLC   R1,REPTYLLN         Length table entry                           
         SHI   R1,REPLN1Q          RE = Number of report codes                  
         LA    RE,REPCODES                                                      
                                                                                
SETRFM14 CLC   STYCODE,0(RE)                                                    
         JE    SETRFM16                                                         
         AHI   RE,L'REPCODES                                                    
         JCT   R1,SETRFM14                                                      
                                                                                
         LLC   R1,REPTYLLN                                                      
         AR    RF,R1                                                            
         J     SETRFM12            Bump to next report type                     
                                                                                
SETRFM16 MVC   LL_FRMTY,REPTYPE    Set report type                              
         J     SETRFM04                                                         
                                                                                
         USING NAMELD,R4                                                        
SETRFM20 LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         JM    SETRFM04                                                         
         BASR  RE,0                                                             
         MVC   LL_FRMNM(0),NAMEREC Set report name                              
         EX    RF,0(RE)                                                         
         J     SETRFM04                                                         
         DROP  R4,R6                                                            
                                                                                
***********************************************************************         
* Get Limit list records                                             *          
***********************************************************************         
                                                                                
NXTACS   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTACS02                                                         
         OC    QPIDB,QPIDB                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XC    LL_VALS(LL_VLNQ),LL_VALS                                         
         MVI   LL_DACS,LL_ALLQ     Set default access                           
         MVC   LL_DACS+L'LL_DACS(LL_DACLQ-L'LL_DACS),LL_DACS                    
         MVI   LL_GRLST,NOQ                                                     
         XC    BYTE1,BYTE1                                                      
*                                                                               
NXTACS02 MVC   IOKEY,SVLLSKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,LLSKEYT,('B#LLSREC',0),          +        
               (0,SAVED),0,0                                                    
         JNE   NXTACS12                                                         
*                                                                               
         MVI   BYTE1,1                                                          
         MVC   SVLLSKEY,IOKEY                                                   
         L     R2,IOADDR                                                        
         USING LLSRECD,R2                                                       
         MVC   LL_GRCOD,LLSKGRP                                                 
         OC    LLSKGRP,LLSKGRP                                                  
         JZ    *+8                                                              
         MVI   LL_GRLST,YESQ                                                    
                                                                                
         LA    R4,LLSRFST                                                       
         USING LIDELD,R4                                                        
         XR    R0,R0                                                            
NXTACS04 CLI   LIDEL,0                                                          
         JE    EXITY                                                            
         CLI   LIDEL,LIDELQ                                                     
         JNE   NXTACS06                                                         
         CLI   LIDTYPE,LIDTCPJL    Client/Product/job limit list                
         JNE   *+8                                                              
         MVI   LL_DCLIA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTMEDL    Media code limit list                        
         JNE   *+8                                                              
         MVI   LL_DMEDA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTEXPL    Expenditure type limit list                  
         JNE   *+8                                                              
         MVI   LL_DEXPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTNCLL    Non-client limit list                        
         JNE   *+8                                                              
         MVI   LL_DNCLA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDT1RAC    1R costing account list                      
         JNE   *+8                                                              
         MVI   LL_DSTFA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTWCL     Work code limit list                         
         JNE   *+8                                                              
         MVI   LL_DWCA,LL_LISTQ                                                 
         CLI   LIDTYPE,LIDTSCRB    Scribe format list                           
         JNE   *+8                                                              
         MVI   LL_DREPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTESCH    Estimate schemel list                        
         JNE   *+8                                                              
         MVI   LL_DESTA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTSUPP    Supplier list                                
         JNE   *+8                                                              
         MVI   LL_DSUPA,LL_LISTQ                                                
         J     NXTACS08                                                         
                                                                                
NXTACS06 CLI   LIDEL,RSTELQ                                                     
         JNE   NXTACS08                                                         
         USING RSTELD,R4                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    NXTACS08                                                         
                                                                                
         TM    RSTACST1,RSTAJOBS                                                
         JZ    *+8                                                              
         MVI   LL_DCLIA,LL_NONEQ                                                
         TM    RSTACST1,RSTAMED    Media code limit list                        
         JZ    *+8                                                              
         MVI   LL_DMEDA,LL_NONEQ                                                
         TM    RSTACST1,RSTAETYP   Expenditure type limit list                  
         JZ    *+8                                                              
         MVI   LL_DEXPA,LL_NONEQ                                                
         TM    RSTACST1,RSTA1NAC   Non-client limit list                        
         JZ    *+8                                                              
         MVI   LL_DNCLA,LL_NONEQ                                                
         TM    RSTACST1,RSTASTAF   1R costing account list                      
         JZ    *+8                                                              
         MVI   LL_DSTFA,LL_NONEQ                                                
         TM    RSTACST1,RSTAWC     Work code limit list                         
         JZ    *+8                                                              
         MVI   LL_DWCA,LL_NONEQ                                                 
         TM    RSTACST1,RSTAREPF   Scribe format list                           
         JZ    *+8                                                              
         MVI   LL_DREPA,LL_NONEQ                                                
         TM    RSTACST1,RSTASCHM   Estimate schemel list                        
         JZ    *+8                                                              
         MVI   LL_DESTA,LL_NONEQ                                                
         TM    RSTACST2,RSTASUPP   Supplier list                                
         JZ    *+8                                                              
         MVI   LL_DSUPA,LL_NONEQ                                                
                                                                                
NXTACS08 IC    R0,RSTLN                                                         
         AR    R4,R0                                                            
         J     NXTACS04                                                         
         DROP  R2,R4                                                            
                                                                                
NXTACS12 CLI   BYTE1,1             Did we find access of person                 
         JE    EXITY               Yes                                          
         GOTOR GETCPXLA            SVOFF SET IN BLDCST!                         
         ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Get CPXEL access rules from company, ofice list, office             *         
* entry: SVOFF set.                                                             
* exit: LL_DACS values set                                                      
***********************************************************************         
         SPACE 1                                                                
GETCPXLA NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         OC    SCPXEL,SCPXEL       No company extra element                     
         JZ    GETCPX10                                                         
         GOTOR SETLAV,SCPXEL       Set limit access values at CPY lvl           
         GOTOR CHKLAV                                                           
         JE    GETCPXNO                                                         
                                                                                
         USING OFLPASD,R2                                                       
GETCPX10 LA    R2,IOKEY                                                         
         XC    OFLPAS,OFLPAS                                                    
         MVI   OFLPTYP,OFLPTYPQ                                                 
         MVI   OFLPSUB,OFLPSUBQ                                                 
         MVC   OFLPREM,CUXCPY                                                   
         MVC   OFLPOFF,SVOFF                                                    
         MVC   CSVKEY1,OFLPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GETCPX20                                                         
         J     GETCPX30                                                         
                                                                                
GETCPX15 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   GETCPX30                                                         
                                                                                
GETCPX20 CLC   OFLPAS(OFLPOFL-OFLPASD),CSVKEY1                                  
         JNE   GETCPX30                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO1                                                          
         LA    R4,OFFRFST-OFFRECD(R4)                                           
N        USING CPXELD,R4                                                        
GETCPX25 CLI   N.CPXEL,0                                                        
         JE    GETCPX15                                                         
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+16                                                             
         LLC   R0,N.CPXLN                                                       
         AR    R4,R0                                                            
         J     GETCPX25                                                         
*                                                                               
         GOTOR SETLAV,(R4)          Set limit access values at OFL lvl          
         GOTOR CHKLAV                                                           
         JE    GETCPXNO                                                         
         J     GETCPX15                                                         
         DROP  R2,N                                                             
                                                                                
GETCPX30 GOTOR CHKLAV                                                           
         JE    GETCPXNO            OK - all values set                          
         LA    R2,IOKEY                                                         
         USING OFFRECD,R2                                                       
         XC    OFFKEY,OFFKEY                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,SVOFF                                                    
         MVC   CSVKEY1,OFFKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   GETCPXX             No office record                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO1                                                          
         LA    R4,OFFRFST-OFFRECD(R4)                                           
N        USING CPXELD,R4                                                        
GETCPX40 CLI   N.CPXEL,0                                                        
         JE    GETCPXX                                                          
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+16                                                             
         LLC   R0,N.CPXLN                                                       
         AR    R4,R0                                                            
         J     GETCPX40                                                         
                                                                                
         GOTOR SETLAV,(R4)          Set limit access values at OFF lvl          
         GOTOR CHKLAV                                                           
         JE    GETCPXNO                                                         
                                                                                
GETCPXX  DS    0H                                                               
         J     EXITY                                                            
GETCPXNO DS    0H                   cpxels block all access                     
         J     EXITNX                                                           
         DROP  R2,N                                                             
                                                                                
***********************************************************************         
* Set limit access values from Company extra element                  *         
* ENTRY: R1=A(CPXEL) TO PROCESS                                                 
***********************************************************************         
         SPACE 1                                                                
SETLAV   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETLAV*'                                                      
*                                                                               
         LR    R2,R1               Company extra element                        
N        USING CPXELD,R2                                                        
         LA    R3,CPXTAB                                                        
         USING CPXTABD,R3                                                       
         LHI   RF,CPXTABN                                                       
SETLAV04 LLC   R1,CPXTVAL                                                       
         LA    R4,LL_DACS(R1)                                                   
         CLI   0(R4),LL_ALLQ                                                    
         JNE   SETLAV08            OK if Access has already been set            
         LLC   R1,CPXTSTA                                                       
         LA    R1,N.CPXELD(R1)                                                  
         MVC   BYTE1,0(R1)         Extra status in CPXELD                       
         NC    BYTE1,CPXTACS       Test None access                             
         JZ    SETLAV08            No - OK                                      
         MVI   0(R4),LL_NONEQ                                                   
*                                                                               
SETLAV08 LA    R3,CPXTABL(,R3)                                                  
         JCT   RF,SETLAV04                                                      
         DROP  R3,N                                                             
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Check all limit access values have already been set                 *         
* Exit: cc = equal if all limit access values set                     *         
***********************************************************************         
         SPACE 1                                                                
CHKLAV   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKLAV*'                                                      
*                                                                               
         LA    RF,LL_DACS                                                       
         LHI   R1,LL_DACLQ                                                      
CHKLAV10 CLI   0(RF),LL_ALLQ                                                    
         JE    EXITN                                                            
         LA    RF,L'LL_DACS(,RF)                                                
         JCT   R1,CHKLAV10                                                      
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
CPXTABD  DSECT                                                                  
CPXTVAL  DS    XL1                                                              
CPXTSTA  DS    XL1                                                              
CPXTACS  DS    XL1                                                              
CPXTABL  EQU   *-CPXTABD                                                        
*                                                                               
TD_ACC   DSECT          TSAR DSECT FOR ACCOUNT FRAGMENTS                        
TD_AACC  DS    XL(L'ACTKACT)                                                    
TD_ACLN  DS    XL1      LENGTH OF FRAGMENT                                      
TD_ACCLQ EQU   *-TD_ACC                                                         
*                                                                               
WRD_D    DSECT          TSAR DSECT FOR SEARCH RESULTS                           
*RD_WORD DS    XL(L'SRCKWRD1)                                                   
WRD_SRT  DS    XL1                 SORT ORDER                                   
WRD_DA   DS    XL(L'SRCKDA)        DA OF FOUND REC                              
WRD_ACC  EQU   X'00'               ACC                                          
WRD_WRD  EQU   X'01'               WORD                                         
WRD_KEYL EQU  *-WRD_D                                                           
WRD_ACT  DS    XL(L'ACTKACT)       ACCOUNT CODE FOR POST-FILTERING              
WRD_IND  DS    XL2                 COUNT OF #MATCHES FOR A/C                    
WRD_LAST DS    CL1                 NUMBER OF LAST WORD MATCHED                  
WRD_RECL EQU  *-WRD_D                      ('AND' SEARCHING)                    
                                                                                
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYAPR   LKOUT A,(R,NXTAPR),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
Array    LKOUT C,01,(A,ARYAPSJ)                                                 
APRGEC   LKOUT C,01,AP_GESTC,CHAR                                               
APRGIEC  LKOUT C,02,AP_GIESC,CHAR                                               
APRGJOB  LKOUT C,03,AP_GJOB,CHAR                                                
APRGEF   LKOUT C,04,AP_GEXPF,CHAR                                               
Array    LKOUT C,02,(A,ARYAP1N)                                                 
Array    LKOUT C,03,(A,ARYAP1R)                                                 
Array    LKOUT C,04,(A,ARYINOR)                                                 
Array    LKOUT C,05,(A,ARYAPBU)                                                 
Array    LKOUT C,06,(A,ARYBAR)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPSJ  LKOUT A,(D,B#APPREC,APPRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,1,(A,ARYAPSJ1),FILTROUT=TSTAPSJ                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPSJ1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDSJLNQ            
                                                                                
PRout    LKOUT P,LIDDATA,SETAPSJ,ND=Y                                           
ASJOff   LKOUT C,1,LIDASJOF,CHAR,FILTROUT=TSTAPSEC,SKIPCOLS=ASJSKIPS,  +        
               ND=Y                                                             
ASJSKIP  EQU   *                                                                
ASJOnm   LKOUT C,2,(D,B#SAVED,AP_1RULA),(U,#EDTANM,$EDTANM),ND=Y                
ASJCcd   LKOUT C,3,(D,B#SAVED,AP_ULA),(U,#EDTCLI,$EDTCLI),ND=Y                  
ASJCnm   LKOUT C,4,(D,B#SAVED,AP_ULA),(U,#EDTCLN,$EDTCLN),ND=Y                  
ASJPcd   LKOUT C,5,(D,B#SAVED,AP_ULA),(U,#EDTPRD,$EDTPRD),ND=Y                  
ASJPnm   LKOUT C,6,(D,B#SAVED,AP_ULA),(U,#EDTPRN,$EDTPRN),ND=Y                  
ASJJcd   LKOUT C,7,(D,B#SAVED,AP_ULA),(U,#EDTJOB,$EDTJOB),ND=Y                  
ASJJnm   LKOUT C,8,(D,B#SAVED,AP_ULA),(U,#EDTJBN,$EDTJBN),ND=Y                  
ASJMed   LKOUT C,9,LIDASJME,CHAR,ND=Y                                           
ASJMnm   LKOUT C,10,LIDASJME,(U,#EDTMCN,$EDTMCN),ND=Y                           
ASJEst   LKOUT C,11,LIDASTAT,(R,EDTSTEST)                                       
ASJEsD   LKOUT C,12,LIDASTAT,(R,EDTSTESD)                                       
ASJEIA   LKOUT C,13,LIDASTA2,(R,EDTSTEIA)                                       
ASJEID   LKOUT C,14,LIDASTA2,(R,EDTSTEID)                                       
ASJE1B   LKOUT C,15,LIDASTA2,(R,EDTSTE1B)                                       
ASJE1N   LKOUT C,16,LIDASTA2,(R,EDTSTE1N)                                       
ASJE2B   LKOUT C,17,LIDASTA2,(R,EDTSTE2B)                                       
ASJE2N   LKOUT C,18,LIDASTA2,(R,EDTSTE2N)                                       
ASJJob   LKOUT C,19,LIDASTAT,(R,EDTSTJOB)                                       
ASJJbD   LKOUT C,20,LIDASTAT,(R,EDTSTJBD)                                       
ASJTim   LKOUT C,21,LIDASTAT,(R,EDTSTTIM)                                       
ASJSKIPS EQU   (*-ASJSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYAP1N  LKOUT A,(D,B#APPREC,APPRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,2,(A,ARYAP1N1),FILTROUT=TSTAP1N                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYAP1N1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LID1NLNQ            
                                                                                
PRout    LKOUT P,LIDA1NAC,SETAP1N                                               
A1NAct   LKOUT C,1,LIDA1NAC,CHAR,FILTROUT=TSTACT,SKIPCOLS=A1NSKIPS              
A1NSKIP  EQU   *                                                                
A1NNam   LKOUT C,2,(D,B#SAVED,AP_ULA),(U,#EDTANM,$EDTANM)                       
A1NTim   LKOUT C,3,LIDASTAT,(R,EDTSTTIM)                                        
A1NSKIPS EQU   (*-A1NSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYAP1R  LKOUT A,(D,B#APPREC,APPRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,3,(A,ARYAP1R1),FILTROUT=TSTAP1R                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYAP1R1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDAPLNQ            
                                                                                
PRout    LKOUT P,LIDAPACC,SETAP1R                                               
A1ROcd   LKOUT C,1,(D,B#SAVED,AP_1RULA),(U,#EDTOFF,$EDTOFF),           +        
               FILTROUT=TSTACT,SKIPCOLS=A1RSKIPS                                
A1RSKIP  EQU   *                                                                
A1ROnm   LKOUT C,2,(D,B#SAVED,AP_1RULA),(U,#EDTOFN,$EDTOFN)                     
A1RDcd   LKOUT C,3,(D,B#SAVED,AP_1RULA),(U,#EDTDPT,$EDTDPT),ND=Y                
A1RDnm   LKOUT C,4,(D,B#SAVED,AP_1RULA),(U,#EDTDPN,$EDTDPN),ND=Y                
A1RScd   LKOUT C,5,(D,B#SAVED,AP_1RULA),(U,#EDTSUB,$EDTSUB),ND=Y                
A1RSnm   LKOUT C,6,(D,B#SAVED,AP_1RULA),(U,#EDTSUN,$EDTSUN),ND=Y                
A1RPcd   LKOUT C,7,(D,B#SAVED,AP_1RULA),(U,#EDTPER,$EDTPER),ND=Y                
A1RPnm   LKOUT C,8,(D,B#SAVED,AP_1RULA),(U,#EDTPEN,$EDTPEN),ND=Y                
A1RTim   LKOUT C,9,LIDAPDTY,(R,EDT1RTIM)                                        
A1RECL   LKOUT C,10,LIDAPDTY,(R,EDT1RECL)                                       
A1REL2   LKOUT C,11,LIDAPDTY,(R,EDT1REL2)                                       
A1RECF   LKOUT C,12,LIDAPDTY,(R,EDT1RECF)                                       
A1RECD   LKOUT C,13,LIDAPDTY,(R,EDT1RECD)                                       
A1REAL   LKOUT C,14,LIDAPEXV,CPAK,ND=Y                                          
A1RSKIPS EQU   (*-A1RSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYINOR  LKOUT A,(D,B#APPREC,APPRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,4,(A,ARYINOR1),FILTROUT=TSTINOR                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYINOR1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDAPLNQ            
                                                                                
PRout    LKOUT P,LIDDATA,SETINOR                                                
AIOTyOr  LKOUT C,1,(D,B#SAVED,AP_APOR),CHAR,FILTROUT=TSTAPSEC,         +        
               SKIPCOLS=AIOSKIPS                                                
AIOSKIP  EQU   *                                                                
AIOTyOrD LKOUT C,2,(D,B#SAVED,AP_APORD),CHAR                                    
AIOTyOf  LKOUT C,3,(D,B#SAVED,AP_APOF),CHAR                                     
AIOTyOfD LKOUT C,4,(D,B#SAVED,AP_APOFD),CHAR                                    
AIOTyIn  LKOUT C,5,(D,B#SAVED,AP_APIN),CHAR                                     
AIOTyInD LKOUT C,6,(D,B#SAVED,AP_APIND),CHAR                                    
AIOSCat  LKOUT C,7,(D,B#SAVED,AP_SUBCT),CHAR                                    
AIOCcd   LKOUT C,8,(D,B#SAVED,AP_ULA),(U,#EDTCLI,$EDTCLI),             +        
               FILTROUT=TSTLGSJ,SKIPCOLS=5,ND=Y                                 
AIOCnm   LKOUT C,9,(D,B#SAVED,AP_ULA),(U,#EDTCLN,$EDTCLN),ND=Y                  
AIOPcd   LKOUT C,10,(D,B#SAVED,AP_ULA),(U,#EDTPRD,$EDTPRD),ND=Y                 
AIOPnm   LKOUT C,11,(D,B#SAVED,AP_ULA),(U,#EDTPRN,$EDTPRN),ND=Y                 
AIOJcd   LKOUT C,12,(D,B#SAVED,AP_ULA),(U,#EDTJOB,$EDTJOB),ND=Y                 
AIOJnm   LKOUT C,13,(D,B#SAVED,AP_ULA),(U,#EDTJBN,$EDTJBN),ND=Y                 
AIOOcd   LKOUT C,14,(D,B#SAVED,AP_1RULA),(U,#EDTOFF,$EDTOFF),ND=Y               
AIOOnm   LKOUT C,15,(D,B#SAVED,AP_1RULA),(U,#EDTOFN,$EDTOFN),ND=Y               
AIODcd   LKOUT C,16,(D,B#SAVED,AP_1RULA),(U,#EDTDPT,$EDTDPT),ND=Y               
AIODnm   LKOUT C,17,(D,B#SAVED,AP_1RULA),(U,#EDTDPN,$EDTDPN),ND=Y               
AIOETC   LKOUT C,18,LIDAPETY,CHAR,ND=Y                                          
AIOETN   LKOUT C,19,(D,B#SAVED,AP_ETYNM),CHAR,ND=Y                              
AIOSLC   LKOUT C,20,(D,B#SAVED,AP_LDG),CHAR,FILTROUT=TSTLGCR,          +        
               SKIPCOLS=3,ND=Y                                                  
AIOSLN   LKOUT C,21,(D,B#SAVED,AP_ULA),(R,EDTLDGNM),ND=Y                        
AIOSAC   LKOUT C,22,(D,B#SAVED,AP_ULA),CHAR,ND=Y                                
AIOSAN   LKOUT C,23,(D,B#SAVED,AP_ULA),(U,#EDTANM,$EDTANM),ND=Y                 
AIO2PC   LKOUT C,24,(D,B#SAVED,AP_ULA),CHAR,FILTROUT=TSTLG2P,          +        
               SKIPCOLS=1,ND=Y                                                  
AIO2PN   LKOUT C,25,(D,B#SAVED,AP_ULA),(U,#EDTANM,$EDTANM),ND=Y                 
AIOMEC   LKOUT C,26,LIDAPMED,CHAR,ND=Y                                          
AIOMEN   LKOUT C,27,LIDAPMED,(U,#EDTMCN,$EDTMCN),ND=Y                           
AIOSAL   LKOUT C,28,LIDAPSEL,CPAK                                               
AIOApL   LKOUT C,29,LIDAPVAL,CPAK                                               
AIOSKIPS EQU   (*-AIOSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPBU  LKOUT A,(D,B#APPREC,APPRFST),EOT=EOR,NEWEL=Y,                 +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
PRout    LKOUT P,LIDTYPE,SETTTYPE                                               
Array    LKOUT C,5,(A,ARYAPBU1),FILTROUT=TSTBACK                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPBU1 LKOUT A,(*,LIDDATA),ROWNAME=LIDEL,NROWS=*,ROWWIDTH=LIDLLN6Q            
                                                                                
BUpTim   LKOUT C,1,LIDLAPPL,(R,EDTAPTIM)                                        
BUpExp   LKOUT C,2,LIDLAPPL,(R,EDTAPEXP)                                        
BUpOrd   LKOUT C,3,LIDLAPPL,(R,EDTAPORD)                                        
BUpInv   LKOUT C,4,LIDLAPPL,(R,EDTAPINV)                                        
BUpPid   LKOUT C,5,LIDLPID,(U,#EDTPID,$EDTPID)                                  
PRout    LKOUT P,LIDLPID,SETPIDN                                                
BUpFNm   LKOUT C,6,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
BUpMNm   LKOUT C,7,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
BUpLNm   LKOUT C,8,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
                                                                                
TSTAPSEC CLI   AP_SEC,YESQ         Test security                                
         BR    RE                                                               
                                                                                
TSTAPSJ  CLI   MYELTYPE,LIDTAPSJ   Test client/product/job approver             
         BR    RE                                                               
                                                                                
TSTAP1N  CLI   MYELTYPE,LIDTAP1N   Test non client approver                     
         BR    RE                                                               
                                                                                
TSTAP1R  CLI   MYELTYPE,LIDTAP1R   Test non client approver                     
         BR    RE                                                               
                                                                                
TSTINOR  CLI   MYELTYPE,LIDTINOR   Test invoice/order approver                  
         BR    RE                                                               
                                                                                
TSTBACK  CLI   MYELTYPE,LIDTBACK   Test back up approver                        
         BR    RE                                                               
                                                                                
TSTLGSJ  L     R1,LP_AINP          Test SJ unit/ledger                          
         CLC   PRODUL,0(R1)                                                     
         BR    RE                                                               
                                                                                
TSTLGCR  L     R1,LP_AINP          Test creditor ledger                         
         CLI   0(R1),C'V'                                                       
         BER   RE                                                               
         CLI   0(R1),C'X'                                                       
         BER   RE                                                               
         CLI   0(R1),C'T'                                                       
         BER   RE                                                               
         CLI   0(R1),C'F'                                                       
         BR    RE                                                               
                                                                                
TSTLG2P  L     R1,LP_AINP          Test 2P ledger                               
         CLC   =C'2P',0(R1)                                                     
         BR    RE                                                               
                                                                                
         USING LIDDATA,R4                                                       
SETAPSJ  L     R4,LP_AINP                                                       
                                                                                
         MVI   AP_SEC,YESQ                                                      
         MVC   AP_UNT(2),PRODUL                                                 
         MVC   AP_ACT,LIDASJAC               Set SJ account                     
         MVC   AP_1RUNT(2),OFFUL                                                
         MVC   AP_1RACT,SPACES                                                  
         MVC   AP_1RACT(L'LIDASJOF),LIDASJOF Set 2D account                     
                                                                                
         CLC   LIDASJOF,SPACES                                                  
         JNH   SETAPSJ2                                                         
         MVC   QULA,AP_1RULA       Test 2D account security                     
         MVI   AC#IND1,AC#IRDLO    Read lowest level account only               
         GOTOR AVALACT             Read account and test security               
         JE    SETAPSJ2                                                         
         MVI   AP_SEC,NOQ                                                       
         J     EXIT                                                             
                                                                                
SETAPSJ2 CLC   LIDASJAC,SPACES                                                  
         JNH   EXIT                                                             
         MVC   QULA,AP_ULA         Test SJ account security                     
         MVI   AC#IND1,0           Read all levels for SJ                       
         GOTOR AVALACT             Read account and test security               
         JE    *+12                                                             
         MVI   AP_SEC,NOQ                                                       
         J     EXIT                                                             
                                                                                
         CLI   AP_GESTC,AP_GNONE   Test estimate approver set                   
         JNE   SETAPSJ4                                                         
         TM    LIDASTAT,LIDAESTY+LIDAESTD                                       
         JZ    *+8                                                              
         MVI   AP_GESTC,AP_GLIST                                                
SETAPSJ4 CLI   AP_GIESC,AP_GNONE   Test internal estimate appr set              
         JNE   SETAPSJ6                                                         
         TM    LIDASTA2,LIDAESI+LIDAESID                                        
         JZ    *+8                                                              
         MVI   AP_GIESC,AP_GLIST                                                
SETAPSJ6 CLI   AP_GJOB,AP_GNONE    Test job approver set                        
         JNE   SETAPSJ8                                                         
         TM    LIDASTAT,LIDAJOBY+LIDAJOBD                                       
         JZ    *+8                                                              
         MVI   AP_GJOB,AP_GLIST                                                 
SETAPSJ8 CLI   AP_GEXPF,AP_GNONE    Test expense finance set                    
         JNE   EXIT                                                             
         TM    LIDASTA2,LIDAEL1N+LIDAEL1N+LIDAEL2B+LIDAEL2N                     
         JZ    *+8                                                              
         MVI   AP_GEXPF,AP_GLIST                                                
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
SETAP1N  L     R1,LP_AINP          Set 1N non client account                    
         MVC   AP_UNT(2),NCLIUL                                                 
         MVC   AP_ACT,0(R1)                                                     
         MVC   QULA,AP_ACT         Set QULA for TSTACT routine                  
         J     EXIT                                                             
                                                                                
SETAP1R  L     R1,LP_AINP          Set 1R staff account                         
         MVC   AP_1RULA(2),COSTUL                                               
         MVC   AP_1RACT,0(R1)                                                   
         MVC   QULA,AP_1RULA       Set QULA for TSTACT routine                  
         J     EXIT                                                             
                                                                                
         USING LIDDATA,R4                                                       
SETINOR  L     R4,LP_AINP                                                       
         MVI   AP_SEC,YESQ                                                      
         MVC   AP_TYPES,=C'NNNNNN'                                              
         MVI   AP_SUBCT,C' '                                                    
         MVC   AP_ETYNM,SPACES                                                  
                                                                                
         CLI   LIDAPTYP,0                                                       
         JE    SETINOR2                                                         
         TM    LIDAPTYP,LIDAPOR    Test order approver                          
         JZ    *+8                                                              
         MVI   AP_APOR,YESQ                                                     
         TM    LIDAPTYP,LIDAPORD   Test order default approver                  
         JZ    *+8                                                              
         MVI   AP_APORD,YESQ                                                    
         TM    LIDAPTYP,LIDAPOF    Test order finance approver                  
         JZ    *+8                                                              
         MVI   AP_APOF,YESQ                                                     
         TM    LIDAPTYP,LIDAPOFD   Test order default finance approver          
         JZ    *+8                                                              
         MVI   AP_APOFD,YESQ                                                    
         TM    LIDAPTYP,LIDAPIN    Test invoice approver                        
         JZ    *+8                                                              
         MVI   AP_APIN,YESQ                                                     
         TM    LIDAPTYP,LIDAPIND   Test invoice default approver                
         JZ    *+8                                                              
         MVI   AP_APIND,YESQ                                                    
                                                                                
SETINOR2 CLI   LIDAPSCT,LIDADFT                                                 
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCDEF                                                
         CLI   LIDAPSCT,LIDACLI                                                 
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCCLI                                                
         CLI   LIDAPSCT,LIDANCLI                                                
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCNCL                                                
         CLI   LIDAPSCT,LIDAEXP                                                 
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCEXP                                                
         CLI   LIDAPSCT,LIDAPROD                                                
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCPRD                                                
         CLI   LIDAPSCT,LIDAART                                                 
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCART                                                
         CLI   LIDAPSCT,LIDAINT                                                 
         JNE   *+8                                                              
         MVI   AP_SUBCT,AP_SCINT                                                
                                                                                
                                                                                
         CLC   LIDAPETY,SPACES                                                  
         JNH   SETINOR4                                                         
         MVC   TEMP2(L'ETYKCODE),LIDAPETY                                       
         GOTOR (#GETETN,AGETETN)                                                
         MVC   AP_ETYNM,TEMP2                                                   
                                                                                
SETINOR4 MVI   AP_UNT,C'S'                                                      
         MVC   AP_LDG,LIDAPACL                                                  
         MVC   AP_ACT,LIDAPACA                 Set SJ/Creditor account          
                                                                                
         MVC   AP_1RUNT(2),COSTUL                                               
         MVC   AP_1RACT,SPACES                                                  
         MVC   AP_1RACT(L'LIDAPOFF),LIDAPOFF   Set 1R account                   
         LA    RF,AP_1RACT+1                                                    
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LA    RF,1(,RF)                        2 char office                   
         MVC   0(L'LIDAPDPT,RF),LIDAPDPT                                        
                                                                                
         CLC   AP_ACT,SPACES                                                    
         JNH   SETINOR6                                                         
         MVC   QULA,AP_ULA         Test SJ/Creditor account security            
         MVI   AC#IND1,0                                                        
         CLC   PRODUL,QUNIT                                                     
         JE    *+8                                                              
         OI    AC#IND1,AC#IRDLO    Read lowest level only if not SJ             
         GOTOR AVALACT                                                          
         JE    SETINOR6                                                         
         MVI   AP_SEC,NOQ                                                       
         J     EXIT                                                             
                                                                                
SETINOR6 CLC   AP_1RACT,SPACES                                                  
         JNH   EXIT                                                             
         MVC   QULA,AP_1RULA       Test 1R account security                     
         OI    AC#IND1,AC#IRDLO    Read lowest level only                       
         GOTOR AVALACT             Read account and test security               
         JE    EXIT                                                             
         MVI   AP_SEC,NOQ                                                       
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
EDTSTEST LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAESTY                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTESD LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAESTD                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTEIA LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAESI                                                    
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTEID LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAESID                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTE1B LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAEL1B                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTE1N LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAEL1N                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTE2B LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAEL2B                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTE2N LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAEL2N                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTJOB LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAJOBD                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTJBD LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAJOBD                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTSTTIM LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDATIME                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDT1RTIM LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAPDTI                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDT1RECL LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAPDEX                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDT1REL2 LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAPDE2                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDT1RECF LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAPDEF                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDT1RECD LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),LIDAPDED                                                   
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get Approver records                                               *          
***********************************************************************         
                                                                                
NXTAPR   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTAPR04                                                         
         OC    QPIDB,QPIDB                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
NXTAPR04 MVC   IOKEY,SVAPRKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,APPKEYT,('B#APPREC',0),          +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
                                                                                
         MVC   SVAPRKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         USING APPRECD,R2          R2=A(PERSONAL KEY STAGE KEY)                 
         XC    AP_VALS(AP_VLNQ),AP_VALS                                         
         MVI   AP_GESTC,AP_GNONE   Set default                                  
         MVI   AP_GIESC,AP_GNONE                                                
         MVI   AP_GJOB,AP_GNONE                                                 
         MVI   AP_GEXPF,AP_GNONE                                                
         TM    APPKSTA2,APPSESTQ   Test estimate approver                       
         JZ    *+8                                                              
         MVI   AP_GESTC,AP_GALL                                                 
         TM    APPKSTA2,APPSESIQ   Test internal estimate approver              
         JZ    *+8                                                              
         MVI   AP_GIESC,AP_GALL                                                 
         TM    APPKSTA2,APPSRJAQ   Test job approver                            
         JZ    *+8                                                              
         MVI   AP_GJOB,AP_GALL                                                  
         TM    APPKSTA2,APPSDJAQ   Test job default approver                    
         JZ    *+8                                                              
         MVI   AP_GJOB,AP_GDEFA                                                 
         TM    APPKSTAT,APPSFINA   Test expense finance approver                
         JZ    *+8                                                              
         MVI   AP_GEXPF,AP_GALL                                                 
         TM    APPKSTAT,APPSFIND   Test expense finance default appr            
         JZ    *+8                                                              
         MVI   AP_GEXPF,AP_GDEFA                                                
                                                                                
NXTAPRX  ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
                                                                                
ARYBAR   LKOUT A,(R,NXTBAR),MULTIROW=Y,ROWNAME=PIDRECD                          
                                                                                
BARPID   LKOUT C,1,PIDKPIDO,(U,#EDTPID,$EDTPID)                                 
PRout    LKOUT P,PIDKPIDO,SETPIDN                                               
BARFNm   LKOUT C,2,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
BARMNm   LKOUT C,3,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
BARLNm   LKOUT C,4,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
BARTim   LKOUT C,5,PIDKAPPL,(R,EDTAPTIM)                                        
BARExp   LKOUT C,6,PIDKAPPL,(R,EDTAPEXP)                                        
BARORD   LKOUT C,7,PIDKAPPL,(R,EDTAPORD)                                        
BARINV   LKOUT C,8,PIDKAPPL,(R,EDTAPINV)                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get Back up approver responsibilities record                       *          
***********************************************************************         
                                                                                
NXTBAR   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTBAR04                                                         
         OC    QPIDB,QPIDB                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
NXTBAR04 MVC   IOKEY,SVBARKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,PBRKEYT,('B#PID',0),(0,SAVED),   +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         MVC   SVBARKEY,IOKEY                                                   
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
ARYROL   LKOUT A,(R,NXTROL),MULTIROW=Y,ROWNAME=PIDRECD                          
                                                                                
RolNum   LKOUT C,1,PIDKNUM,LBIN                                                 
Array    LKOUT C,2,(A,ARYROLN)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYROLN  LKOUT A,(D,B#ROLE,ROLRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
RolNam   LKOUT C,2,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCRO   LKOUT A,(R,NXTCRO),MULTIROW=Y,ROWNAME=PIDRECD                          
                                                                                
CRoCli   LKOUT C,1,(D,B#SAVED,CR_ULA),(U,#EDTCLI,$EDTCLI)                       
CRoCNm   LKOUT C,2,(D,B#SAVED,CR_ULA),(U,#EDTCLN,$EDTCLN)                       
CRoPro   LKOUT C,3,(D,B#SAVED,CR_ULA),(U,#EDTPRD,$EDTPRD),ND=Y                  
CRoPNm   LKOUT C,4,(D,B#SAVED,CR_ULA),(U,#EDTPRN,$EDTPRN),ND=Y                  
CRoJob   LKOUT C,5,(D,B#SAVED,CR_ULA),(U,#EDTJOB,$EDTJOB),ND=Y                  
NamJNm   LKOUT C,6,(D,B#SAVED,CR_ULA),(U,#EDTJBN,$EDTJBN),ND=Y                  
CRoRNm   LKOUT C,7,PIDKROLE,LBIN                                                
Array    LKOUT C,8,(A,ARYCRON)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYCRON  LKOUT A,(D,B#ROLE,ROLRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
CRoNam   LKOUT C,8,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get Role record                                                    *          
***********************************************************************         
                                                                                
NXTROL   CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXTROL04                                                         
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAROLE                                                      
         JZ    NXTROL04                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DROLMAXQ*L'PIDKNUM                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA                                   
                                                                                
NXTROL04 MVC   IOKEY,SVROLKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,PRLKEYT,('B#PID',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
                                                                                
         MVC   SVROLKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,PIDKNUM),DROLEIND,DROLMAXQ,    +        
               LP_D                                                             
         DROP  R2                                                               
         J     NXTROL04                                                         
                                                                                
***********************************************************************         
* Get Client Role record                                             *          
***********************************************************************         
                                                                                
NXTCRO   CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXTCRO04                                                         
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DACROL                                                      
         JZ    NXTCRO04                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DCROMAXQ*L'PIDKNUM                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA                                   
                                                                                
NXTCRO04 MVC   IOKEY,SVCROKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,PCRKEYT,('B#PID',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
                                                                                
         MVC   SVCROKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
         MVC   CR_ULA(L'PRODUL),PRODUL                                          
         MVC   CR_ACT,PIDKSJAC                                                  
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,PIDKNUM),DCROLIND,DCROMAXQ,    +        
               LP_D                                                             
         DROP  R2                                                               
         J     NXTCRO04                                                         
                                                                                
         EJECT ,                                                                
                                                                                
**********************************************************************          
* Person List                                                        *          
**********************************************************************          
                                                                                
REQPLST  LKREQ H,A#PLST,OUTPLST,NEXTREQ=REQUESTX                                
ViewAll  LKREQ F,1,(D,B#SAVED,QVIEWAL),CHAR,OLEN=L'QVIEWAL,            +        
               MAXLEN=L'QVIEWAL,TEXT=(*,VIEWLIT),COL=*                          
OffCd    LKREQ F,2,(D,B#SAVED,QOFFC),CHAR,OLEN=L'QOFFC,                +        
               MAXLEN=L'QOFFC,TEXT=AC#OFFC,COL=*                                
Name     LKREQ F,3,(D,B#SAVED,QNAME),CHAR,OLEN=L'QNAME,                +        
               TEXT=(*,NAMELIT),COL=*                                           
                                                                                
         LKREQ E                                                                
                                                                                
OUTPLST  LKOUT H                                                                
                                                                                
PERLST   LKOUT R,A#PLST            Person listing                               
Array    LKOUT C,1,(A,ARYPER)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYPER   LKOUT A,(R,NXTPER),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
PerPin   LKOUT C,01,QPIDB,LBIN                                                  
PerOcd   LKOUT C,02,PE_ULA,(U,#EDTOFF,$EDTOFF)                                  
PerOnm   LKOUT C,03,PE_ULA,(U,#EDTOFN,$EDTOFN)                                  
PerDcd   LKOUT C,04,PE_ULA,(U,#EDTDPT,$EDTDPT)                                  
PerDnm   LKOUT C,05,PE_ULA,(U,#EDTDPN,$EDTDPN)                                  
PerScd   LKOUT C,06,PE_ULA,(U,#EDTSUB,$EDTSUB)                                  
PerSnm   LKOUT C,07,PE_ULA,(U,#EDTSUN,$EDTSUN)                                  
PerPcd   LKOUT C,08,PE_ULA,(U,#EDTPER,$EDTPER)                                  
PerPnm   LKOUT C,09,PE_ULA,(U,#EDTPEN,$EDTPEN)                                  
PerHDt   LKOUT C,10,PE_HIRDT,PDAT,ND=Y                                          
PerTDt   LKOUT C,11,PE_TERDT,PDAT,ND=Y                                          
PerCrA   LKOUT C,12,PE_CRACT,CHAR,ND=Y                                          
PerCrA   LKOUT C,13,PE_CRACT,CHAR,(U,#EDTANM,$EDTANM),ND=Y                      
Per2PA   LKOUT C,14,PE_2PACT,CHAR,ND=Y                                          
Per2PA   LKOUT C,15,PE_2PULA,CHAR,(U,#EDTANM,$EDTANM),ND=Y                      
Per2DA   LKOUT C,16,PE_2DACT,CHAR,ND=Y                                          
Per2DA   LKOUT C,17,PE_2DULA,CHAR,(U,#EDTANM,$EDTANM),ND=Y                      
Array    LKOUT C,A#PSEC,(A,ARYSEC)                                              
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get 1R account record                                               *         
***********************************************************************         
         SPACE 1                                                                
NXTPER   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPER04                                                         
         MVI   BYTE1,0                                                          
         CLI   QVIEWAL,0                                                        
         JZ    *+12                                                             
         CLI   QVIEWAL,QVIEWLNM                                                 
         JNH   NXTPER04                                                         
         MVI   QVIEWAL,QVIEWLOC    Set default to Location                      
         J     NXTPER12                                                         
                                                                                
NXTPER04 MVC   IOKEY,SVPERKEY                                                   
         CLI   QVIEWAL,QVIEWLOC                                                 
         JE    NXTPER08                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,SRCKEYT,('B#1RACT',0),           +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
                                                                                
         MVC   IODA,IOKEY+(SRCKDA-SRCRECD)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO4             R2 points to 1R account record               
         J     NXTPER12                                                         
                                                                                
NXTPER08 LA    R2,A1RKEYT          Single char office key                       
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LA    R2,A1R2KEYT         Two char office key                          
         GOTOR (#NXTREC,ANXTREC),DMCB,(R2),('B#1RACT',0),(0,SAVED),0,0          
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING ACTRECD,R2                                                       
                                                                                
NXTPER12 MVC   SVPERKEY,IOKEY                                                   
                                                                                
         XC    PE_VALS(PE_VLNQ),PE_VALS                                         
         MVC   PE_PERCD,SPACES                                                  
         MVC   PE_ULA,ACTKULA                                                   
         LLC   RF,ONERL4L                                                       
         LLC   R1,ONERL3L                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         LA    R1,PE_ACT(R1)       Disp. to Person code in 1R a/c               
         BASR  RE,0                                                             
         MVC   PE_PERCD(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         CLC   PE_PERCD,SPACES                                                  
         JNH   NXTPER04            Get next if no Person code                   
                                                                                
         MVI   BYTE1,X'FF'                                                      
         CLI   QVIEWAL,QVIEWLOC                                                 
         JE    NXTPER20                                                         
         CLC   QOFFC,SPACES                                                     
         JNH   NXTPER20                                                         
         LLC   RF,ONERL1L          Length of office code                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNE   NXTPER04            NO                                           
         CLC   ACTKACT(0),QOFFC                                                 
                                                                                
NXTPER20 LA    R4,ACTRFST                                                       
NXTPER22 CLI   0(R4),0             End of record                                
         JE    NXTPER40                                                         
         CLI   0(R4),RSTELQ                                                     
         JE    NXTPER26                                                         
         CLI   0(R4),GPNELQ        Name element                                 
         JE    NXTPER28                                                         
         CLI   0(R4),EMPELQ        Employee history element                     
         JE    NXTPER36                                                         
NXTPER24 LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         J     NXTPER22                                                         
                                                                                
         USING RSTELD,R4                                                        
NXTPER26 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   NXTPER24                                                         
         J     NXTPER04                                                         
                                                                                
         USING GPNELD,R4                                                        
NXTPER28 CLI   QVIEWAL,QVIEWFNM                                                 
         JNE   NXTPER30                                                         
         CLI   GPNTYP,GPNTFST      First name                                   
         JNE   NXTPER24                                                         
         J     NXTPER32                                                         
                                                                                
NXTPER30 CLI   QVIEWAL,QVIEWLNM                                                 
         JNE   NXTPER24                                                         
         CLI   GPNTYP,GPNTLST      Last name                                    
         JNE   NXTPER24                                                         
                                                                                
NXTPER32 LLC   R1,GPNLN                                                         
         SHI   R1,GPNLNQ           Length of name                               
         JZ    NXTPER04            Bad name element!!                           
         CLC   QNAME,GPNNME                                                     
         JNE   NXTPER04                                                         
                                                                                
         MVC   WORK,SPACES                                                      
         LA    RE,WORK                                                          
         LA    RF,GPNNME                                                        
NXTPER33 CLI   0(RF),C'A'                                                       
         JL    NXTPER34            Done if less than 'A'                        
         CLI   0(RF),C'}'                                                       
         JE    *+8                                                              
         CLI   0(RF),C'\'                                                       
         JE    *+14                Both '}' and '\' will be ignored             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         JCT   R1,NXTPER33                                                      
                                                                                
NXTPER34 CLC   WORK(L'SRCKWRD1),SVPERKEY+(SRCKWRD1-SRCRECD)                     
         JNE   NXTPER04                                                         
         J     NXTPER24                                                         
                                                                                
         USING EMPELD,R4                                                        
NXTPER36 CLI   EMPCSTAT,0                                                       
         JNE   NXTPER04            Only show active person                      
         J     NXTPER24                                                         
         DROP  R2,R4                                                            
                                                                                
NXTPER40 DS    0H                                                               
*&&UK                                                                           
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   *+12                                                             
*&&                                                                             
         CLI   CUACCS,0                                                         
         JE    NXTPER42                                                         
                                                                                
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,PE_ACT                                                  
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         MVI   OFFAOFFC+1,C' '                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office code                         
         JNE   NXTPER04                                                         
         DROP  R1                                                               
                                                                                
         USING PERRECD,R2                                                       
NXTPER42 LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,PE_PERCD                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   NXTPER04            Get next if no Person record                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
                                                                                
         L     R2,AIO1                                                          
         LA    R4,PERRFST                                                       
         XR    R0,R0                                                            
NXTPER44 CLI   0(R4),0                                                          
         JE    NXTPER60                                                         
         CLI   0(R4),EMPELQ        Employee history element                     
         JE    NXTPER52                                                         
         CLI   0(R4),PIDELQ        PERSON ID ELEMENT                            
         JE    NXTPER56                                                         
NXTPER48 IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         J     NXTPER44                                                         
                                                                                
         USING EMPELD,R4                                                        
NXTPER52 MVC   PE_HIRDT,EMPHIR                                                  
         MVC   PE_TERDT,EMPTRM                                                  
         J     NXTPER48                                                         
                                                                                
         USING PIDELD,R4                                                        
NXTPER56 MVC   QPIDB,PIDNO                                                      
         J     NXTPER48                                                         
         DROP  R2,R4                                                            
                                                                                
         USING PIDRECD,R2                                                       
NXTPER60 OC    QPIDB,QPIDB                                                      
         JZ    NXTPER04            Don't show this person if no PID             
                                                                                
         LA    R2,IOKEY                                                         
         XC    PIDKEY,PIDKEY       BUILD KEY TO READ                            
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,QPIDB                                                    
         MVI   PIDKSTYP,PIDKCRDQ                                                
         MVC   CSVKEY1,PIDKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
         CLC   PIDKEY(PIDKPER-PIDRECD),CSVKEY1                                  
         JNE   NXTPERX                                                          
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO1                                                          
         MVC   PE_CRACT,ACTKULA-ACTRECD(R4)                                     
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         XR    R0,R0                                                            
         USING SPAELD,R4                                                        
NXTPER64 CLI   SPAEL,0                                                          
         JE    NXTPERX                                                          
         CLI   SPAEL,SPAELQ                                                     
         JE    *+14                                                             
NXTPER68 IC    R0,SPALN                                                         
         AR    R4,R0                                                            
         J     NXTPER64                                                         
                                                                                
         CLI   SPATYPE,SPATDEPT    Test department account                      
         JNE   NXTPER72                                                         
         MVC   PE_2DUL,OFFUL                                                    
         MVC   PE_2DACT,SPAAACT                                                 
         J     NXTPER68                                                         
NXTPER72 CLI   SPATYPE,SPATPERS    Test personnel account                       
         JNE   NXTPER68                                                         
         MVC   PE_2PUL,=C'2P'                                                   
         MVC   PE_2PACT,SPAAACT                                                 
         J     NXTPER68                                                         
         DROP  R4                                                               
                                                                                
NXTPERX  ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GETUSF - Production User Field Download Request                     *         
***********************************************************************         
         SPACE 1                                                                
         USING USFTABD,R4                                                       
T        USING UFSELD,R5                                                        
GETUSF   NTR1                                                                   
         LA    R0,USRENT                                                        
         LHI   R1,USRLEN                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR SETVAL              Establish values                             
         MVI   BYTE4,0             Use BYTE4 to hold number of els              
         LA    R4,USFTAB                                                        
*                                                                               
         USING UFSRECD,R2                                                       
GETUSF02 XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         LA    R6,QUNIT+L'ACTKUNT+L'ACTKLDG                                     
         MVI   UFSKTYP,UFSKTYPQ                                                 
         MVI   UFSKSUB,UFSKSUBQ                                                 
         MVC   UFSKUNT,QUNIT                                                    
         MVC   UFSKLDG,QLEDG                                                    
         MVC   UFSKCPY,CUXCPY                                                   
         TM    USFSTAT,USFMGR      Media group?                                 
         JZ    *+10                                                             
         MVC   UFSKMGR,SVMEDGR                                                  
         TM    USFSTAT,USFMED      Media                                        
         JZ    GETUSF03                                                         
         LLC   RF,PPROLEN                                                       
         AR    RF,R6                                                            
         MVC   UFSKMED,0(RF)                                                    
*                                                                               
GETUSF03 TM    USFSTAT,USFOGR      Office group?                                
         JZ    *+10                                                             
         MVC   UFSKOFG,SVOFFGR                                                  
         TM    USFSTAT,USFOFF      Office                                       
         JZ    *+10                                                             
         MVC   UFSKOFC,AC_OFF                                                   
*                                                                               
         TM    USFSTAT,USFCLI      Client?                                      
         JZ    GETUSF3A                                                         
         LLC   RF,PCLILEN          Store cli/pro/job for later                  
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   UFSKCLI(0),0(R6)                                                 
         EX    RF,0(R1)                                                         
         OC    UFSKCLI,SPACES                                                   
*                                                                               
GETUSF3A TM    USFSTAT,USFPRO      Product?                                     
         JZ    GETUSF3B                                                         
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         LLC   RE,PCLILEN                                                       
         AR    RE,R6                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   UFSKPRO(0),0(RE)                                                 
         EX    RF,0(R1)                                                         
         OC    UFSKPRO,SPACES                                                   
*                                                                               
GETUSF3B TM    USFSTAT,USFJOB      Job?                                         
         JZ    GETUFS3C                                                         
         LLC   RF,PJOBLEN                                                       
         LLC   RE,PPROLEN                                                       
         SR    RF,RE                                                            
         LLC   RE,PPROLEN                                                       
         AR    RE,R6                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   UFSKJOB(0),0(RE)                                                 
         EX    RF,0(R1)                                                         
         OC    UFSKJOB,SPACES                                                   
*                                                                               
GETUFS3C GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   GETUSF22                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,UFSRFST                                                       
*                                                                               
         USING UFSELD,R3                                                        
GETUSF04 CLI   UFSEL,0                                                          
         JE    GETUSF22                                                         
         CLI   UFSEL,UFSELQ                                                     
         JE    GETUSF08                                                         
GETUSF06 LLC   R0,UFSLN                                                         
         AR    R3,R0                                                            
         J     GETUSF04                                                         
*                                                                               
GETUSF08 DS    0H                                                               
*&&US                                                                           
         TM    UFSSTAT,UFSSAUTH    Show on Authorization REC Only?              
         JO    GETUSF06            YES - Skip                                   
*&&                                                                             
         CLC   UFSDESC,SPACES                                                   
         JNH   GETUSF06                                                         
                                                                                
         LA    R5,USRENT                                                        
         LA    R0,MAXUFS                                                        
                                                                                
GETUSF10 OC    T.UFSELD(UFSLN1Q),T.UFSELD Data already there                    
         JZ    GETUSF14            no empty go for next                         
         CLC   UFSCODE,T.UFSCODE   yes- code match?                             
         JNE   GETUSF14            no                                           
         MVC   T.UFSELD(UFSLN1Q),UFSELD                                         
         OC    UFSCUT,UFSCUT       cut off date?                                
         JZ    GETUSF12            no - replace element                         
         CLC   UFSCUT,AC_OPNDT     yes - take it?                               
         JH    GETUSF12            yes                                          
*                                                                               
         BCTR  R0,0                                                             
         MVC   T.UFSELD(UFSLN1Q),UFSLN1Q(R5)                                    
         LA    R5,UFSLN1Q(R5)                                                   
         JCT   R0,*-10                                                          
         XC    T.UFSELD(UFSLN1Q),T.UFSELD                                       
         LLC   RF,BYTE4                                                         
         BCTR  RF,0                                                             
         STC   RF,BYTE4                                                         
         J     GETUSF06                                                         
*                                                                               
GETUSF12 MVC   T.UFSELD(UFSLN1Q),UFSELD                                         
         J     GETUSF06            get next element                             
                                                                                
GETUSF14 AHI   R5,UFSLN1Q          get next                                     
         JCT   R0,GETUSF10                                                      
                                                                                
         LA    R5,USRENT           no match - look for free spot                
         LA    R0,MAXUFS                                                        
                                                                                
GETUSF16 OC    T.UFSELD(UFSLN1Q),T.UFSELD free spot?                            
         JNZ   GETUSF20            no - keep looping                            
         OC    UFSCUT,UFSCUT       yes - cutoff date                            
         JZ    GETUSF18            no - add element                             
         CLC   UFSCUT,AC_OPNDT     yes - take it?                               
         JNH   GETUSF06            no                                           
                                                                                
GETUSF18 MVC   T.UFSEL(UFSLN1Q),UFSEL add element                               
         LLC   RF,BYTE4                                                         
         AHI   RF,1                                                             
         STC   RF,BYTE4                                                         
         J     GETUSF06            get next element                             
                                                                                
GETUSF20 LA    R5,UFSLN1Q(R5)      GET NEXT                                     
         JCT   R0,GETUSF16                                                      
                                                                                
GETUSF22 LA    R4,USFTABL(R4)      Bump to next table entry                     
         CLI   0(R4),X'FF'         Finished?                                    
         JNE   GETUSF02                                                         
         J     EXITY                                                            
         EJECT                                                                  
         DROP  R2,R3                                                            
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
         LKARY T                                                                
         EJECT                                                                  
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
***********************************************************************         
* Validation module: Account code                                     *         
* Etry: QULA   -> Unit/ledger account                                 *         
*       AC#IND1-> AC#IRDLO - Read lowest level account only           *         
* - Sets cc                                                           *         
***********************************************************************         
         DS    0H                                                               
VALACT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*VALACT*'                                                    
                                                                                
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO2                                        
         TM    AC#IND1,AC#IRDLO    Read lowest level a/c only                   
         JO    VALAC00             Yes                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO5                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO6                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
                                                                                
VALAC00  MVC   ANYACCNT,QUNIT                                                   
         OC    ANYACCNT,SPACES                                                  
         CLC   ANYACCD,SPACES                                                   
         JNH   EXITY                                                            
*                                                                               
         MVC   SVSJACT,QUNIT                                                    
         USING ACTRECD,R2                                                       
VALAC01  LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QUNIT                                                    
         MVC   ACTKLDG,QLEDG                                                    
         MVC   ANYACCNT(2),QUNIT                                                
         XR    RF,RF                                                            
         IC    RF,LDGAL1                                                        
         TM    AC#IND1,AC#IRDLO    Read lowest level a/c only                   
         JZ    *+8                 No - OK                                      
         LHI   RF,L'ACTKACT                                                     
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),ANYACCNT+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,0(RE)                                                         
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VALAC02                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    VALAC02                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN               Account not found                            
*                                                                               
VALAC02  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         MVC   AIOACC,AIO2                                                      
         L     R2,AIO2                                                          
         L     R0,AIO4             Copy record to IO4                           
         LA    R1,IOLENQ                                                        
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R2,ACTRFST                                                       
         TM    AC#IND1,AC#IRDLO    Read lowest level a/c only                   
         JO    VALAC03             Yes                                          
         MVI   AC_ACLVL,AC_LEVL1   Client or level 1 found                      
         CLI   LDGAL1,12           Lowest level account                         
         JNE   VALAC04             No                                           
VALAC03  OI    AC#IND1,AC#ILOW     Yes - Set low level account                  
                                                                                
         USING PPRELD,R2                                                        
VALAC04  CLI   PPREL,0                                                          
         JE    VALAC14                                                          
         CLI   PPREL,RSTELQ                                                     
         JE    VALAC10                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    VALAC08                                                          
                                                                                
VALAC06  LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALAC04                                                          
                                                                                
VALAC08  MVC   AC_OFF,PPRGAOFF                                                  
         MVC   AC_GRUP,PPRGRUP                                                  
         OC    AC_OFF,SPACES                                                    
         MVI   AC_SRLVL,AC_LEVL1                                                
         MVC   AC_SRULA,PPRRECVU                                                
         OC    AC_SRULA,SPACES                                                  
         MVI   AC_1CLVL,AC_LEVL1                                                
         MVC   AC_1CULA,PPRCOSTU                                                
         OC    AC_1CULA,SPACES                                                  
         J     VALAC06                                                          
                                                                                
         USING RSTELD,R2                                                        
VALAC10  MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         CLI   LDGAOP,LDGOFLT1     Test offices in filters                      
         JL    VALAC12                                                          
         PACK  DUB,LDGAOP          Office in filters                            
         CVB   R1,DUB              Value is of form X'F1'-X'F4'                 
         LA    R1,AC_FLTS-1(R1)                                                 
         CLI   0(R1),X'40'                                                      
         JNH   VALAC12                                                          
         MVC   AC_OFF(1),0(R1)                                                  
VALAC12  CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALAC06                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
*                                                                               
VALAC14  TM    AC#IND1,AC#IRDLO    Read lowest level a/c only                   
         JO    VALAC54             Yes - done                                   
         LLC   RE,LDGAL1                                                        
         LLC   RF,LDGAL2                                                        
         LTR   RF,RF               Do we have level 2 for this ledger           
         JZ    VALAC54             No                                           
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ANYACCD(RE)                                                   
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JNH   VALAC54                                                          
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),ANYACCNT+2                                            
         EX    RF,0(RE)                                                         
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO5)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VALAC16                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    VALAC16                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN                                                            
                                                                                
VALAC16  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO5                                                          
         LA    R2,ACTRFST                                                       
         MVC   AIOACC,AIO5                                                      
         MVI   AC_ACLVL,AC_LEVL2   Set level 2 account found                    
         CLI   LDGAL2,12           Lowest level account                         
         JNE   VALAC18             No                                           
         OI    AC#IND1,AC#ILOW     Yes - Set low level account                  
         USING PPRELD,R2                                                        
VALAC18  CLI   PPREL,0                                                          
         JE    VALAC28                                                          
         CLI   PPREL,RSTELQ                                                     
         JE    VALAC24                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    VALAC22                                                          
                                                                                
VALAC20  LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALAC18                                                          
                                                                                
VALAC22  MVC   AC_GRUP,PPRGRUP                                                  
         CLI   PPRGAOFF,X'40'                                                   
         JNH   VALAC22A                                                         
         MVC   AC_OFF,PPRGAOFF                                                  
         OC    AC_OFF,SPACES                                                    
VALAC22A CLC   PPRRECV,SPACES                                                   
         JNH   VALAC22B                                                         
         MVC   AC_SRULA,PPRRECVU                                                
         OC    AC_SRULA,SPACES                                                  
         MVI   AC_SRLVL,AC_LEVL2                                                
VALAC22B CLC   PPRCOST,SPACES                                                   
         JNH   VALAC20                                                          
         MVC   AC_1CULA,PPRCOSTU                                                
         OC    AC_1CULA,SPACES                                                  
         MVI   AC_1CLVL,AC_LEVL2                                                
         J     VALAC20                                                          
*                                                                               
         USING RSTELD,R2                                                        
VALAC24  MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         CLI   LDGAOP,LDGOFLT1     Test offices in filters                      
         JL    VALAC26                                                          
         PACK  DUB,LDGAOP          Office in filters                            
         CVB   R1,DUB              Value is of form X'F1'-X'F4'                 
         LA    R1,AC_FLTS-1(R1)                                                 
         CLI   0(R1),X'40'                                                      
         JNH   VALAC26                                                          
         MVC   AC_OFF(1),0(R1)                                                  
VALAC26  CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALAC20                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
*                                                                               
VALAC28  LLC   RE,LDGAL2                                                        
         LLC   RF,LDGAL3                                                        
         LTR   RF,RF                                                            
         JZ    VALAC54                                                          
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ANYACCD(RE)                                                   
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JNH   VALAC54                                                          
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         LLC   RF,LDGAL3                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),ANYACCNT+2                                            
         EX    RF,0(RE)                                                         
         L     R1,=AL4(IORDD+IODIR+IO6)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VALAC30                                                          
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ         app is connected                             
         JE    *+12                                                             
         CLI   IOERR,IOEDEL        If it is say job not found                   
         JE    VALAC30                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN                                                            
*                                                                               
VALAC30  XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ         app is connected                             
         JNE   VALAC31                                                          
         TM    ACTKSTAT,ACTSDELT   Is it deleted?                               
         JZ    VALAC31                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN               Say it is not found                          
*                                                                               
VALAC31  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO6'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO6                                                          
         LA    R2,ACTRFST                                                       
         MVC   AIOACC,AIO6                                                      
         MVI   AC_ACLVL,AC_LEVL3   Set level 3 account found                    
         CLI   LDGAL3,12           Lowest level account                         
         JNE   VALAC32             No                                           
         OI    AC#IND1,AC#ILOW     Yes - Set low level account                  
         USING PPRELD,R2                                                        
VALAC32  CLI   PPREL,0                                                          
         JE    VALAC42                                                          
         CLI   PPREL,RSTELQ                                                     
         JE    VALAC38                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    VALAC36                                                          
                                                                                
VALAC34  LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALAC32                                                          
                                                                                
VALAC36  MVC   AC_GRUP,PPRGRUP                                                  
         CLI   PPRGAOFF,X'40'                                                   
         JNH   VALAC36A                                                         
         MVC   AC_OFF,PPRGAOFF                                                  
         OC    AC_OFF,SPACES                                                    
VALAC36A CLC   PPRRECV,SPACES                                                   
         JNH   VALAC36B                                                         
         MVC   AC_SRULA,PPRRECVU                                                
         OC    AC_SRULA,SPACES                                                  
         MVI   AC_SRLVL,AC_LEVL3                                                
VALAC36B CLC   PPRCOST,SPACES                                                   
         JNH   VALAC36C                                                         
         MVC   AC_1CULA,PPRCOSTU                                                
         OC    AC_1CULA,SPACES                                                  
         MVI   AC_1CLVL,AC_LEVL3                                                
VALAC36C MVC   AC_PRNBL,PPRBILLP                                                
         MVC   AC_OTHR1,SPACES                                                  
         MVC   AC_OTHR2,SPACES                                                  
         MVC   AC_OTHR3,SPACES                                                  
         CLI   PPRLN,PPRLN1Q                                                    
         JNH   VALAC34                                                          
         MVC   AC_PRNBL,PPRBILLP                                                
         LLC   RF,PPRLN                                                         
         SHI   RF,PPRLN1Q+1                                                     
         BASR  RE,0                                                             
         MVC   AC_OTHRI(0),PPRNARRP                                             
         EX    RF,0(RE)                                                         
         J     VALAC34                                                          
*                                                                               
         USING RSTELD,R2                                                        
VALAC38  MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         CLI   LDGAOP,LDGOFLT1     Test offices in filters                      
         JL    VALAC40                                                          
         PACK  DUB,LDGAOP          Office in filters                            
         CVB   R1,DUB              Value is of form X'F1'-X'F4'                 
         LA    R1,AC_FLTS-1(R1)                                                 
         CLI   0(R1),X'40'                                                      
         JNH   VALAC40                                                          
         MVC   AC_OFF(1),0(R1)                                                  
VALAC40  CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALAC34                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
*                                                                               
VALAC42  LLC   RE,LDGAL3                                                        
         LLC   RF,LDGAL4                                                        
         LTR   RF,RF                                                            
         JZ    VALAC54                                                          
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ANYACCD(RE)                                                   
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JNH   VALAC54                                                          
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         LLC   RF,LDGAL4                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),ANYACCNT+2                                            
         EX    RF,0(RE)                                                         
         L     R1,=AL4(IORDD+IODIR+IO7)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    VALAC44                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    VALAC44                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN                                                            
                                                                                
VALAC44  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO7'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO7                                                          
         MVI   AC_ACLVL,AC_LEVL4   Set level 4 account found                    
         OI    AC#IND1,AC#ILOW     Set low level account                        
         MVC   AIOACC,AIO7                                                      
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VALAC46  CLI   PPREL,0                                                          
         JE    VALAC54                                                          
         CLI   PPREL,RSTELQ                                                     
         JE    VALAC50                                                          
                                                                                
VALAC48  IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALAC46                                                          
*                                                                               
         USING RSTELD,R2                                                        
VALAC50  MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         CLI   LDGAOP,LDGOFLT1     Test offices in filters                      
         JL    VALAC52                                                          
         PACK  DUB,LDGAOP          Office in filters                            
         CVB   R1,DUB              Value is of form X'F1'-X'F4'                 
         LA    R1,AC_FLTS-1(R1)                                                 
         CLI   0(R1),X'40'                                                      
         JNH   VALAC52                                                          
         MVC   AC_OFF(1),0(R1)                                                  
VALAC52  CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALAC48                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
*                                                                               
VALAC54  CLI   OFFIND,FULLYQ       Office based agency                          
         JNE   VALAC62                                                          
         CLI   LDGAOP,LDGONONE     Test any office in ledger                    
         JE    VALAC62                                                          
         CLI   LDGAOP,LDGOTRAN     Test offices in transactions                 
         JE    VALAC62                                                          
         CLI   LDGAOP,LDGOPROF     Test offices in profile element              
         JE    VALAC58                                                          
         CLI   LDGAOP,LDGOOFLS     Test office list in key                      
         JE    VALAC58                                                          
         CLI   LDGAOP,LDGOFLT1     Test offices in filters                      
         JNL   VALAC56                                                          
*                                                                               
         MVC   WORK(1),LDGAOP                                                   
         NI    WORK,FF-LDGOKEY2                                                 
         CLI   WORK,LDGOKEY                                                     
         JH    VALAC56                                                          
         XR    R1,R1                                                            
         IC    R1,WORK                                                          
         LA    R1,ANYACCNT+(ACTKACT-ACTRECD-2)(R1)                              
         MVC   AC_OFF+0(1),0(R1)                                                
         TM    LDGAOP,LDGOKEY2     Test 2 character office in key               
         JZ    VALAC56                                                          
         MVC   AC_OFF+1(1),1(R1)                                                
VALAC56  CLC   AC_OFF,SPACES                                                    
         JNE   VALAC58                                                          
         MVC   LP_ERROR,=AL2(AE$INVPO)                                          
         J     EXITN                                                            
*                                                                               
         USING OFFALD,R1                                                        
VALAC58  L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,AC_OFF                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office code                         
         JE    VALAC62                                                          
         CLC   PRODUL,QUNIT        ledger?                                      
         JNE   VALAC60                                                          
         L     R4,AIOACC                                                        
         L     R1,AIO2                                                          
         GOTOR ACHKOFF                                                          
         JE    VALAC62                                                          
VALAC60  MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
         DROP  R1,R2                                                            
VALAC62  DS    0H                                                               
*&&UK                                                                           
         CLI   CUACCS,0            Test any user limit access                   
         JE    VALAC64                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    VALAC64                                                          
         TM    CPXSTATA,CPXLACAC   Test limit account access in use             
         JZ    VALAC64                                                          
         ICM   RF,15,AIOACC                                                     
         JZ    VALAC64                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOPOS,LDGAOP                                                  
         ST    RF,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JE    VALAC64                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     EXITN                                                            
         DROP  R1                                                               
*&&                                                                             
VALAC64  CLC   AC_OFF,SPACES                                                    
         JNH   VALAC66                                                          
         MVC   TEMP2(L'AC_OFF),AC_OFF                                           
         GOTOR (#GETOFN,AGETOFN)                                                
         MVC   AC_OFFN,TEMP2                                                    
                                                                                
VALAC66  DS    0H                                                               
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Check for office code in LIDTPOFC                                   *         
* R1 =  A(ACTREC)                                                               
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
CHKOFF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*CHKOFF*'                                                    
         LR    R4,R1                                                            
         USING ACTRECD,R4                                                       
         LA    R4,ACTRFST                                                       
         USING LIDELD,R4                                                        
CHKOFF02 CLI   LIDEL,0                                                          
         JE    CHKOFFN             No valid office code found                   
         CLI   LIDEL,LIDELQ                                                     
         JNE   CHKOFF04                                                         
         CLI   LIDTYPE,LIDTPOFC      Product office codes                       
         JE    CHKOFF06                                                         
CHKOFF04 XR    RF,RF                                                            
         IC    RF,LIDLN                                                         
         AR    R4,RF                                                            
         J     CHKOFF02                                                         
CHKOFF06 XR    R0,R0                                                            
         LA    RF,LIDDATA-LIDELD                                                
         XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         SR    R1,RF               R1=Length of LIDDATA                         
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         DR    R0,RF               R1=No. of office codes in LIDDATA            
         LR    R3,R1                                                            
         LA    R4,LIDDATA          R4 points to the office codes                
*                                                                               
         USING OFFALD,R1                                                        
CHKOFF08 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(R4)                                                   
         MVI   OFFAACT,OFFAVAL                                                  
         LR    R0,RE                                                            
         GOTO1 VOFFAL                                                           
         LR    RE,R0                                                            
         JE    CHKOFFY                                                          
         LA    R4,L'TRNOFFC(R4)                                                 
         JCT   R3,CHKOFF08                                                      
         J     CHKOFFN                                                          
*                                                                               
CHKOFFY  J     EXITY                                                            
CHKOFFN  J     EXITN                                                            
         DROP  R4                                                               
***********************************************************************         
* Valadation Module: Check is connected user the approver             *         
* - SETS CC                                                           *         
***********************************************************************         
         SPACE 1                                                                
CHKAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*CHKAPP*'                                                    
         MVI   AC_APRVR,NOQ                                                     
         MVI   BYTE1,NOQ                                                        
         CLC   PRODUL,QUNIT                                                     
         JNE   CHKAPPX                                                          
                                                                                
         LLC   RF,PPROLEN                                                       
         LA    RF,ANYACCD(RF)                                                   
         CLI   0(RF),X'40'         Have we got a job                            
         JNH   CHKAPP02            No                                           
         MVC   AC_MEDIA,0(RF)                                                   
CHKAPP02 LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY       Find job, product or client approver         
CHKAPP04 XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVI   SJ.JOBPAPPL,JOBPAJOB                                             
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVC   SJ.JOBPCOFF,SPACES                                               
         MVC   SJ.JOBPCMED,SPACES                                               
         MVC   SJ.JOBPCPJ,SPACES                                                
         OC    APRSTAT,APRSTAT                                                  
         JNZ   CHKAPP06                                                         
         MVI   SJ.JOBPCODE,X'FF'                                                
         MVC   SJ.JOBPCODE+1(L'JOBPCODE-1),SJ.JOBPCODE                          
         J     CHKAPP14                                                         
*                                                                               
CHKAPP06 TM    APRSTAT,APRPRO                                                   
         JZ    CHKAPP08                                                         
         LLC   RF,PCLILEN                                                       
         LA    RF,ANYACCD(RF)                                                   
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   CHKAPP20            No                                           
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),ANYACCD                                            
         EX    RF,0(RE)                                                         
         J     CHKAPP10                                                         
CHKAPP08 TM    APRSTAT,APRCLI                                                   
         JZ    CHKAPP10                                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),ANYACCD                                            
         EX    RF,0(RE)                                                         
                                                                                
CHKAPP10 TM    APRSTAT,APRMED                                                   
         JZ    CHKAPP12                                                         
         CLC   AC_MEDIA,SPACES                                                  
         JNH   CHKAPP20                                                         
         MVC   SJ.JOBPCMED,AC_MEDIA                                             
                                                                                
CHKAPP12 TM    APRSTAT,APROFF                                                   
         JZ    CHKAPP14                                                         
         CLC   AC_OFF,SPACES                                                    
         JNH   CHKAPP20                                                         
         MVC   SJ.JOBPCOFF,AC_OFF                                               
                                                                                
CHKAPP14 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     CHKAPP18                                                         
                                                                                
CHKAPP16 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
                                                                                
CHKAPP18 CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),IOKEYSAV                             
         JE    CHKAPP22                                                         
         CLI   BYTE1,YESQ                                                       
         JE    CHKAPPX                                                          
CHKAPP20 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   CHKAPP04                                                         
         J     CHKAPPX                                                          
                                                                                
CHKAPP22 MVI   BYTE1,YESQ                                                       
         CLC   CCTPID,SJ.JOBPPIDB    client level approver found                
         JNE   CHKAPP16                                                         
         MVI   AC_APRVR,YESQ                                                    
         DROP  SJ                                                               
*                                                                               
CHKAPPX  J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* check for account in Limit List                                     *         
***********************************************************************         
ADVALL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ADVALL*'                                                      
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    ADVALLE                                                          
         GOTOR ACCRUL                                                           
         MVI   BYTE2,0                                                          
         MVI   BYTE3,0                                                          
         MVI   BYTE4,C' '                                                       
         XC    ELEMENT,ELEMENT                                                  
                                                                                
         MVI   BYTE1,LIDTCPJL                                                   
         CLC   ANYACUL,PRODUL                                                   
         JE    ADVALL02                                                         
         MVI   BYTE1,LIDTNCLL                                                   
         CLC   ANYACUL,NCLIUL                                                   
         JE    ADVALL10                                                         
         CLI   QAPPL,QAPPORD   Don't check suppliers for orders                 
         JE    ADVALL01                                                         
         MVI   BYTE1,LIDTSUPP                                                   
         CLC   ANYACUL,SXLEDGER                                                 
         JE    ADVALL10                                                         
         CLC   ANYACUL,SVLEDGER                                                 
         JE    ADVALL10                                                         
ADVALL01 MVI   BYTE1,LIDT1RAC                                                   
         CLC   ANYACUL,COSTUL                                                   
         JNE   ADVALLE                                                          
         J     ADVALL10                                                         
                                                                                
ADVALL02 XR    R1,R1               if job check media LimList                   
         LLC   R1,PPROLEN                                                       
         LA    R1,ANYACCD(R1)                                                   
         MVC   BYTE4,0(R1)         Byte 4 has media code                        
                                                                                
ADVALL10 LA    R1,ANYACCD+L'ACTKACT-1                                           
         LA    R4,L'ACTKACT                                                     
         CLI   LDGAL1,12                                                        
         JE    ADVALL14                                                         
                                                                                
ADVALL12 CLI   0(R1),C' '                                                       
         JH    ADVALL14                                                         
         SHI   R1,1                                                             
         JCT   R4,ADVALL12                                                      
         DC    H'0'                                                             
                                                                                
         USING LLSRECD,R2                                                       
ADVALL14 SHI   R4,1                R4 = length of entry (exec)                  
         LA    R2,IOKEY                                                         
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY1,LLSKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   LLSKEY(LLSKGRP-LLSKEY),CSVKEY1                                   
         JNE   ADVALL60                                                         
         MVI   LL_DACS,LL_ALLQ     Set default access                           
         MVC   LL_DACS+L'LL_DACS(LL_DACLQ-L'LL_DACS),LL_DACS                    
         J     ADVALL18                                                         
                                                                                
ADVALL16 LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1       Reread sequential GrpList record             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         LHI   RF,(LLSKGRP-LLSKEY)-1                                            
         CLI   LLSKSUB,LLSKSUBQ      Test Limit list record                     
         JE    *+8                                                              
         LHI   RF,(GLSKSEQ-GLSKEY)-1 No - use  Group list length                
         BASR  RE,0                                                             
         CLC   LLSKEY(0),CSVKEY1                                                
         EX    RF,0(RE)                                                         
         JNE   ADVALL60                                                         
ADVALL18 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
         L     R2,AIO1                                                          
         MVC   CSVKEY1,0(R2)       Save the record key                          
         AHI   R2,LLSRFST-LLSRECD                                               
         USING LIDELD,R2                                                        
ADVALL20 CLI   LIDEL,0                                                          
         JE    ADVALL16            (no entry found = OK)                        
         CLI   LIDEL,RSTELQ                                                     
         JE    ADVALL24                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   ADVALL22                                                         
         CLI   LIDTYPE,LIDTMEDL                                                 
         JE    ADVALL26                                                         
         CLC   LIDTYPE,BYTE1                                                    
         JE    ADVALL40                                                         
                                                                                
ADVALL22 LLC   R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     ADVALL20                                                         
                                                                                
         USING RSTELD,R2                                                        
ADVALL24 LLC   RF,RSTLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   ELEMENT(0),RSTELD                                                
         EX    RF,0(R1)                                                         
         J     ADVALL22                                                         
                                                                                
         USING LIDELD,R2                                                        
ADVALL26 CLI   BYTE4,C' '                                                       
         JNH   ADVALL22                                                         
         XR    R3,R3                                                            
         LLC   R3,LIDLN                                                         
         AR    R3,R2                                                            
         LA    R1,LIDDATA                                                       
M        USING LIDDATA,R1                                                       
*&&UK*&& MVI   LL_DMEDA,LL_LISTQ                                                
                                                                                
ADVALL28 CR    R1,R3                                                            
         JNL   ADVALL36            (missing in media limlist = bad)             
         LA    RF,APPLTAB                                                       
ADVALL30 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QAPPL,0(RF)                                                      
         JE    ADVALL32                                                         
         LA    RF,APPLTABL(RF)                                                  
         J     ADVALL30                                                         
                                                                                
ADVALL32 LLC   RE,1(RF)                                                         
         BASR  RF,0                                                             
         TM    M.LIDLAPPL,0                                                     
         EX    RE,0(RF)                                                         
         JZ    ADVALL34                                                         
                                                                                
*&&US*&& MVI   LL_DMEDA,LL_LISTQ                                                
         CLC   BYTE4,M.LIDLMED                                                  
         JE    ADVALL22            (match on media = OK)                        
ADVALL34 AHI   R1,LIDLLN3Q                                                      
         J     ADVALL28                                                         
         DROP  M                                                                
                                                                                
ADVALL36 CLI   LL_DMEDA,LL_ALLQ                                                 
         JNE   ADVALLH                                                          
                                                                                
ADVALL40 LLC   RF,LIDITLN                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R2                                                            
         LA    R1,LIDDATA                                                       
AC       USING LIDDATA,R1                                                       
*&&UK                                                                           
         MVI   BYTE2,1                                                          
         CLI   LIDTYPE,LIDTSUPP                                                 
         JNE   *+8                                                              
         MVI   LL_DSUPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDT1RAC                                                 
         JNE   *+8                                                              
         MVI   LL_DSTFA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTCPJL                                                 
         JNE   *+8                                                              
         MVI   LL_DCLIA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTEXPL                                                 
         JNE   *+8                                                              
         MVI   LL_DEXPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTWCL                                                  
         JNE   *+8                                                              
         MVI   LL_DWCA,LL_LISTQ                                                 
         CLI   LIDTYPE,LIDTNCLL                                                 
         JNE   *+8                                                              
         MVI   LL_DNCLA,LL_LISTQ                                                
*&&                                                                             
                                                                                
ADVALL44 CR    R1,R3                                                            
         JNL   ADVALL22                                                         
         LA    RF,APPLTAB                                                       
ADVALL46 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QAPPL,0(RF)                                                      
         JE    ADVALL48                                                         
         LA    RF,APPLTABL(RF)                                                  
         J     ADVALL46                                                         
                                                                                
ADVALL48 LLC   RE,1(RF)                                                         
         BASR  RF,0                                                             
         TM    AC.LIDLAPPL,0                                                    
         EX    RE,0(RF)                                                         
         JZ    ADVALL54                                                         
*&&US                                                                           
         MVI   BYTE2,1                                                          
         CLI   LIDTYPE,LIDTSUPP                                                 
         JNE   *+8                                                              
         MVI   LL_DSUPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDT1RAC                                                 
         JNE   *+8                                                              
         MVI   LL_DSTFA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTCPJL                                                 
         JNE   *+8                                                              
         MVI   LL_DCLIA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTEXPL                                                 
         JNE   *+8                                                              
         MVI   LL_DEXPA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTWCL                                                  
         JNE   *+8                                                              
         MVI   LL_DWCA,LL_LISTQ                                                 
         CLI   LIDTYPE,LIDTNCLL                                                 
         JNE   *+8                                                              
         MVI   LL_DNCLA,LL_LISTQ                                                
*&&                                                                             
         CLI   LIDTYPE,LIDTSUPP                                                 
         JNE   ADVALL49                                                         
         CLC   ANYACUL,AC.LIDLSUNT                                              
         JNE   ADVALL54                                                         
         CLI   AC.LIDLSACT,C' '                                                 
         JE    ADVALL56                                                         
         LA    RE,AC.LIDLSACT+L'LIDLACT-1                                       
         J     *+8                                                              
ADVALL49 LA    RE,AC.LIDLACT+L'LIDLACT-1                                        
         LA    RF,L'LIDLACT                                                     
         CLI   LDGAL1,12                                                        
         JE    ADVALL52                                                         
                                                                                
ADVALL50 CLI   0(RE),C' '                                                       
         JH    ADVALL52                                                         
         SHI   RE,1                                                             
         JCT   RF,ADVALL50                                                      
         J     ADVALL22                                                         
                                                                                
ADVALL52 SHI   RF,1                                                             
         LA    RE,AC.LIDLACT                                                    
         CLI   LIDTYPE,LIDTSUPP                                                 
         JNE   *+8                                                              
         LA    RE,AC.LIDLSACT                                                   
         CR    RF,R4                                                            
         JNH   *+6                                                              
         LR    RF,R4                                                            
         BASR  R6,0                                                             
         CLC   ANYACCD(0),0(RE)                                                 
         EX    RF,0(R6)                                                         
         JNE   ADVALL54                                                         
         CLI   LIDTYPE,LIDTCPJL    For client prod job need to check            
         JNE   ADVALL56                                  office                 
         CLC   AC.LIDLOFF,SPACES   Do we have an office                         
         JNH   ADVALL56            Accept if not                                
         CLC   AC_OFF,AC.LIDLOFF   If we do ensure they match                   
         JE    ADVALL56                                                         
                                                                                
ADVALL54 LLC   RF,LIDITLN                                                       
         AR    R1,RF                                                            
         J     ADVALL44                                                         
         DROP  AC                                                               
                                                                                
ADVALL56 MVI   BYTE3,1             Set we found entry                           
         CLI   BYTE4,C' '          Have we got to check media                   
         JNH   ADVALLE             No - then exit fine now                      
         J     ADVALL22                                                         
                                                                                
ADVALL60 CLI   BYTE3,1             Did we find a match                          
         JE    ADVALLE             Yes                                          
         LA    R2,ELEMENT                                                       
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    ADVALL76                                                         
         TM    RSTACST1,RSTAJOBS                                                
         JZ    *+8                                                              
         MVI   LL_DCLIA,LL_NONEQ                                                
         TM    RSTACST1,RSTAMED    Media code limit list                        
         JZ    *+8                                                              
         MVI   LL_DMEDA,LL_NONEQ                                                
         TM    RSTACST1,RSTAETYP   Expenditure type limit list                  
         JZ    *+8                                                              
         MVI   LL_DEXPA,LL_NONEQ                                                
         TM    RSTACST1,RSTA1NAC   Non-client limit list                        
         JZ    *+8                                                              
         MVI   LL_DNCLA,LL_NONEQ                                                
         TM    RSTACST1,RSTASTAF   1R costing account list                      
         JZ    *+8                                                              
         MVI   LL_DSTFA,LL_NONEQ                                                
         TM    RSTACST2,RSTASUPP   Supplier list                                
         JZ    *+8                                                              
         MVI   LL_DSUPA,LL_NONEQ                                                
                                                                                
ADVALL76 CLI   BYTE2,1                                                          
         JE    ADVALLL                                                          
         CLC   ANYACUL,PRODUL                                                   
         JNE   ADVALL78                                                         
         CLI   LL_DCLIA,LL_ALLQ                                                 
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
ADVALL78 CLC   ANYACUL,NCLIUL                                                   
         JNE   ADVALL80                                                         
         CLI   LL_DNCLA,LL_ALLQ                                                 
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
ADVALL80 CLC   ANYACUL,SXLEDGER                                                 
         JE    *+14                                                             
         CLC   ANYACUL,SVLEDGER                                                 
         JNE   ADVALLL                                                          
         CLI   LL_DSUPA,LL_ALLQ                                                 
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
                                                                                
ADVALLE  LA    RE,1                                                             
         J     ADVALLX                                                          
ADVALLL  SR    RE,RE                                                            
         J     ADVALLX                                                          
ADVALLH  LA    RE,2                                                             
ADVALLX  CHI   RE,1                                                             
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Access rules for connected agency                                   *         
***********************************************************************         
                                                                                
ACCRUL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ACCRUL*'                                                      
*                                                                               
         MVI   LL_DACS,LL_ALLQ     Set default access                           
         MVC   LL_DACS+L'LL_DACS(LL_DACLQ-L'LL_DACS),LL_DACS                    
                                                                                
         GOTOR SETLAV,SCPXEL       Set limit access values at company           
         GOTOR CHKLAV                                                           
         JE    ACCRULX             All values now set                           
                                                                                
         CLI   CUACCS,C'$'         List access single character                 
         JE    ACCRUL40            Yes - don't read for offices                 
         USING OFLPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    OFLPAS,OFLPAS                                                    
         MVI   OFLPTYP,OFLPTYPQ                                                 
         MVI   OFLPSUB,OFLPSUBQ                                                 
         MVC   OFLPREM,CUXCPY                                                   
         MVC   OFLPOFF,CUACCS+1                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFLPOFF,CUACCS+2                                                 
         MVC   CSVKEY1,OFLPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    ACCRUL20                                                         
         J     ACCRUL40                                                         
                                                                                
ACCRUL10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   ACCRUL40                                                         
                                                                                
ACCRUL20 CLC   OFLPAS(OFLPOFL-OFLPASD),CSVKEY1                                  
         JNE   ACCRUL40                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL30 CLI   N.CPXEL,0                                                        
         JE    ACCRUL10                                                         
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+14                                                             
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL30                                                         
*                                                                               
         GOTOR SETLAV,N.CPXEL      SET LIMIT ACCESS VALUES AT OFL LVL           
         J     ACCRUL10                                                         
         DROP  N                                                                
                                                                                
ACCRUL40 GOTOR CHKLAV                                                           
         JE    ACCRULX             OK - ALL VALUES NOW SET                      
                                                                                
         LA    R2,IOKEY            ELSE TRY OFFICE FOR SETTINGS                 
         USING OFFRECD,R2                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,CUACCS+1    *** THIS PROBABLY WON'T DO...                
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFFKOFF,CUACCS+2                                                 
         MVC   CSVKEY1,OFFKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   ACCRULX             NO OFFICE RECORD                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL50 CLI   N.CPXEL,0                                                        
         JE    ACCRULX                                                          
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+14                                                             
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL50                                                         
                                                                                
         GOTOR SETLAV,N.CPXEL    SET LIMIT ACCESS VALUES AT OFF LVL             
         DROP  N                                                                
         DROP  R2                                                               
                                                                                
ACCRULX  J     EXIT                                                             
                                                                                
         EJECT                                                                  
***********************************************************************         
* Check you can close job                                             *         
***********************************************************************         
         SPACE 1                                                                
CHKACT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKACT*'                                                      
         L     R4,AIO2                                                          
         USING ACTRECD,R4                                                       
         CLC   PRODUL,ACTKULA      Only do close check for jobs                 
         JNE   EXITY                                                            
         CLI   AC_ACLVL,AC_LEVL3                                                
         JNE   EXITY                                                            
         MVI   AC_CCLSE,YESQ                                                    
         MVI   AC_CDELT,NOQ                                                     
         MVI   AC_CXJOB,YESQ                                                    
         MVI   AC_TIMIT,NOQ                                                     
         MVI   AC_EXPIT,NOQ                                                     
         MVI   AC_PORIT,NOQ                                                     
         MVI   AC_XORIT,NOQ                                                     
***      MVI   AC_ESTIT,NOQ                                                     
         MVI   AC_DRFTX,NOQ                                                     
         LA    R2,IOKEY            Read for time passives                       
         USING TSJPASD,R2                                                       
         XC    TSJPAS,TSJPAS                                                    
         MVC   TSJPCPY,CUXCPY                                                   
         MVI   TSJPTYP,TSJPTYPQ                                                 
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVI   TSJPVIEW,TSJPSJAQ                                                
         MVC   TSJPACT,ACTKACT                                                  
         MVC   TSJPCOFF,AC_OFF                                                  
         MVC   TSJPMED,AC_MEDIA                                                 
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT04                                                         
*                                                                               
CHKACT02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
*                                                                               
CHKACT04 CLC   TSJPAS(TSJPPEDT-TSJPAS),CSVKEY1                                  
         JNE   CHKACT06                                                         
         TM    TSJPSTAT,TIMSDELT   Is time deleted                              
         JNZ   CHKACT02            Yes - ignore this                            
         MVI   AC_CDELT,YESQ                                                    
         MVI   AC_TIMIT,YESQ                                                    
         MVI   AC_CXJOB,NOQ                                                     
         TM    TSJPSTAT,TIMSFAPP   Is time fully approved                       
         JNZ   CHKACT02            Yes - ignore this                            
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
*                                                                               
         USING OSJPASD,R2                                                       
CHKACT06 LA    R2,IOKEY            Read for unapproved orders                   
         XC    IOKEY,IOKEY                                                      
         MVC   OSJPCPY,CUXCPY                                                   
         MVI   OSJPTYP,OSJPTYPQ                                                 
         MVI   OSJPSUB,OSJPSUBQ                                                 
         MVC   OSJPACT,ACTKACT                                                  
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT10                                                         
*                                                                               
CHKACT08 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
*                                                                               
CHKACT10 CLC   OSJPAS(OSJPMEM-OSJPAS),CSVKEY1                                   
         JNE   CHKACT12                                                         
         TM    OSJPSTAT,ORDSDEL+ORDSFMCH+ORDSLDEL+ORDCLOSE                      
         JNZ   CHKACT08            Get next record                              
         MVI   AC_CDELT,YESQ                                                    
         CLI   OSJPMEM,OSJPMNOQ    Production order                             
         JE    *+12                Yes                                          
         MVI   AC_XORIT,YESQ       No - must be an expense order                
         J     *+8                                                              
         MVI   AC_PORIT,YESQ                                                    
         TM    OSJPSTA2,ORDSAPPR   Is it fully approved                         
         JNZ   CHKACT08            Ignore this                                  
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
                                                                                
         USING EXJPASD,R2                                                       
CHKACT12 LA    R2,IOKEY            Read for unapproved expense claims           
         XC    IOKEY,IOKEY                                                      
         MVC   EXJPCPY,CUXCPY                                                   
         MVI   EXJPTYP,EXJPTYPQ                                                 
         MVI   EXJPSUB,EXJPSUBQ                                                 
         MVI   EXJPVIEW,EXJPCLI1                                                
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    *+8                 No                                           
         MVI   EXJPVIEW,EXJPCBL1                                                
         MVC   EXJPCOFF,AC_OFF                                                  
         MVC   EXJPCPJ,ACTKACT                                                  
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT16                                                         
*                                                                               
CHKACT14 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
*                                                                               
CHKACT16 CLC   EXJPAS(EXJPMED-EXJPAS),CSVKEY1                                   
         JNE   CHKACT18                                                         
         TM    EXJPSTAT,EXCSDELT+EXCSLOGD Is expense claim deleted              
         JNZ   CHKACT14            Yes - Get next record                        
         MVI   AC_CDELT,YESQ                                                    
         MVI   AC_CXJOB,NOQ                                                     
         MVI   AC_EXPIT,YESQ                                                    
         TM    EXJPSTAT,EXCSCOMP                                                
         JNZ   CHKACT14                                                         
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
                                                                                
CHKACT18 TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    CHKACT24            No                                           
         MVC   IOKEY,CSVKEY1                                                    
         MVI   EXJPVIEW,EXJPCNB1                                                
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT22                                                         
*                                                                               
CHKACT20 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
*                                                                               
CHKACT22 CLC   EXJPAS(EXJPMED-EXJPAS),CSVKEY1                                   
         JNE   CHKACT24                                                         
         TM    EXJPSTAT,EXCSDELT+EXCSLOGD Is expense claim deleted              
         JNZ   CHKACT20            Yes - Get next record                        
         MVI   AC_CDELT,YESQ                                                    
         MVI   AC_CXJOB,NOQ                                                     
         MVI   AC_EXPIT,YESQ                                                    
         TM    EXJPSTAT,EXCSCOMP                                                
         JNZ   CHKACT20                                                         
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
                                                                                
         USING ESTRECD,R2                                                       
CHKACT24 LA    R2,IOKEY            Read for unapproved expense claims           
         XC    IOKEY,IOKEY                                                      
         MVC   ESTKCPY,CUXCPY                                                   
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ESTKCLI(0),ACTKACT                                               
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         LA    R1,ACTKACT                                                       
         AR    R1,RF                                                            
         OC    ESTKCLI,SPACES                                                   
         LLC   RE,PPROLEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ESTKPRO(0),0(R1)                                                 
         EX    RE,0(RF)                                                         
         OC    ESTKPRO,SPACES                                                   
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RE,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ESTKJOB(0),0(R1)                                                 
         EX    RF,0(RE)                                                         
         OC    ESTKJOB,SPACES                                                   
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT28                                                         
*                                                                               
CHKACT26 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
*                                                                               
CHKACT28 CLC   ESTKEY(ESTKLNO-ESTKEY),CSVKEY1                                   
         JNE   CHKACT32                                                         
         TM    ESTKSTA1,ESTKDELT+ESTKLOGD Is expense claim deleted              
         JNZ   CHKACT26            Yes - Get next record                        
***      MVI   AC_ESTIT,YESQ                                                    
*                                                                               
CHKACT32 GOTO1 VHELLO,DMCB,(C'G',ACCMST),('ABLELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         USING ABLELD,R2                                                        
         CP    ABLCR,ABLDR         Does account balance                         
         JE    CHKACT34            Yes                                          
         MVI   AC_CCLSE,NOQ                                                     
         MVI   AC_CDELT,YESQ                                                    
         MVI   AC_CXJOB,NOQ                                                     
         J     EXITY                                                            
CHKACT34 GOTO1 VHELLO,DMCB,(C'G',ACCMST),('ASTELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JNE   CHKACT38                                                         
         L     R2,12(R1)                                                        
         USING ASTELD,R2                                                        
         OC    ASTDRAFT,ASTDRAFT                                                
         JZ    CHKACT38                                                         
         MVI   AC_DRFTX,YESQ                                                    
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('JCBELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JNE   CHKACT36                                                         
         L     R3,12(R1)                                                        
         USING JCBELD,R3                                                        
         SR    RF,RF                                                            
         ICM   RF,7,ASTDRAFT       SET RF=#DRAFT TRANSACTIONS                   
         SR    R1,R1               SET R6=#ADVANCE BILLS                        
         ICM   R1,3,JCBADV                                                      
         CR    RF,R1                                                            
         JNH   CHKACT38            ALL DRAFTS ARE ADVANCES                      
CHKACT36 MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
                                                                                
         USING TRNRECD,R2                                                       
CHKACT38 LA    R2,IOKEY                                                         
         MVC   TRNKEY,ACTKEY                                                    
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CHKACT42                                                         
*                                                                               
CHKACT40 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         LA    R2,IOKEY                                                         
*                                                                               
CHKACT42 CLC   TRNKEY(TRNKWORK-TRNKEY),CSVKEY1                                  
         JNE   EXITY               End of transactions                          
         CLC   TRNKCULC,SPACES                                                  
         JNH   CHKACT40                                                         
         CLC   TRNKDATE,SPACES                                                  
         JNH   CHKACT40                                                         
         CLC   TRNKREF,SPACES                                                   
         JNH   CHKACT40                                                         
         MVI   AC_CDELT,YESQ                                                    
         MVI   AC_CXJOB,NOQ                                                     
*&&UK                                                                           
         CLC   TRNKWORK,=C'99'                                                  
         JE    CHKACT40                                                         
*&&                                                                             
         LHI   R1,IOGET+IOMST+IO3                                               
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    *+8                                                              
         LHI   R1,IOGET+IOARC+IO3                                               
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         L     R3,AIO1                                                          
         USING PRORATAD,R3                                                      
         XC    PG$GEN(PA$VALS-PG$GEN),PG$GEN                                    
*&&UK                                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CPRORATA-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',TRNRECD),0,ACOMFACS,0,PRORATAD,0                
*&&                                                                             
*&&US*&& GOTO1 VPRORATA,DMCB,(X'00',TRNRECD),0,ACOMFACS,0,PRORATAD,0            
         CLC   TRNKWORK,=C'**'                                                  
         JNE   CHKACT50                                                         
         TM    CPYSTAT7,CPYSNCOB                                                
         JNZ   CHKACT46                                                         
                                                                                
* Check status first because zero amount could be fully billed                  
                                                                                
         TM    PG$STAT,PG$FULLB    Test order is fully billed                   
         JNZ   CHKACT40                                                         
         CP    PA$NET,=P'0'        Zero amount can't be fully billed            
         JE    CHKACT46                                                         
         DROP  R3                                                               
                                                                                
         LA    RE,TRNRFST                                                       
         USING PTAELD,RE           Test if order is billed - that's ok          
CHKACT44 LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         JE    CHKACT46            No billing on this order                     
         CLI   0(RE),PTAELQ                                                     
         JNE   CHKACT44                                                         
         CLI   PTATYPE,PTATRAL                                                  
         JNE   CHKACT44                                                         
         TM    PTASTAT1,PTASPEND                                                
         JNZ   CHKACT40                                                         
                                                                                
CHKACT46 MVC   CSVKEY2,TRNKEY                                                   
                                                                                
NEW      USING ORDRECD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    NEW.ORDKEY,NEW.ORDKEY                                            
         MVI   NEW.ORDKTYP,ORDKTYPQ                                             
         MVC   NEW.ORDKCPY,CUXCPY                                               
         MVC   NEW.ORDKORD,TRNKREF                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    CHKACT48                                                         
         MVC   IOKEY,CSVKEY2                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         J     CHKACT40                                                         
         DROP  NEW                                                              
*                                                                               
CHKACT48 MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
*                                                                               
         USING GOBLOCKD,RF                                                      
CHKACT50 L     RF,AGOBLOCB                                                      
         CLI   GOBILTYP,C'C'         Is this client billing?                    
         JE    CHKACT54              Yes, use different logic                   
         DROP  RF                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('TRNELQ',TRNRECD),0                   
         CLI   12(R1),0                                                         
         JNE   CHKACT40                                                         
         L     R3,12(R1)                                                        
         USING TRNELD,R3                                                        
         TM    TRNSTAT,TRNSHOLD                                                 
         JNO   *+12                                                             
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
*&&US                                                                           
         L     R3,AIO1                                                          
         USING PRORATAD,R3                                                      
         TM    PG$STAT,PG$FULLB    Test fully billed                            
         JNZ   CHKACT40                                                         
*&&                                                                             
                                                                                
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('TRSELQ',TRNRECD),0                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING TRSELD,R3                                                        
         OC    TRSUDAT,TRSUDAT       No, is it billed ?                         
         JNZ   CHKACT40              Yes                                        
                                                                                
         TM    TRNRSTAT,X'20'        Is it a reversal ?                         
         JO    CHKACT40              Yes - skip transactions                    
                                                                                
         CLC   TRNKULC(2),=C'SK'     Is the contra SK                           
         JNE   CHKACT52              No                                         
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
CHKACT52 GOTO1 VHELLO,DMCB,(C'G',ACCMST),('SPDELQ',TRNRECD),0                   
         CLI   12(R1),0                                                         
         JNE   CHKACT40                                                         
         L     R3,12(R1)                                                        
         USING SPDELD,R3                                                        
         CLC   SPDACCS(2),=C'SK'                                                
         JNE   CHKACT40                                                         
         MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
*                                                                               
CHKACT54 LA    R3,TRNRFST                                                       
         USING BNDELD,R3                                                        
         ZAP   AC_AMT,=P'0'                                                     
         LR    R4,R3                                                            
         XC    BYTE1,BYTE1                                                      
CHKACT56 LLC   R0,BNDLN                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         JE    CHKACT60            No billing on this order                     
         CLI   0(R3),TRNELQ                                                     
         JNE   CHKACT58                                                         
         LR    R4,R3                                                            
CHKACT58 CLI   0(R3),BNDELQ                                                     
         JNE   CHKACT56                                                         
*                                                                               
         CLC   BNDBNO,SPACES       Is this billed ?                             
         JNH   CHKACT56            No, get next                                 
         MVI   BYTE1,1             Set we found a bill                          
         ICM   RF,15,BNDAMNT                                                    
         CVD   RF,DUB                                                           
         AP    AC_AMT,DUB                                                       
         J     CHKACT56                                                         
*                                                                               
         USING TRNELD,R4                                                        
CHKACT60 CLI   BYTE1,1             Did we find a bill                           
         JNE   CHKACT40            No                                           
         CP    AC_AMT,TRNAMNT      Yes - does it match                          
         JNE   CHKACT62                                                         
*                                                                               
         USING PRORATAD,R6                                                      
         L     R6,AIO1                                                          
         TM    PG$STAT3,PG$FULUT   Fully utilized?                              
         JO    CHKACT40            Yes                                          
         TM    TRNSTAT,X'20'       No, is it a reversal ?                       
         JO    CHKACT40            Yes, skip it                                 
*                                                                               
CHKACT62 MVI   AC_CCLSE,NOQ                                                     
         J     EXITY                                                            
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
**********************************************************************          
* Filter group list record according to office security              *          
**********************************************************************          
                                                                                
FLTGRP   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTGRP*'                                                      
         LA    R2,IOKEY                                                         
         USING GLSRECD,R2                                                       
         CLC   GLSKOFF,SPACES                                                   
         JNH   EXITY                                                            
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,GLSKOFF                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         MVI   OFFAOFFC+1,C' '                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office code                         
         JNE   EXITN                                                            
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Interface to TSAR                                                   *         
* P1(1)   = ACTION                                                    *         
* P1(2-4) = A(TSAR BLOCK)                                                       
* P2(1)   = KEY LENGTH IF INIT                                                  
* P2(2-4) = A(RECORD IOAREA) IF INIT, SORTCARD IF ACTION IS SORT                
* P3(1)   = TSRECI BUFFER ID BITS IF INIT, ONLINE  (TSRXTN ALWAYS SET)          
* P3(2)   = N/D                                                                 
* P3(3-4) = MAX REC LENGTH IF INIT                                              
* ACQUIRES 1MB OF STORAGE IF OFFLINE                                            
* ERROR BITS RETURNED IN TSARERRS, CC SET IF ANY ON                             
***********************************************************************         
*                                                                               
GOTSAR   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         LR    R2,R1               R2=A(Caller's parameter list)                
         L     R3,0(R2)                                                         
         USING TSARD,R3            R3=A(TSAR block)                             
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         L     R0,4(R2)                                                         
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,1024                                                          
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVC   TSRECI,8(R2)        IDENTIFY BUFFER TO USE IF ONLINE             
         OI    TSRECI,TSRXTN                                                    
         MVC   TSKEYL,4(R2)        Set key length                               
*                                                                               
         MVC   TSRECL,10(R2)       Set maximum record length                    
*                                                                               
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
GOTSAR02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    GOTSAR06                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
                                                                                
         CLI   TSACTN,TSASRT       Test sorting                                 
         JNE   GOTSAR04                                                         
         L     R1,4(R2)                                                         
         MVC   TSRTPARM,0(R1)      Yes - set sort parameters                    
                                                                                
GOTSAR04 GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   TSARERRS,TSERRS     Return TSARERRS                              
         J     GOTSARX                                                          
                                                                                
GOTSAR06 MVI   TSARERRS,TSEEOF                                                  
                                                                                
GOTSARX  CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Locate an element in record pointed to by (R1)                      *         
*                                                                     *         
* Ntry:- R1-B0    =Element code                                       *         
*        R1-B1-3  =A(record)                                          *         
* Exit:- R1=A(Element) and CC=Equal if element found                  *         
*        CC=Not equal if element not found                            *         
***********************************************************************         
*                                                                               
GETELA   NTR1  BASE=*,LABEL=*                                                   
         L     R1,0(R1)                                                         
         STCM  R1,8,BYTE1                                                       
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
GETELA02 CLI   0(R1),0             Test end of record                           
         JNE   GETELA10                                                         
         J     EXITNX                                                           
*                                                                               
GETELA10 CLC   BYTE1,0(R1)         Test correct element                         
         JE    GETELAX             Yes - exit with CC equal                     
         IC    R0,1(R1)            No - bump to next element on record          
         AR    R1,R0                                                            
         J     GETELA02                                                         
*                                                                               
GETELAX  XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
* READ AND SET VALUES FOR USER FIELD KEYS                             *         
***********************************************************************         
         SPACE                                                                  
         USING PMDRECD,R2                                                       
SETVAL   NTR1  LABEL=*,BASE=*                                                   
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   SVMEDGR,SPACES                                                   
         MVC   SVOFFGR,SPACES                                                   
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         LLC   RF,PPROLEN                                                       
         LA    RF,QUNIT+L'ACTKUNT+L'ACTKLDG(RF)                                 
         CLI   0(RF),C' '                                                       
         JNH   SETVAL06                                                         
         MVC   PMDKMED,0(RF)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   *+2                 Media should exist                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         LA    RE,PMDRFST                                                       
         USING PMDELD,RE                                                        
                                                                                
SETVAL02 CLI   PMDEL,PMDELQ                                                     
         JE    SETVAL04                                                         
         CLI   PMDEL,0                                                          
         JE    SETVAL06                                                         
         LLC   R0,PMDLN                                                         
         AR    RE,R0                                                            
         J     SETVAL02                                                         
                                                                                
SETVAL04 MVC   SVMEDGR,PMDGRP      Extract media group                          
*                                                                               
         USING OGRRECD,R2                                                       
SETVAL06 XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUXCPY                                                   
         MVC   OGRKUNT(L'PRODUL),PRODUL                                         
         MVC   OGRKOFC(L'AC_OFF),AC_OFF                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   *+2                 office should exist                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         LA    RE,OGRRFST                                                       
         USING PGRELD,RE                                                        
                                                                                
SETVAL08 CLI   PGREL,PGRELQ                                                     
         JE    SETVAL10                                                         
         CLI   PGREL,0                                                          
         JE    SETVAL12                                                         
         LLC   R0,PGRLN                                                         
         AR    RE,R0                                                            
         J     SETVAL08                                                         
*                                                                               
SETVAL10 MVC   SVOFFGR,PGRCODE     Extract office group                         
*                                                                               
SETVAL12 DS    0H                                                               
*                                                                               
SETVALX  J     EXIT                                                             
         EJECT                                                                  
         DROP  R2,RE                                                            
***********************************************************************         
* BUFFER CLIENT/PRODUCT RECORDS                                       *         
***********************************************************************         
         SPACE                                                                  
         USING ACTRECD,R2                                                       
BUFCPL   NTR1  LABEL=*,BASE=*                                                   
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO4                                        
*                                                                               
         CLI   QHIER,C' '          No hierarchy filtering                       
         JE    *+12                                                             
         CLI   BINHIER,1           Are we only looking at client level          
         JNH   BUFCPLX                                                          
*                                                                               
         LLC   RF,LDGAL1           RE=client length                             
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '          Is this client or product level              
         JNH   BUFCPL02                                                         
         LLC   RF,LDGAL2           RE=product length                            
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '          Is this product/job level                    
         JH    BUFCPLX                                                          
         CLI   BINHIER,2           Just want to product level                   
         JE    BUFCPLX             Then don't buffer                            
*                                                                               
BUFCPL02 L     RF,AIO4                                                          
         MVC   0(L'ACTKEY,RF),ACTKEY                                            
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    BUFCPLX             Check whether buffered already               
*                                                                               
         L     R0,AIO4                                                          
         L     RE,AIO2                                                          
         LHI   R1,IOLENQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)                                  
*                                                                               
BUFCPLX  J     EXITY               If not add it                                
         DROP  R2                                                               
***********************************************************************         
* BUFFER CLIENT/PRODUCT RECORDS                                       *         
***********************************************************************         
         SPACE                                                                  
         USING ACTRECD,R2                                                       
CHKCPL   NTR1  LABEL=*,BASE=*                                                   
                                                                                
         CLI   QHIER,C' '          No hierarchy filtering                       
         JE    *+12                                                             
         CLI   BINHIER,1           Are we only looking at client level          
         JNH   CHKCPLY                                                          
*                                                                               
         LLC   RF,LDGAL1           RE=client length                             
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '          Is this client or product level              
         JNH   CHKCPLY                                                          
*                                                                               
         LA    R3,LDGAL1                                                        
         LLC   RF,LDGAL2           RE=product length                            
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '          Is this product/job level                    
         JNH   CHKCPL02                                                         
         LA    R3,LDGAL2                                                        
*                                                                               
CHKCPL02 MVC   IOKEY,ACTKEY                                                     
         LA    R1,IOKEY+(ACTKACT-ACTRECD)                                       
         LLC   RE,0(R3)            RE=product length                            
         AR    R1,RE                                                            
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),SPACES      Clear out key to be product length           
         EX    RF,0(RE)                                                         
*                                                                               
         L     RE,AIO4                                                          
         MVC   0(L'ACTKEY,RE),IOKEY                                             
*                                                                               
         GOTOR GOTSAR,DMCB,('TSARDH',TSARSBUF)                                  
         JE    CHKCPL04                                                         
         OI    RUNFLAGS,RFNOSEQ    GETOPT BREAKS READ SEQ                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   CHKCPLN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GOTSAR,DMCB,('TSAADD',TSARSBUF)                                  
*                                                                               
CHKCPL04 L     RF,AIO4                                                          
         TM    ACTRSTAT-ACTRECD(RF),ACTSLOCK                                    
         JNZ   CHKCPLN             Was product/client locked?                   
         LA    RF,LDGAL1                                                        
         CR    R3,RF                                                            
         JE    CHKCPLY                                                          
         JCT   R3,CHKCPL02                                                      
*                                                                               
CHKCPLY  J     EXITY                                                            
*                                                                               
CHKCPLN  J     EXITN                                                            
         EJECT                                                                  
**********************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   SET NORE TO COME                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         STCM  R0,3,LP_ERROR                                                    
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITN    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITNX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITNX   LTR   RB,RB                                                            
         J     EXIT                                                             
EXITY    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITYX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITYX   CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
MAXUFS   EQU  10                   Maximum no. user fields for display          
USRLEN   EQU   MAXUFS*UFSLN1Q      Total length of user fields                  
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
*                                                                               
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
ACTDDL#  DC    AL2(A#ACTD)         Account/client/product/job display           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
ACTSDL#  DC    AL2(A#ACTS)         Account/client/prod/job list/search          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
PSECDL#  DC    AL2(A#PSEC)         Person download                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
PLSTDL#  DC    AL2(A#PLST)         Person listing                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
APRTAB   DS    0XL1                                                             
         DC    B'11011000'         Office/client/product/media                  
         DC    B'10011000'         Office/client/media                          
         DC    B'10010000'         Client/media                                 
         DC    B'00011000'         Office/media                                 
         DC    B'00010000'         Media                                        
         DC    B'11001000'         Office/client/product                        
         DC    B'10001000'         Office/client                                
         DC    B'10000000'         Client                                       
         DC    B'00001000'         Office                                       
         DC    B'00000000'         Agency                                       
         DC    X'FF'                                                            
                                                                                
USFTAB   DS    0XL1                User field table                             
         DC    B'00000000'         Company                                      
         DC    B'10000000'         Media group                                  
         DC    B'01000000'         Media                                        
         DC    B'00100000'         Office group                                 
         DC    B'10100000'         Office group, media group                    
         DC    B'01100000'         Office group, media                          
         DC    B'00010000'         Office                                       
         DC    B'10010000'         Office, media group                          
         DC    B'01010000'         Office, media                                
         DC    B'00001000'         Client                                       
         DC    B'10001000'         Client, media group                          
         DC    B'01001000'         Client, media                                
         DC    B'00001100'         Client, product                              
         DC    B'01001100'         Client, product, media                       
         DC    B'00001110'         Client, product, job                         
         DC    X'FF'                                                            
                                                                                
CPXTAB   DC    0XL3                                                             
                                                                                
         DC    AL1(LL_DCLIA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXAJOBS)                                                    
                                                                                
         DC    AL1(LL_DMEDA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXAMED)                                                     
                                                                                
         DC    AL1(LL_DEXPA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXAETYP)                                                    
                                                                                
         DC    AL1(LL_DNCLA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXA1NAC)                                                    
                                                                                
         DC    AL1(LL_DSTFA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXASTAF)                                                    
                                                                                
         DC    AL1(LL_DWCA-LL_DACS)                                             
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXAWC)                                                      
                                                                                
         DC    AL1(LL_DREPA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXAREPF)                                                    
                                                                                
         DC    AL1(LL_DESTA-LL_DACS)                                            
         DC    AL1(CPXSTAT3-CPXELD)                                             
         DC    AL1(CPXASCHM)                                                    
                                                                                
         DC    AL1(LL_DSUPA-LL_DACS)                                            
         DC    AL1(CPXSTAT4-CPXELD)                                             
         DC    AL1(CPXASUPP)                                                    
                                                                                
CPXTABN  EQU   (*-CPXTAB)/L'CPXTAB                                              
*                                                                               
APPLTAB  DS    0XL1                                                             
         DC    AL1(QAPPTIM,LIDLTIME)                                            
APPLTABL EQU   *-APPLTAB                                                        
         DC    AL1(QAPPORD,LIDLORDS)                                            
         DC    AL1(QAPPEST,LIDLESTM)                                            
         DC    AL1(QAPPEJB,LIDLESTM)                                            
         DC    AL1(QAPPEXP,LIDLEXPN)                                            
         DC    AL1(QAPPJOB,LIDLJOBS)                                            
         DC    AL1(QAPPREP,LIDLREPT)                                            
         DC    AL1(QAPPINV,LIDLINVC)                                            
         DC    AL1(QAPPRES,LIDLRESC)                                            
         DC    AL1(QAPPDSB,LIDLJOBS)                                            
         DC    X'FF'                                                            
*                                                                               
REPTBL   DC    0H                                                               
REPRCV   DC    AL1(REPRCVLN)        LENGTH OF REC    - DEBTORS                  
         DC    AL1(LL_DRQ)          DDICT ENTRY                                 
         DC    AL1(REP#RCV,REP#ADV,REP#BAL)   CODE THAT GO WITH DDICT           
REPRCVLN EQU   *-REPRCV                                                         
                                                                                
REPINC   DC    AL1(REPINCLN)                         - INCOME                   
         DC    AL1(LL_INCQ)                                                     
         DC    AL1(REP#INC,REP#SUP,REP#ICST)                                    
REPINCLN EQU   *-REPINC                                                         
                                                                                
REPPAY   DC    AL1(REPPAYLN)                         - CREDITORS                
         DC    AL1(LL_CRQ)                                                      
         DC    AL1(REP#PAY,REP#PAYQ,REP#PAYS,REP#PAYT,REP#PAYU)                 
         DC    AL1(REP#PAYV,REP#PAYW,REP#PAYX,REP#PAYY,REP#PAYC)                
         DC    AL1(REP#PAYF)                                                    
REPPAYLN EQU   *-REPPAY                                                         
                                                                                
REPEXP   DC    AL1(REPEXPLN)                         - EXPENSES                 
         DC    AL1(LL_EXPQ)                                                     
         DC    AL1(REP#EXP,REP#EXPF,REP#EXPL,REP#EXPD,REP#EXPB)                 
         DC    AL1(REP#EXPP)                                                    
REPEXPLN EQU   *-REPEXP                                                         
                                                                                
REPPRO   DC    AL1(REPPROLN)                         - PRODUCTION               
         DC    AL1(LL_PRODQ)                                                    
         DC    AL1(REP#PROD)                                                    
REPPROLN EQU   *-REPPRO                                                         
                                                                                
REPCST   DC    AL1(REPCSTLN)                         - MANPOWER                 
         DC    AL1(LL_MANQ)                                                     
         DC    AL1(REP#CST)                                                     
REPCSTLN EQU   *-REPCST                                                         
                                                                                
REPCSH   DC    AL1(REPCSHLN)                         - BANK                     
         DC    AL1(LL_CASQ)                                                     
         DC    AL1(REP#CASH)                                                    
REPCSHLN EQU   *-REPCSH                                                         
                                                                                
REPPNL   DC    AL1(REPPNLLN)                         - PROFIT & LOSS            
         DC    AL1(LL_PNLQ)                                                     
         DC    AL1(REP#PNL)                                                     
REPPNLLN EQU   *-REPPNL                                                         
                                                                                
REPGLG   DC    AL1(REPGLGLN)                         - FINANCIALS               
         DC    AL1(LL_FINQ)                                                     
         DC    AL1(REP#GNL,REP#GNLP)                                            
REPGLGLN EQU   *-REPGLG                                                         
                                                                                
REPMED   DC    AL1(REPMEDLN)                         - MEDIA                    
         DC    AL1(LL_MEDQ)                                                     
         DC    AL1(REP#MEDA)                                                    
REPMEDLN EQU   *-REPMED                                                         
                                                                                
REPFI    DC    AL1(REPFILN)                                                     
         DC    AL1(LL_FINQ)                                                     
         DC    AL1(REP#FI)                                                      
REPFILN  EQU   *-REPFI                                                          
                                                                                
REPMAN   DC    AL1(REPMANLN)                                                    
         DC    AL1(LL_MANQ)                                                     
         DC    AL1(REP#M2)                                                      
REPMANLN EQU   *-REPMAN                                                         
                                                                                
         DC    X'00'                                                            
                                                                                
STOPWENG DS    0AL1                ENGLISH STOP WORDS                           
*                                                                               
         DC    CL10'AND'           3 CHARACTER WORDS                            
         DC    CL10'THE'                                                        
*                                                                               
         DC    CL10'OF'            2 CHARACTER WORDS                            
         DC    CL10'IN'                                                         
         DC    CL10'ON'                                                         
         DC    CL10'TO'                                                         
*                                                                               
         DC    AL1(0)              TERMINATOR                                   
*                                                                               
STOPWGER DS    0AL1                GERMAN STOP WORDS                            
*                                                                               
         DC    CL10'DER'           3 CHARACTER WORDS                            
         DC    CL10'DIE'                                                        
         DC    CL10'DAS'                                                        
         DC    CL10'F!R'                                                        
         DC    CL10'DEN'                                                        
         DC    CL10'DES'                                                        
         DC    CL10'UND'                                                        
         DC    CL10'ZUM'                                                        
*                                                                               
         DC    CL10'IN'            2 CHARACTER WORDS                            
         DC    CL10'IM'                                                         
         DC    CL10'ZU'                                                         
*                                                                               
         DC    AL1(0)              TERMINATOR                                   
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#RWKAD,L'AC@RWKAD                                              
         DCDDL AC#ALL,L'AC@ALL                                                  
         DCDDL AC#YES,L'AC@YES                                                  
         DCDDL AC#FLT,L'AC@FLT                                                  
DCDICTLX DC    X'FF'                                                            
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
PZERO    DC    P'0'                                                             
EFFS     DC    X'FFFFFFFF'                                                      
COSTUL   DC    C'1R'                                                            
OFFUL    DC    C'2D'                                                            
NCLIUL   DC    C'1N'                                                            
SXLEDGER DC    C'SX'                                                            
SVLEDGER DC    C'SV'                                                            
*                                                                               
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
         EJECT                                                                  
MJBKEYT  LKKEY H,MJBPAS,SAVED        ** MASTER JOB KEY DRIVER **                
         LKKEY LIT,MJBPTYP,MJBPTYPQ                                             
         LKKEY LIT,MJBPSUB,MJBPSUBQ                                             
         LKKEY SIN,MJBPCPY,AGENCY                                               
         LKKEY LIT,MJBPREM,0                                                    
         LKKEY SIN,MJBPACT,QACT                                                 
         LKKEY ALL,MJBPSUBJ                                                     
         LKKEY E                                                                
                                                                                
SJBKEYT  LKKEY H,SACKEY,SAVED        ** STUDIO AGENCY JOB KEY DRIVER **         
         LKKEY LIT,SACKTYP,SACKTYPQ                                             
         LKKEY LIT,SACKSUB,SACKSUBQ                                             
         LKKEY SIN,SACKCPY,AGENCY                                               
         LKKEY SIN,SACKAJB,QACT                                                 
         LKKEY E                                                                
                                                                                
RSFKEYT1 LKKEY H,RSFKEY,SAVED      ** FILTER NAME DRIVER **                     
         LKKEY LIT,RSFKTYP,RSFKTYPQ                                             
         LKKEY SIN,RSFKCPY,AGENCY                                               
         LKKEY SIN,RSFKUNT,QUNIT                                                
         LKKEY SIN,RSFKLDG,QLEDG                                                
         LKKEY LIT,RSFKREM1,X'40'                                               
         LKKEY LIT,RSFKACT,X'40'                                                
         LKKEY LIT,RSFKREM2,X'40'                                               
         LKKEY LIT,RSFKFLT#,0                                                   
         LKKEY E                                                                
                                                                                
RSFKEYT2 LKKEY H,RSFKEY,SAVED      ** FILTER VALUE NAME DRIVER **               
         LKKEY LIT,RSFKTYP,RSFKTYPQ                                             
         LKKEY SIN,RSFKCPY,AGENCY                                               
         LKKEY SIN,RSFKUNT,QUNIT                                                
         LKKEY SIN,RSFKLDG,QLEDG                                                
         LKKEY LIT,RSFKREM1,X'40'                                               
         LKKEY LIT,RSFKACT,X'40'                                                
         LKKEY LIT,RSFKREM2,X'40'                                               
         LKKEY SIN,RSFKFLT#,MYFLTNUM                                            
         LKKEY E                                                                
                                                                                
AEXKEYT  LKKEY H,AEXKEY,SAVED        ** ACCOUNT EXTENSION DRIVER **             
         LKKEY LIT,AEXKTYP,AEXKTYPQ                                             
         LKKEY LIT,AEXKSUB,AEXKSUBQ                                             
         LKKEY SIN,AEXKCPY,AGENCY                                               
         LKKEY LIT,AEXKREM,0                                                    
         LKKEY SIN,AEXKULA,QULA                                                 
         LKKEY ALL,AEXKSEQ                                                      
         LKKEY E                                                                
                                                                                
LLSKEYT  LKKEY H,LLSKEY,SAVED        ** LIMIT LIST KEY DRIVER **                
         LKKEY LIT,LLSKTYP,LLSKTYPQ                                             
         LKKEY LIT,LLSKSUB,LLSKSUBQ                                             
         LKKEY SIN,LLSKCPY,AGENCY                                               
         LKKEY LIT,LLSKREM,0                                                    
         LKKEY SIN,LLSKPIDB,QPIDB                                               
         LKKEY ALL,LLSKGRP                                                      
         LKKEY ALL,LLSKSEQ                                                      
         LKKEY E                                                                
                                                                                
GLSKEYT  LKKEY H,GLSKEY,SAVED        ** GROUP LIST KEY DRIVER **                
         LKKEY LIT,GLSKTYP,GLSKTYPQ                                             
         LKKEY LIT,GLSKSUB,GLSKSUBQ                                             
         LKKEY SIN,GLSKCPY,AGENCY                                               
         LKKEY LIT,GLSKREM,0                                                    
         LKKEY ALL,GLSKOFF                                                      
         LKKEY ALL,GLSKGRP                                                      
         LKKEY ALL,GLSKSEQ                                                      
         LKKEY E                                                                
                                                                                
APPKEYT  LKKEY H,APPKEY,SAVED        ** APPROVER KEY DRIVER **                  
         LKKEY LIT,APPKTYP,APPKTYPQ                                             
         LKKEY LIT,APPKSUB,APPKSUBQ                                             
         LKKEY SIN,APPKCPY,AGENCY                                               
         LKKEY LIT,APPKREM,0                                                    
         LKKEY SIN,APPKPIDB,QPIDB                                               
         LKKEY ALL,APPKSEQ                                                      
         LKKEY E                                                                
                                                                                
PBRKEYT  LKKEY H,PIDKEY,SAVED      ** PID BACKUP PASSIVE KEY DRIVER **          
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPIDO,QPIDB                                               
         LKKEY LIT,PIDKSTYP,PIDKBAPQ                                            
         LKKEY ALL,PIDKAPPL                                                     
         LKKEY ALL,PIDKAPP2                                                     
         LKKEY LIT,PIDKREST,0                                                   
         LKKEY E                                                                
                                                                                
PRLKEYT  LKKEY H,PIDKEY,SAVED      ** PID ROLE PASSIVE KEY DRIVER **            
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPID,QPIDB                                                
         LKKEY LIT,PIDKSTYP,PIDKROLQ                                            
         LKKEY ALL,PIDKNUM                                                      
         LKKEY SIN,PIDKOFF,USROFF                                               
         LKKEY LIT,PIDKRES2,0                                                   
         LKKEY E                                                                
                                                                                
PCRKEYT  LKKEY H,PIDKEY,SAVED      ** PID CLI ROL PASSIVE KEY DRIVER **         
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPID,QPIDB                                                
         LKKEY LIT,PIDKSTYP,PIDKCLRQ                                            
         LKKEY ALL,PIDKSJAC                                                     
         LKKEY ALL,PIDKROLE                                                     
         LKKEY LIT,PIDKRES3,0                                                   
         LKKEY E                                                                
                                                                                
SRCKEYT  LKKEY H,SRCKEY,SAVED      ** ACCOUNT NAME SEARCH KEY DRIVER **         
         LKKEY LIT,SRCKTYP,SRCKTYPQ                                             
         LKKEY SIN,SRCKCPY,AGENCY                                               
         LKKEY LIT,SRCKUNT,C'1'                                                 
         LKKEY LIT,SRCKLDG,C'R'                                                 
         LKKEY LIT,SRCKSUB,SRCKWDSQ                                             
         LKKEY LIT,SRCKREST,0                                                   
         LKKEY SIN,SRCKFNM,QNAME                                                
         LKKEY ALL,SRCKRNM                                                      
         LKKEY ALL,SRCKSEQ1                                                     
         LKKEY ALL,SRCKACT                                                      
         LKKEY E                                                                
                                                                                
A1RKEYT  LKKEY H,ACTKEY,SAVED      ** 1R ACCOUNT RECORD KEY DRIVER **           
         LKKEY SIN,ACTKCPY,AGENCY                                               
         LKKEY LIT,ACTKUNT,C'1'                                                 
         LKKEY LIT,ACTKLDG,C'R'                                                 
         LKKEY SIN,ACTKOFC,QOFFC                                                
         LKKEY NZR,ACTKRACT                                                     
         LKKEY LIT,ACTREST,X'40'                                                
         LKKEY E                                                                
                                                                                
A1R2KEYT LKKEY H,ACTKEY,SAVED      ** 1R A/C RECORD (2CO) KEY DRIVER **         
         LKKEY SIN,ACTKCPY,AGENCY                                               
         LKKEY LIT,ACTKUNT,C'1'                                                 
         LKKEY LIT,ACTKLDG,C'R'                                                 
         LKKEY SIN,ACTKOFC2,QOFFC                                               
         LKKEY NZR,ACTKRAC2                                                     
         LKKEY LIT,ACTREST,X'40'                                                
         LKKEY E                                                                
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
ALLOWCHR DC    256XL1'00'          ALLOWABLE PROGRAM CODE CHARACTERS            
         ORG   ALLOWCHR+C'a'                                                    
         DC    C'abcdefghi'                                                     
         ORG   ALLOWCHR+C'j'                                                    
         DC    C'jklmnopqr'                                                     
         ORG   ALLOWCHR+C's'                                                    
         DC    C'stuvwxyz'                                                      
         ORG   ALLOWCHR+C'A'                                                    
         DC    C'ABCDEFGHI'                                                     
         ORG   ALLOWCHR+C'J'                                                    
         DC    C'JKLMNOPQR'                                                     
         ORG   ALLOWCHR+C'S'                                                    
         DC    C'STUVWXYZ'                                                      
         ORG   ALLOWCHR+C'0'                                                    
         DC    C'0123456789'                                                    
         ORG                                                                    
                                                                                
VIEWLIT  DC    C'View all'                                                      
NAMELIT  DC    C'Name Letter'                                                   
INITLIT  DC    C'Dummy'                                                         
VATTLIT  DC    C'VAT Type'                                                      
TYVELIT  DC    C'Typeahead or ellipse'                                          
ACTVLIT  DC    C'Active accounts in last year'                                  
                                                                                
                                                                                
LVALUES  DS    0D                                                               
         DC    A(VALACT)                                                        
         DC    A(CHKOFF)                                                        
         DC    A(CHKAPP)                                                        
         DC    A(ADVALL)                                                        
         DC    A(CHKACT)                                                        
         DC    A(GETOPT)                                                        
         DC    A(FLTGRP)                                                        
LVALUESL EQU   *-LVALUES                                                        
                                                                                
COMPQ    EQU   C'C'                    Compulsory                               
                                                                                
B#UFDL   EQU   2                       IO5 - User field record                  
B#ACNT   EQU   3                       IO2 - Account record                     
B#MJB    EQU   4                       IO3 Master job passive                   
B#SJB    EQU   4                       IO3 Studio agency job passive            
B#FNVREC EQU   4                       IO3 Filter name value record             
B#PERREC EQU   5                       IO4 Person record                        
B#LLSREC EQU   5                       IO4 Limit list record                    
B#GLSREC EQU   5                       IO4 Group list record                    
B#APPREC EQU   5                       IO4 Approver record                      
B#PID    EQU   5                       IO4 PID passive                          
B#ROLE   EQU   5                       IO4 Role record                          
B#1RACT  EQU   5                       IO4 1R account record                    
B#PRACT  EQU   5                       IO4 Previous account rec                 
B#ACEX   EQU   6                       IO8 Account extension record             
B#USF    EQU   7                       IO7 User field area                      
B#GOBLK  EQU   7                       AGOBLOCB area                            
B#GOXBLK EQU   8                       AGOXBLCK area                            
B#GOBBLK EQU   9                       AGOBBLCK area                            
B#ACCREC EQU   10                      - GENERAL AREA                           
B#SVRDEF EQU   11                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   12                      - LP_D                                   
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         EJECT                                                                  
SAVED    DSECT                                                                  
WVALUES  DS    0X                                                               
ADCONS   DS    0A                                                               
AVALACT  DS    A                                                                
ACHKOFF  DS    A                                                                
ACHKAPP  DS    A                                                                
AADVALL  DS    A                                                                
ACHKACT  DS    A                                                                
AGETOPT  DS    A                                                                
AFLTGRP  DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
VACCEMU  DS    A                   A(ACCEMU)                                    
AMASTC   DS    A                   A(MASTC)                                     
AIOACC   DS    A                   A(IOAREA IN USE)                             
TSARABUF DS    XL(TSPXTNL)         TSAR block for GAPLST                        
TSARRBLK DS    XL(TSPXTNL)         - FOR KEYFRAGMENTS                           
TSAROBUF DS    XL(TSPXTNL)         - FOR WORD SEARCH OPTIM                      
TSARSBUF DS    XL(TSPXTNL)         - FOR SUPGAP OPTIM                           
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
SAVEVAR  DS    0F                  ** Variables **                              
RUNFLAGS DS    X                                                                
RFGAPLQ  EQU   X'80'               Some GAPLST found                            
RFMEDLQ  EQU   X'40'               Allow all media codes - GAPLST               
RFLIMEOF EQU   X'10'               end of limlist buffer                        
RFLIMFST EQU   X'08'               read first for limlISt buffer                
RFAPPRQ  EQU   X'04'               GAPLST entries from approver record          
RFFRAGQ  EQU   X'02'               some keyfrags found                          
RFNOSEQ  EQU   X'01'               IOSEQ BROKEN (EG FOR GETACT)                 
*                                                                               
RUNFLAG2 DS    X                                                                
R2ACCWOR EQU   X'80'               Non SJ acc search                            
R2ACCSRC EQU   X'40'               Non SJ acc search word search cmplte         
R2MATCH  EQU   X'20'               Found match                                  
USRENT   DS    CL(USRLEN)  User field table                                     
*                                                                               
DSDICTL  DS    0C                                                               
AC@RWKAD DS    CL30                                                             
AC@ALL   DS    CL4                                                              
AC@YES   DS    CL3                                                              
AC@FLT   DS    CL6                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
QINITIAL DS    CL1                 Dummy field                                  
QALPHA   DS    CL(L'SAPEAGY)       Alpha ID                                     
QPIDB    DS    XL2                 Binary PID                                   
QPID     DS    CL8                 PID                                          
                                                                                
QULA     DS    0CL14               Unit/Ledger Account                          
QUL      DS    0CL2                Unit/Ledger                                  
QUNIT    DS    CL1                 Unit code                                    
QLEDG    DS    CL1                 Ledger code                                  
QACT     DS    CL12                Account code                                 
QLIML    DS    CL1                 Override Limit List                          
QAPPL    DS    CL1                 Application                                  
QAPPINV  EQU   QINV                C'I'  Invoices                               
QAPPORD  EQU   QORD                C'O'  Orders                                 
QAPPJOB  EQU   QJOB                C'J'  Jobs                                   
QAPPDSB  EQU   QJOBDSH             C'D'  Jobs dashboard                         
QAPPTIM  EQU   QTIME               C'T'  Time                                   
QAPPEST  EQU   QEST                C'E'  Estimates                              
QAPPEJB  EQU   QESTJBL             C'B'  Estimates FOR JOB LIST                 
QAPPEXP  EQU   QEXPCLM             C'X'  Expenses                               
QAPPREP  EQU   QREPORTS            C'R'  Reports                                
QAPPRES  EQU   QRESRCES            C'S'  Resources                              
QAPPPTO  EQU   C'H'                C'H'  Time off                               
QMED     DS    CL1                 Media code filter                            
QINVDTE  DS    PL3                 Invoice date                                 
QDRAFT   DS    CL1                 Draft status Y/N                             
QPDF     DS    CL1                 PDF request Y/N                              
QBILL    DS    CL1                 Billable entry in order expenses inv         
QFUTIM   DS    CL1                 Future time allowed Y/N                      
QDEPT    DS    CL5                 2P ONLY - SELECTED 2D ACCOUNT FROM           
*                                    WHICH WE CAN GET DEPARTMENT                
QVIEWAL  DS    CL1                                                              
QVIEWLOC EQU   C'1'                Location                                     
QVIEWFNM EQU   C'2'                First name                                   
QVIEWLNM EQU   C'3'                Last name                                    
QOFFC    DS    CL2                                                              
QNAME    DS    CL1                 A-Z                                          
*                                                                               
* list/search values                                                            
QACTIND  DS    XL1                 ACCOUNT ARRAY (LIST/SCH FILTER)              
QAACTS   DS    AL3                                                              
QLOCK    DS    CL1                 LOCKED FILTER Y/N/O/T/A                      
QVWALL   DS    CL1                 VIEW ALL A/Y/N                               
QVTYPE   DS    CL1                 VAT TYPE                                     
QDNOFF   DS    CL1                 OPTION TO SUPPRESS OFF CODE (2D)             
QFNAME   DS    CL1                 RETURN FOREIGN NAME Y/N                      
QACTV    DS    CL1                 ACTIVE ACCOUNTS IN LAST YEAR Y/N             
QFIN     DS    CL1                 FINANCE Y/N                                  
QMASTJ   DS    CL1                 WANT MASTER JOBS? Y/N/BLANK                  
QCLOSED  DS    CL1                 CLOSED FILTER Y/N/O                          
QHIER    DS    CL1                 ACCOUNT LEVEL TO RETURN 1-4                  
QSCHTYP  DS    CL1                 SEARCH TYPE A(ND)/O(R)                       
QWORDIND DS    XL1                 SEARCH WORD ARRAY                            
QAWORDS  DS    AL3                                                              
QWORKC   DS    XL2                 WORKCODE FILTER                              
QSJALL   DS    CL1                 RETURN ALL CLI/PRO/JOBS Y/N                  
QTYVEL   DS    CL1                 TYPEAHEAD OR ELLIPSE                         
QTYAHD   EQU   1                   TYPEAHEAD                                    
QELIPM   EQU   2                   ELLIPSE - MY LIST                            
QELIPA   EQU   3                   ELLIPSE - ALL                                
*                                                                               
         ORG   QVALUES+L'QVALUES                                                
QVALUESL EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
GAPLPARM DS    XL1                 GAPLST parameter                             
                                                                                
DROLEIND DS    X                                                                
DAROLE   DS    AL3                 BUILT ROLE IN WMP                            
DROLMAXQ EQU   10                                                               
                                                                                
DCROLIND DS    X                                                                
DACROL   DS    AL3                 BUILT CLIENT ROLE IN WMP                     
DCROMAXQ EQU   100                                                              
*                                                                               
COUNT    DS    XL1                                                              
*                                                                               
LL_LISTQ EQU   C'1'                                                             
LL_ALLQ  EQU   C'2'                                                             
LL_NONEQ EQU   C'3'                                                             
LL_DACS  DS    0CL1                                                             
LL_DCLIA DS    CL1                 Default Client Access                        
LL_DMEDA DS    CL1                 Default Media Access                         
LL_DEXPA DS    CL1                 Default Expenditure Access                   
LL_DNCLA DS    CL1                 Default Non-Client Access                    
LL_DSTFA DS    CL1                 Default Staff Access                         
LL_DWCA  DS    CL1                 Default Work Code Access                     
LL_DREPA DS    CL1                 Default Report Formats Access                
LL_DESTA DS    CL1                 Default Estimate Scheme Access               
LL_DSUPA DS    CL1                 Default Supplier Access                      
LL_DACLQ EQU   *-LL_DACS                                                        
*                                                                               
DA_TODF  DS    CL6                 Today character                              
DA_TODC  DS    XL2                 Today compressed                             
DA_TODP  DS    XL3                 Today packed                                 
DA_MIN5P DS    XL3                 Today minus 5 year packed                    
AGENCY   DS    XL(L'CUXCPY)                                                     
USRID    DS    XL(L'CUUSER)                                                     
USROFF   DS    CL(L'TRNOFFC)                                                    
MEDAGYB  DS    XL1                 Agency byte                                  
MEDSCNUM DS    0CL4                Security access num                          
MEDSCCRE DS    CL1                 Creative agy number                          
MEDSCBUY DS    CL1                 Buying agy number                            
MEDSCAC1 DS    CL1                 Limit access 1                               
MEDSCAC2 DS    CL1                 Limit access 2                               
MEDSCNM2 DS    0CL4                Extended security access num                 
MEDSCAC3 DS    CL1                 Limit access 3                               
MEDSCAC4 DS    CL1                 Limit access 4                               
MEDSCAC5 DS    CL1                 Limit access 5                               
MEDSCAC6 DS    CL1                 Limit access 6                               
ATSRERRS DS    X                   GOATSR returns error byte                    
*                                                                               
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
SVSJACT  DS    CL(L'ACTKULA)       Saved SJ account                             
SV1RACT  DS    CL(L'ACTKULA)       Saved 1R account                             
PREVACT  DS    CL(L'ACTKULA)       Last account read by NXTPREV                 
PREVAC2  DS    CL(L'ACTKULA)       As above                                     
SVOFF    DS    CL2                 Saved office                                 
AC_AMT   DS    PL6                 Add up billed amount                         
SVMEDGR  DS    CL(L'UFSKMGR)       Saved media group                            
SVOFFGR  DS    CL(L'UFSKOFG)       Saved office group                           
                                                                                
SVPERKEY DS    XL(L'IOKEY)         Saved personal control key                   
SVMJBKEY DS    XL(L'IOKEY)         Saved master job key                         
SVSJBKEY DS    XL(L'IOKEY)         Saved studio link job key                    
SVLLSKEY DS    XL(L'IOKEY)         Saved limit list key                         
SVAPRKEY DS    XL(L'IOKEY)         Saved approver key                           
SVBARKEY DS    XL(L'IOKEY)         Saved backup approver key                    
SVROLKEY DS    XL(L'IOKEY)         Saved role key                               
SVCROKEY DS    XL(L'IOKEY)         Saved client role key                        
SVCPJKEY DS    CL(L'IOKEY)         Saved SJ account                             
                                                                                
AC#IND1  DS    XL1                 Account indicator                            
AC#ILOW  EQU   X'80'               Low level account                            
AC#IRDLO EQU   X'01'               Read low level account only                  
THISPRFX DS    CL1                 PREFIX/WORD PAIR EXTRACTED FROM              
THISWORD DS    CL10                               QAWORDS ARRAY                 
THISWRDL DS    XL1                 EX LENGTH OF SRCKEY TO COMPARE               
THISACC  DS    CL12                ACCOUNT CODE SEARCH VALUE                    
THISACCL DS    XL1                 EX LENGTH OF SRCKEY TO COMPARE               
ATHISACC DS    A                   A(FILTER ENTRY IN PROGRESS)                  
AACTROUT DS    A                   A(ROUTINE FOR READING A/CS)                  
BINHIER  DS    XL1                 BINARY OF QHIER                              
GPLMDLEN DS    X                   LENGTH FIRST LIMLIST MATCH                   
MYELTYPE DS    X                   TYPE FROM SETTTYPE, FOR TST ROUTINES         
MYFLTNUM DS    X                   MY FILTER NUMBER                             
MYFLTVAL DS    C                   MY FILTER VALUE TO TEST FOR NAME             
LASTMEDC DS    C                   LAST MEDIA CODE LOOKED FOR                   
LASTMEDI DS    C                   Y/N, DID WE FIND LASTMEDC IN GAPLST?         
THISOFF  DS    CL2                 OFFICE OF THIS ACCOUNT REC                   
CLIOFF   DS    CL2                 OFFICE OF LAST CLIENT (IF SJ)                
CLIOFCD  DS    CL5                 CLIENT WE SET CLIOFF FROM                    
PROOFF   DS    CL2                 OFFICE OF LAST CLIENT (IF SJ)                
PROOFCD  DS    CL7                 PRODUCT WE SET PROOFF FROM                   
DOFFPOS  DS    XL1                 OFFICE POSITION (2D)                         
DDPTPOS  DS    XL1                 DEPT POSITION (2P)                           
DDPTLEN  DS    XL1                 DEPT CODE LEN                                
IGNWCNT  DS    XL2                 NUMBER OF WORDS TO IGNORE                    
*                                                                               
TSARREC  DS    XL256               TSAR RECORD  IOAREA                          
GAPAREA  DS    XL(GAPTLNQ)         Area to use for GAPLT IO                     
INCSUSAC DS    CL14                SAVED INCOME/SUSPENSE A/C (PMDREC)           
STUFLAG  DS    CL1                 STUDIO FLAG Y/N                              
STUTYPE  DS    CL4                 STUDIO TYPE                                  
DOATTYP  DS    XL1                 LAST OATSUB VALUE                            
DOATACT  DS    CL14                LAST ACCOUNT WE FOUND OATEL ON               
DSAVPMD  DS    CL1                 MEDIA CODE TO WHICH INCSUSAC RELATES         
ACLEVEL  DS    CL1                 SJ ACCOUNT LEVEL                             
DVALUESL EQU   *-DVALUES                                                        
*                                                                               
OVALUES  DS    0X                  ** OUTPUT VALUES **                          
AC_LOCL  DS    CL1                 Local currency bank account                  
AC_FUSR  DS    CL1                 Foreign langauge user                        
AC_APST  DS    CL1                 Approver status                              
AC_APP   EQU   C'3'                Approved                                     
AC_REJ   EQU   C'2'                Rejected                                     
AC_AWAIT EQU   C'1'                Awaiting approval                            
AC_CUR   DS    XL(L'ASTCUR)        Currency                                     
AC_SECY  DS    XL(L'RSTSECY)       Security level                               
AC_COSTG DS    CL(L'RSTCOSTG)      Costing analysis code                        
AC_BBFDT DS    CL(L'RSTBDATE)      Balance brought forward date                 
AC_TRNDT DS    CL(L'RSTTDATE)      Transaction added date                       
AC_GLOFC DS    CL(L'TRNOFFC)       General ledger                               
AC_PNL   DS    CL1                 Profit and loss                              
AC_BAL   DS    CL1                 Balance sheet                                
AC_RCR   DS    CL1                 Peel/close reconcilled CRs                   
AC_RDR   DS    CL1                 Peel/close reconcilled DRs                   
AC_STAF  DS    CL1                 Staff analysis - Expense account             
AC_DEPT  DS    CL1                 Dept analysis - Expense account              
AC_MILES DS    CL1                 Miles analysis - Expense account             
AC_CLOS  DS    CL1                 Closed account                               
AC_LOCK  DS    CL1                 Locked account                               
AC_INVAT DS    CL1                 Input VAT account - SG only                  
AC_PAYLK DS    CL1                 Payee locked                                 
AC_CLICS DS    CL1                 Client cost                                  
AC_NONE  EQU   C'4'                None                                         
AC_NEWBS EQU   C'3'                New business                                 
AC_HOUSE EQU   C'2'                House                                        
AC_CLINT EQU   C'1'                Regular client                               
AC_JBANA DS    CL1                 Job analysis - Expense account               
AC_PRANA DS    CL1                 Product analysis - Expense account           
AC_MRGIN DS    CL1                 Merging invoice same ref and date            
AC_ESTI  DS    CL1                 BrandOcean Estimates in use                  
AC_XJOB  DS    CL1                 Expense Job                                  
AC_MSTJB DS    CL1                 BrandOcean master job                        
AC_CLSDT DS    PL3                 Close date                                   
AC_OPNDT DS    PL3                 Open date                                    
AC_DEFWC DS    CL1                 Default workcode                             
AC_EXCTS DS    CL1                 Exclude timesheets                           
AC_PEXEC DS    CL1                 Person is executive                          
AC_PROCN DS    CL1                 Project control                              
AC_SUNDY DS    CL1                 Sundry creditor                              
AC_PROVV DS    CL1                 Provisional vendor                           
AC_INVRG DS    CL1                 Invoice register                             
AC_FUTIM DS    CL1                 Future time                                  
AC_DSKPR DS    CL1                 Desktop priority                             
AC_LKEST DS    CL1                 Lock from estimates                          
AC_LKORD DS    CL1                 Lock from orders                             
AC_LKBIL DS    CL1                 Lock from billing                            
AC_LKTIM DS    CL1                 Lock from time                               
AC_LKADJ DS    CL1                 Lock from adjustments                        
AC_LKEXT DS    CL1                 Lock from external postings                  
AC_FLTS  DS    0CL5                                                             
AC_FLT1  DS    CL1                 Filter 1                                     
AC_FLT2  DS    CL1                 Filter 2                                     
AC_FLT3  DS    CL1                 Filter 3                                     
AC_FLT4  DS    CL1                 Filter 4                                     
AC_FLT5  DS    CL1                 Filter 5                                     
AC_OFF   DS    CL(L'TRNOFFC)       Office code                                  
AC_OFFN  DS    CL(L'NAMEREC)       Office name                                  
AC_MEDIA DS    CL1                 Media code                                   
AC_DUDT  DS    CL8                 Due date formula                             
AC_FPT   DS    CL(L'GOFPT)         Force product on time sheet                  
AC_ALL   EQU   C'A'                All types of time                            
AC_MEMO  EQU   C'R'                R type of time                               
AC_NONB  EQU   C'N'                N type of time                               
AC_EXCL  EQU   C'X'                Exclude                                      
AC_BLNK  EQU   C' '                Blank optional                               
AC_FJT   DS    CL(L'GOFJT)         Force job on time sheet                      
AC_TFNAR DS    CL(L'GOTFNARR)      Force narrative for time types               
AC_TTALL DS    CL(L'GOTTALLW)      Type of time allowed                         
AC_FNTB  DS    CL1                 Force narrative on billable time             
AC_FNTR  DS    CL1                 Force narrative on chargable time            
AC_FNTN  DS    CL1                 Force narrative on non billable time         
AC_BILA  DS    CL1                 Billable time allowed                        
AC_CHGA  DS    CL1                 Chargabe time allowed                        
AC_NONA  DS    CL1                 Non billable time allowed                    
AC_SAPIN DS    CL1                 Self approval internal order                 
AC_SAPAR DS    CL1                 Self approval artist order                   
AC_SAPEX DS    CL1                 Self approval expense order                  
AC_SAPPR DS    CL1                 Self approval production order               
AC_ESTIN DS    CL1                 Estimate number req'd internal order         
AC_ESTAR DS    CL1                 Estimate number req'd artist order           
AC_ESTEX DS    CL1                 Estimate number req'd expense order          
AC_ESTPR DS    CL1                 Estimate number req'd prod order             
AC_ZERIN DS    CL1                 Prevent zero internal order                  
AC_ZERAR DS    CL1                 Prevent zero artist order                    
AC_ZEREX DS    CL1                 Prevent zero expense order                   
AC_ZERPR DS    CL1                 Prevent zero production order                
AC_AUTIN DS    PL6                 Auto approval limit internal order           
AC_AUTAR DS    PL6                 Auto approval limit artist order             
AC_AUTEX DS    PL6                 Auto approval limit expense order            
AC_AUTPR DS    PL6                 Auto approval limit production order         
AC_ACLVL DS    CL1                 Account level                                
AC_SJLVL DS    CL1                 SJ Level                                     
AC_LEVL1 EQU   C'1'                Client or 1st level                          
AC_LEVL2 EQU   C'2'                Product or 2nd level                         
AC_LEVL3 EQU   C'3'                Job or 3rd level                             
AC_LEVL4 EQU   C'4'                4th Level                                    
AC_SJULA DS    0CL14               Unit/Ledger/Account                          
AC_SJUNT DS    CL1                 Unit                                         
AC_SJLDG DS    CL1                 Ledger                                       
AC_SJACT DS    CL12                Account                                      
AC_SRLVL DS    CL1                 Level SR account found                       
AC_SRULA DS    0CL14               Unit/Ledger/Account                          
AC_SRUNT DS    CL1                 Unit                                         
AC_SRLDG DS    CL1                 Ledger                                       
AC_SRACT DS    CL12                Account                                      
AC_1CLVL DS    CL1                 Level 1C account found                       
AC_1CULA DS    0CL14               Unit/Ledger/Account                          
AC_1CUNT DS    CL1                 Unit                                         
AC_1CLDG DS    CL1                 Ledger                                       
AC_1CACT DS    CL12                Account                                      
AC_PRNBL DS    CL(L'PPRBILLP)      Print on bill                                
AC_OTHRI DS    0CL(L'PPRNARRP)     Other info                                   
AC_OTHR1 DS    CL50                Other info line 1                            
AC_OTHR2 DS    CL50                Other info line 2                            
AC_OTHR3 DS    CL50                Other info line 3                            
AC_GRUP  DS    CL(L'PPRGRUP)       Billing group                                
AC_TIME  DS    CL6                 Time                                         
AC_IDNUM DS    XL(L'JOBREVNO)      ID number                                    
AC_PAYMD DS    CL1                 Payment type                                 
AC_CHQQ  EQU   C'1'                Cheques                                      
AC_PAYQ  EQU   C'2'                Payments                                     
AC_BACQ  EQU   C'3'                BACS Payments                                
AC_MCHQ  EQU   C'4'                Manual Cheques                               
AC_INTQ  EQU   C'5'                International                                
AC_TRFQ  EQU   C'6'                Transfers                                    
AC_AFPQ  EQU   C'7'                Auto foreign payments                        
AC_SEPQ  EQU   C'8'                SEPA payments <xml>                          
AC_VATC  DS    CL1                 VAT Code                                     
AC_KSVR  DS    CL4                 KSV rate                                     
AC_WCRQD DS    CL1                 Workcode required                            
AC_APRVR DS    CL1                 Connected user is approver                   
AC_CCLSE DS    CL1                 Can close account                            
AC_CDELT DS    CL1                 Cannot delete account                        
AC_DELT  DS    CL1                 Account is deleted                           
AC_PRVCD DS    CL(L'ACTKACT)       Previous account code                        
AC_CAEST DS    PL6                 Client approved estimate total               
AC_RPID  DS    XL2                 Record change PID                            
AC_RDTE  DS    XL3                 Record change date                           
AC_RTIM  DS    XL4                 Record change time                           
AC_TIMIT DS    CL1                 Time items exist on job                      
AC_EXPIT DS    CL1                 Expense items exist on job                   
AC_PORIT DS    CL1                 Production orders exist on job               
AC_XORIT DS    CL1                 Expense orders exist on job                  
AC_ESTIT DS    CL1                 Estimates exist on job                       
AC_DRFTX DS    CL1                 Draft transactions exist                     
AC_CXJOB DS    CL1                 Can amend expense job setting                
AC_THRDP DS    CL1                 Third party upload flag                      
AC_SJSTT DS    CL4                 Studio Type                                  
                                                                                
         ORG   OVALUES                                                          
SE_VALS  DS    0X                                                               
SE_FSTNM DS    CL20                First name                                   
SE_MIDNM DS    CL20                Middle name                                  
SE_LSTNM DS    CL58                Last name                                    
SE_TITLE DS    CL30                Title                                        
SE_WKEXT DS    CL20                Work extension                               
SE_PHONE DS    CL20                Home phone                                   
SE_NHADR DS    H                   Number of Home addresses                     
SE_HADR  DS    CL16                Home address                                 
         DS    (MAXHADR-1)XL(L'SE_HADR)                                         
MAXHADR  EQU   2                                                                
SE_CITY  DS    CL16                City                                         
SE_PSTCD DS    CL10                Post code                                    
SE_CTRY  DS    CL20                Country                                      
SE_ODCOD DS    0CL5                Office/Department CODE                       
SE_OFCOD DS    CL2                 Office code                                  
SE_DEPCD DS    CL3                 Department code                              
SE_SECGR DS    CL8                 Security group                               
SE_TSAGR DS    CL8                 TS approver group                            
SE_NICOD DS    CL10                NI code                                      
SE_NCUID DS    H                   Number of company user ids                   
SE_CUID  DS    CL10                Company user id                              
         DS    (MAXCUID-1)XL(L'SE_CUID)                                         
MAXCUID  EQU   12                  Max number of company user ids               
SE_EFFDT DS    XL2                 Effective date                               
SE_HIRDT DS    XL2                 Hire date                                    
SE_IVFRM DS    XL2                 Invalid from                                 
SE_EMAIL DS    CL44                Email address                                
SE_STFCD DS    CL7                 Staff code                                   
SE_VLNQ  EQU   *-SE_VALS                                                        
                                                                                
         ORG   OVALUES                                                          
CS_VALS  DS    0X                                                               
CS_HIRDT DS    PL3                 Hire Date                                    
CS_TERDT DS    PL3                 Termination Date                             
CS_TSLCK DS    PL3                 Timesheet Lock                               
CS_ACTHR DS    CL1                 Acutal hour                                  
*                                                                               
CS_STAT  DS    CL1                 Status                                       
CS_PER   DS    CL(L'PERKCODE)      Person code                                  
CS_1RULA DS    0CL14               Unit/Ledger/Account                          
CS_1RUNT DS    CL1                 Unit                                         
CS_1RLDG DS    CL1                 Ledger                                       
CS_1RACT DS    CL12                Account                                      
                                                                                
CS_VLNQ  EQU   *-CS_VALS                                                        
                                                                                
         ORG   OVALUES                                                          
LL_VALS  DS    0X                                                               
LL_GRCOD DS    CL8                 Group code                                   
LL_GRLST DS    CL1                 Group List In Use (Y/N)                      
*                                                                               
LL_FRMNM DS    CL36                Report format name                           
LL_FRMTY DS    CL1                 - scribe type                                
LL_MANQ  EQU   C'1'                - manpower                                   
LL_PRODQ EQU   C'2'                - production                                 
LL_FINQ  EQU   C'3'                - financials                                 
LL_CRQ   EQU   C'4'                - creditors                                  
LL_DRQ   EQU   C'5'                - debtors                                    
LL_INCQ  EQU   C'6'                - income                                     
LL_CASQ  EQU   C'7'                - cash                                       
LL_MEDQ  EQU   C'8'                - media                                      
LL_EXPQ  EQU   C'9'                - expense                                    
LL_PNLQ  EQU   C'A'                - profit and loss                            
*                                                                               
LL_ULA   DS    0CL14                                                            
LL_UNT   DS    CL1                                                              
LL_LDG   DS    CL1                                                              
LL_ACT   DS    CL12                                                             
LL_VLNQ  EQU   *-LL_VALS                                                        
*                                                                               
         ORG   OVALUES                                                          
AP_VALS  DS    0X                                                               
                                                                                
AP_GESTC DS    CL1                 Global estimate client approver              
AP_GIESC DS    CL1                 Global internal estimate client appr         
AP_GJOB  DS    CL1                 Global job approver                          
AP_GEXPF DS    CL1                 Global expense finance approver              
AP_GLIST EQU   C'1'                List                                         
AP_GALL  EQU   C'2'                All Clients                                  
AP_GNONE EQU   C'3'                None                                         
AP_GDEFA EQU   C'4'                Default all                                  
                                                                                
AP_TYPES DS    0CL6                Approver type                                
AP_APOR  DS    CL1                 Order approver - Y/N                         
AP_APORD DS    CL1                 Order default approver - Y/N                 
AP_APOF  DS    CL1                 Order finance approver - Y/N                 
AP_APOFD DS    CL1                 Order default finance approver - Y/N         
AP_APIN  DS    CL1                 Invoice approver - Y/N                       
AP_APIND DS    CL1                 Invoice default approver - Y/N               
AP_SUBCT DS    CL1                 Approver sub category                        
AP_SCDEF EQU   C'1'                                                             
AP_SCCLI EQU   C'2'                                                             
AP_SCNCL EQU   C'3'                                                             
AP_SCEXP EQU   C'4'                                                             
AP_SCPRD EQU   C'5'                                                             
AP_SCART EQU   C'6'                                                             
AP_SCINT EQU   C'7'                                                             
AP_ETYNM DS    CL36                Expenditure type name                        
AP_SEC   DS    CL1                 Y/N                                          
                                                                                
AP_ULA   DS    0CL14                                                            
AP_UNT   DS    CL1                                                              
AP_LDG   DS    CL1                                                              
AP_ACT   DS    CL12                                                             
                                                                                
AP_1RULA DS    0CL14               1R/2D account                                
AP_1RUNT DS    CL1                                                              
AP_1RLDG DS    CL1                                                              
AP_1RACT DS    CL12                                                             
                                                                                
AP_VLNQ  EQU   *-AP_VALS                                                        
                                                                                
         ORG   OVALUES                                                          
CR_VALS  DS    0X                                                               
                                                                                
CR_ULA   DS    0CL14                                                            
CR_UNT   DS    CL1                                                              
CR_LDG   DS    CL1                                                              
CR_ACT   DS    CL12                                                             
                                                                                
CR_VLNQ  EQU   *-CR_VALS                                                        
                                                                                
         ORG   OVALUES                                                          
PE_VALS  DS    0X                                                               
                                                                                
PE_PERCD DS    CL(L'PERKCODE)      Person code                                  
PE_ULA   DS    0CL14                                                            
PE_UNT   DS    CL1                                                              
PE_LDG   DS    CL1                                                              
PE_ACT   DS    CL12                                                             
                                                                                
PE_HIRDT DS    PL3                 Hire Date                                    
PE_TERDT DS    PL3                 Termination Date                             
                                                                                
PE_CRACT DS    CL14                Creditor account linked                      
PE_2PULA DS    0CL14                                                            
PE_2PUL  DS    CL2                                                              
PE_2PACT DS    CL12                2P account linked                            
PE_2DULA DS    0CL14                                                            
PE_2DUL  DS    CL2                                                              
PE_2DACT DS    CL12                2D account linked                            
                                                                                
                                                                                
PE_VLNQ  EQU   *-PE_VALS                                                        
         ORG                                                                    
OVALUESL EQU   *-OVALUES                                                        
SAVEVARL EQU   *-SAVEVAR                                                        
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* included books and DSECTS                                           *         
***********************************************************************         
***********************************************************************         
* Approval hierarchy search table DSECT                               *         
***********************************************************************         
APRTABD  DSECT                                                                  
APRSTAT  DS    XL1                 Levels to include                            
APRCLI   EQU   X'80'               Client                                       
APRPRO   EQU   X'40'               Product                                      
APRJOB   EQU   X'20'               Job                                          
APRMED   EQU   X'10'               Media                                        
APROFF   EQU   X'08'               Office                                       
APRTABL  EQU   *-APRTABD                                                        
                                                                                
***********************************************************************         
* User field hierarchy search table DSECT                             *         
***********************************************************************         
USFTABD  DSECT                                                                  
USFSTAT  DS    XL1                 Levels to include                            
USFMGR   EQU   X'80'               Client                                       
USFMED   EQU   X'40'               Product                                      
USFOGR   EQU   X'20'               Job                                          
USFOFF   EQU   X'10'               Media                                        
USFCLI   EQU   X'08'               Client                                       
USFPRO   EQU   X'04'               Product                                      
USFJOB   EQU   X'02'               Job                                          
USFTABL  EQU   *-USFTABD                                                        
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
PIDRECD  DSECT                                                                  
         ORG   PIDKAPP2+L'PIDKAPP2                                              
PIDKREST DS    XL(L'PIDKEY-(*-PIDKEY))                                          
         ORG   PIDKOFF+L'PIDKOFF                                                
PIDKRES2 DS    XL(L'PIDKEY-(*-PIDKEY))                                          
         ORG   PIDKROLE+L'PIDKROLE                                              
PIDKRES3 DS    XL(L'PIDKEY-(*-PIDKEY))                                          
                                                                                
SRCRECD  DSECT                                                                  
         ORG   SRCKSUB+L'SRCKSUB                                                
SRCKREST DS    XL12                                                             
         ORG   SRCKWRD1                                                         
SRCKFNM  DS    CL1                                                              
SRCKRNM  DS    XL9                                                              
*                                                                               
ACTRECD  DSECT                                                                  
         ORG   ACTKACT                                                          
ACTKOFC  DS    CL1                                                              
ACTKRACT DS    CL11                                                             
         ORG   ACTKACT                                                          
ACTKOFC2 DS    CL2                                                              
ACTKRAC2 DS    CL10                                                             
ACTREST  DS    XL(L'ACTKEY-ACTKEND)                                             
*                                                                               
OB_D     DSECT                                                                  
*                    SUPPLIER GAP OPTIMISATION                                  
OB_KEY   DS    XL42                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
*    SUPGAP OB_D LAYOUT :                                                       
         ORG   OB_OTHER                                                         
OB_STAT7 DS    CL(L'RSTSTAT7)                                                   
*                                                                               
*&&US                                                                           
       ++INCLUDE ACCATCALLD                                                     
*&&                                                                             
                                                                                
* DDCONBLK                                                                      
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
* MEFILCLID                                                                     
         PRINT OFF                                                              
       ++INCLUDE MEFILCLID                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACBRA28   08/19/20'                                      
         END                                                                    
