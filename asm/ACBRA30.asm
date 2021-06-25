*          DATA SET ACBRA30    AT LEVEL 050 AS OF 06/24/20                      
*PHASE T62430A                                                                  
*                                                                               
ACBRA30  TITLE '- BRA JobBag downloader'                                        
*                                                                               
***********************************************************************         
* Downloads costs, estimates, and unapproved orders for an account code         
*                                                                               
* Currently written for SJ only, please write new code in ways that             
*  make it easy to extend the book to handle other ledgers.                     
*                                                                               
*NRAK 15JUN10 001 <PR000235> Rewritten from ACBRA19                             
*NSHE 07JUN11 002 Fix to show memo invoices in dashboard for US                 
*JFOS 15JUN11 002 <BR18162D> Don't suppress desc on internal w/cs               
*JFOS 21JUN11 004 <BR18184D> Read old estimates in UK too                       
*NSHE 24FEB12 005 Read for sub jobs when processing master job                  
*NSHE         005 Add additional fields required by client solutions            
*NSHE 30MAR12 006 Skip details for invicta call                                 
*NSHE 12APR12 007 Contingency bug                                               
*NSHE 15MAY12 008 Estimate % bug with R-Time                                    
*MPEN 18APR12 011 <PR002888> Sub jobs and cols for memo and write-offs          
*NSHE 14JUN12     Fix bug in error handling and increase buffer size            
*NSHE 15AUG12     Change commission calculation                                 
*JFOS 29MAY13 012 <OT     L> Fix bad branch in GETRANS                          
*NRAK 26JUN13 013 <DSBO-86> Mixed JOBSMCSE settings in master jobs              
*NSHE 19JUL13 014 Set SETFAC earlier for online processing                      
*NSHE 26JUL13     DSBO-151 Issue error when job no longer exists                
*NSHE 17OCT13 015 US Fix for jobs dash                                          
*JFOS 26NOV13 017 <DSPCA241> GETRANS fix for 2CO + non-SJ request               
*TKLU 17Apr14 018 <PCA00932> Exclude draft postings via parameter or            
*                            always for INVICTA                                 
*YNGX 18SEP14 019 <FLX00071> HANDLE FLEXIBILL FEE BILLING AMOUNTS               
*NSHE 24Apr15 021 <DSBO-1406> Remove SETFAC                                     
*NSHE 26May15 022 <DSRD-7336> Return bill type for US                           
*                 <DSRD-7339> Show net allocation and write off                 
*                 <DSRD-7475> Show pending batch items - draft trans            
*                 <DSRD-7498> Error when job not found                          
*                 <DSRD-7503> Show exchange rate in character format            
*                 <DSRD-7571> Show job details in new D4 response               
*                 <DSBO-1504> Deal with group billing where multiple            
*                             jobs post one debtor posting                      
*                 <DSRD-7708> Fix bad arithmetic in GETDDET routine             
*                 <DSRD-7719> Deal with retail billing                          
*                 <DSRD-7769> Left to bill to vary based on opt maint           
*                 <DSRD-7813> Show billings from BT6 and media system           
*YNGX 07JUL15 022 <DSBO-1496> Show VAT 1-9 amount from billing postings         
*NSHE 06AUG15 023 <DSRD-8240> Don't send WC 99 code for NA                      
*NSHE 10AUG15     <DSRD-8259> Add estimate date                                 
*YNGX 11AUG15     <DSFLX-299> Don't read bill alloc from Estimate (UK)          
*NSHE 12AUG15     <DSRD-8274> Show fully approved expense as expenses           
*NSHE 12AUG15     <DSRD-8280> Show only CE for NA for estimate details          
*NSHE 13AUG15     <DSRD-8287> Show non commissionable amount                    
*NSHE 13AUG15     <DSRD-8301> Show only 6 character ref for bills in NA         
*NSHE 20AUG15     <DSRD-8349> Don't read PTA element for estimates              
*MPEN 28AUG15 024 <DSRD-8446> Return est/wc check and int/ext flag              
*NSHE 08SEP15     <DSRD-8364> Return orders even when fully matched             
*NSHE 15SEP15     <DSRD-8700> Ensure order number is ref when approved          
*NSHE 15SEP15     <DSRD-8697> Ensure export works in all modes                  
*NSHE 15SEP15     <DSRD-8722> Don't add fully matched order detail              
*                             lines to summary                                  
*MPEN 19OCT15 025 <DSRD-9102> Return fully invoiced w/c rows for Aura           
*NSHE 05NOV15     <DSRD-9198> Change how NAE works for EU                       
*NSHE 09DEC15 026 <DSRD-9721> Don't show deleted orders in order list           
*TKLU             <DSRD-9620> Show order creator                                
*NSHE             <DSRD-9813> Show estimates for NA when sub job                
*NSHE 25Jan16 027 <DSRD-10083> Add bill type to bill records                    
*NSHE             <DSRD-9911> Ensure prebilling works with master/sub           
*NSHE 08Apr16 028 <DSRD-10970> Deal with with est hrs with zero rate            
*JSHA 21Apr16 029 <DSSUP-7319> Fix dump in GETDDET routine                      
*NSHE             <DSRD-10697> Add currency to allocation and estimates         
*NSHE 17May16 030 <DSRD-11494> Currency issues with commission                  
*NSHE 19May16     <DSRD-10707> Return batch type for billing                    
*NSHE 16Jun16     <DSRD-11826> Display order text in narrative                  
*NSHE 27Jun16 031 <DSRD-12040> Display correct length of narrative              
*NSHE 20Jul16 032 Fix dumps seen in production with short scield                
*NSHE 26Oct16 033 Ensure record length is set                                   
*TKLU 14Dec16 034 <DSRD-14417> Clear DS_ORDN each time                          
*MPEN 08Dec16 035 <DSRD-14354> Skip new debtor type scields                     
*                 <DSRD-14374> Fix for US GOTO jobs                             
*NSHE 09Mar17 036 <DSRD-15116> Set highest estimate for presto ests             
*NSHE 16Mar17 037 <DSRD-15061> Show paid amount in detail line                  
*MPEN 24Mar17     <DSRD-15247> Fix for not showing est/ord no.                  
*MPEN 17Jul17 038 <DSRD-15589> Extend timeline narr to 200 chars                
*MPEN 27Jul17     <DSRD-16466> Fix issue where inv identified as exp            
*NSHE 17Aug17     <DSRD-16241> Cancelled orders                                 
*TKLU 21Dec17 039 <DSRD-17792> Add FFTTBSOU Billing source support              
*NSHE 27Dec17 040 <DSRD-17744> Deal with type correctly                         
*NSHE 12Apr18 041 <DSRD-18588> Suppress payment information by date             
*NSHE 22May18 042 <DSRD-19067> Show net paid as zero and not blank              
*NSHE 14Jun18     <DSRD-19128> Showing incorrect creator for orders             
*NSHE 10Jul18 043 <DSRD-19664> Show memo/non memo orders correctly              
*NSHE 06Aug18 044 <DSRD-19263> Exclude non bill from percent estimate           
*YNGX 19Aug18 045 <DSIVC-81> Reinstate creidt field for INVICTA                 
*NSHE 25Oct18 046 DSRD-20532 Work code to be excluded when est check=N          
*NSHE 04Jan18 047 DSRD-21194 Allow more WMP table entries for sub jobs          
*MPEN 13Feb19 048 <SPEC-32181> Fix for credit/debit flag                        
*YNGX 30Sep19 049 <DSRD-23818> Display estimate list number                     
*NSHE 24Jun20 050 <DSRD-26786> Deal with US work codes                          
***********************************************************************         
*                                                                               
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,SYSTEM=ACCSYSQ,SYSTEM2=MEDSYSQ,  +        
               CODE=CODE,FILES=FILES,FILES2=FILES2,                    +        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,+        
               SYSPHASE=X'0624',                                       +        
               APPEND=Y,SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,    +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,B#TWA,TWAD,                               +        
               B#LP_D,LP_D,                                            +        
               B#TRN,TRNRECD,                                          +        
               B#TIM,TIMRECD,                                          +        
               B#ORD,ORDRECD,                                          +        
               B#EXC,EXCRECD,                                          +        
               B#EST,ESTRECD,                                          +        
               B#BUFREC,WCS_D,                                         +        
               B#GOXBLK,GOXBLKD,                                       +        
               B#COBLCK,COBLOCKD)                                               
*                                                                               
*                                                                               
SLOWS    DC    C':'            send notification if slow runner?                
FAILS    DC    C':'            send notification if runner fails?               
*                                                                               
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO30**,R7,CLEAR=YES,RR=RE                                    
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6                                                      
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE                                                   
         J     INIT03                                                           
*                                                                               
*   assign saved storage, working storage (offline only)                        
*   names are rather confusing, but correct!                                    
*                                                                               
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
INIT03   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA                                                     
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         MVI   TWAMODE,0                                                        
         USING TWAD,R7                                                          
         L     R7,ATWA             RA=A(ON/OFFLINE TWA)                         
                                                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         JE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         JE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         JE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
*                                                                               
* be careful with initialising storage - don't want to do this too              
* often <expand this note>                                                      
***********************************************************************         
RUNSTR   TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNSTR02            NO                                           
         L     RF,ACOMFACS         YES - LOAD FACILITIES OVERLAYS               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
         J     RUNSTR04                                                         
*                                                                               
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
*                                                                               
*  set addresses of storage for DDLINK to manage - reading records              
*  through keydrivers, profile blocks, etc.                                     
*                                                                               
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#TRN-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                    
         MVC   LP_BLKS+((B#EST-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                    
         MVC   LP_BLKS+((B#EXC-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         MVC   LP_BLKS+((B#ORD-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         MVC   LP_BLKS+((B#TIM-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         LAY   R0,EX_BUFF                                                       
         ST    R0,LP_BLKS+((B#BUFREC-1)*L'LP_BLKS)                              
         MVC   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)(L'LP_BLKS),ACOBLOCK             
         MVC   LP_BLKS+((B#GOXBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOXBLCK             
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
*                                                                               
         MVC   WVALUES(WVALUEL),LVALUES  <---^                                  
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
*                                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
PRCWRK   LA    R0,SAVEVAR                                                       
         LHI   R1,SAVELN1Q                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(2,DS_TODC)                                   
         GOTOR (RF),(R1),,(1,DS_TODP)                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    PRCWRK04                                                         
                                                                                
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
                                                                                
RUNREQ   MVI   GIND2,GI2EEXP                                                    
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         MVC   USRID,CUUSER                                                     
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
                                                                                
         LA    RE,SWSTAB                                                        
         USING SYSSWTAB,RE                                                      
RUNREQ01 CLI   SYSSWSYS,0                                                       
         JE    RUNREQ02                                                         
         CLI   SYSSWSOV,QSMED                                                   
         JNE   *+16                                                             
         MVC   MEDAGYB,SYSSWAGB                                                 
         MVC   MEDSCNUM,SYSSWACS                                                
         LA    RE,SYSSWLEN(RE)                                                  
         J     RUNREQ01                                                         
         DROP  RE                                                               
                                                                                
RUNREQ02 L     RE,LP_AWMP          BUILD DEFAULT RANGE ELEMENTS IN WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    ALL VALUES                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    NON-ZERO VALUES                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
RUNREQ04 XC    MAPI,MAPI           SET MAP INDICATORS FOR THIS REQ              
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   RUNREQ10                                                         
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     RUNREQ12                                                         
RUNREQ10 AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
*                                                                               
RUNREQ12 DS    0H                                                               
*                                                                               
         GOTOR INIOBUF                                                          
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNREQX                                                          
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
*    open all files to read from/write to (offline only)                        
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
*                                                                               
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
*                                                                               
*                                                                               
*   End of generic initialisation code. Start of request processors             
*         DDLINK will find 1st processor wthout explicit reference              
*                                                                               
*                                                                               
***********************************************************************         
*** Acc summary download **********************************************         
***********************************************************************         
                                                                                
REQACSD  LKREQ H,A#ACSD,OUTACSD,NEXTREQ=REQUESTX                                
*                                                                               
Unit     LKREQ F,01,(D,B#SAVED,QS_UNIT),CHAR,OLEN=L'QS_UNIT,           +        
               MAXLEN=L'QS_UNIT,TEXT=AC#UNIT,COL=*                              
Ledger   LKREQ F,02,(D,B#SAVED,QS_LDGR),CHAR,OLEN=L'QS_LDGR,           +        
               MAXLEN=L'QS_LDGR,TEXT=AC#LGR,COL=*                               
Account  LKREQ F,03,(D,B#SAVED,QS_ACCT),CHAR,OLEN=L'QS_ACCT,           +        
               MAXLEN=L'QS_ACCT,TEXT=AC#ACC,COL=*                               
WC       LKREQ F,04,(I,B#SAVED,QS_WCIND),CHAR,LIST=F,DEFAULT=NOT,      +        
               SORT=NO,OLEN=L'TRNKWORK,TEXT=AC#WC,COL=*                         
Off      LKREQ F,05,(I,B#SAVED,QS_OFIND),CHAR,LIST=F,DEFAULT=NOT,      +        
               SORT=NO,OLEN=L'TRNKWORK,TEXT=AC#OFFC,COL=*                       
EstChk   LKREQ F,06,(D,B#SAVED,QS_ESTC),CHAR,OLEN=L'QS_ESTC,           +        
               MAXLEN=L'QS_ESTC,TEXT=AC#ESCHK,COL=*                             
Contin   LKREQ F,07,(D,B#SAVED,QS_ICON),CHAR,OLEN=L'QS_ICON,           +        
               MAXLEN=L'QS_ICON,TEXT=AC#CTY,COL=*                               
InclHrs  LKREQ F,08,(D,B#SAVED,QS_HRS),CHAR,OLEN=L'QS_HRS,             +        
               MAXLEN=L'QS_HRS,TEXT=AC#HOURS,COL=*                              
InclRev  LKREQ F,09,(D,B#SAVED,QS_REVRS),CHAR,OLEN=L'QS_REVRS,         +        
               MAXLEN=L'QS_REVRS,TEXT=AC#RVRS,COL=*                             
ActStrt  LKREQ F,10,(D,B#SAVED,QS_STACT),CDAT,OLEN=L'QS_STACT,         +        
               TEXT=AC#ACTST,COL=*                                              
ActEnd   LKREQ F,11,(D,B#SAVED,QS_ENACT),CDAT,OLEN=L'QS_ENACT,         +        
               TEXT=AC#ACTND,COL=*                                              
OWConly  LKREQ F,12,(D,B#SAVED,QS_OWCON),CHAR,OLEN=L'QS_OWCON,         +        
               TEXT=(*,WCONLIT),COL=*                                           
ExclDrft LKREQ F,13,(D,B#SAVED,QS_XDRFT),CHAR,OLEN=L'QS_XDRFT,         +        
               MAXLEN=L'QS_XDRFT,TEXT=(*,XDRFTLIT),COL=*                        
Export   LKREQ F,14,(D,B#SAVED,QS_EXPRT),CHAR,OLEN=L'QS_EXPRT,         +        
               MAXLEN=L'QS_EXPRT,TEXT=(*,EXPTLIT),COL=*                         
OrdLst   LKREQ F,15,(D,B#SAVED,QS_ORDL),CHAR,OLEN=L'QS_ORDL,           +        
               MAXLEN=L'QS_ORDL,TEXT=(*,ORDLLIT),COL=*                          
EstLst   LKREQ F,16,(D,B#SAVED,QS_ESTL),CHAR,OLEN=L'QS_ESTL,           +        
               MAXLEN=L'QS_ESTL,TEXT=(*,ESTLLIT),COL=*                          
Payinf   LKREQ F,17,(D,B#SAVED,QS_PAYI),CHAR,OLEN=L'QS_PAYI,           +        
               MAXLEN=L'QS_PAYI,TEXT=(*,PAYINFO),COL=*                          
         LKREQ E                                                                
         EJECT                                                                  
********************************************************************            
* output processor for Account summary                                          
********************************************************************            
*                                                                               
OUTACSD  LKOUT H                                                                
*                                                                               
OUTACSI  LKOUT R,R#ACSD   initial - read all data into TSAR                     
Array    LKOUT C,1,(A,ARYRIN)                                                   
         LKOUT E                                                                
*                                                                               
* output is split by returned record map codes                                  
OUTACS   LKOUT R,R#ACSD                                                         
Array    LKOUT C,1,(A,ARYSUM)        account summary                            
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACSA  LKOUT R,R#ACSDA                                                        
Array    LKOUT C,1,(A,ARYACC),FILTROUT=TSTOWCO  Accounting detail               
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACSE  LKOUT R,R#ACSDE             Estimate Detail                            
Array    LKOUT C,1,(A,ARYEST),FILTROUT=TSTOWCO                                  
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACSB  LKOUT R,R#ACSDB             Billing detail                             
Array    LKOUT C,1,(A,ARYBIL),FILTROUT=TSTOWCO                                  
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACJBD LKOUT R,R#ACJBD             Job detail                                 
Array    LKOUT C,1,(A,ARYJOB),FILTROUT=TSTOWEX                                  
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACORL LKOUT R,R#ACORL             Order list                                 
Array    LKOUT C,1,(A,ARYORDL),FILTROUT=TSTORDL                                 
         LKOUT E                                                                
*                                                                               
*                                                                               
OUTACESL LKOUT R,R#ACESL             Estimate list                              
Array    LKOUT C,1,(A,ARYESTL),FILTROUT=TSTESTL                                 
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
*  READ ALL RECORDS UPFRONT THROUGH KEYDRIVERS - no output here                 
*                                                                               
ARYRIN   LKOUT A,(R,NXTSREC),MULTIROW=Y,ROWNAME=WCS_D                           
         LKOUT E                                                                
*                                                                               
*                                                                               
*  'proper' output processors - have to use arrays for all, to handle           
*      TSAR reading/EOF.                                                        
*                                                                               
*     Extract summary details from TSAR                                         
ARYSUM   LKOUT A,(R,NXTSUM),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
WCode    LKOUT C,01,WCS_WCOD,CHAR,ND=Y,FILTROUT=FLTWC99                         
WRout    LKOUT P,WCS_WCOD,GETWDES                                               
WCdsc    LKOUT C,02,(D,B#SAVED,DS_WDESC),CHAR,LEN=L'DS_WDESC,          +        
               FILTROUT=FLTWC                                                   
*                                                                               
OffC     LKOUT C,03,WCS_WCOD,CHAR,ND=Y,FILTROUT=FLTNOSJ,SKIPCOLS=1              
OffN     LKOUT C,04,WCS_WCOD,(U,#EDTOFN,$EDTOFN)                                
*                                                                               
IntExt   LKOUT C,05,(D,B#SAVED,DS_WINEX),CHAR,L'DS_WINEX,              X        
               FILTROUT=FLTWC99                                                 
EstT     LKOUT C,06,WCS_AEST,SPAK,PZERO=S                                       
DbtT     LKOUT C,07,WCS_DBA,SPAK,PZERO=S                                        
CrdT     LKOUT C,08,WCS_CRA,SPAK,PZERO=S,FILTROUT=TSTIVIC                       
OrdT     LKOUT C,09,WCS_ORA,SPAK,PZERO=S                                        
HrsT     LKOUT C,10,WCS_HRS,SPAK,PZERO=S,FILTROUT=TSTHRS                        
AllT     LKOUT C,11,WCS_ALA,SPAK,PZERO=S                                        
***      LKOUT C,12,WCS_GRSS,SPAK,PZERO=S                                       
***      LKOUT C,13,WCS_COM,SPAK,PZERO=S                                        
ChrgT    LKOUT C,14,WCS_CHT,SPAK,PZERO=S                                        
TimSal   LKOUT C,15,WCS_TSR,SPAK,PZERO=S                                        
***      LKOUT C,16,WCS_TCR,SPAK,PZERO=S                                        
ComBil   LKOUT C,17,WCS_COB,SPAK,PZERO=S                                        
***      LKOUT C,18,WCS_COMR,SPAK,PZERO=S,FILTROUT=FLTHASDB,SKIPCOLS=1          
***      LKOUT C,19,WCS_VATR,LBIN,ND=Y                                          
***      LKOUT C,20,WCS_NBIL,SPAK,PZERO=S                                       
***      LKOUT C,21,WCS_WROF,SPAK,PZERO=S                                       
EstHrs   LKOUT C,28,WCS_AEHR,SPAK,PZERO=S,FILTROUT=TSTHRS                       
WCEstCk  LKOUT C,29,(D,B#SAVED,DS_WCEST),CHAR,LEN=L'DS_WCEST                    
LftBill  LKOUT C,30,WCS_LBIL,SPAK,PZERO=S                                       
SubMast  LKOUT C,31,WCS_SORM,LBIN,ND=Y                                          
CCdWC    LKOUT C,32,(D,B#GOXBLK,GOCCW),CHAR,LEN=L'GOCCW                         
WCF      LKOUT C,33,(D,B#GOXBLK,GOWCF),CHAR,LEN=L'GOWCF                         
***      LKOUT C,34,WCS_NTEX,SPAK,PZERO=S                                       
NonComm  LKOUT C,35,WCS_NCOM,SPAK,PZERO=S                                       
LftSpnd  LKOUT C,36,WCS_LSPD,SPAK,PZERO=S                                       
LftSpHd  LKOUT C,37,WCS_LSPH,SPAK,PZERO=S                                       
Commitd  LKOUT C,38,WCS_CMTD,SPAK,PZERO=S                                       
ActHdr   LKOUT C,39,WCS_ACTH,SPAK,PZERO=S                                       
OrdHdr   LKOUT C,40,WCS_ORAH,SPAK,PZERO=S                                       
*                                                                               
         LKOUT E                                                                
*                                                                               
*     Extract Accounting details from Transaction TSARs                         
ARYACC   LKOUT A,(R,NXTACC),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
Type     LKOUT C,01,WCS_TTYP,LBIN                                               
Contra   LKOUT C,02,WCS_CTRA,CHAR                                               
ConRout  LKOUT P,WCS_CTRA,GETCNAME                                              
ConNam   LKOUT C,03,(D,B#SAVED,DS_CTRN),CHAR,LEN=L'DS_CTRN                      
*                                                                               
WCode    LKOUT C,04,WCS_WCOD,CHAR,ND=Y,FILTROUT=FLTWC99                         
WRout    LKOUT P,WCS_WCOD,GETWDES                                               
WCdsc    LKOUT C,05,(D,B#SAVED,DS_WDESC),CHAR,LEN=L'DS_WDESC,          +        
               FILTROUT=FLTWC                                                   
*                                                                               
OffC     LKOUT C,06,WCS_WCOD,CHAR,ND=Y,FILTROUT=FLTNOSJ,SKIPCOLS=1              
OffN     LKOUT C,07,WCS_WCOD,(U,#EDTOFN,$EDTOFN)                                
*                                                                               
Date     LKOUT C,08,WCS_DAT,PDAT,FILTROUT=FLTNFBAD                              
Ref      LKOUT C,09,WCS_LREF,CHAR                                               
IdbRout  LKOUT P,,GETISDB                                                       
IsDbt    LKOUT C,10,(D,B#SAVED,DS_ISDBT),CHAR,LEN=L'DS_ISDBT                    
BType    LKOUT C,11,WCS_BTYP,LBIN,ND=Y                                          
TxAmt    LKOUT C,12,(D,B#SAVED,DS_TAMT),SPAK,LEN=L'DS_TAMT,PZERO=S              
Hrs      LKOUT C,13,WCS_HRS,SPAK,ND=Y,PZERO=S,FILTROUT=TSTHRS                   
MemHCst  LKOUT C,14,WCS_CHT,SPAK,ND=Y,PZERO=S                                   
OrdAmt   LKOUT C,15,WCS_ORA,SPAK,ND=Y,PZERO=S                                   
OrdIAm   LKOUT C,16,WCS_OIN,SPAK,ND=Y,PZERO=S                                   
IRef     LKOUT C,17,WCS_IREF,CHAR,ND=Y                                          
ONum     LKOUT C,18,WCS_ONUM,CHAR,ND=Y                                          
EstGlob  LKOUT C,19,WCS_ESTG,CHAR,ND=Y                                          
Curr     LKOUT C,20,WCS_CURR,CHAR,ND=Y                                          
CExcR    LKOUT C,21,WCS_CURX,CHAR,ND=Y                                          
CurAmt   LKOUT C,22,WCS_CURA,SPAK,ND=Y,PZERO=S                                  
TimSal   LKOUT C,23,WCS_TSR,SPAK,ND=Y,PZERO=S                                   
TimCst   LKOUT C,24,WCS_TCR,SPAK,ND=Y,PZERO=S                                   
ILogN    LKOUT C,25,WCS_ILOG,CHAR,ND=Y                                          
ExpTyp   LKOUT C,26,WCS_EXPT,CHAR,ND=Y                                          
AppSta   LKOUT C,27,WCS_APST,CHAR,ND=Y                                          
ComBil   LKOUT C,28,WCS_COB,SPAK,ND=Y,PZERO=S                                   
BilSta   LKOUT C,29,WCS_BLST,LBIN,ND=Y                                          
AllAmt   LKOUT C,30,WCS_ALA,SPAK,ND=Y,PZERO=S                                   
GAPStat  LKOUT C,31,WCS_GPST,LBIN,ND=Y                                          
*                                                                               
IntExt   LKOUT C,32,(D,B#SAVED,DS_WINEX),CHAR,L'DS_WINEX,              X        
               FILTROUT=FLTWC99                                                 
ComRte   LKOUT C,33,WCS_COMR,SPAK,ND=Y,PZERO=S                                  
VATRte   LKOUT C,34,WCS_VATR,LBIN,ND=Y                                          
UniqID   LKOUT C,35,WCS_UQID,LBIN,ND=Y                                          
IsQrd    LKOUT C,36,WCS_IQRD,CHAR,ND=Y                                          
IsHeld   LKOUT C,37,WCS_IHLD,CHAR,ND=Y                                          
IsRev    LKOUT C,38,WCS_IREV,CHAR,ND=Y                                          
BatchR   LKOUT C,39,WCS_BREF,CHAR,ND=Y                                          
CliRf1   LKOUT C,40,WCS_CLR1,CHAR,ND=Y                                          
CliRf2   LKOUT C,41,WCS_CLR2,CHAR,ND=Y                                          
Camrf    LKOUT C,42,WCS_CREF,CHAR,ND=Y                                          
DueDte   LKOUT C,43,WCS_DUED,CDAT,ND=Y                                          
ActDte   LKOUT C,44,WCS_ACDT,CDAT,ND=Y                                          
MOA      LKOUT C,45,WCS_MOA,PDAT,ND=Y                                           
Client   LKOUT C,46,WCS_CLIC,CHAR,ND=Y                                          
Prodct   LKOUT C,47,WCS_PROC,CHAR,ND=Y                                          
Job      LKOUT C,48,WCS_JOBC,CHAR,ND=Y                                          
Campgn   LKOUT C,49,WCS_CAMC,CHAR,ND=Y                                          
Media    LKOUT C,50,WCS_MEDC,CHAR,ND=Y                                          
OthAcc   LKOUT C,51,WCS_OULA,CHAR,ND=Y                                          
PIDRout  LKOUT P,WCS_BUID,GETUSR                                                
UID      LKOUT C,52,(D,B#SAVED,DS_BLUID),CHAR,ND=Y                              
VatAmt   LKOUT C,53,WCS_VAT,SPAK,ND=Y,PZERO=S                                   
DrAllc   LKOUT C,54,WCS_ATYP,LBIN,ND=Y                                          
DrAlDt   LKOUT C,55,WCS_ADT,PDAT,ND=Y                                           
DrDpDt   LKOUT C,56,WCS_DPDT,PDAT,ND=Y                                          
DrAlRf   LKOUT C,57,WCS_AREF,CHAR,ND=Y                                          
DrAlAc   LKOUT C,58,WCS_AULA,CHAR,ND=Y                                          
IsMemo   LKOUT C,59,WCS_IMEM,CHAR,ND=Y                                          
MemAmt   LKOUT C,60,WCS_NBIL,SPAK,ND=Y,PZERO=S                                  
Wrioff   LKOUT C,61,WCS_WROF,SPAK,ND=Y,PZERO=S                                  
Subcli   LKOUT C,62,WCS_SJAC,(R,EDTCLC),ND=Y                                    
Subcln   LKOUT C,63,WCS_SJAC,(R,EDTCLN),ND=Y                                    
Subpro   LKOUT C,64,WCS_SJAC,(R,EDTPRC),ND=Y                                    
Subprn   LKOUT C,65,WCS_SJAC,(R,EDTPRN),ND=Y                                    
Subjob   LKOUT C,66,WCS_SJAC,(R,EDTJBC),ND=Y                                    
Subjbn   LKOUT C,67,WCS_SJAC,(R,EDTJBN),ND=Y                                    
WCEstCk  LKOUT C,68,(D,B#SAVED,DS_WCEST),CHAR,LEN=L'DS_WCEST                    
*&&US                                                                           
BilTyp   LKOUT C,69,WCS_BLTY,LBIN,ND=Y                                          
*&&                                                                             
IsCom    LKOUT C,70,WCS_ICOM,CHAR,ND=Y                                          
WCF      LKOUT C,71,(D,B#GOXBLK,GOWCF),CHAR,LEN=L'GOWCF                         
OrdStat  LKOUT C,72,WCS_ORST,LBIN,ND=Y                                          
AllFCA   LKOUT C,73,WCS_FCAL,SPAK,ND=Y,PZERO=S                                  
AllCur   LKOUT C,74,WCS_ACUR,CHAR,ND=Y                                          
FCmAmt   LKOUT C,75,WCS_COMF,SPAK,PZERO=S                                       
PayAmnt  LKOUT C,76,WCS_PAMT,SPAK,FILTROUT=TSTPAYI,                    +        
               SKIPCOLS=PAYSKIPS                                                
PAYCOLS  EQU   *                                                                
PayRef   LKOUT C,77,WCS_PREF,CHAR,ND=Y                                          
PayDate  LKOUT C,78,WCS_PDAT,CDAT,ND=Y                                          
PAYSKIPS EQU   (*-PAYCOLS)/LX_COLSL                                             
*                                                                               
Narr     LKOUT C,1,(A,ARYNAR)                                                   
*                                                                               
BilNo    LKOUT C,2,(A,ARYBNO)                                                   
*                                                                               
         LKOUT E                                                                
*  Accounting extract sub-arrays                                                
*     Narrative array output                                                    
ARYNAR   LKOUT A,(D,B#BUFREC,WCS_TERM),EOT=EOR,NEWEL=B,                X        
               ROWID=(WCS_ELET,WCS_ELRQ),ROWWIDTH=(V,WCS_ELEL)                  
         LKOUT C,1,WCS_ELED,CHAR,ND=Y,LEN=V                                     
         LKOUT E                                                                
*                                                                               
ARYBNO   LKOUT A,(D,B#BUFREC,WCS_TERM),EOT=EOR,NEWEL=B,                X        
               ROWID=(WCS_ELET,WCS_ELBQ),ROWWIDTH=(V,WCS_ELEL)                  
         LKOUT C,1,WCS_ELED,CHAR,ND=Y,LEN=L'PTARBLNO                            
         LKOUT E                                                                
*                                                                               
*                                                                               
*                                                                               
*  Extract Estimate details from TSAR                                           
ARYEST   LKOUT A,(R,NXTEST),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
WCode    LKOUT C,01,WCS_WCOD,CHAR,ND=Y,FILTROUT=FLTWC,SKIPCOLS=2                
WRout    LKOUT P,WCS_WCOD,GETWDES                                               
WCdsc    LKOUT C,02,(D,B#SAVED,DS_WDESC),CHAR,LEN=L'DS_WDESC                    
*                                                                               
EstAmt   LKOUT C,03,WCS_AEST,SPAK                                               
EstGlob  LKOUT C,04,WCS_ESTG,CHAR                                               
EstLoc   LKOUT C,05,WCS_ENUM,LBIN                                               
EstNam   LKOUT C,06,WCS_ESTD,CHAR                                               
EstSta   LKOUT C,07,WCS_ESTS,LBIN                                               
EstGAP   LKOUT C,08,WCS_GPST,LBIN                                               
EstHrs   LKOUT C,09,WCS_AEHR,SPAK,,ND=Y,PZERO=S,FILTROUT=TSTHRS                 
Subcli   LKOUT C,10,WCS_SJAC,(R,EDTCLC),ND=Y                                    
Subcln   LKOUT C,11,WCS_SJAC,(R,EDTCLN),ND=Y                                    
Subpro   LKOUT C,12,WCS_SJAC,(R,EDTPRC),ND=Y                                    
Subprn   LKOUT C,13,WCS_SJAC,(R,EDTPRN),ND=Y                                    
Subjob   LKOUT C,14,WCS_SJAC,(R,EDTJBC),ND=Y                                    
Subjbn   LKOUT C,15,WCS_SJAC,(R,EDTJBN),ND=Y                                    
Date     LKOUT C,16,WCS_ESDT,PDAT                                               
WCEstCk  LKOUT C,17,(D,B#SAVED,DS_WCEST),CHAR,LEN=L'DS_WCEST                    
WCF      LKOUT C,18,(D,B#GOXBLK,GOWCF),CHAR,LEN=L'GOWCF                         
Int/ext  LKOUT C,19,(D,B#SAVED,DS_WINEX),CHAR,L'DS_WINEX,              X        
               FILTROUT=FLTWC99                                                 
Curr     LKOUT C,20,WCS_CURR,CHAR,ND=Y                                          
CExcR    LKOUT C,21,WCS_CURX,CHAR,ND=Y                                          
CurAmt   LKOUT C,22,WCS_CURA,SPAK,ND=Y,PZERO=S                                  
         LKOUT E                                                                
*                                                                               
*     Extract Billing details from TSAR                                         
ARYBIL   LKOUT A,(R,NXTBIL),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
BilNum   LKOUT C,01,WCS_LREF,CHAR                                               
BilDat   LKOUT C,02,WCS_DAT,PDAT                                                
IdbRout  LKOUT P,,GETISDB                                                       
NetAmt   LKOUT C,03,(D,B#SAVED,DS_TAMT),SPAK,LEN=L'DS_TAMT                      
BilCur   LKOUT C,04,WCS_CURR,CHAR,ND=Y                                          
BilXRat  LKOUT C,05,WCS_CURX,CHAR,ND=Y                                          
BilCAmt  LKOUT C,06,WCS_CURA,SPAK,ND=Y                                          
VatAmt   LKOUT C,07,WCS_VAT,SPAK                                                
TotAmt   LKOUT C,08,WCS_BGRS,SPAK                                               
PIDRout  LKOUT P,WCS_PID,GETPID                                                 
PID      LKOUT C,09,(D,B#SAVED,DS_BLPID),CHAR,LEN=8                             
PIDRout  LKOUT P,WCS_D,GETPIN           uses DS_BLPID                           
PerFst   LKOUT C,10,(D,B#SAVED,DS_PIDFN),CHAR,LEN=16                            
PerMid   LKOUT C,11,(D,B#SAVED,DS_PIDMN),CHAR,LEN=16                            
PerSur   LKOUT C,12,(D,B#SAVED,DS_PIDLN),CHAR                                   
ComRat   LKOUT C,13,WCS_COMR,SPAK,PZERO=S                                       
ComAmt   LKOUT C,14,WCS_COM,SPAK,PZERO=S                                        
ComBil   LKOUT C,15,WCS_COB,SPAK,PZERO=S                                        
DbtSta   LKOUT C,16,WCS_BDST,LBIN                                               
DbtAmt   LKOUT C,17,WCS_BDAM,SPAK,PZERO=S                                       
WrtAmt   LKOUT C,18,WCS_BWAM,SPAK,PZERO=S                                       
PayDat   LKOUT C,19,WCS_BPDT,PDAT,ND=Y                                          
BilUID   LKOUT C,20,WCS_BUID,LBIN,ND=Y                                          
DbtAmt   LKOUT C,21,WCS_BDNA,SPAK,PZERO=S                                       
WrtAmt   LKOUT C,22,WCS_BWNA,SPAK,PZERO=S                                       
*&&US                                                                           
BilTyp   LKOUT C,23,WCS_BLTY,LBIN,ND=Y                                          
*&&                                                                             
FCmAmt   LKOUT C,24,WCS_COMF,SPAK,PZERO=S                                       
*&&US                                                                           
BilDesc  LKOUT C,25,WCS_BLDS,CHAR,ND=Y                                          
*&&                                                                             
BType    LKOUT C,26,WCS_BTYP,LBIN,ND=Y                                          
*&&UK                                                                           
EstLst   LKOUT C,27,(A,ARYESL)                                                  
*&&                                                                             
         LKOUT E                                                                
*&&UK                                                                           
ARYESL   LKOUT A,(D,B#BUFREC,WCS_TERM),EOT=EOR,                        X        
               ROWID=(WCS_ELET,WCS_ELEQ),ROWWIDTH=(V,WCS_ELEL)                  
         LKOUT C,27,WCS_ELED,CHAR,ND=Y,LEN=L'WCS_ESTG                           
         LKOUT E                                                                
*&&                                                                             
*                                                                               
ARYJOB   LKOUT A,(R,JOBDTL),ROWNAME=SAVED                                       
*                                                                               
CliCode  LKOUT C,01,CLI_COD,CHAR,ND=Y                                           
CliName  LKOUT C,02,CLI_NAM,CHAR,ND=Y                                           
PrdCode  LKOUT C,03,PRD_COD,CHAR,ND=Y                                           
PrdName  LKOUT C,04,PRD_NAM,CHAR,ND=Y                                           
JobCode  LKOUT C,05,JOB_COD,CHAR,ND=Y                                           
JobName  LKOUT C,06,JOB_NAM,CHAR,ND=Y                                           
Userid   LKOUT C,07,USRID,(U,#EDTUSR,$EDTUSR)                                   
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
*     Extract order details from TSAR                                           
*                                                                               
ARYORDL  LKOUT A,(R,NXTORD),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
ONum     LKOUT C,01,WCS_ONUM,CHAR,ND=Y                                          
RqNum    LKOUT C,02,WCS_LREF,CHAR,ND=Y                                          
OrdTyp   LKOUT C,03,WCS_OTYP,CHAR,ND=Y                                          
AppSta   LKOUT C,04,WCS_APST,CHAR,ND=Y                                          
OrdStat  LKOUT C,05,WCS_ORST,LBIN,ND=Y                                          
SJAcc    LKOUT C,06,WCS_SJAC,CHAR,ND=Y                                          
ExpAcc   LKOUT C,07,WCS_OULA,CHAR,ND=Y                                          
Supplr   LKOUT C,08,WCS_CTRA,CHAR,ND=Y                                          
Prout    LKOUT P,WCS_CTRA,GETCNAME                                              
SupNam   LKOUT C,09,(D,B#SAVED,DS_CTRN),CHAR,LEN=L'DS_CTRN                      
Orddte   LKOUT C,10,WCS_DAT,PDAT                                                
PIDRout  LKOUT P,WCS_PID,GETPID                                                 
PID      LKOUT C,11,(D,B#SAVED,DS_BLPID),CHAR,LEN=8                             
PIDRout  LKOUT P,WCS_D,GETPIN           uses DS_BLPID                           
PerFst   LKOUT C,12,(D,B#SAVED,DS_PIDFN),CHAR,LEN=16                            
PerMid   LKOUT C,13,(D,B#SAVED,DS_PIDMN),CHAR,LEN=16                            
PerSur   LKOUT C,14,(D,B#SAVED,DS_PIDLN),CHAR                                   
EstGlob  LKOUT C,15,WCS_ESTG,CHAR,ND=Y                                          
OrdAmt   LKOUT C,16,WCS_ORA,SPAK,ND=Y,PZERO=S                                   
OrFCAmt  LKOUT C,17,WCS_CURA,SPAK,ND=Y,PZERO=S                                  
OrdCur   LKOUT C,18,WCS_CURR,CHAR,ND=Y                                          
OrdName  LKOUT C,19,WCS_ORDN,CHAR,ND=Y                                          
GoodsRc  LKOUT C,20,WCS_GDRC,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
*                                                                               
*                                                                               
*     Extract estimate details from TSAR                                        
*                                                                               
ARYESTL  LKOUT A,(R,NXTESL),MULTIROW=Y,ROWNAME=WCS_D                            
*                                                                               
EstNum   LKOUT C,01,WCS_ESTG,CHAR,ND=Y                                          
EstDte   LKOUT C,02,WCS_ESDT,PDAT,ND=Y                                          
EstNam   LKOUT C,03,WCS_ESTD,CHAR,ND=Y                                          
EstSta   LKOUT C,04,WCS_ESTS,LBIN                                               
NetTot   LKOUT C,05,WCS_AEST,SPAK,ND=Y,PZERO=S                                  
FCAmt    LKOUT C,06,WCS_CURA,SPAK,ND=Y,PZERO=S                                  
ForCur   LKOUT C,07,WCS_CURR,CHAR,ND=Y                                          
PIDRout  LKOUT P,WCS_PID,GETPID                                                 
PID      LKOUT C,08,(D,B#SAVED,DS_BLPID),CHAR,LEN=8                             
PIDRout  LKOUT P,WCS_D,GETPIN           uses DS_BLPID                           
PerFst   LKOUT C,09,(D,B#SAVED,DS_PIDFN),CHAR,LEN=16                            
PerMid   LKOUT C,10,(D,B#SAVED,DS_PIDMN),CHAR,LEN=16                            
PerSur   LKOUT C,11,(D,B#SAVED,DS_PIDLN),CHAR                                   
SJAcc    LKOUT C,12,WCS_SJAC,CHAR,ND=Y                                          
EstLoc   LKOUT C,13,WCS_ENUM,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
*                                                                               
JOBDTL   ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
***********************************************************************         
*  End of array, edit and filter routines are usually kept here                 
***********************************************************************         
         SPACE 1                                                                
* field filter routines                                                         
*        COMMON EXITS                                                           
FLTOKX   CR    RB,RB            ok, process                                     
         BR    RE                                                               
*                                                                               
FLTNOX   LA    RF,1             reject                                          
         CHI   RF,0                                                             
         BR    RE                                                               
*                                                                               
         USING WCS_D,R4                                                         
FLTWC99  L     R4,LP_AINP                                                       
         CLC   PRODUL,QS_UNIT                                                   
         BNER  RE                                                               
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   *+14                                                             
         CLC   BILWC,WCS_WCOD      If Aura don't send out work code 99          
         JE    SETCCC                                       field               
*&&                                                                             
         OC    WCS_WCOD,WCS_WCOD                                                
         J     SETCCC                                                           
*                                                                               
         USING WCS_D,R4                                                         
FLTWC    L     R4,LP_AINP                                                       
         CLC   PRODUL,QS_UNIT                                                   
         BNER  RE                                                               
         OC    WCS_WCOD,WCS_WCOD                                                
         J     SETCCC                                                           
*                                                                               
FLTYSSJ  CLC   PRODUL,QS_UNIT                                                   
         BR    RE                                                               
*                                                                               
FLTNOSJ  CLC   PRODUL,QS_UNIT                                                   
         JNE   FLTOKX                                                           
         J     FLTNOX                                                           
*                                                                               
         USING WCS_D,R4                                                         
FLTHASDB L     R4,LP_AINP                                                       
         CP    WCS_DBA,PZERO                                                    
         JE    FLTNOX                                                           
         J     FLTOKX                                                           
*                                                                               
FLTNFBAD SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JE    FLTOKX                                                           
         L     R4,LP_AINP        A(WCS_D RECORD)                                
         CLI   WCS_TYPE,WCS_FBAD      SKIP FLEXI ADV BILLS                      
         JNE   FLTOKX                                                           
         J     FLTNOX                                                           
         EJECT                                                                  
TSTOWCO  CLI   QS_OWCON,YESQ                                                    
         J     SETCCC                                                           
                                                                                
TSTORDL  CLI   QS_OWCON,YESQ                                                    
         JE    SETCCC                                                           
         CLI   QS_ORDL,YESQ                                                     
         BR    RE                                                               
                                                                                
TSTESTL  CLI   QS_OWCON,YESQ                                                    
         JE    SETCCC                                                           
         CLI   QS_ESTL,YESQ                                                     
         BR    RE                                                               
                                                                                
TSTHRS   CLI   QS_HRS,NOQ                                                       
         J     SETCCC                                                           
                                                                                
TSTOWEX  CLI   QS_EXPRT,YESQ                                                    
         BR    RE                                                               
                                                                                
TSTIVIC  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPINVTAQ         Test app is INVICTA                          
         BR    RE                                                               
                                                                                
TSTPAYI  CLI   QS_PAYI,YESQ                                                     
         BNER  RE                                                               
         OC    CPXJBOSP,CPXJBOSP   Do we have job date control                  
         BZR   RE                  No - show payment information                
         CLC   CPXJBOSP,DS_JOPDC   Don't show payment information for           
         BER   RE                   jobs opened prior to the control            
         BHR   RE                    date on the company record                 
         J     SETCCC                                                           
***********************************************************************         
* 'Next Record' routine - DDLINK hooks here to look for more records            
* to pass to the output processors.                                             
*    Note this routine processes multiple record types for each request         
*    passed to it, and stores the data in TSAR, there are no output             
*    processors linked to the array.                                            
*    The output processors will have to read from TSAR, DDLINK won't            
*    manage EOF.                                                                
***********************************************************************         
                                                                                
NXTSREC  LAY   R4,EX_BUFF                                                       
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LAY   RF,L'EX_BUFF        CHECK BUFFER BIG ENOUGH FOR BASIC            
         LA    RE,WCS_DLNQ                                  TSAR                
         CR    RF,RE                                                            
         JH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    QS_ENACT,QS_ENACT                                                
         JNZ   *+10                                                             
         MVC   QS_ENACT,FFS                                                     
                                                                                
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
         MVC   DS_OTXWC,SPACES                                                  
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NSR005                                                           
         GOTOR SJINIT              RETRIEVE SJ A/C DATA                         
         JNE   QERROR              ERROR                                        
         MVC   DS_OTXWC,ORDWC                                                   
         J     NSR015                                                           
*                                                                               
         USING LW_D,R2                                                          
NSR005   XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUBJA                                                    
         JZ    NSR010                                                           
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear entries                                
         LHI   R1,DS_SUBJM*L'ACTKACT                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               Clear area                                   
NSR010   GOTOR LP_AAWMP,DMCB,(L'ACTKACT,QS_ACCT),DS_SUBJI,             +        
               DS_SUBJM,LP_D                                                    
         DROP  R2                                                               
*                                                                               
NSR015   XC    SEQCTR,SEQCTR                                                    
         OC    QS_AWORK,QS_AWORK   ANY WORKCODES PASSED?                        
         JZ    NSR020                                                           
         USING LW_D,RF                                                          
         SR    RF,RF                                                            
         ICM   RF,7,QS_AWORK                                                    
         OC    LW_NUMN,LW_NUMN     ANY ENTRIES                                  
         JNZ   NSR021                                                           
NSR020   MVC   QS_AWORK,AALL       NO, READ ALL                                 
*                                                                               
NSR021   OC    QS_AOFFC,QS_AOFFC   ANY OFFICE PASSED?                           
         JZ    NSR026                                                           
         ICM   RF,7,QS_AOFFC                                                    
         OC    LW_NUMN,LW_NUMN     ANY ENTRIES                                  
         JZ    NSR026                                                           
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN                                                     
         LA    R3,LW_DATA2                                                      
*                                                                               
NSR024   OC    0(L'TRNOFFC,R3),0(R3)    scan wmp for match                      
         JZ    NSR030              No match, next element                       
         GOTOR LP_AAWMP,DMCB,(L'TRNOFFC,0(R3)),DS_ROFFI,OFFMAXQ,       +        
               LP_D                                                             
         LA    R3,L'TRNOFFC(R3)                                                 
         JCT   R0,NSR024                                                        
         J     NSR030                                                           
         DROP  RF                                                               
*                                                                               
NSR026   TM    CPYSTAT4,CPYSOFF2                                                
         JZ    NSR030              If 2 char office don't want to clear         
         CLC   PRODUL,QS_UNIT      Skip if SJ no need to check                  
         JE    NSR030                                                           
         L     R3,AOFFAREA                                                      
         AHI   R3,OFFAWORK-OFFALD+2                                             
*                                                                               
NSR028   CLC   0(L'TRNKOFF,R3),=X'FFFF'                                         
         JE    NSR050              Hit end of table?                            
         CLC   0(L'TRNKOFF,R3),SPACES                                           
         JNH   NSR050                                                           
         GOTOR LP_AAWMP,DMCB,(L'TRNKOFF,0(R3)),DS_ROFFI,OFFMAXQ,       +        
               LP_D                                                             
         LA    R3,L'TRNOFFC(R3)                                                 
         J     NSR028                                                           
*                                                                               
NSR030   OC    DS_AROFF,DS_AROFF   ANY OFFICE PASSED?                           
         JNZ   *+10                                                             
         MVC   DS_AROFF,AALL       NO, READ ALL                                 
*                                                                               
NSR050   DS    0H                                                               
*                                                                               
* add estimate records to TSAR                                                  
*                                                                               
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NSR060                                                           
         GOTOR GETOEST             DERIVE FROM EVERECS                          
         JNE   QERROR              ERROR                                        
*                                                                               
NSR055   GOTOR GETEST              READ ESTRECS                                 
         JNE   QERROR              ERROR                                        
*                                                                               
* add transactions to TSAR                                                      
*                                                                               
NSR060   GOTOR GETRANS                                                          
         JNE   QERROR              ERROR                                        
*                                                                               
* add order transactions/unapproved orders to TSAR                              
*                                                                               
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NSR100                                                           
         GOTOR GETOTRN                                                          
         JNE   QERROR              ERROR                                        
         GOTOR GETUORD                                                          
         JNE   QERROR              ERROR                                        
*                                                                               
* add expense records to TSAR                                                   
*                                                                               
         GOTOR GETEXP                                                           
         JNE   QERROR              ERROR                                        
*                                                                               
* add timsheet record data to TSAR                                              
*                                                                               
         GOTOR GETTIME                                                          
         JNE   QERROR              ERROR                                        
*                                                                               
NSR100   LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         XC    DS_PRPID,DS_PRPID                                                
*                                                                               
NSREOF   MVI   LP_RMODE,LP_RLAST   NO MORE TSARS TO ADD                         
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for summary call - accumulates TSARs                    
***********************************************************************         
         USING WCS_D,R4                                                         
NXTSUM   LAY   R4,EX_BUFF          TSAR io area                                 
         TM    DS_IND1,DS_ITOT                                                  
         JNZ   NXTSUM60                                                         
         TM    DS_IND1,DS_IEOB                                                  
         JNZ   NXSMEOF                                                          
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXTSUM10            No                                           
*                                                                               
         ZAP   DS_TOTBL,PZERO                                                   
         MVI   LP_RMODE,LP_RNEXT                                                
         MVC   LP_ADATA,AIO2                                                    
         LAY   R0,EX_BUFF          Clear buffer area                            
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R0,AIO2             Clear IO area 2                              
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)  Read first record in buffer            
         J     NXTSUM04                                                         
NXTSUM02 GOTOR GOTSAR,DMCB,('TSANXT',0)  Next record for current wc             
NXTSUM04 TM    TSARERRS,TSEEOF                                                  
         JNZ   NXSMEOF             No records - exit                            
         CLI   WCS_ORST,WCS_FMCH   Don't put fully matched or cancelled         
         JE    NXTSUM02              orders to summary                          
         CLI   WCS_ORST,WCS_CLLD                                                
         JE    NXTSUM02                                                         
         CLI   WCS_TYPE,WCS_ESTL   Ignore estimate/order lists                  
         JE    NXTSUM02                                                         
         CLI   WCS_TYPE,WCS_ORDL                                                
         JE    NXTSUM02                                                         
*&&US                                                                           
         CLI   WCS_TYPE,WCS_EST    US wants only 'current' estimate in          
         JNE   NXTSUM10            Summary                                      
*                                                                               
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of accounts                        
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
NXTSUM06 CLC   WCS_SJAC,0(RF)                                                   
         JE    NXTSUM08                                                         
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,NXTSUM06                                                      
         DC    H'0'                Sub job should be found!                     
*                                                                               
NXTSUM08 CLC   WCS_ENUM,0(R1)      Check highest revision est on job            
         JNE   NXTSUM02                                                         
*&&                                                                             
*&&UK                                                                           
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NXTSUM10                                                         
         CLC   WCS_WCOD,BILWC      No workcode 99's in UK summary               
         JNE   NXTSUM10                                                         
         CLI   WCS_BTYP,TRNTMABL   Only summarise manual bills                  
         JE    NXTSUM10              and media production bills                 
         CLI   WCS_BTYP,TRNTMEBL                                                
         JNE   NXTSUM02                                                         
*&&                                                                             
SUMR     USING WCS_D,R2                                                         
NXTSUM10 TM    DS_IND1,DS_ISUBJ    Are we outputting 2 totals lines             
         JZ    NXTSUM12            No                                           
         L     R2,AIO2             Ioarea 2 to hold summary and total           
         LA    R2,WCS_DLNQ(R2)                               line               
         CLC   WCS_WCOD,SUMR.WCS_WCOD Change of work code                       
         JE    NXTSUM12            No                                           
         LA    RE,WCS_KEY          Yes - create total summary for WC            
         LHI   RF,WCS_DLNQ                                                      
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
         L     R2,AIO2                                                          
         LA    R2,WCS_DLNQ(R2)                                                  
         MVI   SUMR.WCS_TYPE,WCS_SUM                                            
         MVI   SUMR.WCS_SORM,WCS_TOTJ                                           
         LHI   RF,WCS_DLNQ                                                      
         STH   RF,SUMR.WCS_LEN                                                  
                                                                                
NXTSUM12 L     R2,AIO2             Build summary record key                     
         LA    RE,WCS_KEY                                                       
         LHI   RF,WCS_DLNQ                                                      
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
         L     R2,AIO2                                                          
         MVI   SUMR.WCS_TYPE,WCS_SUM                                            
         LHI   RF,WCS_DLNQ                                                      
         STH   RF,SUMR.WCS_LEN                                                  
         MVI   SUMR.WCS_SORM,WCS_TOTJ                                           
         TM    DS_IND1,DS_ISUBJ    Are we outputting 2 totals lines             
         JZ    *+10                No                                           
         MVC   SUMR.WCS_SORM,WCS_SORM                                           
*&&US                                                                           
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of accounts                        
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
         USING JB_D,R1                                                          
NXTSUM14 CLC   WCS_SJAC,0(RF)                                                   
         JE    NXTSUM16                                                         
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,NXTSUM14                                                      
         DC    H'0'                Sub job should be found!                     
*                                                                               
NXTSUM16 LA    RF,JB_TOTAL                                                      
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NXTSUM18                                                         
         CLC   WCS_WCOD,BILWC      Use allocated or billed                      
         JNE   NXTSUM18                                                         
         LA    RF,JB_TOTBL                                                      
NXTSUM18 AP    0(L'JB_TOTBL,RF),WCS_ALA Add up total billed for job             
         DROP  R1                                                               
*&&                                                                             
*                                                                               
NXTSUM20 GOTOR GOTSAR,DMCB,('TSANXT',0)  Next record for current wc             
         TM    TSARERRS,TSEEOF                                                  
         JNZ   NXTSUM40            No more recs, process last summary           
         CLI   WCS_ORST,WCS_FMCH   Don't put fully matched or cancelled         
         JE    NXTSUM20               orders to summary                         
         CLI   WCS_ORST,WCS_CLLD                                                
         JE    NXTSUM20                                                         
         CLI   WCS_TYPE,WCS_ESTL   Ignore estimate/order lists                  
         JE    NXTSUM20                                                         
         CLI   WCS_TYPE,WCS_ORDL                                                
         JE    NXTSUM20                                                         
*&&US                                                                           
         CLI   WCS_TYPE,WCS_EST    US wants only 'current' estimate in          
         JNE   NXTSUM26            Summary                                      
*                                                                               
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of accounts                        
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
NXTSUM22 CLC   WCS_SJAC,0(RF)                                                   
         JE    NXTSUM24                                                         
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,NXTSUM22                                                      
         DC    H'0'                Sub job should be found!                     
*                                                                               
NXTSUM24 CLC   WCS_ENUM,0(R1)      Check highest revision est on job            
         JNE   NXTSUM20                                                         
*&&                                                                             
*&&UK                                                                           
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NXTSUM26                                                         
         CLC   WCS_WCOD,BILWC      No workcode 99's in UK summary               
         JNE   NXTSUM26                                                         
         CLI   WCS_BTYP,TRNTMABL   Only summarise manual bills                  
         JE    NXTSUM26              and media production bills                 
         CLI   WCS_BTYP,TRNTMEBL                                                
         JNE   NXTSUM20                                                         
*&&                                                                             
NXTSUM26 CLC   WCS_WCOD,SUMR.WCS_WCOD Change of work code                       
         JNE   NXTSUM42                                                         
         TM    DS_IND1,DS_ISUBJ    Are we outputting total line per wc          
         JZ    NXTSUM30            No                                           
         L     R2,AIO2                                                          
         LA    R2,WCS_DLNQ(R2)     First total area for master                  
         LA    RF,WCS_SPNS                                                      
         LA    RE,WCS_SPNQ         Number of accumulators to roll up            
         LA    R1,SUMR.WCS_SPNS                                                 
NXTSUM28 AP    0(L'WCS_SPNS,R1),0(L'WCS_SPNS,RF)                                
         LA    RF,L'WCS_SPNS(RF)                                                
         LA    R1,L'WCS_SPNS(R1)                                                
         JCT   RE,NXTSUM28                                                      
         L     R2,AIO2                                                          
         CLC   WCS_SORM,SUMR.WCS_SORM  Sub or master job match                  
         JNE   NXTSUM44                                                         
*                                                                               
NXTSUM30 DS    0H                                                               
*&&US                                                                           
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of accounts                        
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
         USING JB_D,R1                                                          
NXTSUM32 CLC   WCS_SJAC,0(RF)                                                   
         JE    NXTSUM34                                                         
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,NXTSUM32                                                      
         DC    H'0'                Sub job should be found!                     
*                                                                               
NXTSUM34 LA    RF,JB_TOTAL                                                      
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NXTSUM36                                                         
         CLC   WCS_WCOD,BILWC      Use allocated or billed                      
         JNE   NXTSUM36                                                         
         LA    RF,JB_TOTBL                                                      
NXTSUM36 AP    0(L'JB_TOTBL,RF),WCS_ALA Add up total billed for job             
         DROP  R1                                                               
*&&                                                                             
         GOTOR SUMTRN              Roll transaction (not bill)                  
         J     NXTSUM20            next tsar rec                                
*                                                                               
NXTSUM40 OI    DS_IND1,DS_IEOB     Set end of buffer                            
*                                                                               
NXTSUM42 TM    DS_IND1,DS_ISUBJ    Are we outputting total line per wc          
         JZ    NXTSUM44            No                                           
         OI    DS_IND1,DS_ITOT                                                  
NXTSUM44 CLC   PRODUL,QS_UNIT                                                   
         JNE   NXTSUM48                                                         
         CP    SUMR.WCS_COMR,PZERO                                              
         JNE   NXTSUM48                                                         
         CLC   LASTWC,SUMR.WCS_WCOD                                             
         JE    NXTSUM46                                                         
         MVC   LASTWC,SUMR.WCS_WCOD                                             
         MVC   DS_ACCT,QS_ACCT                                                  
         GOTOR GETOPTW                                                          
*                                                                               
         USING GOBLOCKD,R3                                                      
NXTSUM46 L     R3,AGOBLOCB                                                      
         ZAP   SUMR.WCS_COMR,GOAGYCOM                                           
         DROP  R3                                                               
*                                                                               
NXTSUM48 MVC   LP_ADATA,AIO2                                                    
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JNE   EXITY                                                            
         CLC   PRODUL,QS_UNIT                                                   
         JNE   EXITY                                                            
         CLC   SUMR.WCS_WCOD,BILWC                                              
         JNE   EXITY                                                            
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of accounts                        
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R3,DS_ESCEA                                                      
         USING JB_D,R3                                                          
*                                                                               
                                                                                
NXTSUM49 CLI   SUMR.WCS_SORM,WCS_TOTJ  Is it master/total job                   
         JE    *+12                                                             
         CLI   SUMR.WCS_SORM,WCS_MSTJ                                           
         JNE   NXTSUM50                No - it's sub jobs                       
         CLC   QS_ACCT,0(RF)                                                    
         JE    NXTSUM54                                                         
         J     NXTSUM52                                                         
                                                                                
NXTSUM50 CLC   QS_ACCT,0(RF)                                                    
         JNE   NXTSUM54                                                         
                                                                                
NXTSUM52 LA    RF,L'ACTKACT(RF)                                                 
         LA    R3,JB_LNQ(R3)                                                    
         JCT   R0,NXTSUM49                                                      
                                                                                
         CP    SUMR.WCS_ALA,PZERO   Any prebilling                              
         JNE   EXITY                Yes                                         
         J     NXTSUM                                                           
                                                                                
NXTSUM54 SP    SUMR.WCS_ALA,JB_TOTAL To work out prebilling subtract            
         AP    SUMR.WCS_LBIL,JB_TOTAL actuals allocated from billing            
         TM    DS_IND1,DS_ISUBJ    Are we outputting 2 totals lines             
         JZ    NXTSUM52                                                         
         L     R2,AIO2                                                          
         LA    R2,WCS_DLNQ(R2)                                                  
         SP    SUMR.WCS_ALA,JB_TOTAL To work out prebilling subtract            
         AP    SUMR.WCS_LBIL,JB_TOTAL actuals allocated from billing            
         L     R2,AIO2                                                          
         J     NXTSUM52                                                         
         DROP  R3                                                               
*&&                                                                             
         J     EXITY                                                            
*                                                                               
NXTSUM60 L     R2,AIO2                                                          
         LA    R2,WCS_DLNQ(R2)     First total area for master                  
         ST    R2,LP_ADATA                                                      
         NI    DS_IND1,X'FF'-DS_ITOT                                            
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JNE   EXITY                                                            
         CLC   PRODUL,QS_UNIT                                                   
         JNE   EXITY                                                            
         CLC   SUMR.WCS_WCOD,BILWC                                              
         JNE   EXITY                                                            
         CP    SUMR.WCS_ALA,PZERO   Any prebilling                              
         JE    NXSMEOF              No                                          
*&&                                                                             
         J     EXITY                                                            
*                                                                               
NXSMEOF  MVI   LP_RMODE,LP_RLAST   No more records                              
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for accounting detail view                              
***********************************************************************         
                                                                                
NXTACC   LAY   R4,EX_BUFF          TSAR io area                                 
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXAC100             No                                           
*                                                                               
*&&US*&& GOTOR PROCBLS             Pre process billing                          
*                                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXAC110                                                          
*                                                                               
NXAC100  LAY   R0,EX_BUFF          Set A(Returned record)                       
         ST    R0,LP_ADATA                                                      
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
NXAC110  TM    TSARERRS,TSEEOF                                                  
         JNZ   NXACEOF                                                          
*                                                                               
*      Filter by record type                                                    
*                                                                               
         CLI   WCS_TYPE,WCS_TIME                                                
         JE    NXAC140             time                                         
         CLI   WCS_TYPE,WCS_UORD                                                
         JE    NXAC140             Unapproved orders/requisitions               
         CLI   WCS_TYPE,WCS_EXP                                                 
         JE    NXAC140             Expenses                                     
         CLI   WCS_TYPE,WCS_TRN                                                 
         JE    NXAC140             transaction recs (incl order t/x)            
         CLI   WCS_TYPE,WCS_FBAD                                                
         JNE   NXAC100             Don't want this                              
*                                                                               
NXAC140  CLC   PRODUL,QS_UNIT                                                   
         JNE   NXAC160                                                          
         CLC   WCS_WCOD,BILWC                                                   
         JNE   NXAC160             Don't want billing in acc detail             
*&&UK                                                                           
         CLI   WCS_BTYP,TRNTMABL   Only show manual bills                       
         JE    NXAC160               and media production bills                 
         CLI   WCS_BTYP,TRNTMEBL                                                
         JNE   NXAC100                                                          
*&&                                                                             
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   NXAC160             Brandocean                                   
         XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)  Number of accounts                        
         LA    R2,LW_DATA2-LW_D(R2)                                             
         LA    R3,DS_ESCEA                                                      
                                                                                
NXAC142  CLC   WCS_SJAC,0(R2)                                                   
         JE    NXAC144                                                          
         LA    R3,JB_LNQ(R3)                                                    
         LA    R2,L'ACTKACT(R2)                                                 
         JCT   R0,NXAC142                                                       
         DC    H'0'                                                             
                                                                                
         USING JB_D,R3                                                          
NXAC144  TM    JB_IND1,JB_IPRE     Do we have prebilling to show                
         JZ    NXACEOF             No - finish                                  
         CLI   WCS_BLTY,TRNBTMAN   Only show prebilling types                   
         JE    NXAC160              which is manual, percent of                 
         CLI   WCS_BLTY,TRNBTPER     estimate and special                       
         JE    NXAC160                                                          
         CLI   WCS_BLTY,TRNBTSPE                                                
         JNE   NXAC100                                                          
*&&                                                                             
NXAC160  J     NXACOKX                                                          
*                                                                               
NXACEOF  MVI   LP_RMODE,LP_RLAST   No more records                              
         J     EXITY                                                            
*                                                                               
NXACOKX  LAY   R0,EX_BUFF          Pass TSAR to DDLINK For output               
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for Estimates - uses TSAR buffered recods     *         
***********************************************************************         
                                                                                
NXTEST   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXES100             No                                           
*                                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXES110                                                          
*                                                                               
NXES100  LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
*                                                                               
NXES110  TM    TSARERRS,TSEEOF                                                  
         JNZ   NXESEOF                                                          
         CLI   WCS_TYPE,WCS_EST    Only want estimate records                   
         JNE   NXES100                                                          
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   NXESOKX             Brandocean                                   
*                                                                               
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of address lines                   
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
NXES115  CLC   WCS_SJAC,0(RF)                                                   
         JE    NXES120                                                          
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,NXES115                                                       
         DC    H'0'                Sub job should be found!                     
*                                                                               
NXES120  CLC   WCS_ENUM,0(R1)      For Aura check it's curr estimate            
         JNE   NXES100             if not don't return estimate                 
*&&                                                                             
         J     NXESOKX                                                          
*                                                                               
NXESEOF  MVI   LP_RMODE,LP_RLAST   No more records to dis                       
         J     EXITY                                                            
*                                                                               
NXESOKX  LAY   R0,EX_BUFF          PASS TSAR TO DDLINK FOR OUTPUT               
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for BILLING   - uses buffered transactions    *         
* with WC 99                                                          *         
***********************************************************************         
                                                                                
NXTBIL   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         CLC   PRODUL,QS_UNIT                                                   
         JNE   NXBLEOF                                                          
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXBL100             No                                           
*                                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   WCS_WCOD,BILWC                                                   
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXBL110                                                          
*                                                                               
NXBL100  LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
*        ST    R0,LP_ADATA                                                      
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
*                                                                               
NXBL110  TM    TSARERRS,TSEEOF                                                  
         JNZ   NXBLEOF                                                          
         CLI   WCS_TYPE,WCS_TRN                                                 
         JNE   NXBL100             DON'T WANT THIS                              
         CLC   WCS_WCOD,BILWC                                                   
         JNE   NXBL100             DON'T WANT THIS                              
         J     NXBLOKX                                                          
*                                                                               
NXBLEOF  MVI   LP_RMODE,LP_RLAST   NO MORE RECORDS                              
         J     EXITY                                                            
*                                                                               
NXBLOKX  DS    0H                                                               
*&&UK*&& GOTOR RDESTL              READ ESTIMATE LIST and SAVED IN AIO2         
         LAY   R0,EX_BUFF          PASS TSAR TO DDLINK FOR OUTPUT               
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for Orders  - uses buffered records           *         
***********************************************************************         
                                                                                
NXTORD   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXOR100             No                                           
*                                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXOR110                                                          
*                                                                               
NXOR100  LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
*                                                                               
NXOR110  TM    TSARERRS,TSEEOF                                                  
         JNZ   NXOREOF                                                          
         CLI   WCS_TYPE,WCS_ORDL   Only want order list records                 
         JNE   NXOR100                                                          
         J     NXOROKX                                                          
*                                                                               
NXOREOF  MVI   LP_RMODE,LP_RLAST   No more records to dis                       
         J     EXITY                                                            
*                                                                               
NXOROKX  LAY   R0,EX_BUFF          Pass TSAR to DDLINK for output               
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for Estimates list - uses buffered records    *         
***********************************************************************         
                                                                                
NXTESL   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         CLI   LP_RMODE,LP_RNEXT   Test first time                              
         JE    NXEL100             No                                           
*                                                                               
         MVI   LP_RMODE,LP_RNEXT                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXEL110                                                          
*                                                                               
NXEL100  LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
*                                                                               
NXEL110  TM    TSARERRS,TSEEOF                                                  
         JNZ   NXELEOF                                                          
         CLI   WCS_TYPE,WCS_ESTL   Only want estimate list records              
         JNE   NXEL100                                                          
         J     NXELOKX                                                          
*                                                                               
NXELEOF  MVI   LP_RMODE,LP_RLAST   No more records to dis                       
         J     EXITY                                                            
*                                                                               
NXELOKX  LAY   R0,EX_BUFF          Pass TSAR to DDLINK for output               
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for Estimates - uses keydrivers, passive keys *         
*    DDLINK doesn't like passives so you need to read the DA rec and            
*    pass that.                                                                 
***********************************************************************         
                                                                                
NXTNAR   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         MVI   LP_RMODE,LP_RLAST                                                
         LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* 'Next record' routine for Estimates - uses keydrivers, passive keys *         
*    DDLINK doesn't like passives so you need to read the DA rec and            
*    pass that.                                                                 
***********************************************************************         
                                                                                
NXTBNO   LAY   R4,EX_BUFF          TSAR IO AREA                                 
         MVI   LP_RMODE,LP_RLAST                                                
         LAY   R0,EX_BUFF          SET A(RETURNED REC)                          
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit client code out                                                *         
***********************************************************************         
         SPACE 1                                                                
EDTCLC   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STCM  RF,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit client name out                                                *         
* Reads from buffer for efficiency                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OB_D,GBUFAREA                                                    
EDTCLN   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'SJ'                                
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),DS_ACCT                             
         EX    RF,0(RE)                                                         
         MVC   OB_KEY(L'ACTKEY),TEMP2                                           
         GOTOR GETBUF,OB_D                                                      
         JL    EDTCL02                                                          
         JH    EXITN                                                            
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
         J     EDTCL04                                                          
*                                                                               
EDTCL02  GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
         GOTOR ADDBUF,OB_D                                                      
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
EDTCL04  LHI   R3,L'NAMEREC                                                     
         STCM  R3,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit product code out                                               *         
***********************************************************************         
         SPACE 1                                                                
EDTPRC   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         LLC   R1,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,R1               RF=Length of product code                    
         SHI   RF,1                                                             
         AR    R2,R1               R2=A(product code)                           
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STCM  RF,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit product name out                                               *         
* Reads from buffer for efficiency                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OB_D,GBUFAREA                                                    
EDTPRN   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKACT,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'SJ'                                
                                                                                
         LLC   RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RF,0(R1)                                                         
         JNH   EXITY               NO                                           
*                                                                               
         LLC   RF,PPROLEN                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),0(R2)                               
         EX    RF,0(RE)                                                         
         MVC   OB_KEY(L'ACTKEY),TEMP2                                           
         GOTOR GETBUF,OB_D                                                      
         JL    EDTPR02                                                          
         JH    EXITN                                                            
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
         J     EDTPR04                                                          
*                                                                               
EDTPR02  GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
         GOTOR ADDBUF,OB_D                                                      
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
EDTPR04  LHI   R3,L'NAMEREC                                                     
         STCM  R3,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit job code out                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTJBC   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         LLC   R1,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         SR    RF,R1               RF=Length of job code                        
         SHI   RF,1                                                             
         AR    R2,R1               R2=A(job code)                               
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STCM  RF,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit job name out                                                   *         
* Reads from buffer for efficiency                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OB_D,GBUFAREA                                                    
EDTJBN   LM    R2,R4,LP_AINP                                                    
         LA    R6,LP_AINP                                                       
         CLC   0(L'ACTKACT,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'SJ'                                
                                                                                
         LLC   RE,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         SR    RF,RE               RF=LENGTH OF JOB CODE                        
         AR    RE,R2                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A JOB CODE                        
         EX    RF,0(R1)                                                         
         JNH   EXITY               NO EXIT                                      
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R2)                       
         MVC   OB_KEY(L'ACTKEY),TEMP2                                           
         GOTOR GETBUF,OB_D                                                      
         JL    EDTJR02                                                          
         JH    EXITN                                                            
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
         J     EDTJR04                                                          
*                                                                               
EDTJR02  GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
         GOTOR ADDBUF,OB_D                                                      
         MVC   0(L'NAMEREC,R4),OB_NAME                                          
EDTJR04  LHI   R3,L'NAMEREC                                                     
         STCM  R3,15,4(R6)                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* End of known request processors - will error if we get this far     *         
* without a match                                                     *         
***********************************************************************         
REQUESTX LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* Common routines                                                               
***********************************************************************         
*&&US                                                                           
         USING WCS_D,R4                                                         
PROCBLS  NTR1  BASE=*,LABEL=NO     *** PROCESS BILLS ***                        
         J     *+12                                                             
         DC    C'*PROCBLS'                                                      
         CLC   PRODUL,QS_UNIT                                                   
         JNE   EXITY                                                            
*                                                                               
         XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R5,R5                                                            
         ICM   R5,3,LW_NUMN-LW_D(R2)  Number of accounts                        
         LA    R2,LW_DATA2-LW_D(R2)                                             
         LA    R3,DS_ESCEA                                                      
         MVI   DS_SORM,WCS_MSTJ                                                 
                                                                                
         USING JB_D,R3                                                          
PRBLS04  CP    JB_TOTBL,JB_TOTAL   Does billing equal allocation                
         JNE   PRBLS08             No - prebilling exists                       
                                                                                
PRBLS06  MVI   DS_SORM,WCS_SUBJ    Yes - try next entry                         
         MVC   WCS_SJAC,0(R2)                                                   
         LA    R3,JB_LNQ(R3)                                                    
         LA    R2,L'ACTKACT(R2)                                                 
         JCT   R5,PRBLS04                                                       
         J     PROCBLSX                                                         
*                                                                               
PRBLS08  OI    JB_IND1,JB_IPRE                                                  
         DROP  R3                                                               
                                                                                
         LAY   R4,EX_BUFF          TSAR io area                                 
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   WCS_WCOD,BILWC                                                   
         MVC   WCS_SORM,DS_SORM                                                 
         MVC   WCS_SJAC,0(R2)                                                   
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     PRBLS20                                                          
*                                                                               
PRBLS10  LAY   R0,EX_BUFF          Set A(Returned record)                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
PRBLS20  TM    TSARERRS,TSEEOF                                                  
         JNZ   PRBLS06                                                          
*                                                                               
*      Filter by record type                                                    
*                                                                               
PRBLS30  CLC   WCS_WCOD,BILWC      Is it a bill?                                
         JNE   PRBLS06             No                                           
         CLC   WCS_SORM,DS_SORM                                                 
         JNE   PRBLS06                                                          
         CLC   WCS_SJAC,0(R2)                                                   
         JNE   PRBLS06                                                          
         CLI   WCS_BLTY,TRNBTTOT   Is it total billing?                         
         JNE   PRBLS10             No                                           
         CLI   WCS_IREV,YESQ       If total billing reversed ignore it          
         JE    PRBLS10                                                          
         MVC   STSARKEY,WCS_KEY1   Yes - make all previous pre-billing          
                                                                                
         LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   WCS_WCOD,BILWC                                                   
         MVC   WCS_SORM,DS_SORM                                                 
         MVC   WCS_SJAC,0(R2)                                                   
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     PRBLS50                                                          
*                                                                               
PRBLS40  LAY   R0,EX_BUFF          Set A(Returned record)                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
PRBLS50  TM    TSARERRS,TSEEOF                                                  
         JZ    *+6                                                              
         DC    H'0'                                                             
         CLC   STSARKEY,WCS_KEY1   Have we reached total bill rec               
         JE    PRBLS10             Yes go back to main loop                     
         MVI   WCS_BLTY,TRNBTTOT   No - reset all billing to total              
         GOTOR GOTSAR,DMCB,('TSAWRT',0)                                         
         JE    PRBLS40                                                          
         DC    H'0'                                                             
                                                                                
PROCBLSX J     EXITY                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* GET TRANSACTION DETAILS  - ORDER TXS NOT HANDLED HERE!              *         
***********************************************************************         
         USING TRNRECD,R2                                                       
GETRANS  NTR1   BASE=*,LABEL=NO     *** TRANSACTIONS ***                        
         J     *+12                                                             
         DC    C'*GETRANS'                                                      
*                                                                               
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
         XC    SAVEKEY1,SAVEKEY1                                                
*                                                                               
         CLC   QS_AWORK,AALL       SET WORKCODE RANGE IF NO FILTER              
         JNE   GETRN002                                                         
*&&UK*&& MVC   DS_TRWCS,=C'AA'     UK enforces no special characters            
*&&US*&& MVC   DS_TRWCS,=C'*A'     US doesn't                                   
         MVC   DS_TRWCE,=C'99'                                                  
         CLC   PRODUL,QS_UNIT                                                   
         JE    GETRN002                                                         
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   GETRN002            If 2 character office don't clear            
         MVC   DS_TRWCS,=C'  '                                                  
         MVC   DS_TRWCE,=C'  '                                                  
*                                                                               
GETRN002 LA    R2,IOKEY                                                         
         MVC   TRNKEY,SAVEKEY1                                                  
*                                                                               
         CLC   PRODUL,QS_UNIT      SJ LEDGER?                                   
         JE    GETRN003                                                         
         CLC   DS_AROFF,AALL       ANY OFFICE LIST PASSED?                      
         JE    GETRN03A                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,ORNKEYT,('B#TRN',0),             +        
               (0,SAVED),AFLTRAND,AFLTRANF                                      
         JNE   EXITY                                                            
         J     GETRN006                                                         
*                                                                               
GETRN003 CLC   QS_AWORK,AALL       ALL WORKCODES?                               
         JNE   GETRN004                                                         
*                                                                               
GETRN03A GOTOR (#NXTREC,ANXTREC),DMCB,ATRKEYT,('B#TRN',0),             +        
               (0,SAVED),AFLTRAND,AFLTRANF                                      
         JNE   EXITY               NO MORE RECORDS                              
         J     GETRN006                                                         
*                                                                               
*                                  WORKCODE FILTER SUPPLIED                     
GETRN004 GOTOR (#NXTREC,ANXTREC),DMCB,TRNKEYT,('B#TRN',0),             +        
               (0,SAVED),AFLTRAND,AFLTRANF                                      
         JNE   EXITY               NO MORE RECORDS                              
*                                                                               
GETRN006 MVI   LP_RMODE,LP_RNEXT                                                
         L     R2,IOADDR           A(NEXT T/X DA REC)                           
         MVC   SAVEKEY1,TRNKEY     save, as some subroutines break seq          
*                                                                               
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         GOTOR CLRJBV,WCS_KEY      clear output values                          
         MVI   WCS_TYPE,WCS_TRN                                                 
         XC    DS_GINEL,DS_GINEL                                                
*                                                                               
         USING TRNELD,R3                                                        
         LA    R3,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   GETRN002            WHAT IS THIS?                                
         CLC   PRODUL,TRNKULA                                                   
         JNE   GETRN012                                                         
         TM    TRNRSTAT,TRNSDRFT   FlexiBill advance?                           
         JZ    GETRN008            NO                                           
         CLI   TRNTYPE,99                                                       
         JNE   GETRN008            NO                                           
         MVI   WCS_TYPE,WCS_FBAD                                                
*                                                                               
GETRN008 CLI   TRNTYPE,15          Is it type 15 write back?                    
         JE    GETRN010            Yes                                          
         CLI   TRNTYPE,57          Is it type 57 write-off?                     
         JE    GETRN010            Yes                                          
         CLI   TRNTYPE,14          Is it type 14 write-off?                     
         JNE   GETRN012            No                                           
GETRN010 ZAP   DUB,TRNAMNT                                                      
         MP    DUB,PMONE                                                        
         ZAP   WCS_WROF,DUB                                                     
GETRN012 LA    RF,WCS_DLNQ                                                      
         STCM  RF,3,WCS_LEN                                                     
         MVC   DS_ACCT,TRNKACT                                                  
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   PRODUL,TRNKULA                                                   
         JNE   GETRN014                                                         
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,TRNKACT    Save account code                            
*                                                                               
GETRN014 MVC   WCS_LREF(L'TRNREF),TRNREF                                        
         MVC   WCS_CTRA,TRNKULC                                                 
         MVC   WCS_DAT,TRNKDATE                                                 
         MVC   WCS_SREF,TRNKSBR                                                 
         MVI   WCS_TTYP,WCS_TTRN                                                
         MVC   WCS_BTYP,TRNTYPE         mem t/x and mem time                    
         MVC   WCS_BREF,TRNBTCH         Batch ref including moa                 
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVI   WCS_ICOM,YESQ                                                    
         TM    TRNSTAT,TRNSREV                                                  
         JZ    *+8                                                              
         MVI   WCS_IREV,YESQ                                                    
*                                                                               
         MVC   WCS_WCOD,TRNKWORK        Set workcode                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   GETRN016                                                         
         CLC   PRODUL,TRNKULA                                                   
         JE    GETRN016                                                         
         MVC   WCS_WCOD,TRNANAL         Set office code if not SJ               
*                                                                               
GETRN016 MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
*                                                                               
         MVC   WCS_IREF,SPACES                                                  
         CLC   PRODUL,TRNKULA                                                   
         JNE   GETRN018                                                         
         CLC   WCS_WCOD,BILWC                                                   
         JNE   GETRN018                                                         
*&&US                                                                           
         CLI   TRNLN,TRNLN1Q                                                    
         JNH   GETRN020                                                         
         ZAP   WCS_COB,TRNBLCOM                                                 
         ZAP   WCS_BGRS,TRNBLPAY                                                
         MVC   WCS_BLTY,TRNBTYPE   Bill type                                    
         MVC   WCS_BLDS,TRNBLTYP   Bill type description                        
         OC    TRNUNBIL,TRNUNBIL   Have we unbilled                             
         JZ    *+8                                                              
         MVI   WCS_IREV,YESQ       Set bill is reversed                         
*&&                                                                             
         J     GETRN020            SKIP NARRATIVES IF BILL TX                   
                                                                                
GETRN018 GOTOR GETNARR,(R2)        BUILD NARRATIVES                             
*                                                                               
         USING FFTELD,R3                                                        
GETRN020 XR    R0,R0                                                            
         MVI   BYTE1,NOQ                                                        
                                                                                
GETRN030 IC    R0,FFTLN            read for real reference number               
         AR    R3,R0               and long invoice number                      
         CLI   FFTEL,0                                                          
         JE    GETRN106                                                         
         CLI   FFTEL,SCIELQ                                                     
         JE    GETRN056                                                         
*&&UK*&& CLI   FFTEL,GPXELQ                                                     
*&&UK*&& JE    GETRN078                                                         
*&&US*&& CLI   FFTEL,PRTELQ                                                     
*&&US*&& JE    GETRN052                                                         
         CLI   FFTEL,GINELQ                                                     
         JE    GETRN043                                                         
         CLI   FFTEL,RALELQ                                                     
         JE    GETRN044                                                         
         CLI   FFTEL,TRSELQ                                                     
         JE    GETRN050                                                         
         CLI   FFTEL,FFNELQ                                                     
         JE    GETRN084                                                         
         CLI   FFTEL,OTHELQ                                                     
         JE    GETRN082                                                         
         CLI   FFTEL,TIMELQ                                                     
         JE    GETRN088                                                         
         CLI   FFTEL,AFCELQ                                                     
         JE    GETRN048                                                         
         CLI   FFTEL,PIDELQ                                                     
         JE    GETRN086                                                         
         CLI   FFTEL,TRXELQ                                                     
         JE    GETRN092                                                         
         CLI   FFTEL,SERELQ                                                     
         JE    GETRN094                                                         
         CLI   FFTEL,DUEELQ                                                     
         JE    GETRN096                                                         
         CLI   FFTEL,SORELQ                                                     
         JE    GETRN098                                                         
         CLI   FFTEL,CPJELQ                                                     
         JE    GETRN102                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   GETRN030                                                         
*                                                                               
*                                                                               
         CLI   FFTTYPE,FFTTKREF         KEY REFERENCE BANK VOID                 
         JNE   GETRN032                                                         
         CLI   BYTE1,YESQ               SKIP IF WE'VE ALREADY HAD SUPP          
         JE    GETRN030                                      REFERENCE          
         MVC   WCS_LREF,SPACES                                                  
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         MVC   WCS_LREF(0),FFTDATA                                              
         EX    RE,*-6                                                           
         J     GETRN030                                                         
*                                                                               
GETRN032 CLI   FFTTYPE,FFTTEXTY         EXPENDITURE TYPE                        
         JNE   GETRN034                                                         
         MVC   WCS_EXPT,FFTDATA                                                 
         J     GETRN030                                                         
*                                                                               
GETRN034 CLI   FFTTYPE,FFTTESTN         ESTIMATE GLOBAL NUM                     
         JNE   GETRN036                                                         
         MVC   WCS_ESTG,FFTDATA                                                 
         J     GETRN030                                                         
*                                                                               
GETRN036 CLC   PRODUL,TRNKULA      If production and billing only               
         JNE   GETRN037             show 6 character reference                  
*&&US                                for NA only                                
         CLC   WCS_WCOD,BILWC                                                   
         JE    GETRN038                                                         
*&&                                                                             
GETRN037 CLI   FFTTYPE,FFTTINVN    Supplier invoice reference                   
         JNE   GETRN038                                                         
         MVI   BYTE1,YESQ          Set to skip FFTTKREF                         
         MVC   WCS_LREF,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         JNP   GETRN030                                                         
         MVC   WCS_LREF(0),FFTDATA                                              
         EX    RE,*-6                                                           
         J     GETRN030                                                         
*                                                                               
GETRN038 CLI   FFTTYPE,FFTTCAMR    Campaign reference                           
         JNE   GETRN040                                                         
         MVC   WCS_CREF,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         JNP   GETRN030                                                         
         MVC   WCS_CREF(0),FFTDATA                                              
         EX    RE,*-6                                                           
         J     GETRN030                                                         
*                                                                               
GETRN040 CLI   FFTTYPE,FFTTCLR1    Client reference 1                           
         JNE   GETRN042                                                         
         MVC   WCS_CLR1,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         JNP   GETRN030                                                         
         MVC   WCS_CLR1(0),FFTDATA                                              
         EX    RE,*-6                                                           
         J     GETRN030                                                         
*                                                                               
GETRN042 CLI   FFTTYPE,FFTTCLR1    Client reference 2                           
         JNE   GETRN030                                                         
         MVC   WCS_CLR2,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         JNP   GETRN030                                                         
         MVC   WCS_CLR2(0),FFTDATA                                              
         EX    RE,*-6                                                           
         J     GETRN030                                                         
*                                                                               
GETRN043 MVI   DS_GINEL,YESQ       Set GINELD found                             
         J     GETRN030             only used on BT72 and invoice log           
                                                                                
         USING RALELD,R3                                                        
GETRN044 MVC   WCS_ATYP,RALTYPE                                                 
         CLI   RALTYPE,RALTOFS                                                  
         JNE   *+14                                                             
         MVC   WCS_ADT,RALODAT                                                  
         J     GETRN030                                                         
         CLI   RALTYPE,RALTALC                                                  
         JNE   GETRN045                                                         
         MVC   WCS_AREF,RALAREF                                                 
         MVC   WCS_ADT,RALADAT                                                  
         MVC   WCS_DPDT,RALADEP                                                 
         J     GETRN030                                                         
                                                                                
GETRN045 CLI   RALTYPE,RALTWOF                                                  
         JNE   GETRN046                                                         
         MVC   WCS_AREF,RALWREF                                                 
         MVC   WCS_ADT,RALWDAT                                                  
*&&UK*&& MVC   WCS_DPDT,RALWDEP                                                 
         MVC   WCS_AULA,RALWULA                                                 
         J     GETRN030                                                         
                                                                                
GETRN046 CLI   RALTYPE,RALTTTO                                                  
         JE    *+12                                                             
         CLI   RALTYPE,RALTTFR                                                  
         JNE   GETRN030                                                         
         MVC   WCS_AREF,RALTRF                                                  
         MVC   WCS_ADT,RALTDAT                                                  
         MVC   WCS_AULA,RALTULA                                                 
         J     GETRN030                                                         
                                                                                
         USING AFCELD,R3                                                        
GETRN048 MVC   WCS_CURR,AFCCURR    Foreign currrency                            
         ZAP   WCS_CURA,AFCAMNT                                                 
         GOTOR VHEXOUT,DMCB,AFCX,WCS_CURX,L'AFCX                                
         MVI   WCS_IMEM,NOQ                                                     
         TM    AFCXSTA2,AFCXSMEM                                                
         JZ    GETRN030                                                         
         MVI   WCS_IMEM,YESQ                                                    
         J     GETRN030                                                         
*                                                                               
         USING TRSELD,R3                                                        
GETRN050 MVC   WCS_BUID,TRSUSER    Save the user id number                      
         MVC   WCS_ACDT,TRSDATE    Save activity date                           
         MVC   WCS_MOA,TRSPMOS     Save posting MOA                             
         MVI   WCS_MOA+L'TRSPMOS,X'01'                                          
         MVI   WCS_APST,WCS_ASFA   trx appr status - def=approved               
         CLI   TRSLN,TRSLN2Q                                                    
         JL    *+10                                                             
         MVC   WCS_GPST,TRSGSTAT                                                
*                                                                               
         CLC   PRODUL,TRNKULA                                                   
         JNE   *+14                                                             
         CLC   WCS_WCOD,BILWC      FlexiBill advance OR plain bill t/x          
         JE    GETRN030                                                         
*                                                                               
         OC    TRSSTAT4,TRSSTAT4   set trx status                               
         JZ    GETRN030                                                         
         MVI   WCS_APST,WCS_ASIP                                                
         TM    TRSSTAT4,TRSSSAVT                                                
         JNZ   GETRN030                                                         
         MVI   WCS_APST,WCS_ASRJ                                                
         TM    TRSSTAT4,TRSSREJT                                                
         JNZ   GETRN030                                                         
         MVI   WCS_APST,WCS_ASSB                                                
         TM    TRSSTAT4,TRSSSUBT                                                
         JNZ   GETRN030                                                         
         MVI   WCS_APST,WCS_ASPA                                                
         J     GETRN030                                                         
*                                                                               
*&&US                                                                           
         USING PRTELD,R3                                                        
GETRN052 TM    PRTSTAT,PRTSBILQ    Is this B-Time?                              
         JNO   GETRN054                                                         
         ZAP   WCS_HRS,PRTHOUR                                                  
         MVI   WCS_TTYP,WCS_TBTI                                                
         J     GETRN030                                                         
GETRN054 ZAP   WCS_HRS,PRTHOUR     add Non-B-Time hours                         
         MVI   WCS_TTYP,WCS_TNTI                                                
         TM    PRTSTAT,PRTSRTEQ    Is this R-Time?                              
         JNO   GETRN030                                                         
         ZAP   DUB,PRTRATE                                                      
         MP    DUB,PRTHOUR                                                      
         DP    DUB,=P'100'                                                      
         ZAP   WCS_CHT,DUB(6)                                                   
         MVI   WCS_TTYP,WCS_TMTI                                                
         L     RF,AGOXBLCK                                                      
         CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
         JE    *+12                Yes                                          
         CLI   QS_ESTC,YESQ        Estimate checking?                           
         JNE   GETRN030                                                         
         ZAP   WCS_DBA,DUB(6)                                                   
         J     GETRN030                                                         
         DROP  R3                                                               
*&&                                                                             
*                                                                               
         USING SCIELD,R3                                                        
GETRN056 CLI   SCITYPE,SCITCPAD    Payment information on SJ posting            
         JNE   GETRN057                                                         
         CP    SCIAMNT,PZERO       No payment info                              
         JE    GETRN030                                                         
         ZAP   WCS_PAMT,SCIAMNT    Amount                                       
         MVC   WCS_PREF,SCITCPNO                                                
         MVC   WCS_PDAT,SCITCPDT                                                
         J     GETRN030                                                         
*                                                                               
GETRN057 DS    0H                                                               
*&&UK                                                                           
         CLI   SCITYPE,SCITSJHR    real hours                                   
         JNE   GETRN060                                                         
         CLC   PRODUL,TRNKULA      If not production ledger will be VAT         
         JNE   GETRN058                                 not hours               
         ZAP   WCS_HRS,SCIAMNT                                                  
         MVI   WCS_TTYP,WCS_TNTI   mark as (billable?) time                     
         J     GETRN030                                                         
*                                                                               
GETRN058 ZAP   WCS_VAT,SCIAMNT                                                  
         J     GETRN030                                                         
*                                                                               
GETRN060 CLI   SCITYPE,SCITCRAT    memo hours and other odds and sods           
         JNE   GETRN064                                                         
         CLC   TRNKWORK,BILWC      scitcrat==scitcomm. 99s are comm             
         JE    GETRN062                                                         
         CLI   WCS_TYPE,WCS_FBAD   SKIP FOR ADV FLEXI                           
         JE    GETRN030                                                         
         LA    RF,TRNRFST                                                       
         MVI   WCS_TTYP,WCS_TBTI         mark as billable time                  
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   GETRN030            Skip if billable time                        
         ZAP   WCS_CHT,SCIAMNT     memo time cost counts as chargeable          
         ZAP   WCS_NBIL,SCIAMNT    memo amount total                            
         MVI   WCS_TTYP,WCS_TMTI                                                
         J     GETRN030                                                         
*                                                                               
GETRN062 ZAP   WCS_COM,SCIAMNT     Commission on bill transaction               
         ZAP   WCS_COB,SCIAMNT                                                  
         CLI   SCILN,SCILN1Q       Is it extended element                       
         JNH   GETRN030            No                                           
         ZAP   WCS_COMF,SCICURA    Yes - extract foreign commission             
         J     GETRN030                                                         
*                                                                               
GETRN064 CLI   SCITYPE,SCITMCRT    COST AMOUNT                                  
         JNE   GETRN066                                                         
         ZAP   WCS_TCR,SCIAMNT     TIME COST AMOUNT                             
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   GETRN030            Skip if billable time                        
         L     RF,AGOXBLCK                                                      
         CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
         JE    *+12                Yes                                          
         CLI   QS_ESTC,YESQ        Estimate checking?                           
         JNE   GETRN030                                                         
                                                                                
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JNE   GETRN030                                                         
                                                                                
         ZAP   WCS_DBA,SCIAMNT     Debit amount                                 
         ZAP   WCS_NBIL,SCIAMNT    Memo amount                                  
         J     GETRN030                                                         
*                                                                               
GETRN066 CLI   SCITYPE,SCITMSRT    SALES AMOUNT                                 
         JNE   GETRN068                                                         
         ZAP   WCS_TSR,SCIAMNT     TIME SALES AMOUNT                            
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   GETRN030            Skip if billable time                        
         L     RF,AGOXBLCK                                                      
         CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
         JE    *+12                Yes                                          
         CLI   QS_ESTC,YESQ        Estimate checking?                           
         JNE   GETRN030                                                         
                                                                                
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JE    GETRN030                                                         
                                                                                
         ZAP   WCS_DBA,SCIAMNT     Debit amount                                 
         ZAP   WCS_NBIL,SCIAMNT    Memo amount                                  
         J     GETRN030                                                         
*                                                                               
GETRN068 CLI   SCITYPE,SCITMEXP    MEMO EXPENSES (EXPENSE INVOICES)             
         JNE   GETRN074                                                         
*&&                                                                             
*&&US                                                                           
GETRN068 CLI   SCITYPE,SCITSJXP    Memo expenses (expense invoices)             
         JNE   GETRN030                                                         
*&&                                                                             
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   GETRN030                                                         
         MVI   WCS_TTYP,WCS_TMTR                                                
         L     RF,AGOXBLCK                                                      
         CLI   QS_ESTC,YESQ        Estimate checking?                           
         JNE   GETRN070                                                         
         CLI   GOEMIN-GOXBLOCK(RF),GONO  Do we include expense items            
         JNE   GETRN072                  Yes                                    
         J     GETRN002                                                         
*                                                                               
GETRN070 CLI   GODEIS-GOXBLOCK(RF),GODEIN Dashboard includes exp items          
         JE    GETRN002                   No ignore transaction                 
*                                                                               
         CLI   GODEIS-GOXBLOCK(RF),GODAMT amount?                               
         JNE   GETRN030                   no, skip element                      
*                                         yes, buffer it                        
*                                                                               
GETRN072 ZAP   WCS_DBA,SCIAMNT        Add expense amount to buffer              
         ZAP   WCS_NBIL,SCIAMNT       Add memo amount to buffer                 
         LA    RF,TRNRFST                                                       
         TM    TRNSTAT-TRNELD(RF),TRNSDR  Is it debit                           
         JNZ   GETRN030               Yes                                       
         ZAP   WCS_DBA,PZERO          No                                        
         ZAP   WCS_CRA,SCIAMNT                                                  
         J     GETRN030                                                         
*                                                                               
*&&UK                                                                           
GETRN074 TM    SCITYPE,X'40'       VAT TYPE?                                    
         JNZ   GETRN030                                                         
         CLI   SCITYPE,SCITCPAD    Skip new debtor SCIELDs                      
         JNH   GETRN030                                                         
         CLI   SCITYPE,SCITAVT0    Test VAT code a - z                          
         JL    GETRN076            Yes, include them                            
         CLI   SCITYPE,SCITAVT9    Test VAT code 0 to 9                         
         JH    GETRN030            No - skip                                    
*                                                                               
         LA    RF,TRNRFST                                                       
         TM    TRNSTAT-TRNELD(RF),TRNSDR  Is it debit                           
         JO    GETRN030                   Yes - not wc 99 billings              
         CLC   TRNKWORK,BILWC      B0 TO B9 are vat amounts on wc 99s           
         JNE   GETRN030                                                         
GETRN076 CLI   SCILN,SCILN1Q       If short scield skip the extended            
         JNH   GETRN077                                      fields             
         AP    WCS_VAT,SCIVBIL                                                  
         AP    WCS_BGRS,SCIVBIL                                                 
GETRN077 AP    WCS_BGRS,SCIGBIL                                                 
         J     GETRN030                                                         
*                                                                               
         USING GPXELD,R3                                                        
GETRN078 CLI   GPXTYP,GPXNVLQ      ONLY FOR DETAILS AND EXTENDED DETAIL         
         JNE   GETRN030                                                         
         MVC   WCS_ILOG,SPACES                                                  
         XR    RF,RF                                                            
         IC    RF,GPXLN                                                         
         SHI   RF,GPXLN1Q+1                                                     
         EX    RF,GETRN080                                                      
         J     GETRN030                                                         
*                                                                               
GETRN080 MVC   WCS_ILOG(0),GPXDATA INVOICE NUMBER                               
*                                                                               
*&&                                                                             
*                                                                               
         USING OTHELD,R3                                                        
GETRN082 MVC   WCS_IREF,OTHNUM     internal reference                           
         J     GETRN030                                                         
*                                                                               
         USING FFNELD,R3                                                        
GETRN084 MVC   WCS_ONUM,FFNONUM    set order number from matching               
         J     GETRN030                                                         
*                                                                               
         USING PIDELD,R3                                                        
GETRN086 MVC   WCS_PID,PIDNO       BILLER PID (SHOULD BE 99 ONLY)               
         J     GETRN030                                                         
*                                                                               
*                                                                               
         USING TIMELD,R3                                                        
GETRN088 CLI   TIMETYP,TIMEEST                                                  
         JNE   GETRN090                                                         
         MVC   WCS_ESTG,TIMSESNM                                                
         J     GETRN030                                                         
*                                                                               
GETRN090 CLI   TIMETYP,TIMEORDR                                                 
         JNE   GETRN030                                                         
         MVC   WCS_ONUM,TIMOORDR                                                
         J     GETRN030                                                         
*                                                                               
         USING TRXELD,R3                                                        
GETRN092 TM    TRXSTA1,TRXSRQRD                                                 
         JZ    *+8                                                              
         MVI   WCS_IQRD,YESQ                                                    
         TM    TRXSTA1,TRXSRHLD                                                 
         JZ    *+8                                                              
         MVI   WCS_IHLD,YESQ                                                    
         J     GETRN030                                                         
*                                                                               
         USING SERELD,R3                                                        
GETRN094 MVC   WCS_UQID,SERNM      Unique id for accent cashflow                
         J     GETRN030                                                         
*                                                                               
         USING DUEELD,R3                                                        
GETRN096 MVC   WCS_DUED,DUEDATE    Due date of bill                             
         J     GETRN030                                                         
*                                                                               
         USING SORELD,R3                                                        
GETRN098 CLI   SORSYS,SORSACC                                                   
         JNE   GETRN100                                                         
         CLC   PRODUL,SORAULA                                                   
         JE    *+14                                                             
         MVC   WCS_OULA,SORAULA                                                 
         J     GETRN030                                                         
         MVC   WCS_CLIC,SPACES                                                  
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         MVC   WCS_CLIC(0),SORAACT                                              
         EX    RE,*-6                                                           
         MVC   WCS_PROC,SPACES                                                  
         LLC   R1,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         LA    RE,SORAACT(R1)                                                   
         MVC   WCS_PROC(0),0(RE)                                                
         EX    RF,*-6                                                           
         MVC   WCS_JOBC,SPACES                                                  
         LLC   R1,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         SR    RF,R1                                                            
         CHI   RF,L'WCS_JOBC                                                    
         JNH   *+8                                                              
         LA    RF,L'WCS_JOBC                                                    
         SHI   RF,1                                                             
         LA    RE,SORAACT(R1)                                                   
         MVC   WCS_JOBC(0),0(RE)                                                
         EX    RF,*-6                                                           
         J     GETRN030                                                         
                                                                                
GETRN100 CLI   SORSYS,SORSMED                                                   
         JNE   GETRN030                                                         
         MVC   WCS_MEDC(L'SORMMED),SORMMED                                      
         MVC   WCS_CLIC(L'SORMCLI),SORMCLI                                      
         MVC   WCS_PROC(L'SORMPRO),SORMPRO                                      
         MVC   WCS_CAMC(L'SORMCAM),SORMCAM                                      
         J     GETRN030                                                         
*                                                                               
         USING CPJELD,R3                                                        
GETRN102 CLI   CPJTYPE,CPJTJOB                                                  
         JNE   GETRN104                                                         
         MVC   WCS_CLIC,CPJCLI     Client code                                  
         MVC   WCS_PROC,CPJPRO     Product code                                 
         MVC   WCS_JOBC,CPJJOB     Job code                                     
         J     GETRN030                                                         
*                                                                               
GETRN104 CLI   CPJTYPE,CPJTEXP                                                  
         JNE   *+16                                                             
         MVC   WCS_OACT,CPJEXP     Expense code                                 
         MVC   WCS_OULA(L'ACTKUNT+L'ACTKLDG),=C'SE'                             
         CLI   CPJTYPE,CPJTOTH                                                  
         JNE   GETRN030                                                         
         MVC   WCS_OULA,CPJOULA    Other unit ledger account                    
         J     GETRN030                                                         
*                                                                               
*  End of element loop (GETRN030)                                               
*                                                                               
GETRN106 LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         OC    WCS_EXPT,WCS_EXPT   Etype and no GINELD mean this tx             
         JZ    GETRN107             is from a fully approved expense            
         CLI   DS_GINEL,YESQ         claim                                      
         JE    GETRN107                                                         
*&&UK*&& CLC   WCS_ILOG,SPACES     If invoice log number must be inv            
*&&UK*&& JH    GETRN107                                                         
         CLI   CUCTRY,CTRYGER      if Germany - no expense claim users          
         JE    GETRN107                                                         
         MVI   WCS_TYPE,WCS_EXP    Set as expense claim                         
         MVC   WCS_SJAC,TRNKACT                                                 
         MVI   WCS_TTYP,WCS_TBEX     and assume billable expense                
         CP    TRNAMNT,PZERO       Check if memo expense as amount will         
         JNE   GETRN107             be zero                                     
         MVI   WCS_TTYP,WCS_TNEX   Set as non billable expense                  
                                                                                
GETRN107 CP    TRNAMNT,PZERO       If memo amounts have been set                
         JE    GETRN108             from reading element above                  
         ZAP   WCS_CRA,TRNAMNT                                                  
         TM    TRNSTAT,TRNSDR                                                   
         JZ    GETRN109                                                         
         ZAP   WCS_CRA,PZERO                                                    
         ZAP   WCS_DBA,TRNAMNT                                                  
         J     GETRN109                                                         
*                                                                               
GETRN108 CP    WCS_CURA,PZERO                                                   
         JE    GETRN109                                                         
         ZAP   WCS_CURC,WCS_CURA                                                
         TM    TRNSTAT,TRNSDR                                                   
         JZ    GETRN109                                                         
         ZAP   WCS_CURC,PZERO                                                   
         ZAP   WCS_CURD,WCS_CURA                                                
                                                                                
GETRN109 CLC   WCS_WCOD,BILWC                                                   
         JNE   GETRN110                                                         
         CLC   PRODUL,TRNKULA                                                   
         JNE   GETRN110                                                         
         ZAP   WCS_ALA,TRNAMNT                                                  
*&&UK                                                                           
         CLI   TRNLN,TRNLNBQ       BILLING NARRATIVE RECALCULATES               
         JNE   GETRN110                                                         
         ZAP   DUB1,TRNAMNT        Add up payables + commission                 
         AP    DUB1,TRNCOMM                                                     
         ZAP   WCS_COB,TRNCOMM                                                  
         ZAP   DUB2,TRNVATR1       Add up VAT                                   
         AP    DUB2,TRNVATR2                                                    
         AP    DUB2,TRNVATR3                                                    
         AP    DUB2,TRNVATR4                                                    
         AP    DUB2,TRNVATR5                                                    
         ZAP   WCS_VAT,DUB2        Set VAT                                      
         ZAP   WCS_BGRS,DUB2       Set gross billed amount                      
         AP    WCS_BGRS,DUB1                                                    
*&&                                                                             
*                                                                               
GETRN110 CLC   PRODUL,QS_UNIT                                                   
         JNE   GETRN150                                                         
         GOTOR SETALL                                                           
         JNE   EXITN                                                            
         GOTOR SETPRAT                                                          
*                                                                               
*   Summary values (where not already set)                                      
*                                                                               
         CLI   WCS_APST,WCS_ASFA   Only want commission on approved             
         JNE   GETRN112            clear commission if not                      
         TM    TRNSTAT,TRNSNOCM    Is the posting commissionable                
         JNZ   GETRN112            No                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOEXM-GOXBLOCK(RF),GOYES  Exclude commission                     
         JNE   GETRN118            No                                           
         CLI   WCS_TTYP,WCS_TNEX   Non billable expense                         
         JE    GETRN112            Yes                                          
         CLI   WCS_TTYP,WCS_TMTR   Memo transaction                             
         JNE   GETRN118            No                                           
GETRN112 ZAP   WCS_COM,PZERO       Yes - clear rate                             
         ZAP   WCS_COMR,PZERO             and commission value                  
         ZAP   WCS_NCOM,WCS_DBA    Add up non commissionable amount             
         MVI   WCS_ICOM,NOQ                                                     
GETRN118 DS    0H                                                               
*ETRN118 AP    WCS_GRSS,WCS_COM                                                 
*        AP    WCS_GRSS,WCS_CRA                                                 
*        AP    WCS_GRSS,WCS_DBA                                                 
**       AP    WCS_NTEX,WCS_CRA                                                 
**       AP    WCS_NTEX,WCS_DBA                                                 
**       AP    WCS_NTEX,WCS_WROF                                                
         ZAP   WCS_LBIL,WCS_DBA  Actuals                                        
         ZAP   WCS_CMTD,WCS_DBA                                                 
         ZAP   WCS_ACTH,WCS_DBA                                                 
         SP    WCS_LSPD,WCS_DBA                                                 
         SP    WCS_LSPH,WCS_DBA                                                 
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOXNBPE-GOXBLOCK(RF),GOYES                                       
         JNE   *+22                                                             
         AP    WCS_LSPH,WCS_NBIL   Remove non billable amounts                  
         SP    WCS_CMTD,WCS_NBIL                                                
         SP    WCS_ACTH,WCS_NBIL                                                
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    *+16              estimate or actuals                            
         ZAP   WCS_LBIL,PZERO     Estimate                                      
         SP    WCS_LBIL,WCS_COB    Remove commission billed                     
*&&                                                                             
*                                                                               
         L     RF,AGOXBLCK                                                      
         CLI   GOEXM-GOXBLOCK(RF),GOYES  Exclude memo and write off             
         JNE   GETRN119            No                                           
*        SP    WCS_GRSS,WCS_NBIL                                                
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JNE   *+10                  estimate or actuals                        
*&&                                                                             
         SP    WCS_LBIL,WCS_NBIL   Subtract memo and write off                  
*                                                                               
GETRN119 SP    WCS_LBIL,WCS_ALA    Remove if already billed                     
         CLI   WCS_TTYP,WCS_TMTI   If internal charge as time remove            
         JNE   *+10                from non billable column                     
         ZAP   WCS_NBIL,PZERO                                                   
         CLI   GOEXM-GOXBLOCK(RF),GOYES  Exclude memo and write off             
         JE    *+10                No                                           
         ZAP   WCS_WROF,PZERO                                                   
*                                                                               
         CLC   WCS_WCOD,BILWC      Billing TX specific                          
         JNE   GETRN120                                                         
*        ZAP   WCS_GRSS,PZERO                                                   
*        ZAP   WCS_COB,PZERO                                                    
*                                                                               
GETRN120 GOTOR PROBIL              process bill number(s)                       
*                                                                               
         CLC   WCS_WCOD,BILWC                                                   
         JNE   GETRN150                                                         
*                                  Billing view processing                      
GETRN125 GOTOR GETDDET                                                          
*                                                                               
GETRN150 CLC   WCS_WCOD,SPACES                                                  
         JH    *+6                                                              
         DC    H'0'                Missing workcode                             
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',0)    All T/X'S aaded here                 
         JE    GETRN160                                                         
*        TM    TSARERRS,TSEEOF     Test end of buffer                           
*        JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
                                                                                
GETRN160 MVC   IOKEY,SAVEKEY1      Restore read sequence for NXTREC             
         J     GETRN002                                                         
*                                                                               
         DROP  R3,R4,R2                                                         
         EJECT                                                                  
***********************************************************************         
* GET ORDER TRANSACTIONS                                              *         
***********************************************************************         
                                                                                
GETOTRN  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETOTX*'         *** ORDER TRXS ***                           
*                                                                               
         L     RF,AGOXBLCK                                                      
         CLI   QS_ESTC,YESQ        do we want to do estimate check?             
         JNE   *+12                                                             
         LA    RF,GODIOS-GOXBLKD(RF)                                            
         J     *+8                                                              
         LA    RF,GOEORN-GOXBLKD(RF)                                            
*                                                                               
         CLI   0(RF),C'N'          don't want any orders                        
         JE    EXITY                                                            
*                                                                               
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
         J     GETOTX10                                                         
*                                                                               
         USING TRNRECD,R2                                                       
GETOTX09 L     R2,IOKEY                                                         
         MVC   IOKEY,SAVEKEY1                                                   
GETOTX10 GOTOR (#NXTREC,ANXTREC),DMCB,OTRKEYT,('B#TRN',0),             +        
               (0,SAVED),AFLTRAND,0                                             
         JNE   EXITY               No more records                              
         XC    BYTE1,BYTE1                                                      
         MVI   LP_RMODE,LP_RNEXT                                                
         L     R2,IOADDR           A(Next T/X rec)                              
         MVC   SAVEKEY1,TRNKEY     save, as some subroutines break seq          
*                                                                               
         USING TRNELD,R3                                                        
         USING WCS_D,R4                                                         
         L     R2,IOADDR           Process order transaction                    
         LA    R3,TRNRFST                                                       
         LAY   R4,EX_BUFF                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   GETOTX09                                                         
*                                                                               
         GOTOR CLRJBV,WCS_KEY                                                   
         MVI   WCS_TYPE,WCS_TRN                                                 
         XR    R0,R0                                                            
*                                                                               
GETOTX15 MVI   WCS_TTYP,WCS_TORD                                                
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVI   WCS_APST,WCS_ASFA   if ord trx exists then fully appr            
         MVI   WCS_ICOM,YESQ                                                    
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVC   WCS_CTRA,TRNKULC                                                 
         MVC   WCS_DAT,TRNKDATE                                                 
*                                                                               
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,TRNKACT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,TRNKACT    Save account code                            
*                                                                               
         MVC   WCS_LREF(L'TRNREF),TRNREF                                        
         MVC   WCS_BTYP,TRNTYPE                                                 
         GOTOR GETNARR,(R2)                                                     
*                                                                               
         USING FFTELD,R3                                                        
GETOTX20 LA    R3,TRNRFST                                                       
         XR    R0,R0                                                            
                                                                                
GETOTX25 CLI   FFTEL,0                                                          
         JE    GETOTX38                                                         
         CLI   FFTEL,FFTELQ                                                     
         JE    GETOTX29                                                         
         CLI   FFTEL,AFCELQ                                                     
         JE    GETOTX35                                                         
         CLI   FFTEL,TRSELQ                                                     
         JE    GETOTX36                                                         
GETOTX26 IC    R0,FFTLN                                                         
         AR    R3,R0                                                            
         J     GETOTX25                                                         
*                                                                               
GETOTX29 CLI   FFTTYPE,FFTTESTN                                                 
         JNE   GETOTX32                                                         
         MVC   WCS_ESTG,FFTDATA                                                 
         J     GETOTX26                                                         
                                                                                
GETOTX32 CLI   FFTTYPE,FFTTEXTY                                                 
         JNE   GETOTX26                                                         
         MVC   WCS_EXPT,FFTDATA                                                 
         J     GETOTX26                                                         
*                                                                               
         USING AFCELD,R3                                                        
GETOTX35 MVC   WCS_CURR,AFCCURR    Foreign currrency code                       
         GOTOR VHEXOUT,DMCB,AFCX,WCS_CURX,L'AFCX                                
         J     GETOTX26                                                         
*                                                                               
         USING TRSELD,R3                                                        
GETOTX36 CLI   TRSLN,TRSLN2Q                                                    
         JL    GETOTX26                                                         
         MVC   WCS_GPST,TRSGSTAT                                                
         J     GETOTX26                                                         
*                                                                               
GETOTX38 LA    R3,TRNRFST          Scan for OAMELs to see whether it's          
         USING OAMELD,R3                                                        
*&&US                                                                           
         MVI   DS_ORST,WCS_NMCH     part matched                                
GETOTX40 CLI   OAMEL,0             End of record                                
         JE    GETOTX46                                                         
         CLI   OAMEL,OAMELQ                                                     
         JE    GETOTX44                                                         
GETOTX42 LLC   RF,OAMLN                                                         
         AR    R3,RF                                                            
         J     GETOTX40                                                         
                                                                                
GETOTX44 CP    OAMINUM,PZERO       Any matching?                                
         JE    GETOTX42                                                         
         MVI   DS_ORST,WCS_PMCH    Yes - set as part matched                    
                                                                                
GETOTX46 LA    R3,TRNRFST          Scan for OAMELs, one tsar per el             
*&&                                                                             
         CLI   OAMEL,OAMELQ        Starts with OAMEL?                           
         JNE   GETOTX80                                                         
GETOTX48 SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JE    *+14                                                             
         CP    OAMAMNT,OAMIVAL     Has full amount been invoiced?               
         JE    GETOTX80            Yes - Skip display                           
         SR    RF,RF                                                            
         CLC   QS_AWORK,AALL       any workcode filter?                         
         JE    GETOTX55                                                         
         ICM   RF,7,QS_AWORK       Point to WMP entry                           
         LA    RF,10(RF)                                                        
*                                                                               
GETOTX50 OC    0(L'OAMWORK,RF),0(RF)    scan wmp for match                      
         JZ    GETOTX80            No match, next element                       
         CLC   OAMWORK,0(RF)                                                    
         JE    GETOTX55                                                         
         LA    RF,L'OAMWORK(RF)                                                 
         J     GETOTX50                                                         
*                                                                               
GETOTX55 DS    0H                                                               
*&&UK                                                                           
         CLI   OAMLN,OAMLN1Q                                                    
         JNH   GETOTX60                                                         
         CLC   WCS_CURR,SPACES     Any currency                                 
         JNH   GETOTX60            No                                           
         CLC   WCS_CURR,AGYCURR    Any currency                                 
         JE    GETOTX60            No                                           
         ZAP   WCS_CURA,OAMFCAMT   Outstanding order amount (F/C)               
         SP    WCS_CURA,OAMFCIVL                                                
*&&                                                                             
GETOTX60 MVC   WCS_WCOD,OAMWORK    loop through order and add one               
         ZAP   WCS_ORA,OAMAMNT                                                  
         SP    WCS_ORA,OAMIVAL     Not invoiced                                 
         ZAP   WCS_CMTD,WCS_ORA    Committed is total of orders                 
         ZAP   WCS_ORAH,WCS_ORA                                                 
         ZAP   WCS_LSPD,PZERO                                                   
         SP    WCS_LSPD,WCS_ORA                                                 
         ZAP   WCS_LSPH,PZERO                                                   
         SP    WCS_LSPH,WCS_ORA                                                 
         ZAP   WCS_OIN,OAMIVAL     Invoiced amount                              
*&&US*&& MVC   WCS_ORST,DS_ORST                                                 
*&&UK                                                                           
         MVI   WCS_ORST,WCS_NMCH                                                
         CP    OAMINUM,PZERO       Any matching                                 
         JE    *+8                                                              
         MVI   WCS_ORST,WCS_PMCH   Yes - set as part matched                    
*&&                                                                             
         CLC   WCS_WCOD,SPACES     Is this an Extra WC OAMELD?                  
         JNH   GETOTX80            Yes - Skip it.                               
*                                                                               
         CLI   BYTE1,1             Have we processed billing                    
         JE    GETOTX70            Yes                                          
         GOTOR SETALL              No - work out allocation                     
         JNE   EXITN                and apply to first work code                
         GOTOR SETPRAT                                                          
         GOTOR PROBIL              process bill number(s)                       
         MVI   BYTE1,1                                                          
GETOTX70 DS    0H                                                               
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    GETOTX75          estimate or actuals                            
         ZAP   WCS_LBIL,PZERO     Estimate                                      
         SP    WCS_LBIL,WCS_COB    Remove commission billed                     
         SP    WCS_LBIL,WCS_ALA    Remove billed amount                         
*&&                                                                             
*                                                                               
GETOTX75 GOTOR GOTSAR,DMCB,('TSAADD',0)     record per oamel                    
         JE    GETOTX80                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
                                                                                
GETOTX80 LLC   R0,OAMLN                                                         
         AR    R3,R0                                                            
         CLI   OAMEL,0                                                          
         JE    GETOTX09            Next record                                  
         CLI   OAMEL,OAMELQ        assumes els in a single block                
         JNE   GETOTX80            next element                                 
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR           Ensure unique                                
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         J     GETOTX48                                                         
*                                                                               
         DROP  R3,R4,R2                                                         
         EJECT                                                                  
***********************************************************************         
* GET unapproved orders (requisitions)                               *          
*   And handles approved memo orders and fully matched for Aura                 
***********************************************************************         
                                                                                
GETUORD  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETUORD'         *** ORDER TRXS ***                           
*                                                                               
         USING WCS_D,R4                                                         
         LAY   R4,EX_BUFF                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
*                                                                               
         XC    DS_RPRE,DS_RPRE                                                  
         XC    DS_RSUF,DS_RSUF                                                  
         LA    RF,SCPYEL                                                        
         CLI   CPYLN,CPYLN4Q                                                    
         JL    GUORD004                                                         
         MVC   DS_RPRE,CPYREQPF                                                 
         MVC   DS_RSUF,CPYREQSF                                                 
         J     GUORD004                                                         
*                                                                               
         USING OSJPASD,R2                                                       
GUORD002 L     R2,IOKEY                                                         
         MVC   IOKEY,SAVEKEY1                                                   
GUORD004 GOTOR (#NXTREC,ANXTREC),DMCB,OSJKEYT,('B#ORD',0),             +        
               (0,SAVED),AFLTUORD,0                                             
         JNE   EXITY               NO MORE RECORDS                              
         MVI   LP_RMODE,LP_RNEXT                                                
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   SAVEKEY1,OSJPAS     save, as some subroutines break seq          
         MVC   IODA,OSJPDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
*                                                                               
         XC    DS_GPST,DS_GPST                                                  
         XC    DS_ORAU,DS_ORAU                                                  
         XC    DS_PRPID,DS_PRPID                                                
         XC    DS_ORST,DS_ORST                                                  
         XC    DS_EGLO,DS_EGLO                                                  
         XC    DS_FFTWC,DS_FFTWC                                                
         XC    DS_ECUR,DS_ECUR                                                  
         XC    DS_EXRT,DS_EXRT                                                  
         XC    DS_OREQN,DS_OREQN                                                
         XC    DS_ACCT,DS_ACCT                                                  
         XC    DS_EXULA,DS_EXULA                                                
         ZAP   DS_TOIN,PZERO                                                    
         ZAP   DS_TORA,PZERO                                                    
         ZAP   DS_FORA,PZERO                                                    
         ZAP   DS_TAMT,PZERO                                                    
         XC    DS_ORDN,DS_ORDN                                                  
*                                                                               
         USING ORDRECD,R2                                                       
         L     R2,AIO3                                                          
         MVI   DS_APST,WCS_ASDL                                                 
         TM    ORDRSTAT,ORDSLDEL                                                
         JNZ   GUORD006                                                         
         MVI   DS_APST,WCS_ASPA                                                 
         TM    ORDRSTA2,ORDSPAPP                                                
         JNZ   GUORD006                                                         
         MVI   DS_APST,WCS_ASSB                                                 
         TM    ORDRSTA2,ORDSSUBM                                                
         JNZ   GUORD006                                                         
         MVI   DS_APST,WCS_ASFA                                                 
         TM    ORDRSTA2,ORDSAPPR                                                
         JNZ   GUORD006                                                         
         MVI   DS_APST,WCS_ASRJ                                                 
         TM    ORDRSTA2,ORDSOREJ                                                
         JNZ   GUORD006                                                         
         MVI   DS_APST,WCS_ASIP                                                 
         TM    ORDRSTA2,ORDSDRFT   Return draft as 'in progress'                
         JNZ   GUORD006                                                         
         TM    ORDRSTA2,ORDSEXEX    otherwise assume legacy system              
         JNZ   GUORD006              and is fully approved                      
         MVI   DS_APST,WCS_ASFA                                                 
         USING ORDELD,R3                                                        
GUORD006 LA    R3,ORDRFST          Read basic order information                 
         XR    R0,R0                                                            
         MVI   DS_ORIN,NOQ                                                      
*                                                                               
GUORD008 CLI   ORDEL,0                                                          
         JE    GUORD056                                                         
         CLI   ORDEL,ORDELQ                                                     
         JE    GUORD012                                                         
*&&US                                                                           
         CLI   ORDEL,OAMELQ                                                     
         JE    GUORD015                                                         
*&&                                                                             
         CLI   ORDEL,SORELQ                                                     
         JE    GUORD016                                                         
         CLI   ORDEL,FFTELQ                                                     
         JE    GUORD022                                                         
         CLI   ORDEL,ENMELQ                                                     
         JE    GUORD018                                                         
         CLI   ORDEL,AFCELQ                                                     
         JE    GUORD036                                                         
         CLI   ORDEL,SCMELQ                                                     
         JE    GUORD038                                                         
*                                                                               
GUORD010 IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     GUORD008                                                         
*                                                                               
GUORD012 MVC   DS_OCTRA,ORDSUPU    contra account = supplier                    
         MVC   DS_ODAT,ORDDATE                                                  
         MVC   DS_ORAU,ORDAUTH     order authoriser                             
         MVI   DS_ORST,WCS_NMCH                                                 
         TM    ORDSTAT,ORDSPART+ORDSMNUP   Pending fully matched                
         JZ    *+8                           treated as part matched            
         MVI   DS_ORST,WCS_PMCH    Yes - set as part matched                    
         TM    ORDRSTAT,ORDSFMCH   Check status as coule be                     
         JZ    GUORD013            closed or cancelled orders                   
         MVI   DS_ORST,WCS_FMCH                                                 
         TM    ORDSTAT,ORDSMNUP+ORDSPART                                        
         JNZ   GUORD013                                                         
         MVI   DS_ORST,WCS_CLLD                                                 
                                                                                
GUORD013 DS    0H                                                               
*&&UK*&& CLI   ORDLN,ORDLN2Q                                                    
*&&US*&& CLI   ORDLN,ORDLN3Q                                                    
         JL    *+16                                                             
         MVC   DS_PRPID,ORDCPID    creator pid                                  
         MVC   DS_GPST,ORDGSTAT    GAP STATUS                                   
                                                                                
         CLC   ORDACCU(L'ACTKUNT+L'ACTKLDG),=C'SJ'                              
         JNE   GUORD014                                                         
         MVC   DS_ACCT,ORDACCA     Save job account                             
         MVI   DS_TYPE,C'P'                                                     
         J     GUORD010                                                         
GUORD014 MVC   DS_EXULA,ORDACCU    Save expense account                         
         MVI   DS_TYPE,C'E'                                                     
         J     GUORD010                                                         
*                                                                               
*&&US                                                                           
         USING OAMELD,R3                                                        
GUORD015 CP    OAMIVAL,PZERO       Is there any invoicing                       
         JE    *+8                 no                                           
         MVI   DS_ORIN,YESQ        Yes                                          
         CLI   DS_ORST,WCS_FMCH    Does the order amount element                
         JNE   GUORD010             have pending invoices if so                 
         OC    OAMIPND,OAMIPND       can't be fully matched                     
         JZ    GUORD010                                                         
         MVI   DS_ORST,WCS_PMCH     so set as partly invoiced                   
         J     GUORD010                                                         
*&&                                                                             
*                                                                               
         USING SORELD,R3                                                        
GUORD016 CLI   SORSYS,SORSACC                                                   
         JNE   GUORD010                                                         
         MVC   DS_ACCT,SORAACT                                                  
         J     GUORD010                                                         
                                                                                
         USING ENMELD,R3                                                        
GUORD018 LLC   R1,ENMLN                                                         
         SHI   R1,ENMLNQ                                                        
         CHI   R1,0                                                             
         JNH   GUORD010                                                         
         CHI   R1,L'DS_ORDN                                                     
         JNH   GUORD020                                                         
         LHI   R1,L'DS_ORDN                                                     
                                                                                
GUORD020 SHI   R1,1                                                             
         MVC   DS_ORDN(0),ENMNAME                                               
         EX    R1,*-6                                                           
         J     GUORD010                                                         
                                                                                
         USING FFTELD,R3                                                        
GUORD022 CLI   FFTTYPE,FFTTESTN                                                 
         JNE   GUORD024                                                         
         MVC   DS_EGLO,FFTOESTN                                                 
         J     GUORD010                                                         
                                                                                
GUORD024 CLI   FFTTYPE,FFTTEXTY                                                 
         JNE   GUORD026                                                         
         MVC   DS_EXPT,FFTDATA                                                  
         J     GUORD010                                                         
                                                                                
GUORD026 CLI   FFTTYPE,FFTTORNO                                                 
         JNE   GUORD030                                                         
                                                                                
         LA    RE,DS_OREQN         set prefix and suffix if given               
         CLI   DS_RPRE,C' '                                                     
         JNH   GUORD028                                                         
         MVC   0(1,RE),DS_RPRE                                                  
         AHI   RE,1                                                             
                                                                                
GUORD028 MVC   0(6,RE),FFTDATA                                                  
         AHI   RE,6                                                             
         CLI   DS_RSUF,C' '                                                     
         JNH   GUORD010                                                         
         MVC   0(1,RE),DS_RSUF                                                  
         J     GUORD010                                                         
*                                                                               
GUORD030 CLI   FFTTYPE,FFTTWRKC    match on memo work code                      
         JNE   GUORD010                 IRRELEVANT, NEXT ELE                    
         SR    RF,RF                                                            
         CLC   QS_AWORK,AALL       any workcode filter?                         
         JE    GUORD034                                                         
         USING LW_D,RF                                                          
         ICM   RF,7,QS_AWORK         SCAN WMP ENTRIES                           
         XR    R1,R1                                                            
         ICM   R1,3,LW_NUMN                                                     
         LA    R6,LW_DATA2                                                      
*                                                                               
GUORD032 OC    0(L'OAMWORK,R6),0(R6)    END OF WMP?                             
         JZ    GUORD010            NO MATCH, NEXT ELEMENT                       
         CLC   FFTWORK,0(R6)                                                    
         JE    GUORD034                MATCH                                    
         LA    R6,L'OAMWORK(R6)                                                 
         JCT   R1,GUORD032                                                      
         J     GUORD010                                                         
         DROP  RF                                                               
*                                                                               
GUORD034 MVC   DS_FFTWC,FFTWORK                                                 
         J     GUORD010                                                         
*                                                                               
         USING AFCELD,R3                                                        
GUORD036 MVC   DS_ECUR,AFCCURR     Foreign currrency                            
         ZAP   DS_TAMT,AFCAMNT                                                  
         MVC   DS_EXRT,AFCX                                                     
         J     GUORD010                                                         
*                                                                               
         USING SCMELD,R3                                                        
GUORD038 CLI   SCMTYPE,SCMTOMOC    Matching description                         
         JNE   GUORD040                                                         
         LLC   RF,SCMLN                                                         
         SHI   RF,SCMLN1Q+1                                                     
         STC   RF,DS_MDESL                                                      
         BASR  RE,0                                                             
         MVC   DS_MDESC(0),SCMNARR                                              
         EX    RF,0(RE)                                                         
*                                                                               
GUORD040 CLI   SCMTYPE,SCMTSTND    Printed description                          
         JNE   GUORD010                                                         
         LLC   RF,SCMLN                                                         
         SHI   RF,SCMLN1Q+1                                                     
         STC   RF,DS_PDESL                                                      
         BASR  RE,0                                                             
         MVC   DS_PDESC(0),SCMNARR                                              
         EX    RF,0(RE)                                                         
         J     GUORD010                                                         
*                                                                               
*                                                                               
* NOW READ OAMELS FOR COSTS, ADD ONE TSAR PER ELE                               
         USING OAMELD,R3                                                        
GUORD056 LA    R3,ORDRFST                                                       
*&&US                                                                           
         CLI   DS_ORST,WCS_PMCH    Check whether any real matching has          
         JNE   GUORD058             taken place                                 
         CLI   DS_ORIN,YESQ                                                     
         JE    GUORD058                                                         
         MVI   DS_ORST,WCS_NMCH    No then set no matching                      
*&&                                                                             
GUORD058 LLC   R0,OAMLN            FIND OAMELS                                  
         AR    R3,R0                                                            
         CLI   OAMEL,0                                                          
         JE    GUORD080                                                         
         CLI   OAMEL,OAMELQ                                                     
         JNE   GUORD058                                                         
         ZAP   DS_ORA,OAMAMNT      Extract order amount                         
         SP    DS_ORA,OAMIVAL                                                   
         ZAP   DS_OIN,OAMIVAL      Extract invoice amount                       
         AP    DS_TORA,OAMAMNT     Total order amount per order                 
         AP    DS_TOIN,OAMIVAL     Total invoiced amount per order              
*&&UK                                                                           
         CLI   OAMLN,OAMLN1Q                                                    
         JNH   GUORD060                                                         
         CLC   DS_ECUR,SPACES      Any currency                                 
         JNH   GUORD060            No                                           
         CLC   DS_ECUR,AGYCURR     Any currency                                 
         JE    GUORD060            No                                           
         ZAP   DS_FORA,OAMFCAMT    Outstanding order amount (F/C)               
         SP    DS_FORA,OAMFCIVL                                                 
*&&                                                                             
GUORD060 MVC   FULL1(L'OAMWORK),DS_FFTWC                                        
         CLC   DS_FFTWC,SPACES                                                  
         JH    GUORD062                                                         
         MVC   FULL1(L'OAMWORK),OAMWORK    PUT WC IN ONE PLACE                  
         CLC   OAMWORK,SPACES                                                   
         JNH   GUORD058            ignore non-workcode data                     
*                                                                               
GUORD062 CLI   IOKEY+OSJPMEM-OSJPASD,0   skip for memo                          
         JNE   GUORD066                                                         
         SR    RF,RF                                                            
         USING LW_D,RF                                                          
         CLC   QS_AWORK,AALL       any workcode filter?                         
         JE    GUORD066                                                         
         ICM   RF,7,QS_AWORK       RF=A(Work code WMP)                          
         XR    R1,R1                                                            
         ICM   R1,3,LW_NUMN        R1=number of entries                         
         LA    R6,LW_DATA2                                                      
*                                                                               
GUORD064 CLC   FULL1(L'OAMWORK),0(R6)     Have we got a match                   
         JE    GUORD066            Yes                                          
         LA    R6,L'OAMWORK(R6)    Scan rest of WMP for work codes              
         JCT   R1,GUORD064                                                      
         J     GUORD058            Finished list - get next element             
         DROP  RF                                                               
*                                                                               
* ADD RECORD FOR EACH WORKCODE (OAMEL)                                          
GUORD066 GOTOR FLTORDS             Filter orders                                
         JNE   GUORD058                                                         
         GOTOR CLRJBV,WCS_KEY      Build return values                          
         MVC   WCS_WCOD,FULL1                                                   
         CLC   WCS_WCOD,SPACES                                                  
         JH    *+10                                                             
         MVC   WCS_WCOD,DS_FFTWC   Memo work code                               
         CLC   WCS_WCOD,SPACES     Do we have a work code                       
         JNH   GUORD002            No - not interested                          
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,DS_ACCT    Save account code                            
*                                                                               
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVI   WCS_TYPE,WCS_UORD                                                
         MVC   WCS_LREF(L'ORDKORD),ORDKORD                                      
         MVC   WCS_CTRA,DS_OCTRA                                                
         MVC   WCS_DAT,DS_ODAT                                                  
         MVC   WCS_ORAU,DS_ORAU                                                 
         MVC   WCS_OTYP,DS_TYPE                                                 
         MVC   WCS_APST,DS_APST                                                 
         MVC   WCS_PID,DS_PRPID                                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   GUORD068            Brandocean                                   
         TM    IOKEY+OSJPSTA2-OSJPASD,ORDSAPPR For Aura don't overwrite         
         JNZ   GUORD070              ref with requisition number if             
*                                     approved                                  
GUORD068 OC    DS_OREQN,DS_OREQN   Do we have a requisition number              
         JZ    GUORD070            No                                           
         MVC   WCS_LREF(L'DS_OREQN),DS_OREQN  Yes use this for ref              
*                                                                               
GUORD070 TM    IOKEY+OSJPSTA2-OSJPASD,ORDSPAPP+ORDSSUBM+ORDSDRFT                
         JNZ   GUORD072                                                         
         MVC   WCS_ONUM,ORDKORD                                                 
*                                                                               
GUORD072 MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         MVI   WCS_ICOM,NOQ                                                     
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVC   WCS_ESTG,DS_EGLO                                                 
         MVC   WCS_EXPT,DS_EXPT                                                 
         MVC   WCS_CURR,DS_ECUR                                                 
         ZAP   WCS_CURA,DS_FORA                                                 
         GOTOR VHEXOUT,DMCB,DS_EXRT,WCS_CURX,L'DS_EXRT                          
         MVC   WCS_GPST,DS_GPST                                                 
*                                                                               
GUORD074 OC    DS_OREQN,DS_OREQN          REQUISITION?                          
         JZ    GUORD076                                                         
*        MVI   WCS_TTYP,WCS_TMOR                                                
*        TM    IOKEY+OSJPSTA2-OSJPASD,ORDSPAPP+ORDSSUBM+ORDSDRFT                
*        JZ    GUORD078                   APPROVED MEMO ORDER                   
         MVI   WCS_TTYP,WCS_TREQ                                                
         CLI   IOKEY+OSJPMEM-OSJPASD,0                                          
         JE    GUORD078                                                         
         MVI   WCS_TTYP,WCS_TMRQ                                                
         J     GUORD077                                                         
*                                                                               
GUORD076 MVI   WCS_TTYP,WCS_TORD                                                
         CLI   IOKEY+OSJPMEM-OSJPASD,0                                          
         JE    GUORD078                                                         
         MVI   WCS_TTYP,WCS_TMOR                                                
*                                                                               
GUORD077 ZAP   WCS_NBIL,DS_ORA     memo is non billable                         
*                                                                               
GUORD078 ZAP   WCS_ORA,DS_ORA                                                   
         ZAP   WCS_OIN,DS_OIN                                                   
         ZAP   WCS_CMTD,WCS_ORA    committed, orders and costs                  
         ZAP   WCS_ORAH,WCS_ORA                                                 
         SP    WCS_LSPD,WCS_ORA                                                 
         SP    WCS_LSPH,WCS_ORA                                                 
         MVC   WCS_ORST,DS_ORST                                                 
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOXNBPE-GOXBLOCK(RF),GOYES  Exclude non billable from            
         JNE   *+22                          header                             
         AP    WCS_LSPH,WCS_NBIL   Remove non billable amounts                  
         SP    WCS_CMTD,WCS_NBIL                                                
         SP    WCS_ORAH,WCS_NBIL                                                
         GOTOR GETDESC,(R2)                                                     
*&&                                                                             
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GUORD058                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
*                                                                               
GUORD080 SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   GUORD002            Brandocean                                   
         CLI   QS_ORDL,YESQ        Do we want an order list                     
         JNE   GUORD002            No                                           
         GOTOR CLRJBV,WCS_KEY      Build return values                          
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,DS_ACCT    Set SJ account                               
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVI   WCS_TYPE,WCS_ORDL                                                
         MVC   WCS_LREF(L'ORDKORD),ORDKORD                                      
         MVC   WCS_CTRA,DS_OCTRA                                                
         MVC   WCS_DAT,DS_ODAT                                                  
         MVC   WCS_ORAU,DS_ORAU                                                 
         MVC   WCS_OULA,DS_EXULA                                                
         MVC   WCS_ORDN,DS_ORDN                                                 
         MVC   WCS_PID,DS_PRPID                                                 
         TM    IOKEY+OSJPSTA2-OSJPASD,ORDSAPPR For Aura don't overwrite         
         JNZ   GUORD082              ref with requisition number if             
*                                     approved                                  
         OC    DS_OREQN,DS_OREQN   Do we have a requisition number              
         JZ    *+10                No                                           
         MVC   WCS_LREF(L'DS_OREQN),DS_OREQN  Yes use this for ref              
GUORD082 MVC   WCS_ONUM,ORDKORD                                                 
         MVI   WCS_ICOM,NOQ                                                     
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVI   WCS_GDRC,NOQ                                                     
         TM    IOKEY+OSJPSTAT-OSJPASD,ORDGDRCV                                  
         JZ    *+8                                                              
         MVI   WCS_GDRC,YESQ       Set goods received                           
         MVC   WCS_OTYP,DS_TYPE                                                 
         MVC   WCS_ESTG,DS_EGLO                                                 
         MVC   WCS_EXPT,DS_EXPT                                                 
         MVC   WCS_CURR,DS_ECUR                                                 
         ZAP   WCS_CURA,DS_TAMT                                                 
         GOTOR VHEXOUT,DMCB,DS_EXRT,WCS_CURX,L'DS_EXRT                          
         MVC   WCS_GPST,DS_GPST                                                 
         ZAP   WCS_ORA,DS_TORA                                                  
         ZAP   WCS_OIN,DS_TOIN                                                  
         MVC   WCS_ORST,DS_ORST                                                 
         MVC   WCS_APST,DS_APST                                                 
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GUORD002                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET EXPENSE CLAIM ITEMS                                             *         
***********************************************************************         
GETEXP   NTR1  BASE=*,LABEL=NO     *** EXPENSE CLAIM ITEMS ***                  
         J     *+12                                                             
         DC    C'*GETEXP*'                                                      
*                                                                               
         L     RF,AGOXBLCK                                                      
         USING GOXBLKD,RF          do we want any expenses?                     
         LA    RE,GODIEB           billable?                                    
         CLI   QS_ESTC,YESQ                                                     
         JNE   *+8                                                              
         LA    RE,GOEXPB                                                        
         CLI   0(RE),C'N'                                                       
         JNE   GETEXP01            yes - go read all                            
*                                                                               
         LA    RE,GODIEN           non-billable?                                
         CLI   QS_ESTC,YESQ                                                     
         JNE   *+8                                                              
         LA    RE,GOEXPN                                                        
         CLI   0(RE),C'N'                                                       
         JE    EXITY               NO, neither - exit                           
*                                                                               
         USING WCS_D,R4                                                         
GETEXP01 LAY   R4,EX_BUFF                                                       
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
*                                                                               
         USING LW_D,R2                                                          
         XR    R2,R2               build exp types in wmp                       
         ICM   R2,7,DS_EXTYA                                                    
         JZ    GETEXP05            already cleared                              
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear area                                   
         LHI   R1,DS_EXTYM*L'EXJPVIEW                                           
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
GETEXP05 LA    R6,EXPTYTAB         Add new                                      
         LA    R3,EXPTTB#                                                       
GETEXP06 GOTOR LP_AAWMP,DMCB,(L'EXJPVIEW,(R6)),DS_EXTYI,DS_EXTYM,LP_D           
         DROP  R2                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R6,1(R6)                                                         
         JCT   R3,GETEXP06                                                      
         J     GETEXP11                                                         
*                                                                               
GETEXP10 MVC   IOKEY,SAVEKEY1                                                   
         L     R5,ALP                                                           
GETEXP11 GOTOR (#NXTREC,ANXTREC),DMCB,EXJKEYT,('B#EXC',0),(0,SAVED),   +        
               AFLTEXPD,0                                                       
         JNE   EXITY               NO MORE RECORDS                              
         MVI   LP_RMODE,LP_RNEXT                                                
*                                                                               
         USING EXJPASD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   SAVEKEY1,EXJPAS     save, as some subroutines break seq          
         MVC   DS_ACCT,EXJPCPJ     save account for element filter              
         MVC   IODA,EXJPDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR CLRJBV,WCS_KEY      we want this cluster                         
*                                                                               
         USING EXCRECD,R2                                                       
         L     R2,AIO3                                                          
         USING CIDELD,R3                                                        
         LA    R3,EXCRFST                                                       
         J     GETEXP21                                                         
                                                                                
GETEXP19 LLC   R0,CIDLN            find appropriate CIDEL cluster(s)            
         AR    R3,R0                                                            
                                                                                
GETEXP21 CLI   CIDEL,0             LOOP FOR MAIN CIDELS (R3)                    
         JE    GETEXP10              EOR - GET NEXT REC                         
         CLI   CIDEL,CIDELQ                                                     
         JNE   GETEXP19                                                         
         CLI   CIDTYPE,CIDTYMQ                                                  
         JNE   GETEXP19                                                         
                                                                                
         CLC   CIDMWCD,SPACES      Have we got a workcode                       
         JNH   GETEXP19                                                         
         CLI   CIDMTYP,C'N'        Non-billable item?                           
         JNE   *+10                                                             
         ZAP   WCS_NBIL,CIDMAMT    Total up                                     
*                                                                               
         SR    RF,RF                                                            
         CLC   QS_AWORK,AALL       any workcode filter?                         
         JE    GETEXP24                                                         
         ICM   RF,7,QS_AWORK         POINT TO WMP ENTRY                         
         LA    RF,10(RF)                                                        
*                                                                               
GETEXP23 OC    0(L'OAMWORK,RF),0(RF)    scan wmp for match                      
         JZ    GETEXP19            NO MATCH, NEXT ELEMENT                       
         CLC   CIDMWCD,0(RF)                                                    
         JE    GETEXP24                                                         
         LA    RF,L'OAMWORK(RF)                                                 
         J     GETEXP23                                                         
*                                                                               
GETEXP24 GOTOR FLTEXL,DMCB,CIDEL,EXCRECD filter CIDEL (GODIEL/GODIEB)           
         JNE   GETEXP19                                                         
                                                                                
CUR      USING CIDELD,R5                                                        
         LA    R5,CIDELD           filter CIDEL and its cluster                 
GETEXP25 LLC   R0,CUR.CIDLN        LOOP OVER THE CLUSTER                        
         AR    R5,R0                         R5/CUR.                            
         CLI   CUR.CIDEL,CIDELQ                                                 
         JNE   GETEXP19                                                         
         CLC   CUR.CIDSEQ,CIDSEQ                                                
         JNE   GETEXP19                                                         
         CLI   CUR.CIDTYPE,CIDTYCQ       FIND ACCOUNT EL                        
         JNE   GETEXP25                                                         
*                                                                               
         CLC   CUR.CIDCCPJ,DS_ACCT                                              
         JNE   GETEXP19            look for this job's items                    
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,DS_ACCT                                                 
*                                                                               
         MVC   WCS_WCOD,CIDMWCD    build return values                          
         MVC   WCS_CTRA,CUR.CIDCSUP                                             
         MVI   WCS_TYPE,WCS_EXP                                                 
         MVI   WCS_TTYP,WCS_TBEX                                                
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVI   WCS_ICOM,NOQ                                                     
         CLI   CIDMTYP,C'B'                                                     
         JE    *+8                                                              
         MVI   WCS_TTYP,WCS_TNEX                                                
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_LREF(L'CIDMREF),CIDMREF                                      
         GOTO1 VDATCON,DMCB,(2,CIDMDAT),(1,WCS_DAT)                             
*                                                                               
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
*                                                                               
         MVI   WCS_APST,WCS_ASPA   set trx status from expense record           
         TM    EXCRSTAT,EXCSPAPP+EXCSFNTA                                       
         JNZ   GETEXP32                                                         
         MVI   WCS_APST,WCS_ASSB                                                
         TM    EXCRSTAT,EXCSSUBM                                                
         JNZ   GETEXP32                                                         
         MVI   WCS_APST,WCS_ASRJ                                                
         TM    EXCRSTAT,EXCSREJE                                                
         JNZ   GETEXP32                                                         
         MVI   WCS_APST,WCS_ASIP                                                
*                                                                               
GETEXP32 MVC   WCS_EXPT,CIDMEXP                                                 
         ZAP   WCS_DBA,CIDMAMT     Debit amount                                 
*        ZAP   WCS_NTEX,WCS_DBA                                                 
         ZAP   WCS_NCOM,WCS_DBA    Add up non commissionable amount             
         GOTOR SETALL              To get VAT rate and commission               
         JNE   EXITN                                                            
*        ZAP   WCS_GRSS,CIDMAMT    Gross amount                                 
         ZAP   WCS_LBIL,WCS_DBA                                                 
         ZAP   WCS_CMTD,WCS_DBA    committed, orders and costs                  
         ZAP   WCS_ACTH,WCS_DBA                                                 
         SP    WCS_LSPD,WCS_DBA                                                 
         SP    WCS_LSPH,WCS_DBA                                                 
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOXNBPE-GOXBLOCK(RF),GOYES                                       
         JNE   *+22                                                             
         AP    WCS_LSPH,WCS_NBIL   Remove non billable amounts                  
         SP    WCS_CMTD,WCS_NBIL                                                
         SP    WCS_ACTH,WCS_NBIL                                                
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    *+16                  estimate or actuals                        
         ZAP   WCS_LBIL,PZERO     Estimate                                      
         SP    WCS_LBIL,WCS_COB    Remove commission billed                     
*&&                                                                             
         CLI   CIDMTYP,C'N'        Non billable                                 
         JNE   GETEXP38            No                                           
         L     RF,AGOXBLCK         Check whether we want to include             
         CLI   GOEXM-GOXBLOCK(RF),GOYES  Exclude memo                           
         JNE   GETEXP38            No                                           
*        ZAP   WCS_GRSS,PZERO      Yes - clear gross                            
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JNE   *+10                  estimate or actuals                        
*&&                                                                             
         SP    WCS_LBIL,WCS_NBIL   Remove memo charges                          
                                                                                
GETEXP38 MVC   WCS_IREF(L'CIDMINT),CIDMINT                                      
         CLC   CIDMCUR,AGYCURR                                                  
         JE    GETEXP40                                                         
         MVC   WCS_CURR,CIDMCUR                                                 
         ZAP   WCS_CURA,CIDMFCA                                                 
*                                                                               
         CLI   CUR.CIDLN,CIDCLNQ                                                
         JNH   GETEXP40                                                         
         MVC   WCS_ESTG,CUR.CIDCEGN                                             
*                                                                               
GETEXP40 LLC   R0,CUR.CIDLN              look for narr cidel                    
         AR    R5,R0                                                            
         CLI   CUR.CIDEL,0                                                      
         JE    GETEXP50                  none                                   
         CLI   CUR.CIDEL,CIDELQ                                                 
         JNE   GETEXP50                                                         
         CLC   CUR.CIDSEQ,CIDSEQ                                                
         JNE   GETEXP50                                                         
         CLI   CUR.CIDTYPE,CIDTYOQ                                              
         JNE   GETEXP40                                                         
         CLI   CUR.CIDLN,CIDDATA-CIDELD                                         
         JNH   GETEXP40                                                         
         XR    RE,RE                                                            
         IC    RE,CUR.CIDLN                                                     
         SHI   RE,CIDDATA-CIDELD                                                
* put this in if you're sick of extended narrs mcuking up your screen           
*     ideally FALINK is smart enough, of course...                              
*        TM    LP_FLAG,LP_FOFFL                                                 
*        JNZ   GETEXP45                                                         
*        CHI   RE,70                                                            
*        JNH   GETEXP45                                                         
*        LA    RE,70               ONLINE NEEDS TO TRUNCATE                     
*        MVC   WCS_ELED+70(3),=C'...'                                           
*                                                                               
GETEXP45 DS    0H                                                               
         SHI   RE,1                                                             
         MVC   WCS_ELED(0),CUR.CIDNARR                                          
         EX    RE,*-6                                                           
         MVI   WCS_ELET,WCS_ELRQ                                                
         LA    RE,3(RE)            LENGTH OF WCS SUBEL                          
         STC   RE,WCS_ELEL                                                      
         LA    RF,WCS_TERM                                                      
         AR    RE,RF                                                            
         MVI   0(RE),0             NEW TERMIN                                   
         LA    RE,1(RE)                                                         
         LR    RF,R4               A(EX_BUFF)                                   
         SR    RE,RF               NEW LEN WCS REC                              
         STH   RE,WCS_LEN                                                       
         LA    RF,L'EX_BUFF                                                     
         CR    RF,RE                                                            
         JH    GETEXP50                                                         
         DC    H'0'                                                             
*                                                                               
*                                                                               
GETEXP50 DS    0H                  ADD TO TSAR                                  
         CLC   WCS_WCOD,SPACES                                                  
         JH    *+6                                                              
         DC    H'0'                MISSING WORKCODE                             
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GETEXP19            NEXT CLUSTER                                 
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                PROBABLY DUPLICATE KEY                       
         MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
*                                                                               
         DROP  R2,R3,CUR,RF                                                     
***********************************************************************         
* GET ESTIMATE RECORDS                                                *         
***********************************************************************         
GETEST   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETEST*'         *** ESTIMATES ***                            
         USING ESTRECD,R2                                                       
*                                                                               
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
         NI    DS_IND1,X'FF'-DS_IEST                                            
         XC    BYTE2,BYTE2                                                      
         XC    IOKEY,IOKEY                                                      
         XC    SAVEKEY1,SAVEKEY1                                                
         MVI   DS_ESTHS,0                                                       
         OC    DS_ESTHI,DS_ESTHI   Highest REV from GETOEST?                    
         JNZ   GEST004                                                          
         MVI   DS_ESTHI,0                                                       
         J     GEST004                                                          
*                                                                               
GEST002  LA    R2,IOKEY                                                         
         MVC   ESTKEY,SAVEKEY1                                                  
GEST004  GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#EST',0),             +        
               (0,SAVED),0,0                                                    
*                                                                               
* For UK CE depends on NAE setting:                                             
* If NAE=N: sum of estimates on job with submitted+approved status,             
* If NAE=Y: sum of estimates on job with approved status.                       
*                                                                               
*&&UK*&& JNE   GEST066                                                          
*&&US                                                                           
* US have only one 'current' estimate and some number of revisions              
*    we need to find the current estimate and save its number, so the           
* summary can ignore the others.                                                
*                                                                               
* the current estimate is the highest approved estimate, if any exist,          
* else it is the highest submitted estimate. Note this is different             
* from Prod/Presto estimates (GETOEST), where GONEEDAE is checked.              
*                                                                               
* derive current estimate on exit                                               
*        this code immediately follows the NXTREC call                          
         JE    GEST008                                                          
         GOTOR PROEST                                                           
         J     GEST066                                                          
*&&                                                                             
GEST008  L     R2,IOADDR                                                        
         MVC   DS_SCLI(L'ESTKCLI),ESTKCLI                                       
         MVC   DS_SPRO(L'ESTKPRO),ESTKPRO                                       
         MVC   DS_SJOB(L'ESTKJOB),ESTKJOB                                       
         GOTOR BLDSCPJ                                                          
         USING EMDELD,R3                                                        
         USING WCS_D,R4                                                         
         LAY   R4,EX_BUFF                                                       
         MVI   LP_RMODE,LP_RNEXT                                                
         OC    SAVEKEY1,SAVEKEY1   Any previous estimate read                   
         JZ    GEST010             No                                           
*&&US                                                                           
         CLC   DS_SACCT,DS_ACCT    Same job as last estimate                    
         JE    GEST009             Yes                                          
         GOTOR PROEST              No - save current estimate                   
*&&                                                                             
GEST009  CLC   SAVEKEY1(ESTKSEQ-ESTKEY),ESTKEY Yes - does it match              
         JNE   GEST070             No - send estimate to list buffer            
GEST010  MVC   SAVEKEY1(L'ESTKEY),ESTKEY Set key for next time                  
         MVC   DS_SACCT,DS_ACCT    Set job for next                             
*&&US                                                                           
*                                                                               
* maintain approved/submitted highwater marks for each rec                      
*                                                                               
         CLI   ESTKSEQ,ESTKSMQ                                                  
         JNE   GEST014                                                          
         TM    ESTRSTA1,ESTKCAPP                                                
         JNZ   GEST012                                                          
         TM    ESTRSTA1,ESTKSUBM                                                
         JZ    GEST014                                                          
         MVC   DS_ESTHS,ESTKLNO                                                 
         J     GEST014                                                          
*                                                                               
GEST012  MVC   DS_ESTHI,ESTKLNO                                                 
*&&                                                                             
                                                                                
GEST014  LR    R3,R2                                                            
         AHI   R3,ESTRFST-ESTRECD                                               
*                                                                               
GEST016  CLI   EMDEL,0                                                          
         JE    GEST026                                                          
         CLI   EMDEL,EMDELQ        Main data element                            
         JE    GEST020                                                          
         CLI   EMDEL,ENMELQ        Name element                                 
         JE    GEST024                                                          
         CLI   EMDEL,ERDELQ        Row data                                     
         JE    GEST028                                                          
*                                                                               
GEST018  XR    R0,R0               Next element                                 
         IC    R0,EMDLN                                                         
         AR    R3,R0                                                            
         J     GEST016                                                          
*                                                                               
* Main data element - save for each TSAR record                                 
*                                                                               
GEST020  MVC   DS_EGLO,EMDGNO         Global number                             
         MVC   DS_ELOC,ESTKLNO        Local number (hex)                        
         MVC   DS_EDSC,SPACES         Name                                      
         MVC   DS_ECUR,SPACES         Foreign currency code                     
         ZAP   DS_TAMT,PZERO          Total foreign currency amount             
         ZAP   DS_AEST,PZERO          Total estimate amount                     
         XC    DS_EXRT,DS_EXRT        EXCH RATE                                 
         XC    DS_ESTS,DS_ESTS        STATUS                                    
         MVC   DS_GPST,EMDGSTAT       STATUS                                    
         MVC   DS_ESTDT,EMDDAT        DATE                                      
         MVC   DS_PRPID,EMDAPI        CREATOR                                   
*&&UK                                                                           
         CLC   EMDCUR,SPACES                                                    
         JNH   GEST022                                                          
         CLC   EMDCUR,AGYCURR                                                   
         JE    GEST022                                                          
         MVC   DS_ECUR,EMDCUR      Yes - extract currency code                  
         MVC   DS_EXRT,EMDRAT      and exchange rate                            
*&&                                                                             
GEST022  MVI   DS_ESTS,WCS_ESCA    STATUS - client approved                     
         TM    ESTRSTA1,ESTKCAPP                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESDL    Deleted                                      
         TM    ESTRSTA1,ESTKLOGD                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESRJ    Rejected                                     
         TM    ESTRSTA1,ESTKREJE                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESIA    Internally approved                          
         TM    ESTRSTA1,ESTKINTA                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESPG    In progress                                  
         TM    ESTRSTA1,ESTKCREA                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESIS    Submitted internally                         
         TM    ESTRSTA2,ESTKSINA                                                
         JNZ   GEST018                                                          
         MVI   DS_ESTS,WCS_ESCS    Submitted                                    
         J     GEST018                                                          
*                                  Name/description element                     
         USING ENMELD,R3                                                        
GEST024  XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         LTR   RE,RE                                                            
         JM    GEST018                                                          
         MVC   DS_EDSC(0),ENMNAME                                               
         EX    RE,*-6                                                           
         J     GEST018                                                          
                                                                                
GEST026  CLI   ESTKSEQ,ESTKSMQ     Only need to call filter routine             
         JNE   GEST002              for main record                             
         GOTOR FLTESTD             This will filter main estimate rec           
         J     GEST002              to check status is correct                  
*                                                                               
*   ROW DATA - ADD TO TSAR BUFFER                                               
*   NOTE THERE MAY BE MULTIPLE ELEMENTS FOR 1 WORKCODE. MERGE THESE             
*                                                                               
         USING ERDELD,R3                                                        
GEST028  CLI   ERDTYP,ERDTWDQ      look for w/c amounts only                    
         JNE   GEST018                                                          
         AP    DS_AEST,ERDWAMT                                                  
         AP    DS_TAMT,ERDWFCA                                                  
         GOTOR FLTESTD             This will filter main estimate rec           
         JNE   GEST018              to check status is correct                  
GEST030  CLC   QS_AWORK,AALL       Any work code filters                        
         JE    GEST034                                                          
         ICM   RF,7,QS_AWORK       Point to WMP entry                           
         LA    RF,10(RF)                                                        
*                                                                               
GEST032  OC    0(L'OAMWORK,RF),0(RF)    scan wmp for match                      
         JZ    GEST018             NO MATCH, NEXT ELEMENT                       
         CLC   ERDWCOD,0(RF)                                                    
         JE    GEST034                                                          
         LA    RF,L'OAMWORK(RF)                                                 
         J     GEST032                                                          
*                                                                               
GEST034  GOTOR CLRJBV,WCS_KEY                                                   
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_WCOD,ERDWCOD                                                 
         MVI   WCS_TYPE,WCS_EST                                                 
         MVC   WCS_ENUM,DS_ELOC                                                 
         LHI   RF,WCS_DLNQ                                                      
         STH   RF,WCS_LEN                                                       
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    GEST036             FOUND SOMETHING - CHECK IT                   
         TM    TSARERRS,TSERNF                                                  
         JNZ   GEST050             NOT FOUND, ADD IT.                           
         DC    H'0'                SOME OTHER ERROR - STOP                      
                                                                                
GEST036  CLC   WCS_WCOD,ERDWCOD    SAME ONE?                                    
         JNE   GEST050                                                          
         CLI   WCS_TYPE,WCS_EST                                                 
         JNE   GEST050                                                          
         CLC   WCS_ENUM,DS_ELOC                                                 
         JNE   GEST050                                                          
*                                                                               
         MVC   WCS_GPST,DS_GPST                                                 
         ZAP   MYPL16,ERDWAMT                                                   
         CLI   QS_ICON,YESQ        Include contingency                          
         JNE   GEST038             No                                           
         L     RF,AGOBLOCB         Yes                                          
         MP    MYPL16,GOOVRPER-GOBLOCKD(L'GOOVRPER,RF) 2 decimal places         
         SRP   MYPL16,64-4,5       Calculate current est*maxper                 
GEST038  AP    WCS_AEST,MYPL16                                                  
         AP    WCS_LSPD,MYPL16     Left to spend is estimate minus              
         AP    WCS_LSPH,MYPL16           current spend and orders               
         AP    WCS_CURA,ERDWFCA                                                 
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    GEST040           estimate or actuals                            
         AP    WCS_LBIL,MYPL16   Estimate                                       
*&&                                                                             
                                                                                
GEST040  CLI   ERDLN,ERDWLX1Q      Work code level time                         
         JL    GEST044             No                                           
         AP    WCS_AEHR,ERDWHRS    Yes - Add hours                              
                                                                                
GEST044  LLC   R0,ERDLN                                                         
         AR    R3,R0                                                            
         CLI   ERDEL,0                                                          
         JE    GEST048                                                          
         CLI   ERDEL,ERDELQ                                                     
         JNE   GEST048                                                          
         CLI   ERDTYP,ERDTWDQ      Is it next workcode element                  
         JE    GEST048             Yes - finish                                 
         CLI   ERDTYP,ERDTIDQ      Look for time items                          
         JNE   GEST046                                                          
         TM    ERDIIND,ERDIITQ     Is it time item                              
         JZ    GEST044                                                          
         AP    WCS_AEHR,ERDIMUL    Add hours                                    
         J     GEST044                                                          
                                                                                
GEST046  CLI   ERDTYP,ERDTIRQ      or time                                      
         JNE   GEST044                                                          
         AP    WCS_AEHR,ERDIHRS    Add hours                                    
         J     GEST044                                                          
                                                                                
GEST048  GOTOR GOTSAR,DMCB,('TSAWRT',0)                                         
         JE    GEST016                                                          
         DC    H'0'                                                             
                                                                                
GEST050  GOTOR CLRJBV,WCS_KEY      Not in buffer - build record                 
         MVC   WCS_SJAC,DS_ACCT                                                 
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_WCOD,ERDWCOD                                                 
         MVI   WCS_TYPE,WCS_EST                                                 
         MVC   WCS_ENUM,DS_ELOC                                                 
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_ESTG,DS_EGLO Add details of estimate                         
         MVC   WCS_GPST,DS_GPST                                                 
         MVC   WCS_ESDT,DS_ESTDT                                                
         ZAP   MYPL16,ERDWAMT                                                   
         CLI   QS_ICON,YESQ        Include contingency                          
         JNE   GEST052             No                                           
         L     RF,AGOBLOCB         Yes                                          
         MP    MYPL16,GOOVRPER-GOBLOCKD(L'GOOVRPER,RF)                          
         SRP   MYPL16,64-4,5       Calculate current est*maxper                 
GEST052  ZAP   WCS_AEST,MYPL16                                                  
         AP    WCS_LSPD,MYPL16     Left to spend is estimate minus              
         AP    WCS_LSPH,MYPL16           current spend and orders               
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    GEST053           estimate or actuals                            
         ZAP   WCS_LBIL,MYPL16   Estimate                                       
*&&                                                                             
GEST053  MVC   WCS_ESTD,DS_EDSC                                                 
         MVC   WCS_ESTS,DS_ESTS                                                 
         ZAP   WCS_CURA,ERDWFCA                                                 
         MVC   WCS_CURR,DS_ECUR                                                 
         GOTOR VHEXOUT,DMCB,DS_EXRT,WCS_CURX,L'DS_EXRT                          
         CLC   WCS_WCOD,SPACES                                                  
         JH    *+6                                                              
         DC    H'0'                Missing work code                            
         CLI   ERDLN,ERDWLX1Q      Work code level time                         
         JL    GEST054             No                                           
         AP    WCS_AEHR,ERDWHRS    Yes - Add hours                              
GEST054  LLC   R0,ERDLN                                                         
         AR    R3,R0                                                            
         CLI   ERDEL,0                                                          
         JE    GEST058                                                          
         CLI   ERDEL,ERDELQ                                                     
         JNE   GEST058                                                          
         CLI   ERDTYP,ERDTWDQ      Is it next workcode element                  
         JE    GEST058             Yes - finish                                 
         CLI   ERDTYP,ERDTIDQ      Look for time items                          
         JNE   GEST056                                                          
         TM    ERDIIND,ERDIITQ     Is it time item                              
         JZ    GEST054                                                          
         AP    WCS_AEHR,ERDIMUL    Add hours                                    
         J     GEST054                                                          
                                                                                
GEST056  CLI   ERDTYP,ERDTIRQ      or time                                      
         JNE   GEST054                                                          
         AP    WCS_AEHR,ERDIHRS    Add hours                                    
         J     GEST054                                                          
                                                                                
GEST058  CP    WCS_AEHR,PZERO                                                   
         JNE   GEST059                                                          
         CP    WCS_AEST,PZERO                                                   
         JE    GEST016                                                          
GEST059  GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GEST016                                                          
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   GEST060                                                          
         DC    H'0'                Duplicate key                                
GEST060  MVC   LP_ERROR,=AL2(AE$MAX#) Buffer full                               
         J     EXITN                                                            
*                                                                               
GEST066  OC    SAVEKEY1,SAVEKEY1   Any record to put out                        
         JZ    EXITY               No                                           
         MVI   BYTE2,YESQ          Yes - set last record                        
*                                                                               
GEST070  NI    DS_IND1,X'FF'-DS_IEST Reset skip estimate indicator              
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ         is connected                                 
         JNE   GEST074             Brandocean                                   
         CLI   QS_ESTL,YESQ        Do we want an order list                     
         JNE   GEST074             No                                           
         GOTOR CLRJBV,WCS_KEY      Build estimate list entry in buffer          
         MVC   WCS_SJAC,DS_SACCT                                                
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_SACCT    Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVI   WCS_TYPE,WCS_ESTL                                                
         MVC   WCS_ENUM,DS_ELOC                                                 
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_ESTG,DS_EGLO Add details of estimate                         
         MVC   WCS_GPST,DS_GPST                                                 
         MVC   WCS_ESTS,DS_ESTS                                                 
         MVC   WCS_ESDT,DS_ESTDT                                                
         MVC   WCS_PID,DS_PRPID                                                 
         MVC   WCS_ESTD,DS_EDSC                                                 
         MVC   WCS_CURR,DS_ECUR                                                 
         ZAP   WCS_CURA,DS_TAMT                                                 
         ZAP   WCS_AEST,DS_AEST                                                 
         ZAP   DS_TAMT,PZERO                                                    
         ZAP   DS_AEST,PZERO                                                    
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GEST074                                                          
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   GEST072                                                          
         DC    H'0'                Duplicate key                                
GEST072  MVC   LP_ERROR,=AL2(AE$MAX#) Buffer full                               
         J     EXITN                                                            
GEST074  CLI   BYTE2,YESQ          Last record process by nxtrec?               
         JNE   GEST010             No                                           
         J     EXITY               Yes - exit routine                           
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* Process estimates to save current estimate for summary              *         
***********************************************************************         
PROEST   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*PROEST*'         *** process estimates **                     
         OC    DS_ESTHI,DS_ESTHI   Do we have highest approved                  
         JNZ   *+10                Yes                                          
         MVC   DS_ESTHI,DS_ESTHS   If not get highest submitted                 
*                                                                               
         CLC   DS_SACCT,SPACES     Any account code?                            
         JNH   PROESTX                                                          
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of jobs                            
         LTR   R0,R0                                                            
         JZ    PROESTX                                                          
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
PROEST02 CLC   DS_SACCT,0(RF)      Find job code to update                      
         JE    PROEST04                                                         
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,PROEST02                                                      
         DC    H'0'                Must find a match on job                     
*                                                                               
PROEST04 MVC   0(1,R1),DS_ESTHI                                                 
         XC    DS_ESTHI,DS_ESTHI                                                
         XC    DS_ESTHS,DS_ESTHS                                                
PROESTX  J     EXITY                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* GET OLD ESTIMATE RECORDS                                            *         
***********************************************************************         
GETOEST  NTR1  BASE=*,LABEL=*                                                   
         USING EVERECD,R2                                                       
*                                                                               
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
         XC    IOKEY,IOKEY                                                      
         MVI   MYBYTE,0                                                         
         MVI   DS_ESTHI,0                                                       
         J     GOES003                                                          
*                                                                               
GOES002  LA    R2,IOKEY                                                         
         MVC   EVEKEY,SAVEKEY1                                                  
GOES003  GOTOR (#NXTREC,ANXTREC),DMCB,EVEKEYT,('B#EST',0),             X        
               (0,SAVED),0,0                                                    
         JNE   EXITY               DONE                                         
         L     R2,IOADDR                                                        
* US have only one 'current' estimate and some number of revisions              
*    we need to find the current estimate and save its number, so the           
* summary can ignore the others. Note NXTREC filters.                           
*                                                                               
         MVC   DS_ELOC,EVEKVER     LOCAL (VERSION) NO                           
*                                                                               
         CLI   EVEKSEQ,0           IF NOT MAIN REC, SKIP 'CURRENT' SAVE         
         JNE   GOES028                                                          
*                                                                               
         L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         CLI   GONEEDAE,YESQ                                                    
         JNE   GOES010             ANY WILL DO                                  
*                                                                               
* APPROVED ONLY                                                                 
         LA    R3,EVERFST                                                       
         XR    RF,RF                                                            
         USING EAPELD,R3                                                        
*                                                                               
GOES008  CLI   EAPEL,0                                                          
         JE    GOES012             UNAPPROVED, SKIP 'CURRENT' SAVE              
         CLI   EAPEL,EAPELQ                                                     
         JE    GOES010             APPROVAL ELEMENT, OK                         
         IC    RF,EAPLN                                                         
         AR    R3,RF                                                            
         J     GOES008                                                          
         DROP  RF                                                               
*                                                                               
GOES010  MVI   MYBYTE,1            Show that we got an approved est             
         CLC   DS_ELOC,DS_ESTHI                                                 
         JNH   GOES012                                                          
         MVC   DS_ESTHI,DS_ELOC    SAVE 'CURRENT' FOR SUMM FILT                 
*                                                                               
GOES012  CLI   MYBYTE,1                                                         
         JE    GOES016             Approved already found.                      
         CLC   DS_ELOC,DS_ESTHI                                                 
         JNH   GOES016                                                          
         MVC   DS_ESTHI,DS_ELOC    SAVE 'CURRENT' FOR SUMM FILT                 
*                                                                               
GOES016  DS    0H                                                               
*&&US                                                                           
         USING WCS_D,R4                                                         
         MVC   DS_SCLI(L'EVEKCLI),EVEKCLI                                       
         MVC   DS_SPRO(L'EVEKPRO),EVEKPRO                                       
         MVC   DS_SJOB(L'EVEKJOB),EVEKJOB                                       
         GOTOR BLDSCPJ                                                          
         MVC   DS_SACCT,DS_ACCT                                                 
         CLC   DS_SACCT,SPACES     Any account code?                            
         JNH   GOES028                                                          
         XR    RF,RF               Point to list in wmp                         
         ICM   RF,7,DS_SUBJA                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)  Number of jobs                            
         LTR   R0,R0                                                            
         JZ    GOES028                                                          
         LA    RF,LW_DATA2-LW_D(RF)                                             
         LA    R1,DS_ESCEA                                                      
*                                                                               
GOES018  CLC   DS_SACCT,0(RF)      Find job code to update                      
         JE    GOES020                                                          
         LA    RF,L'ACTKACT(RF)                                                 
         LA    R1,JB_LNQ(R1)                                                    
         JCT   R0,GOES018                                                       
         DC    H'0'                Must find a match on job                     
*                                                                               
GOES020  MVC   0(1,R1),DS_ESTHI                                                 
*&&                                                                             
                                                                                
         USING WCS_D,R4                                                         
GOES028  LAY   R4,EX_BUFF                                                       
         MVI   LP_RMODE,LP_RNEXT                                                
         GOTOR CLRJBV,WCS_KEY      INIT BUFFER REC                              
         MVI   DS_ESTS,WCS_ESCS    DEFAULT TO 'SUBMITTED'?                      
*                                                                               
         MVC   SAVEKEY1(L'EVEKEY),EVEKEY SAVE IN CASE SUBROUT DOES I/O          
         LR    R3,R2                                                            
         AHI   R3,ESTRFST-ESTRECD                                               
*                                                                               
         USING EDAELD,R3                                                        
GOES030  CLI   EDAEL,0                                                          
         JE    GOES002                                                          
         CLI   EDAEL,EDAELQ        ROW DATA                                     
         JE    GOES055                                                          
         CLI   EDAEL,EAPELQ        ROW DATA                                     
         JE    GOES040                                                          
*                                                                               
GOES035  XR    R0,R0               NEXT ELEMENT                                 
         IC    R0,EDALN                                                         
         AR    R3,R0                                                            
         J     GOES030                                                          
*                                                                               
*                              APPROVAL ELEM                                    
GOES040  MVI   DS_ESTS,WCS_ESCA                                                 
         J     GOES035                                                          
*                                                                               
*   ROW DATA - ADD TO TSAR BUFFER                                               
*                                                                               
         USING EDAELD,R3                                                        
                                                                                
GOES055  TM    EDATYPE,EDATWORK    TEST FOR WORKCODE ESTIMATE                   
         JNO   GOES035                                                          
         CP    EDACOMM,PZERO                                                    
         JNE   GOES056                                                          
         CLI   EDALN,EDALNQ2                                                    
         JL    GOES056                                                          
         CP    EDANCOM,PZERO                                                    
         JE    GOES035                                                          
*                                                                               
GOES056  CLC   QS_AWORK,AALL       Workcode filters                             
         JE    GOES060                                                          
         ICM   RF,7,QS_AWORK       Point to WMP entry                           
         LA    RF,10(RF)                                                        
*                                                                               
GOES058  OC    0(L'OAMWORK,RF),0(RF)    scan wmp for match                      
         JZ    GOES035             No match, next element                       
         CLC   EDAWORK,0(RF)                                                    
         JE    GOES060                                                          
         LA    RF,L'OAMWORK(RF)                                                 
         J     GOES058                                                          
*                                                                               
GOES060  GOTOR CLRJBV,WCS_KEY                                                   
         MVC   DS_SCLI(L'EVEKCLI),EVEKCLI                                       
         MVC   DS_SPRO(L'EVEKPRO),EVEKPRO                                       
         MVC   DS_SJOB(L'EVEKJOB),EVEKJOB                                       
         GOTOR BLDSCPJ                                                          
         MVC   WCS_SJAC,DS_ACCT                                                 
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_WCOD,EDAWORK                                                 
         MVI   WCS_TYPE,WCS_EST                                                 
         MVC   WCS_ENUM,DS_ELOC                                                 
         MVC   WCS_ESTS,DS_ESTS                                                 
         LHI   RF,WCS_DLNQ                                                      
         STH   RF,WCS_LEN                                                       
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    GOES062             FOUND SOMETHING - CHECK IT                   
         TM    TSARERRS,TSERNF                                                  
         JNZ   GOES065             NOT FOUND, ADD IT.                           
         DC    H'0'                SOME OTHER ERROR - STOP                      
                                                                                
GOES062  CLC   WCS_WCOD,EDAWORK    SAME ONE?                                    
         JNE   GOES065                                                          
         CLI   WCS_TYPE,WCS_EST                                                 
         JNE   GOES065                                                          
         CLC   WCS_ENUM,DS_ELOC                                                 
         JNE   GOES065                                                          
         DC    H'0'                                                             
                                                                                
GOES065  GOTOR CLRJBV,WCS_KEY        NOT IN BUFFER -BUILD NEW REC               
         MVC   DS_SCLI(L'EVEKCLI),EVEKCLI                                       
         MVC   DS_SPRO(L'EVEKPRO),EVEKPRO                                       
         MVC   DS_SJOB(L'EVEKJOB),EVEKJOB                                       
         GOTOR BLDSCPJ                                                          
         MVC   WCS_SJAC,DS_ACCT                                                 
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_WCOD,EDAWORK                                                 
         MVI   WCS_TYPE,WCS_EST                                                 
         MVC   WCS_ENUM,DS_ELOC                                                 
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_ESTS,DS_ESTS                                                 
**                                                                              
         AP    WCS_AEST,EDACOMM  Yes - Add to overall amount                    
         CLI   EDALN,EDALNQ2                                                    
         JL    *+10                                                             
         AP    WCS_AEST,EDANCOM                                                 
*&&US                                                                           
         TM    EDATYPE,EDATTEST    Time Estmate                                 
         JNO   *+18                                                             
         CLI   EDALN,EDALN4Q                                                    
         JL    *+10                                                             
         AP    WCS_AEHR,EDAHOURS   Estimated Hours                              
*&&                                                                             
         CLC   WCS_WCOD,SPACES                                                  
         JH    *+6                                                              
         DC    H'0'                MISSING WORKCODE                             
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GOES035                                                          
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   GOES068                                                          
         DC    H'0'                Duplicate key                                
GOES068  MVC   LP_ERROR,=AL2(AE$MAX#)    BUFFER FULL                            
         J     EXITN                                                            
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET TIMESHEET LINES                                                 *         
***********************************************************************         
GETTIME  NTR1  BASE=*,LABEL=NO     *** TIME SHEET LINES ***                     
         J     *+12                                                             
         DC    C'*GETTIME'                                                      
*                                                                               
         MVI   LP_RMODE,LP_RFRST   maintain RMODE for NXTREC                    
         J     GTIM003                                                          
*                                                                               
         USING TSJPASD,R2                                                       
GTIM002  LA    R2,IOKEY                                                         
         MVC   TSJPAS,SAVEKEY1             RESTORE RSEQ KEY                     
GTIM003  GOTOR (#NXTREC,ANXTREC),DMCB,TIMKEYT,('B#TIM',0),             X        
               (0,SAVED),AFLTMKD,0                                              
         JNE   EXITY                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         MVI   LP_RMODE,LP_RNEXT                                                
         MVC   SAVEKEY1,TSJPAS   Save last passive found                        
         MVC   DS_ACCT,TSJPACT                                                  
         MVC   IODA,TSJPKDA                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TIMRECD,R2                                                       
         L     R2,AIO3                                                          
         USING TIMELD,R3                                                        
         LA    R3,TIMRFST                                                       
         J     GTIM011                                                          
                                                                                
GTIM010  LLC   R0,TIMLN            find appropriate TIMEL cluster(s)            
         AR    R3,R0                                                            
*                                                                               
*  LOOK FOR VALID TIMEINP ELEMENTS, ADD TO TSAR (TACK ON NARR, ORD OR           
*     EST NUMBERS). FOR VALID TIMEINPS, ALSO ADD A TSAR FOR EACH                
*     ASSOCIATED TIMEITMS ELEMENT (TACK ON ORD/EST, BUT NOT NARR)               
GTIM011  CLI   TIMEL,0                                                          
         JE    GTIM002                                                          
         CLI   TIMEL,TIMELQ                                                     
         JNE   GTIM010                                                          
*                                                                               
*  TIMEINP ELEMENTS                                                             
         CLI   TIMETYP,TIMEINP                                                  
         JNE   GTIM035                                                          
         XC    FULL1,FULL1                                                      
         CLC   TIMACC(L'PRODUL),QS_UNIT   FILTER BY REQ ACCOUNT                 
         JNE   GTIM010                                                          
         CLC   TIMACC+2(L'DS_ACCT),DS_ACCT                                      
         JNE   GTIM010                                                          
                                                                                
         CLC   TIMTSK,SPACES       Have we got a workcode                       
         JNH   GTIM010                                                          
*                                                                               
         CLC   QS_AWORK,AALL       any workcode filter?                         
         JE    GTIM016                                                          
         ICM   RF,7,QS_AWORK       Point to WMP entry                           
         LA    RF,10(RF)                                                        
*                                                                               
GTIM015  OC    0(L'OAMWORK,RF),0(RF)    scan wmp for match                      
         JZ    GTIM010             No match, next record                        
         CLC   TIMTSK,0(RF)                                                     
         JE    GTIM016                                                          
         LA    RF,L'OAMWORK(RF)                                                 
         J     GTIM015                                                          
*                                                                               
GTIM016  GOTOR FLTTMP,DMCB,TIMELD,SAVEKEY1                                      
         JNE   GTIM010                                                          
         MVC   FULL1(L'TIMSEQ),TIMSEQ  Want this seq number                     
         GOTOR CLRJBV,WCS_KEY      build return values                          
*                                                                               
         MVI   WCS_SORM,WCS_MSTJ   Set as master job                            
         CLC   QS_ACCT,DS_ACCT     Is the account master                        
         JE    *+8                 Yes                                          
         MVI   WCS_SORM,WCS_SUBJ   Set as sub job                               
         MVC   WCS_SJAC,DS_ACCT                                                 
*                                                                               
         MVI   WCS_TYPE,WCS_TIME                                                
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_WCOD,TIMTSK                                                  
         MVC   WCS_LREF,AC@TIME                                                 
         MVI   WCS_IREV,NOQ                                                     
         MVI   WCS_IHLD,NOQ                                                     
         MVI   WCS_IQRD,NOQ                                                     
         MVI   WCS_ICOM,NOQ                                                     
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         ZAP   WCS_HRS,TIMHRS                                                   
*                                                                               
         LR    RF,R3                                                            
         XR    RE,RE                     LINE NARRATIVE?                        
GTIM020  IC    RE,TIMLN-TIMELD(RF)                                              
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         JE    GTIM022                   NO-EOR                                 
         CLC   TIMSEQ-TIMELD(L'TIMSEQ,RF),FULL1                                 
         JNE   GTIM022                   NO - NEXT SEQ CLUSTER                  
         CLI   TIMETYP-TIMELD(RF),TIMENAR                                       
         JNE   GTIM020                                                          
         IC    RE,TIMLN-TIMELD(RF)                                              
         SHI   RE,TIMHLNQ+1                                                     
*                                                                               
GTIM021  MVI   WCS_ELET,WCS_ELRQ                                                
         MVC   WCS_ELED(0),TIMNARR-TIMELD(RF)                                   
         EX    RE,*-6                                                           
         LA    RE,3(RE)                                                         
         STC   RE,WCS_ELEL                                                      
         LA    RF,WCS_TERM                                                      
         AR    RF,RE                                                            
         MVI   0(RF),0             NEW TERMINATOR                               
         LA    RF,1(RF)                                                         
         SR    RF,R4                                                            
         STH   RF,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,RF                                                            
         JH    GTIM022                                                          
         DC    H'0'                EX_BUFF TOO SMALL                            
*                                                                               
GTIM022  CLI   TIMTTYP,TIMTCN      N-TIME                                       
         JNE   GTIM025                                                          
         MVI   WCS_TTYP,WCS_TNTI   NON-BILLABLE TIME                            
         ZAP   WCS_TCR,PZERO                                                    
         ZAP   WCS_TSR,PZERO                                                    
         J     GTIM045                                                          
*                                                                               
GTIM025  ZAP   DUB1,TIMRATE        Amount = Rate * Hours                        
         MP    DUB1,TIMHRS                                                      
         SRP   DUB1,64-2,5         / 100 (hours is 2 dec places)                
*                                                                               
*&&US*&& ZAP   DUB2,PZERO          No sales amount in US.                       
*                                                                               
*&&UK                                                                           
         ZAP   DUB2,TIMCRATE       Amount = Rate * Hours                        
         MP    DUB2,TIMHRS                                                      
         SRP   DUB2,64-2,5         / 100 (hours is 2 dec places)                
*&&                                                                             
         ZAP   WCS_TCR,DUB2                                                     
         ZAP   WCS_TSR,DUB1                                                     
*                                                                               
GTIM027  CLI   TIMTTYP,TIMTCR        R-time                                     
         JNE   GTIM030                                                          
         MVI   WCS_TTYP,WCS_TMTI     Memo (R) time                              
         ZAP   WCS_CHT,DUB1          Set memo cost of hours                     
*                                                                               
         L     RF,AGOXBLCK                                                      
         CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
         JE    *+12                Yes                                          
         CLI   QS_ESTC,YESQ        Estimate checking?                           
         JNE   GTIM045                                                          
         ZAP   WCS_DBA,DUB1                                                     
         ZAP   WCS_NBIL,DUB1       Memo amount                                  
*&&UK                                                                           
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JNE   GTIM045                                                          
         ZAP   WCS_DBA,DUB2        Debit amount                                 
         ZAP   WCS_NBIL,DUB2       Memo amount                                  
*&&                                                                             
         J     GTIM045                                                          
*                                                                               
GTIM030  CLI   TIMTTYP,TIMTCB      B-time                                       
         JE    *+6                                                              
         DC    H'0'                Unknown type of time                         
         MVI   WCS_TTYP,WCS_TBTI                                                
         ZAP   WCS_DBA,TIMAMNT                                                  
         J     GTIM045                                                          
*                                                                               
* TIMEITMS ELEMENTS                                                             
*                                                                               
GTIM035  CLI   TIMETYP,TIMEITMS                                                 
         JNE   GTIM010                                                          
         CLC   TIMSEQ,FULL1        Matches last relevant timeinp?               
         JNE   GTIM010             No                                           
*                                                                               
         GOTOR CLRJBV,WCS_KEY      build return values for materials            
*                                                                               
         MVI   WCS_TYPE,WCS_TIME                                                
         MVC   WCS_SJAC,DS_ACCT                                                 
         MVI   WCS_TTYP,WCS_TBTI                                                
         MVC   WCS_LEN,=AL2(WCS_DLNQ)                                           
         MVC   WCS_WCOD,TIMITSK                                                 
         MVC   WCS_SEQ,SEQCTR                                                   
         LH    RF,SEQCTR                                                        
         AHI   RF,1                                                             
         STH   RF,SEQCTR                                                        
         MVC   WCS_LREF,AC@MAT                                                  
*                                                                               
         ZAP   WCS_DBA,TIMITOT                                                  
         CLI   TIMLN,TIMITLNQ                                                   
         JNH   GTIM045                                                          
         XR    RE,RE                                                            
         IC    RE,TIMLN                                                         
         SHI   RE,TIMITLNQ+1                                                    
         MVI   WCS_ELET,WCS_ELRQ   Material narrative                           
         MVC   WCS_ELED(0),TIMITEXT                                             
         EX    RE,*-6                                                           
         LA    RE,3(RE)                                                         
         STC   RE,WCS_ELEL                                                      
         LA    RF,WCS_TERM                                                      
         AR    RF,RE                                                            
         MVI   0(RF),0             New terminator                               
         LA    RF,1(RF)                                                         
         SR    RF,R4                                                            
         STH   RF,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,RF                                                            
         JH    GTIM045                                                          
         DC    H'0'                EX_BUFF too small                            
*                                                                               
* COMMON CODE (TIMEINP/TIMEITMS)                                                
*                                                                               
GTIM045  MVC   WCS_DAT,TIMKPEDT                                                 
         MVC   WCS_CTRA,TIMKULA                                                 
*        ZAP   WCS_GRSS,WCS_DBA                                                 
*        ZAP   WCS_NTEX,WCS_DBA    Net excluding write off                      
         ZAP   WCS_LBIL,WCS_DBA    Actuals left to bill                         
         ZAP   WCS_CMTD,WCS_DBA    Committed, orders and costs                  
         ZAP   WCS_ACTH,WCS_DBA                                                 
         SP    WCS_LSPD,WCS_DBA    Left to spend is minus actuals               
         SP    WCS_LSPH,WCS_DBA                                                 
         ZAP   WCS_NCOM,WCS_DBA    Make non commissionable amount               
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOXNBPE-GOXBLOCK(RF),GOYES                                       
         JNE   *+22                                                             
         AP    WCS_LSPH,WCS_NBIL   Remove non billable amounts                  
         SP    WCS_CMTD,WCS_NBIL                                                
         SP    WCS_ACTH,WCS_NBIL                                                
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JE    *+16                  estimate or actuals                        
         ZAP   WCS_LBIL,PZERO     Estimate                                      
         SP    WCS_LBIL,WCS_COB    Remove commission billed                     
*&&                                                                             
                                                                                
         L     RF,AGOXBLCK                                                      
         CLI   GOEXM-GOXBLOCK(RF),GOYES  Exclude memo                           
         JNE   GTIM050             No                                           
*&&UK                                                                           
         L     RF,AGOXBLCK                                                      
         CLI   GOTBL-GOXBLOCK(RF),GOTBACTQ Is left to bill based on             
         JNE   *+10                  estimate or actuals                        
*&&                                                                             
         SP    WCS_LBIL,WCS_NBIL   Yes - remove from left to bill               
*        SP    WCS_GRSS,WCS_NBIL            and gross                           
                                                                                
GTIM050  ZAP   WCS_NBIL,PZERO      Clear non billable                           
         MVI   WCS_APST,WCS_ASFA                                                
         TM    TIMRSTAT,TIMSFAPP                                                
         JNZ   GTIM052                                                          
         MVI   WCS_APST,WCS_ASPA                                                
         TM    TIMRSTAT,TIMSPAPP                                                
         JNZ   GTIM052                                                          
         MVI   WCS_APST,WCS_ASSB                                                
         TM    TIMRSTAT,TIMSSUBM                                                
         JNZ   GTIM052                                                          
         MVI   WCS_APST,WCS_ASRJ                                                
         TM    TIMRSTAT,TIMSREJE                                                
         JNZ   GTIM052                                                          
         MVI   WCS_APST,WCS_ASIP                                                
*                                                                               
CUR      USING TIMELD,RF                                                        
GTIM052  LA    RF,TIMRFST          add estimate/order number                    
*                                                                               
GTIM055  LLC   R0,CUR.TIMLN                                                     
         AR    RF,R0                                                            
         CLI   CUR.TIMEL,0                                                      
         JE    GTIM070                                                          
         CLI   CUR.TIMEL,TIMELQ                                                 
         JNE   GTIM055                                                          
         CLC   CUR.TIMSEQ,FULL1                                                 
         JNE   GTIM055                                                          
         CLI   CUR.TIMETYP,TIMEORDR                                             
         JNE   GTIM060                                                          
         MVC   WCS_ONUM,CUR.TIMOORDR          Order                             
         J     GTIM055                                                          
*                                                                               
GTIM060  CLI   CUR.TIMETYP,TIMEEST                                              
         JNE   GTIM055                                                          
         MVC   WCS_ESTG,CUR.TIMSESNM          Estimate                          
         J     GTIM055                                                          
*                                                                               
GTIM070  CLC   WCS_WCOD,SPACES                                                  
         JH    *+6                                                              
         DC    H'0'                Missing workcode                             
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    GTIM010             Next timel if ok                             
         TM    TSARERRS,TSEEOF                                                  
         JNZ   GTIM074                                                          
         DC    H'0'                Duplicate key?                               
GTIM074  MVC   LP_ERROR,=AL2(AE$MAX#)    Buffer full                            
         J     EXITN                                                            
         DROP  R2,R3,CUR                                                        
         EJECT                                                                  
***********************************************************************         
* KEYDRIVER FILTER - TRANSACTIONS DIRECTORY RECORD                    *         
*   NOTE USED FOR ORDER TXS AS WELL RIGHT NOW!                                  
***********************************************************************         
FLTRAND  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTRAND'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
         CLC   TRNKREF,SPACES      Skip non-transactions                        
         JNH   EXITN                                                            
         XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPINVTAQ         INVICTA: exclude drafts                      
         JE    FLTRAN02                                                         
         CLI   QS_XDRFT,YESQ       Parm: exclude drafts                         
         JNE   FLTRAN04                                                         
*                                                                               
*LTRAN02 CLC   PRODUL,TRNKULA      (not for SJ, see GETRANS)                    
*        JE    FLTRAN04                                                         
FLTRAN02 TM    TRNKSTAT,TRNSDRFT   skip drafts                                  
         JNZ   EXITN                                                            
*                                                                               
FLTRAN04 CLI   QS_REVRS,YESQ       Include reversals                            
         JE    FLTRAN06                                                         
         TM    TRNKSTAT,TRNSREVS   skip reversals                               
         JNZ   EXITN                                                            
*                                                                               
         USING OFFALD,R1                                                        
FLTRAN06 CLC   PRODUL,QS_UNIT      SJ ledger? Skip as will have w/c             
         JE    FLTRANX             not office                                   
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   FLTRANX                                                          
         CLC   TRNKSANL,SPACES     Only include if office                       
         JNH   EXITN                                                            
         CLI   TRNKSANL,C'*'       Any blank office?                            
         JE    EXITN               then no filtering                            
*                                                                               
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC(L'TRNKSANL),TRNKSANL                                    
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              validate office                              
         JNE   EXITN               no office match then skip                    
*                                                                               
FLTRANX  J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* KEYDRIVER FILTER - TRANSACTIONS FILE RECORD                                   
***********************************************************************         
FLTRANF  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTRANF'                                                      
*                                                                               
         L     R2,AIO2                                                          
         USING TRNRECD,R2                                                       
         CLC   TRNKULC(L'TRNKCUNT+L'TRNKCLDG),=C'1R'  Is it timesheets?         
         JNE   FLTRAN2                                                          
         CLC   TRNKDATE,SPACES                                                  
         JNH   EXITY                                                            
         GOTOR FLTTIM,DMCB,AIO2    Filter transactions                          
         JNE   EXITN                                                            
*        J     EXITY                                                            
                                                                                
FLTRAN2  LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
         USING TRSELD,R3                                                        
FLTRAN4  CLI   TRSEL,0                                                          
         JE    EXITN                                                            
         CLI   TRSEL,TRSELQ                                                     
         JE    FLTRAN8                                                          
FLTRAN6  IC    R0,TRSLN                                                         
         AR    R3,R0                                                            
         J     FLTRAN4                                                          
*                                                                               
FLTRAN8  CLC   QS_STACT,TRSDATE                                                 
         JH    EXITN                                                            
         CLC   QS_ENACT,TRSDATE                                                 
         JL    EXITN                                                            
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* keydriver filter for orders                                         *         
***********************************************************************         
FLTUORD  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTUORD'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING OSJPASD,R2                                                       
         TM    OSJPSTAT,ORDSLDEL                                                
         JNZ   EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* KEYDRIVER FILTER - ESTIMATES DIRECTORY RECORD                    *            
***********************************************************************         
FLTESTD  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTESTD'                                                      
*                                                                               
         L     R2,IOADDR                                                        
         USING ESTRECD,R2                                                       
*                                                                               
         CLI   ESTKSEQ,ESTKSMQ     If main record                               
         JNE   FLTEST02                                                         
         TM    ESTRSTA1,ESTKCAPP+ESTKSUBM+ESTKINTA only want estimates          
         JNZ   FLTEST02            that are submitted or approved               
         OI    DS_IND1,DS_IEST     Set skip estimate records                    
         J     EXITN                                                            
*                                                                               
FLTEST02 TM    DS_IND1,DS_IEST     This indicator is tested for                 
         JNZ   EXITN                      records after the main rec            
*&&UK                                                                           
* CE depends on NAE setting:                                                    
* If NAE=N: sum of estimates on job with submitted+approved status,             
* If NAE=Y: sum of estimates on job with approved status.                       
*                                                                               
         CLI   ESTKSEQ,ESTKSMQ                                                  
         JNE   EXITY                                                            
                                                                                
         L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         CLI   GONEEDAE,YESQ       Do we need to check status further           
         JNE   EXITY                No - other check client approved            
         DROP  RF                                                               
                                                                                
         TM    ESTRSTA1,ESTKCAPP                                                
         JNZ   EXITY                                                            
         OI    DS_IND1,DS_IEST     Set to skip subsequent recs for same         
         J     EXITN                 estimate number                            
*&&                                                                             
         J     EXITY                                                            
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* KEYDRIVER FILTER - ESTIMATES DIRECTORY RECORD                    *            
***********************************************************************         
FLTMKD   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTMKD*'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING TSJPASD,R2                                                       
         USING TMSTTABD,R3                                                      
         LA    R3,RTIMTUPT                                                      
         LA    R1,TMSTNMB                                                       
FLTTMK02 CLI   TMSTMPTY,YESQ       Are we interested in saved timesheet         
         JNE   FLTTMK04            No                                           
         OC    TSJPSTAT,TSJPSTAT   Yes - have we got one                        
         JZ    EXITY               Yes - want this time record                  
FLTTMK04 OC    TMSTSTAT,TMSTSTAT   Are we interested in other statuses          
         JZ    FLTTMK10            No - get next time type                      
         MVC   BYTE1,TMSTSTAT      Yes - do we have a match                     
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    FLTTMK10            No                                           
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    FLTTMK06            No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match                                    
         JNZ   FLTTMK10            Yes - get next time type                     
FLTTMK06 OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    EXITY               No - we want time record                     
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,TSJPKSTA                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITY               Match found accept timel                     
                                                                                
FLTTMK10 LA    R3,TMSTTABL(R3)                                                  
         JCT   R1,FLTTMK02                                                      
         J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TIME RECORDS ACCORDING TO COTUP AND GODITSB, GODITSR,GODITSN *         
* ROUTINE #1-FILTER TSJPASD                                           *         
* ON NTRY P1=A(TIMELD)                                                *         
*         P2=A(TSJPASD)                                               *         
***********************************************************************         
                                                                                
FLTTMP   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTTMP*'                                                      
                                                                                
         L     R2,4(R1)         R2=A(TSJPASD)                                   
         L     R3,0(R1)         R3=A(TIMEL)                                     
                                                                                
         USING TSJPASD,R2                                                       
         USING TIMELD,R3                                                        
*                                                                               
         USING TMSTTABD,R1                                                      
         CLI   TIMTTYP,TIMTCB      Billable time?                               
         JNE   FLTTMP02                                                         
         LA    R1,BTIMTUPT                                                      
         J     FLTTMP06                                                         
                                                                                
FLTTMP02 CLI   TIMTTYP,TIMTCR      R type time?                                 
         JNE   FLTTMP04                                                         
         LA    R1,RTIMTUPT                                                      
         J     FLTTMP06                                                         
                                                                                
FLTTMP04 CLI   TIMTTYP,TIMTCN      non billable time?                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NTIMTUPT                                                      
                                                                                
FLTTMP06 OC    TMSTSTAT,TMSTSTAT   Are we interested in any statuses            
         JNZ   FLTTMP08            Yes                                          
         CLI   TMSTMPTY,NOQ        Are we interested in saved TS                
         JE    EXITN                                                            
*                                                                               
FLTTMP08 CLI   TMSTMPTY,YESQ       Are we interested in saved TS                
         JNE   FLTTMP10            No                                           
         OC    TSJPSTAT,TSJPSTAT   Yes - do we have a saved timesheet           
         JZ    EXITY               Yes - accept time line                       
         OC    TMSTSTAT,TMSTSTAT   No - are we interested in any other          
         JZ    EXITN                            statuses                        
FLTTMP10 MVC   BYTE1,TMSTSTAT                                                   
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    EXITN               No - don't want this timel                   
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    FLTTMP12            No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITN               Reject timel if found                        
FLTTMP12 DS    0H                                                               
         OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    EXITY               No                                           
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,TSJPKSTA                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITY               Match found accept timel                     
         J     EXITN               Reject timel                                 
                                                                                
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* Filter expenses according to GODIEN and GODIEB - DIRECTORY LEVEL    *         
*                                                                               
* PASSIVE IN IOKEY. EXCLUDES RECORDS THAT WOULD BE IGNORED BY BOTH              
* PROFILES. FLTEXL THEN FILTERS AT CIDEL LEVEL, BASED ON WHETHER A              
* PARTICULAR (MAIN) CIDEL IS MARKED AS BILLABLE OR NOT.                         
***********************************************************************         
FLTEXPD  NTR1  BASE=*,LABEL=*                                                   
         B     *+12                                                             
         DC    C'*FLTEXPD'                                                      
         LA    R2,IOKEY                                                         
         L     R3,AGOXBLCK                                                      
         USING EXJPASD,R2                                                       
         USING GOXBLKD,R3                                                       
*                                                                               
         TM    EXJPSTAT,EXCSLOGD   ignore logically deleted                     
         JNZ   EXITN                                                            
*                                                                               
         LA    RF,GODIEN                                                        
         LA    RE,GODIEB                                                        
         CLI   QS_ESTC,YESQ        doing est check instead?                     
         JNE   *+12                                                             
         LA    RF,GOEXPN                                                        
         LA    RE,GOEXPB                                                        
         CLI   0(RF),GOINPR        find lowest level of both settings           
         JE    EXITY                                                            
         CLI   0(RE),GOINPR                                                     
         JE    EXITY                                                            
         OC    EXJPSTAT,EXJPSTAT   ignore in progress                           
         JZ    EXITN                                                            
         TM    EXJPSTAT,EXCSREJE   ignore and rejected                          
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB         Submitted?                                   
         JE    EXITY                                                            
         CLI   0(RE),GOSUB                                                      
         JE    EXITY                                                            
         TM    EXJPSTAT,EXCSSUBM   Ignore submitted                             
         JNZ   EXITN                                                            
         CLI   0(RF),GOPAP         Part approved                                
         JE    EXITY                                                            
         CLI   0(RE),GOPAP                                                      
         JE    EXITY                                                            
         TM    EXJPSTAT,EXCSPAPP+EXCSFNTA                                       
         JNZ   EXITN                                                            
         TM    EXJPSTAT,EXCSCOMP                                                
         JZ    EXITN                                                            
         CLI   0(RF),GOAPPR        Approved                                     
         JE    EXITY                                                            
         CLI   0(RE),GOAPPR                                                     
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Filter CIDELs according to GODIEN OR GODIEB (CIDMTYP TO CHOOSE)     *         
* ON NTRY P1=A(CIDELD)                                                *         
*         P2=A(EXCRECD)                                               *         
***********************************************************************         
FLTEXL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTEXL*'                                                      
         LM    R2,R3,0(R1)         R2=A(CIDELD),R3=A(EXCRECD)                   
*                                                                               
         L     R4,AGOXBLCK                                                      
         USING CIDELD,R2                                                        
         USING GOXBLKD,R4                                                       
         USING EXCRECD,R3                                                       
*                                                                               
         CLI   CIDMTYP,C'B'        Billable                                     
         JNE   FLTEXL02                                                         
         TM    EXCRSTAT,EXCSCOMP   Ignore fully approved - double count         
         JNZ   EXITN                                                            
         LA    RF,GODIEB                                                        
         CLI   QS_ESTC,YESQ                                                     
         JNE   FLTEXL04                                                         
         LA    RF,GOEXPB                                                        
         J     FLTEXL04                                                         
*                                                                               
FLTEXL02 CLI   CIDMTYP,C'N'        Non-billable                                 
         JE    *+6                                                              
         DC    H'0'                This cideld is bad                           
         LA    RF,GODIEN                                                        
         LA    RE,GODEIS                                                        
         CLI   QS_ESTC,YESQ                                                     
         JNE   *+12                                                             
         LA    RF,GOEXPN                                                        
         LA    RE,GOEMIN                                                        
*                                                                               
         CLI   0(RE),NOQ           Have we already got memo invoice             
         JE    FLTEXL04            No                                           
*                                                                               
         TM    EXCRSTAT,EXCSCOMP   Yes ignore fully approved to avoid           
         JNZ   EXITN                               double counting              
*                                                                               
FLTEXL04 CLI   0(RF),GONONE        Filter by GODIEB OR GODIEN                   
         JE    EXITN               None                                         
         CLI   0(RF),GOINPR        In progress?                                 
         JE    EXITY                                                            
         OC    EXCRSTAT,EXCRSTAT   Ignore in progress                           
         JZ    EXITN                                                            
         TM    EXCRSTAT,EXCSREJE   Ignore rejected                              
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB         Submitted?                                   
         JE    EXITY                                                            
         TM    EXCRSTAT,EXCSSUBM   Ignore submitted                             
         JNZ   EXITN                                                            
         CLI   0(RF),GOPAP         Part approved                                
         JE    EXITY                                                            
         TM    EXCRSTAT,EXCSPAPP+EXCSFNTA  ignore part appr                     
         JNZ   EXITN                                                            
         TM    EXCRSTAT,EXCSCOMP                                                
         JZ    EXITN                                                            
         CLI   0(RF),GOAPPR        approved                                     
         JE    EXITY                                                            
         DC    H'0'                bad getopt setting                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* FILTER TIME TRNSACT ACCORDING TO  GODITSB, GODITSR, GOETMB, GOETMR            
* ON NTRY P1=A(TRNRECD)                                               *         
***********************************************************************         
FLTTIM   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTTIM*'                                                      
                                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         LA    R4,TRNRFST                                                       
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
*                                                                               
         XR    RF,RF                                                            
         USING TRNELD,R4                                                        
FLTTIM02 CLI   TRNEL,0                                                          
         JE    FLTTIM10                                                         
         CLI   TRNEL,TRNELQ        find time type                               
         JE    FLTTIM06                                                         
         CLI   TRNEL,PRTELQ        check time type                              
         JE    FLTTIM08                                                         
         CLI   TRNEL,TRSELQ                                                     
         JNE   FLTTIM04                                                         
         ST    R4,FULL1                                                         
FLTTIM04 LLC   R0,TRNLN                                                         
         AR    R4,R0                                                            
         J     FLTTIM02                                                         
*                                                                               
FLTTIM06 CP    TRNAMNT,PZERO       Is it billable                               
         JE    FLTTIM04            No                                           
         LA    RF,GODITSB          Billable                                     
         CLI   QS_ESTC,YESQ        doing est checking instead?                  
         JNE   *+8                                                              
         LA    RF,GOETMB                                                        
         J     FLTTIM04                                                         
*                                                                               
         USING PRTELD,R4                                                        
FLTTIM08 OR    RF,RF                                                            
         JNZ   FLTTIM04                                                         
         LA    RF,GODITSR                                                       
         CLI   QS_ESTC,YESQ        doing est checking instead?                  
         JNE   *+8                                                              
         LA    RF,GOETMR                                                        
         CP    PRTRATE,PZERO       R type has a rate                            
         JNE   FLTTIM04            Must be R type                               
         OC    PRTSTRT,PRTSTRT                                                  
         JNZ   FLTTIM04            non-bill doesn't                             
         LA    RF,GODITSN                                                       
         J     FLTTIM04                                                         
*                                                                               
         USING TRSELD,R4                                                        
FLTTIM10 L     R4,FULL1                                                         
         CLI   0(RF),GONONE                 want none included?                 
         JE    EXITN                                                            
         CLI   0(RF),GOINPR                 in progress or higher               
         JE    EXITY                                                            
         TM    TRSSTAT4,TRSSSAVT            ignore in progress                  
         JNZ   EXITN                                                            
         TM    TRSSTAT4,TRSSREJT            and rejected                        
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB                  submitted or higher                 
         JE    EXITY                                                            
         TM    TRSSTAT4,TRSSSUBT            ignore submitted                    
         JNZ   EXITN                                                            
         CLI   0(RF),GOMANPAP               line manager approved               
         JNE   FLTTIM12                                                         
         TM    TRSSTAT4,TRSSMAAP            ignore part approved                
         JNZ   EXITY                                                            
         OC    TRSSTAT4,TRSSTAT4                                                
         JZ    EXITY                                                            
         J     EXITN                                                            
*                                                                               
FLTTIM12 CLI   0(RF),GOCLIPAP      Client approved                              
         JNE   FLTTIM14            No                                           
         TM    TRSSTAT4,TRSSSJAT   Is the time client approved                  
         JNZ   EXITY               Yes                                          
         OC    TRSSTAT4,TRSSTAT4   Or fully approved                            
         JZ    EXITY                                                            
         J     EXITN                                                            
                                                                                
FLTTIM14 TM    TRSSTAT4,TRSSSJAT+TRSSMAAP   ignore part approved                
         JNZ   EXITN                                                            
         CLI   0(RF),GOAPPR                 approved                            
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* filter order - process all for order list but remove from job detail*         
***********************************************************************         
FLTORDS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*FLTORDS'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING OSJPASD,R2                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   FLORD004            Brandocean                                   
         TM    OSJPSTAT,ORDSFMCH                                                
         JNZ   FLORD008                                                         
         TM    OSJPSTAT,ORDCLOSE+ORDGDRCV                                       
         JNZ   EXITN                                                            
         J     FLORD006                                                         
                                                                                
FLORD004 TM    OSJPSTAT,ORDCLOSE+ORDSFMCH+ORDGDRCV                              
         JNZ   EXITN                                                            
FLORD006 TM    OSJPSTA2,ORDSOREJ   Ignore rejected orders                       
         JNZ   EXITN                                                            
*                                                                               
         CLI   OSJPMEM,0           Test memo order                              
         JNE   FLORD008                                                         
         TM    OSJPSTA2,ORDSAPPR   If not, check unapproved                     
         JNZ   EXITN               No, GETOTX will handle                       
*                                                                               
FLORD008 L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
         CLI   QS_ESTC,YESQ                                                     
         JNE   FLORD010                                                         
         LA    RF,GOEORN           non memo orders                              
         CLI   OSJPMEM,0           is it memo?                                  
         JE    FLORD020                                                         
         LA    RF,GOEORM           yes-use memo order option                    
         J     FLORD020                                                         
*                                                                               
FLORD010 LA    RF,GODIOS           order option                                 
         CLI   OSJPMEM,0           is it memo?                                  
         JE    FLORD020                                                         
         LA    RF,GODIEO           yes-use memo order option                    
*                                                                               
FLORD020 CLI   0(RF),GONONE        none                                         
         JE    EXITN                                                            
         CLI   0(RF),GOINPR        in progress                                  
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSDRFT   ignore in progress                           
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB         submitted                                    
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSSUBM   ignore submitted                             
         JNZ   EXITN                                                            
         CLI   0(RF),GOPAP         part approved                                
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSPAPP   ignore part approved                         
         JNZ   EXITN                                                            
         CLI   0(RF),GOAPPR        approved                                     
         JE    EXITY                                                            
         DC    H'0'                Status not known                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR                                                   *         
*                                                                     *         
***********************************************************************         
                                                                                
GOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOTSAR*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
TB       USING TSARD,TSARRECS      R3=A(TSAR block)                             
         LAY   R1,EX_BUFF                                                       
         ST    R1,TB.TSAREC        Address of record buffer area                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TB.TSARD(TSPNEWL),TB.TSARD                                       
         MVC   TB.TSACTN,0(R2)                                                  
         MVC   TB.TSACOM,ACOMFACS                                               
         LH    R0,=AL2(100)                                                     
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL      Set require 1MB off-line                    
         MVI   TB.TSRECI,TSRXTN+TSRMINB1+TSRVAR+TSRMGB                          
         MVI   TB.TSKEYL,WCS_TKYL   Set key length                              
         MVI   TB.TSINDS,TSINODSK   Set no disk writes (save/restore)           
         MVI   TB.TSIND2,TSI2MANY                                               
         LHI   R0,L'EX_BUFF                                                     
         STCM  R0,3,TB.TSRECL       Set maximum record length                   
         GOTOR VTSAR,TB.TSARD                                                   
         TM    TB.TSINDS,TSIINIOK                                               
         JNZ   EXITY                                                            
         DC    H'0'                 Initialisation failure                      
                                                                                
GOTSAR02 TM    TB.TSINDS,TSIINIOK   Test initialised                            
         JZ    GOTSAR06                                                         
                                                                                
         MVC   TB.TSACTN,0(R2)      Set action                                  
                                                                                
         CLI   TB.TSACTN,TSASRT     Test sorting                                
         JNE   GOTSAR04                                                         
         L     R1,4(R2)                                                         
         MVC   TB.TSRTPARM,0(R1)    Yes - set sort parameters                   
                                                                                
GOTSAR04 GOTOR VTSAR,TB.TSARD       Call TSAR                                   
         MVC   TSARERRS,TB.TSERRS   Return TSARERRS                             
         J     GOTSARX                                                          
                                                                                
GOTSAR06 MVI   TSARERRS,TSEEOF                                                  
                                                                                
GOTSARX  CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  TB                                                               
         EJECT                                                                  
**********************************************************************          
* Get workcode description, and whether internal or external                    
*    runs off ex_buff!                                                          
**********************************************************************          
                                                                                
GETWDES  NTR1                                                                   
         L     R1,LP_AINP                                                       
         CLC   DS_WCLST,0(R1)                                                   
         JE    GWDESX                                                           
         MVC   DS_WDESC,SPACES                                                  
         MVI   DS_WCEST,YESQ                                                    
         MVC   DS_WCLST,0(R1)                                                   
*                                                                               
*&&US                                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         JNE   GWDES02             Brandocean                                   
         CLC   BILWC,0(R1)         In Aura do some trickery with                
         JNE   GWDES02                                prebilling                
         MVC   DS_WDESC,AC@PREBL                                                
         MVI   DS_WINEX,C'E'                                                    
         J     GWDESX                                                           
*&&                                                                             
                                                                                
GWDES02  MVI   DS_WINEX,C'E'                                                    
         CLI   CUCTRY,CTRYGER                                                   
         JNE   GWDES10                                                          
         CLI   0(R1),C'0'                                                       
         JNL   GWDES10                                                          
         MVI   DS_WINEX,C'I'                                                    
*                                                                               
GWDES10  LA    R2,IOKEY                                                         
         USING WCORECD,R2                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(2),PRODUL                                                
         MVC   WCOKWRK,0(R1)                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   GWDESX                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   GWDESX                                                           
                                                                                
         L     R2,AIO1                                                          
         LA    R2,WCORFST                                                       
         USING WCOELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
GWDES20  CLI   WCOEL,WCOELQ                                                     
         JE    GWDES30                                                          
         CLI   WCOEL,0                                                          
         JE    GWDESX                                                           
         IC    R0,WCOLN                                                         
         AR    R2,R0                                                            
         J     GWDES20                                                          
*                                                                               
GWDES30  MVC   DS_WDESC(L'WCODESC),WCODESC                                      
         TM    WCOSTAT,WCOSHCOE                                                 
         JZ    GWDES40                                                          
         MVI   DS_WINEX,C'I'                                                    
*&&US                                                                           
GWDES40  CLI   WCOTYPE,C'T'                                                     
         JNE   GWDESX                                                           
         MVI   DS_WINEX,C'I'                                                    
*&&                                                                             
                                                                                
*&&UK                                                                           
GWDES40  TM    WCOSTAT3,WCOSESCK                                                
         JZ    GWDESX                                                           
         MVI   DS_WCEST,NOQ       if not included in estimate checking          
         L     R2,LP_ADATA         remove from left to spend                    
BUF      USING WCS_D,R2                                                         
         ZAP   BUF.WCS_LSPD,PZERO                                               
         ZAP   BUF.WCS_LSPH,PZERO                                               
         DROP  BUF                                                              
*&&                                                                             
*                                                                               
GWDESX   J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* Get contra account name  - MERGE WITH WDES/OFFN?                              
* Destroys keydriver ioarea                                                     
**********************************************************************          
*                                                                               
GETCNAME NTR1                                                                   
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         CLC   DS_CALST,WCS_CTRA                                                
         JE    GCNAME50                                                         
         MVC   TEMP2(L'WCS_CTRA),WCS_CTRA                                       
         GOTOR (#GETACN,AGETACN)                                                
         MVC   DS_CTRN,TEMP2                                                    
*                                                                               
GCNAME50 MVC   WCS_CTRN,DS_CTRN                                                 
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* Translate User ID  (SETS AS DS_BLUID)                                         
**********************************************************************          
*                                                                               
GETUSR   NTR1                                                                   
         L     R1,LP_AINP                                                       
         CLC   DS_PRUID,0(R1)                                                   
         JE    EXIT                Already known                                
         OC    0(L'WCS_BUID,R1),0(R1) Have we got a user id                     
         JZ    GETUSRN             No                                           
         MVC   DS_PRUID,0(R1)                                                   
         MVC   TEMP2(L'WCS_BUID),0(R1)                                          
         GOTOR (#GETUSR,AGETUSR)                                                
         JNE   GETUSRN                                                          
         MVC   DS_BLUID,TEMP2                                                   
         J     EXIT                                                             
*                                                                               
GETUSRN  MVC   DS_BLUID,SPACES     Bad user id                                  
         XC    DS_PRUID,DS_PRUID                                                
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* Get 8CHAR PID FROM HEX CODE                                                   
**********************************************************************          
*                                                                               
GETPID   NTR1                                                                   
         L     R1,LP_AINP                                                       
         CLC   DS_PRPID,0(R1)                                                   
         JE    EXIT                Alreayd known                                
         MVC   DS_PRPID,0(R1)                                                   
         MVC   TEMP2(L'WCS_PID),0(R1)                                           
         GOTOR (#GETPID,AGETPID)                                                
         JNE   GETPIDN                                                          
         MVC   DS_BLPID,TEMP2                                                   
         J     EXIT                                                             
*                                                                               
GETPIDN  MVC   DS_BLPID,SPACES                                                  
         XC    DS_PRPID,DS_PRPID                                                
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* Get names for an 8byte person code (in DS_BLPID)                              
**********************************************************************          
*                                                                               
GETPIN   NTR1                                                                   
         L     R1,LP_AINP                                                       
         USING WCS_D,R1                                                         
         OC    WCS_PID,WCS_PID     No PID then look for authoriser              
         JZ    GETPIN2                                                          
         CLC   DS_PRPIN,DS_BLPID                                                
         JE    EXIT                Already known                                
         MVC   DS_PRPIN,DS_BLPID                                                
         MVC   TEMP2(L'DS_BLPID),DS_BLPID                                       
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   GETPINN                                                          
         MVC   DS_PIDFN,TEMP2                                                   
         MVC   DS_PIDLN,WORK2                                                   
         MVC   DS_PIDMN,TEMP2+32                                                
         J     EXIT                                                             
*                                                                               
GETPIN2  CLI   WCS_TYPE,WCS_ORDL Only use authoriser when order list            
         JNE   GETPINN                                                          
         CLC   WCS_ORAU,SPACES   Do we have anything in authoriser              
         JNH   GETPINN                       if so set as first name            
         MVC   DS_PIDFN(L'WCS_ORAU),WCS_ORAU   and last name as                 
         MVC   DS_PIDMN,SPACES                  full stop so front end          
         MVC   DS_PIDLN,SPACES                   doesn't show null              
         MVI   DS_PIDLN,C'.'                                                    
         XC    DS_PRPIN,DS_PRPIN                                                
         J     EXIT                                                             
GETPINN  DS    0H                   Bad pid                                     
         MVC   DS_PIDFN,SPACES                                                  
         MVC   DS_PIDMN,SPACES                                                  
         MVC   DS_PIDLN,SPACES                                                  
         XC    DS_PRPIN,DS_PRPIN                                                
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Find narratives and shove into array                                          
***********************************************************************         
         SPACE 1                                                                
GETNARR  NTR1                                                                   
         LR    R2,R1                                                            
         USING TRNRECD,R2                                                       
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         LA    R3,WCS_TERM         R3 scans subels for free slot                
         USING WCS_TERM,R3         Note dependent using for subel!!!!           
         SR    RE,RE                                                            
GNARR20  CLI   WCS_ELET,0          Find end of subels                           
         JE    GNARR30                                                          
         CLI   WCS_ELET,WCS_ELRQ   Already have one, try extended               
         JE    GNARR50                                                          
         LLC   RE,WCS_ELEL                                                      
         AR    R3,RE                                                            
         J     GNARR20                                                          
*                                                                               
GNARR30  LA    R1,TRNRFST                                                       
         DROP  R2                                                               
         USING TRNELD,R1           always first elem                            
         CLI   TRNLN,TRNLN1Q                                                    
         JNH   GNARR50                                                          
         XR    RE,RE               Add as new subel                             
         IC    RE,TRNLN                                                         
         SHI   RE,TRNLN1Q                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   WCS_ELED(0),TRNNARR                                              
         EX    RE,0(RF)                                                         
         MVI   WCS_ELET,WCS_ELRQ                                                
         LA    RE,3(RE)                                                         
         STC   RE,WCS_ELEL         L'subel                                      
         AR    R3,RE                                                            
         MVI   WCS_ELET,0          New terminator                               
         LA    RF,1(R3)            Update length                                
         SR    RF,R4                                                            
         STH   RF,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,RF                                                            
         JH    GNARR50                                                          
         DC    H'0'                EX_BUFF too small                            
*                                                                               
         USING FFTELD,R1                                                        
GNARR50  CLI   FFTEL,0             Look for extended narratives                 
         JE    EXITY               Done                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   GNARR51                                                          
         CLI   FFTTYPE,FFTTNARR                                                 
         JE    GNARR52                                                          
GNARR51  LLC   RE,FFTLN                                                         
         AR    R1,RE                                                            
         J     GNARR50                                                          
*                                                                               
GNARR52  LLC   RE,FFTLN                                                         
         SHI   RE,FFTWCLN+1                                                     
         USING WCS_TERM,R3                                                      
         BASR  RF,0                                                             
         MVC   WCS_ELED(0),FFTWCNAR                                             
         EX    RE,0(RF)                                                         
*                                                                               
         TM    LP_FLAG,LP_FOFFL    Next bit ought to go in FALINK,              
         JNZ   GNARR55                               but...                     
         CHI   RE,132-1                                                         
         JNH   GNARR55                                                          
         MVC   WCS_ELED+129(3),=C'...'                                          
         LA    RE,132-1            Online can only take so much data            
*                                                                               
GNARR55  MVI   WCS_ELET,WCS_ELRQ                                                
         LA    RE,3(RE)                                                         
         STC   RE,WCS_ELEL         L'subel                                      
         AR    R3,RE                                                            
         MVI   WCS_ELET,0          New terminator                               
         LA    RF,1(R3)            Update length                                
         SR    RF,R4                                                            
         STH   RF,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,RF                                                            
         JH    GNARR51             Any more?                                    
         DC    H'0'                EX_BUFF too small                            
         J     EXITY                                                            
         DROP  R1,R4,R3                                                         
         EJECT                                                                  
***********************************************************************         
* Find order description and shove into array                                   
***********************************************************************         
         SPACE 1                                                                
GETDESC  NTR1                                                                   
         LR    R2,R1                                                            
         USING ORDRECD,R2                                                       
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         LA    R3,WCS_TERM         R3 scans subels for free slot                
         USING WCS_TERM,R3         Note dependent using for subel!!!!           
         SR    RE,RE                                                            
GDESC02  CLI   WCS_ELET,0          Find end of subels                           
         JE    GDESC04                                                          
         LLC   RE,WCS_ELEL                                                      
         AR    R3,RE                                                            
         J     GDESC02                                                          
*                                                                               
GDESC04  LA    R1,ORDRFST                                                       
         USING SCMELD,R1           always first elem                            
GDESC06  CLI   SCMEL,0                                                          
         JE    EXITY                                                            
         CLI   SCMEL,SCMELQ                                                     
         JE    GDESC10                                                          
GDESC08  LLC   RF,SCMLN                                                         
         AR    R1,RF                                                            
         J     GDESC06                                                          
*                                                                               
GDESC10  CLI   SCMTYPE,SCMTOMOC                                                 
         JE    GDESC14                                                          
*                                                                               
GDESC12  CLI   SCMTYPE,SCMTSTND                                                 
         JNE   GDESC08                                                          
*                                                                               
GDESC14  LLC   RE,SCMLN                                                         
         SHI   RE,SCMLN1Q                                                       
         CHI   RE,L'TRNNARR                                                     
         JNH   *+8                                                              
         LA    RE,L'TRNNARR                                                     
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   WCS_ELED(0),SCMNARR                                              
         EX    RE,0(RF)                                                         
         MVI   WCS_ELET,WCS_ELRQ                                                
         LA    RE,3(RE)                                                         
         STC   RE,WCS_ELEL         L'subel                                      
         AR    R3,RE                                                            
         MVI   WCS_ELET,0          New terminator                               
         LA    RF,1(R3)            Update length                                
         SR    RF,R4                                                            
         STH   RF,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,RF                                                            
         JH    EXITY                                                            
         DC    H'0'                EX_BUFF too small                            
*                                                                               
         DROP  R1,R4,R3                                                         
         EJECT                                                                  
***********************************************************************         
* GET DEBTOR PAYMENT DETAILS AND SEE WHETHER FULLY PAID, PART PAID OR *         
* UNPAID                                                              *         
* ON NTRY IOADDR CONTAINS TRANSACTION RECORD                          *         
* Uses AIO2                                                                     
* overwrites IOKEY, so restore after calling this routine if reading            
* sequentially (including ANXTREC keydrivers)                                   
***********************************************************************         
GETDDET  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETDT**'                                                      
         MVC   DS_MEDDS,SPACES                                                  
         MVC   DS_SRACT,SPACES                                                  
         ZAP   DUB,PZERO                                                        
         ZAP   DS_TOTBL,PZERO                                                   
         XC    BYTE1,BYTE1                                                      
*                                                                               
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         L     R2,IOADDR                                                        
SJ       USING TRNRECD,R2                                                       
         LA    R3,SJ.TRNRFST                                                    
         USING TRNELD,R3                                                        
         LLC   RF,PPROLEN                                                       
         LA    RF,SJ.TRNKACT(RF)        RF=A(media code)                        
*                                                                               
         CLI   TRNTYPE,TRNTMEBL    Media billing                                
*&&UK*&& JNE   GETDDT00            No                                           
*&&US*&& JNE   GETDDT04            No                                           
                                                                                
         LA    R3,IOKEY            Yes - read media passive                     
         USING M_MEDKEY,R3          to get debtor contra name                   
         XC    IOKEY,IOKEY                                                      
         MVI   M_MEDKTYP,M_MEDKPASQ                                             
         MVC   M_MEDKAM,MEDAGYB                                                 
         MVC   M_MEDKCODE,0(RF)                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOMEDDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMEDFIL+IO4'                           
         JNE   *+2                                                              
         L     R3,AIO4                  Media account record                    
         CLI   M_MEDEL,M_MEDELQ                                                 
         JNE   *+2                                                              
         MVC   DS_MEDDS(L'M_MEDREVNM),M_MEDREVNM Extract media name             
         J     GETDDT16                                                         
*&&UK                                                                           
         USING TRNELD,R3                                                        
         USING FFTELD,RE                                                        
GETDDT00 LA    R3,SJ.TRNRFST       Locate Billing Source on SJ trx              
         LA    RE,TRNELD                                                        
         CLI   TRNTYPE,TRNTMABL                                                 
         JNE   GETDDT04                                                         
GETDDT02 LLC   R0,FFTLN                                                         
         AR    RE,R0                                                            
         CLI   FFTEL,0                                                          
         JE    GETDDT04                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   GETDDT02                                                         
         CLI   FFTTYPE,FFTTBSOU                                                 
         JNE   GETDDT02                                                         
         MVC   DS_MEDDS,FFTDATA+3                                               
         J     GETDDT16                                                         
         DROP  R3,RE                                                            
*&&                                                                             
         USING PMDRECD,R3                                                       
GETDDT04 LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES             Read media record to get                
         MVI   PMDKTYP,PMDKTYPQ         media description                       
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(RF)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JNE   *+2                                                              
         L     R3,AIO4                                                          
         LA    R3,PMDRFST                                                       
*                                                                               
         USING PMDELD,R3                                                        
GETDDT06 CLI   PMDEL,0                                                          
         JE    *+2                                                              
         CLI   PMDEL,PMDELQ                                                     
         JE    GETDDT08                                                         
         LLC   R0,PMDLN                                                         
         AR    R3,R0                                                            
         J     GETDDT06                                                         
                                                                                
GETDDT08 MVC   DS_MEDDS(L'PMDDESC),PMDDESC Save media code description          
*                                                                               
         CLI   SJ.TRNKCUNT,C'3'         Is retail billing in use                
         JNE   GETDDT16                 No                                      
U3       USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES             Read the unit 3 account for             
         MVC   U3.ACTKCPY,CUXCPY          debtor account                        
         MVC   U3.ACTKULA,SJ.TRNKULC                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   *+2                                                              
         LHI   R1,IOGET+IOMST+IO4                                               
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
         L     R3,AIO4                                                          
         LA    RF,U3.ACTRFST                                                    
*                                                                               
         USING RBRELD,RF                                                        
GETDDT10 CLI   RBREL,0                  Get debtor payment details              
         JE    *+2                                                              
         CLI   RBREL,RBRELQ                                                     
         JE    GETDDT14                                                         
GETDDT12 LLC   R0,RBRLN                                                         
         AR    RF,R0                                                            
         J     GETDDT10                                                         
*                                                                               
GETDDT14 MVC   DS_SRACT(L'RBRRECB),RBRRECB Save debtor account                  
                                                                                
SR       USING TRNRECD,R3                                                       
GETDDT16 LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES             Read the SR postings for this           
         MVC   SR.TRNKCPY,CUXCPY                        reference               
         MVC   SR.TRNKULA,SJ.TRNKULC                                            
         CLC   DS_SRACT,SPACES          Do we have a debtor account             
         JNH   *+10                           from retail billing               
         MVC   SR.TRNKULA,DS_SRACT      Yes - use this                          
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JZ    *+10                                                             
         MVC   SR.TRNKOFF,DS_JBOFF                                              
         MVC   SR.TRNKCACT,DS_MEDDS                                             
         CLI   BYTE1,0             First time try media name                    
         JE    GETDDT17                                                         
                                                                                
         CLC   DS_MEDDS,PRODLIT    If reading the same key - Skip               
         JE    GETDDT42                                                         
         MVC   SR.TRNKCACT,PRODLIT Second time try production                   
GETDDT17 MVC   SR.TRNKDATE,SJ.TRNKDATE                                          
         MVC   SR.TRNKREF,SJ.TRNKREF                                            
         XC    SR.TRNKSBR,SR.TRNKSBR                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     GETDDT20                                                         
*                                                                               
GETDDT18 LA    R3,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
*                                                                               
GETDDT20 CLC   IOKEYSAV(TRNKSBR-TRNRECD),SR.TRNKEY                              
         JE    GETDDT22            Matches key we read for                      
         CLI   BYTE1,0             If this the first time                       
         JNE   GETDDT42            No                                           
         MVI   BYTE1,1             Yes - set 1 for another loop                 
         J     GETDDT16                                                         
*                                                                               
GETDDT22 TM    SR.TRNKSTAT,TRNSREVS                                             
         JNZ   GETDDT18                                                         
         LHI   R1,IOGET+IOMST+IO4                                               
         TM    SR.TRNKSTAT,TRNSARCH                                             
         JZ    *+8                                                              
         LHI   R1,IOGET+IOARC+IO4                                               
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
         L     R3,AIO4                                                          
         LA    RF,SR.TRNRFST                                                    
*                                                                               
         USING TRNELD,RF                                                        
GETDDT24 CLI   TRNEL,0                  Get debtor payment details              
         JE    GETDDT18                                                         
         CLI   TRNEL,TRNELQ                                                     
         JE    GETDDT28                                                         
         CLI   TRNEL,RALELQ                                                     
         JE    GETDDT34                                                         
GETDDT26 LLC   R0,TRNLN                                                         
         AR    RF,R0                                                            
         J     GETDDT24                                                         
*                                                                               
GETDDT28 ZAP   DUB,TRNAMNT              T/X amount                              
         TM    TRNSTAT,TRNSDR           If debit make amount negative           
         JZ    GETDDT30                                                         
         MP    DUB,PMONE                                                        
GETDDT30 CLI   TRNTYPE,TRNTBILL                                                 
         JE    GETDDT32                                                         
         CLI   TRNTYPE,TRNTMEBL                                                 
         JE    GETDDT32                                                         
         CLI   TRNTYPE,TRNTCLBL                                                 
         JNE   GETDDT26                                                         
GETDDT32 ZAP   DS_TOTBL,DUB                                                     
         MP    DS_TOTBL,PMONE                                                   
         J     GETDDT26                                                         
*                                                                               
         USING RALELD,RF                                                        
GETDDT34 CLI   RALTYPE,RALTWOF          Check what type of debtor               
         JNE   GETDDT36                 allocation                              
         AP    WCS_BWAM,DUB             TRNAMNT is write-off                    
         AP    WCS_BWNA,DUB                                                     
         J     GETDDT26                                                         
*                                                                               
GETDDT36 CLI   RALTYPE,RALTALC                                                  
         JNE   GETDDT38                                                         
         AP    WCS_BDAM,DUB             TRNAMNT is regular allocation           
         AP    WCS_BDNA,DUB                                                     
         CLC   WCS_BPDT,RALADEP                                                 
         JH    GETDDT26                                                         
         MVC   WCS_BPDT,RALADEP         Make sure latest date stored            
         J     GETDDT26                                                         
*                                                                               
GETDDT38 CLI   RALTYPE,RALTOFS                                                  
         JNE   GETDDT40                                                         
         AP    WCS_BDAM,DUB             TRNAMNT is offset                       
         AP    WCS_BDNA,DUB                                                     
         CLC   WCS_BPDT,RALODAT                                                 
         JH    GETDDT26                                                         
         MVC   WCS_BPDT,RALODAT         Make sure latest date stored            
         J     GETDDT26                                                         
*                                                                               
GETDDT40 CLI   RALTYPE,RALTTFR          Transfer from                           
         JE    GETDDT26                                                         
         CLI   RALTYPE,RALTTTO          Transfer to                             
         JE    *+6                                                              
         DC    H'0'                     Bad RALELD!                             
         LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES             Read the SR posting                     
         MVC   SR.TRNKCPY,CUXCPY                                                
         MVC   SR.TRNKULA,RALTULA       Transfer/from/to account                
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JZ    *+10                                                             
         MVC   SR.TRNKOFF,DS_JBOFF                                              
         MVC   SR.TRNKCACT,DS_MEDDS     Media reference                         
         CLI   BYTE1,0                                                          
         JE    *+10                                                             
         MVC   SR.TRNKCACT,PRODLIT                                              
         MVC   SR.TRNKDATE,SJ.TRNKDATE  Transaction date                        
         MVC   SR.TRNKREF,SJ.TRNKREF    Transaction reference                   
         XC    SR.TRNKSBR,SR.TRNKSBR                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     GETDDT20                     Read for new acc as well            
*                                                                               
GETDDT42 CP    DS_TOTBL,PZERO      Did we find a debtor bill                    
         JE    GETDDT43            No                                           
         CP    DS_TOTBL,WCS_BGRS   Does the gross from SJ and SR differ         
         JE    GETDDT43            No                                           
         ZAP   MYPL16,WCS_BGRS     Yes - work out the proportion that           
         ZAP   MYPL8,DS_TOTBL       applies to this bill on SJ                  
         MP    MYPL16,WCS_BDAM     Multiply net by gross allocation             
*&&UK*&& SRP   MYPL16,5,0                                                       
*&&US*&& SRP   MYPL16,3,0                                                       
         DP    MYPL16,MYPL8        Divide by gross amount                       
*&&UK*&& SRP   MYPL16(8),64-5,5                                                 
*&&US*&& SRP   MYPL16(8),64-3,5                                                 
         ZAP   WCS_BDAM,MYPL16(8)                                               
*                                                                               
         ZAP   MYPL16,WCS_BGRS                                                  
         ZAP   MYPL8,DS_TOTBL                                                   
         MP    MYPL16,WCS_BWAM                                                  
*&&UK*&& SRP   MYPL16,5,0                                                       
*&&US*&& SRP   MYPL16,3,0                                                       
         DP    MYPL16,MYPL8                                                     
*&&UK*&& SRP   MYPL16(8),64-5,5                                                 
*&&US*&& SRP   MYPL16(8),64-3,5                                                 
         ZAP   WCS_BWAM,MYPL16(8)                                               
*                                                                               
GETDDT43 ZAP   WCS_BDNA,WCS_BDAM        Assume no VAT and make net              
         ZAP   WCS_BWNA,WCS_BWAM           amounts the same                     
                                                                                
*&&US                                                                           
         CP    WCS_COB,PZERO                                                    
         JNE   *+14                                                             
*&&                                                                             
         CP    WCS_VAT,PZERO                                                    
         JE    GETDDT46                                                         
*                                                                               
* Calculate net amount of cash received and write offs                          
*                                                                               
         CP    WCS_BDAM,PZERO      Any allocation amount                        
         JE    GETDDT44                                                         
         ZAP   MYPL16,WCS_BGRS                                                  
         ZAP   MYPL8,WCS_BGRS                                                   
*        ZAP   MYPL8,DS_TOTBL                                                   
         SP    MYPL16,WCS_VAT                                                   
*&&US*&& SP    MYPL16,WCS_COB      In NA take away commission                   
         MP    MYPL16,WCS_BDAM     Multiply net by gross allocation             
         SRP   MYPL16,5,0                                                       
         DP    MYPL16,MYPL8        Divide by gross amount                       
         SRP   MYPL16(8),64-5,5                                                 
         ZAP   WCS_BDNA,MYPL16(8)                                               
                                                                                
GETDDT44 CP    WCS_BWAM,PZERO      Any write off amount                         
         JE    GETDDT46                                                         
         ZAP   MYPL16,WCS_BGRS                                                  
         ZAP   MYPL8,WCS_BGRS                                                   
*        ZAP   MYPL8,DS_TOTBL                                                   
         SP    MYPL16,WCS_VAT                                                   
*&&US*&& SP    MYPL16,WCS_COB      In NA take away commission                   
         MP    MYPL16,WCS_BWAM     Multiply net by gross write off              
         SRP   MYPL16,5,0                                                       
         DP    MYPL16,MYPL8        Divide by gross amount                       
         SRP   MYPL16(8),64-5,5                                                 
         ZAP   WCS_BWNA,MYPL16(8)                                               
                                                                                
GETDDT46 ZAP   DUB,WCS_BDAM             Found all, finish off                   
         AP    DUB,WCS_BWAM                                                     
         CP    DUB,PZERO                Anything paid?                          
         JNE   GETDDT48                                                         
         MVI   WCS_BDST,WCS_BDSO        No, outstanding                         
         J     EXITY                                                            
*                                                                               
GETDDT48 CP    DUB,WCS_BGRS             Everything paid?                        
         JNL   GETDDT50                                                         
         MVI   WCS_BDST,WCS_BDSP        No                                      
         J     EXITY                                                            
*                                                                               
GETDDT50 MVI   WCS_BDST,WCS_BDSF        Fully paid                              
         J     EXITY                                                            
         DROP  SJ,SR,RF                                                         
         EJECT                                                                  
***********************************************************************         
* Set DS_ISDBT, DS_TAMT from debit/CREDIT VALUES                                
***********************************************************************         
         SPACE 1                                                                
         USING WCS_D,R4                                                         
GETISDB  NTR1  BASE=*,LABEL=NO                                                  
         LAY   R4,EX_BUFF                                                       
         MVI   DS_ISDBT,C'N'                                                    
         ZAP   DS_TAMT,PZERO                                                    
         CP    WCS_DBA,PZERO                                                    
         JNE   GETIS10                                                          
         CP    WCS_CRA,PZERO                                                    
         JNE   GETIS10                                                          
*                                                                               
GETIS05  CP    WCS_CURD,PZERO      If both are zero do we have                  
         JE    EXITY                 debit currency amount                      
         MVI   DS_ISDBT,C'Y'       If so set is debit                           
         J     EXITY                                                            
*                                                                               
GETIS10  ZAP   DS_TAMT,WCS_DBA                                                  
         AP    DS_TAMT,WCS_CRA                                                  
         CP    WCS_DBA,PZERO                                                    
         JE    EXITY                                                            
         MVI   DS_ISDBT,C'Y'                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Getopt calls for QS_ACCT                                                      
***********************************************************************         
         SPACE 1                                                                
         USING WCS_D,R4                                                         
GETOPTM  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETOPTM'                                                      
         L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOSELCUL(1),AGENCY                                               
         MVC   GOSELCUL+1(1),QS_UNIT                                            
         MVC   GOSELCUL+2(1),QS_LDGR                                            
         MVC   GOSELCLI,DS_ALEV1                                                
         MVC   GOSELPRO,DS_ALEV2                                                
         MVC   GOSELJOB,DS_ALEV3                                                
         MVC   GOCTRY,CUCTRY                                                    
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Getopt calls for DS_ACCT + workcode (in LASTWC)                               
***********************************************************************         
         SPACE 1                                                                
         USING WCS_D,R4                                                         
GETOPTW  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL(L'AGENCY),AGENCY                                        
         MVC   GOSELCUL+1(2),QS_UNIT                                            
         MVC   GOSELCLI,SPACES                                                  
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         MVC   GOSELCLI(0),DS_ACCT                                              
         EX    RE,*-6                                                           
         MVC   GOSELPRO,SPACES                                                  
         LLC   R1,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         LA    RE,DS_ACCT(R1)                                                   
         MVC   GOSELPRO(0),0(RE)                                                
         EX    RF,*-6                                                           
         MVC   GOSELJOB,SPACES                                                  
         LLC   R1,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         SR    RF,R1                                                            
         CHI   RF,L'GOSELJOB                                                    
         JNH   *+8                                                              
         LA    RF,L'GOSELJOB                                                    
         SHI   RF,1                                                             
         LA    RE,DS_ACCT(R1)                                                   
         MVC   GOSELJOB(0),0(RE)                                                
         EX    RF,*-6                                                           
         MVI   GOANYWC,YESQ                                                     
         MVC   GOSELWC,LASTWC                                                   
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOCTRY,CUCTRY                                                    
         L     RF,ACOMFACS                                                      
                                                                                
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+10                                                             
         MVC   GOACOVL,VCOVAIL                                                  
         MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Get TUP profile and 'translate' filter out based on GODITSB and     *         
* GODITSN and GODITSR (must be called after getopt call)              *         
***********************************************************************         
GETTUP   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETTUP*'                                                      
         USING COBLOCKD,R2                                                      
         USING GOXBLKD,R4                                                       
         L     R2,ACOBLOCK         Get cost allocation profiles                 
         L     R4,AGOXBLCK                                                      
         GOTOR (#CSTPRF,ACSTPRF),DMCB,SPACES                                    
*                                                                               
         LA    R1,RTIMTUPT                                                      
         LA    R0,TMSTNMB          Number of settings r+b                       
         LA    R3,GOETMR                                                        
         CLI   QS_ESTC,YESQ        are we doing estimate checking?              
         JE    GETTUP2             yes                                          
         LA    R0,TMSTNMN          else r+b+n                                   
         LA    R3,GODITSR                                                       
         USING COGOTABD,R4                                                      
GETTUP2  LA    R4,COGOTAB                                                       
*                                                                               
GETTUP4  CLC   COGOCAP,COTUP       Find match on COTUP                          
         JNE   GETTUP6             Get next entry in table if no match          
         CLC   COGOGOPT,0(R3)      Find match on getopt setting                 
         JE    GETTUP8                                                          
GETTUP6  LA    R4,COGOTABL(R4)                                                  
         J     GETTUP4                                                          
*                                                                               
GETTUP8  MVC   0(TMSTTABL,R1),COGOMPTY Set whether to read passives             
         LA    R3,1(R3)                                                         
         LA    R1,TMSTTABL(R1)     Point R1 to next type of time                
         BCT   R0,GETTUP2                                                       
*                                                                               
GETTUPX  J     EXITY                                                            
         DROP  R2,R4                                                            
COGOTAB  DS    0X                                                               
         DC    AL1(COSAVED,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOINPR,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOSUB,NOQ,0,0,0)                                     
         DC    AL1(COSAVED,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COSAVED,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COSUBMD,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COSUBMD,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COSUBMD,GOINPR,YESQ,0,0,0)                                   
         DC    AL1(COSUBMD,GOSUB,NOQ,0,0,0)                                     
         DC    AL1(COSUBMD,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COSUBMD,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COCLAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COCLAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COCLAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP)              
         DC    AL1(0,TIMSAWAP)                                                  
         DC    AL1(COCLAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,0,TIMSAWAP)              
         DC    AL1(COCLAPR,GOMANPAP,NOQ,TIMSMAAP,0,TIMSAWAP)                    
         DC    AL1(COCLAPR,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COLMAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COLMAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COLMAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP)              
         DC    AL1(TIMSMAAP,0)                                                  
         DC    AL1(COLMAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,TIMSMAAP,0)              
         DC    AL1(COLMAPR,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COLMAPR,GOCLIPAP,NOQ,TIMSPAPP,0,TIMSAWAP)                    
         DC    AL1(COFUAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COFUAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COFUAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP,0,0)          
         DC    AL1(COFUAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,0,0)                     
         DC    AL1(COFUAPR,GOMANPAP,NOQ,TIMSMAAP,0,0)                           
         DC    AL1(COFUAPR,GOCLIPAP,NOQ,TIMSPAPP,0,TIMSFAPP)                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* Initialise WCS_D record for any type (key and accumulators)                   
*                                                                               
* ENTRY: R1 points to record are to clear.                                      
* WCS_TYPE should be set                                                        
***********************************************************************         
         SPACE 1                                                                
         USING WCS_D,R4                                                         
CLRJBV   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*CLRJBV*'                                                      
                                                                                
         LR    R4,R1                point to rec                                
         LR    R0,R1                                                            
         LA    R1,WCS_DLNQ                                                      
CLRJBV20 XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                  initialise packed numbers                    
         LA    RE,WCS_SPNS                                                      
         LA    RF,WCS_TPNQ                                                      
         MVI   WCS_ELET,0          TERMINATOR (NO SUBELEMENTS)                  
*                                                                               
CLRJBV50 ZAP   0(L'WCS_SPNS,RE),=P'0'                                           
         LA    RE,L'WCS_SPNS(RE)                                                
         JCT   RF,CLRJBV50                                                      
*                                                                               
CLRJBVX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate SJ account for JobBag module                                         
***********************************************************************         
         SPACE 1                                                                
SJINIT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SJINIT*'                                                      
*                                                                               
         OC    QS_ACCT,SPACES                                                   
         OC    DS_JBOFF,SPACES                                                  
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,QS_UNIT                                                   
         GOTOR (#SETLDG,ASETLDG)                                                
         LLC   RF,LDGAL2                                                        
         LA    RF,QS_ACCT(RF)                                                   
         CLI   0(RF),C' '                                                       
         JH    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$INJOB)  Not a job account - issue error         
         J     EXITN                                                            
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUBJA                                                    
         JZ    SJINT02                                                          
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear entries                                
         LHI   R1,DS_SUBJM*L'ACTKACT                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               Clear area                                   
         XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUESA                                                    
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear entries                                
         LHI   R1,DS_SUESM*L'ACTKACT                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               Clear area                                   
         XR    R2,R2               Point to list in wmp                         
         ICM   R2,7,DS_SUEVA                                                    
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear entries                                
         LHI   R1,DS_SUEVM*L'ACTKACT                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               Clear area                                   
         DROP  R2                                                               
*                                                                               
SJINT02  DS    0H                                                               
*&&US                                                                           
         USING JB_D,R2                                                          
         LA    R2,DS_ESCEA                                                      
         LA    RF,DS_SUBJM                                                      
SJINT04  ZAP   JB_TOTBL,PZERO                                                   
         ZAP   JB_TOTAL,PZERO                                                   
         LA    R2,JB_LNQ(R2)                                                    
         JCT   RF,SJINT04                                                       
         DROP  R2                                                               
*&&                                                                             
         CLI   QS_EXPRT,YESQ       If exporting need names                      
         JE    *+12                                                             
         CLI   OFFIND,NONEQ        skip for non office agencies                 
         JE    SJINT32                                                          
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY            Read client                                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),QS_ACCT                                               
         EX    RE,0(RF)                                                         
         MVC   CLI_COD,ACTKACT     Save client code                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SJINT10                                                          
         MVC   LP_ERROR,=AL2(AE$INCLI)                                          
         J     EXITN                                                            
                                                                                
SJINT10  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
                                                                                
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
SJINT12  CLI   PPREL,0             Set office from client                       
         JE    SJINT20                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SJINT16                                                          
         CLI   PPREL,NAMELQ                                                     
         JE    SJINT18                                                          
SJINT14  IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     SJINT12                                                          
                                                                                
SJINT16  MVC   DS_JBOFF,PPRGAOFF                                                
         OC    DS_JBOFF,SPACES                                                  
         J     SJINT14                                                          
                                                                                
         USING NAMELD,R2                                                        
SJINT18  XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,3                                                             
         BASR  RE,0                                                             
         MVC   CLI_NAM(0),NAMEREC                                               
         EX    RF,0(RE)                                                         
         J     SJINT14                                                          
                                                                                
         USING ACTRECD,R2           Read product                                
SJINT20  LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         LLC   RE,PPROLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),QS_ACCT                                               
         EX    RE,0(RF)                                                         
         LA    R3,QS_ACCT                                                       
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         AR    R3,RE               R3=A(product code)                           
         SR    RF,RE                                                            
         SHI   RF,1                RF=Length of product code                    
         BASR  RE,0                                                             
         MVC   PRD_COD(0),0(R3)                                                 
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SJINT22                                                          
         MVC   LP_ERROR,=AL2(AE$INPRO)                                          
         J     EXITN                                                            
                                                                                
SJINT22  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
                                                                                
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
SJINT24  CLI   PPREL,0                                                          
         JE    SJINT32                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SJINT28                                                          
         CLI   PPREL,NAMELQ                                                     
         JE    SJINT30                                                          
SJINT26  IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     SJINT24                                                          
                                                                                
SJINT28  CLC   PPRGAOFF,SPACES      SET OFFICE FROM PROD, IF SET                
         JNH   SJINT26                                                          
         MVC   DS_JBOFF,PPRGAOFF                                                
         OC    DS_JBOFF,SPACES                                                  
         J     SJINT26                                                          
                                                                                
         USING NAMELD,R2                                                        
SJINT30  XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,3                                                             
         BASR  RE,0                                                             
         MVC   PRD_NAM(0),NAMEREC                                               
         EX    RF,0(RE)                                                         
         J     SJINT26                                                          
                                                                                
         USING ACTRECD,R2                                                       
SJINT32  LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT,QS_ACCT                                                  
         LA    R3,QS_ACCT                                                       
         LA    RF,L'ACTKACT                                                     
         LLC   RE,PPROLEN                                                       
         AR    R3,RE               R3=A(job code)                               
         SR    RF,RE                                                            
         CHI   RF,6                Job code can never be longer than            
         JNH   *+8                   six                                        
         LA    RF,6                                                             
         SHI   RF,1                RF=Length of job code                        
         BASR  RE,0                                                             
         MVC   JOB_COD(0),0(R3)                                                 
         EX    RF,0(RE)                                                         
         L     R1,=AL4(IORDD+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    SJINT34                                                          
         MVC   LP_ERROR,=AL2(AE$INJOB)                                          
         TM    IOERR,IOEDEL        Could be deleted if draft to live            
         JZ    *+10                                                             
         MVC   LP_ERROR,=AL2(AE$JBNEX)                                          
         J     EXITN                                                            
                                                                                
SJINT34  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,QS_ACCT),DS_SUBJI,             +        
               DS_SUBJM,LP_D                                                    
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING JOBELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
SJINT36  CLI   JOBEL,0                                                          
         JE    SJINT50                                                          
         CLI   JOBEL,JOBELQ                                                     
         JE    SJINT42                                                          
         CLI   JOBEL,NAMELQ                                                     
         JE    SJINT40                                                          
SJINT38  LLC   R0,JOBLN                                                         
         AR    R2,R0                                                            
         J     SJINT36                                                          
                                                                                
         USING NAMELD,R2                                                        
SJINT40  XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,3                                                             
         BASR  RE,0                                                             
         MVC   JOB_NAM(0),NAMEREC                                               
         EX    RF,0(RE)                                                         
         J     SJINT38                                                          
                                                                                
         USING JOBELD,R2                                                        
SJINT42  MVC   DS_JOPDP,JOBADATE                                                
         CLC   JOBODATE,SPACES                                                  
         JNH   *+10                                                             
         MVC   DS_JOPDP,JOBODATE                                                
         GOTOR VDATCON,DMCB,(1,DS_JOPDP),(2,DS_JOPDC)                           
         TM    JOBSTA2,JOBSMST                                                  
         JZ    SJINT38                                                          
                                                                                
         OI    DS_IND1,DS_IMSTJ    Set is a master job                          
* add subjobs to estimate read list                                             
K        USING MJBPASD,IOKEY                                                    
         XC    K.MJBPAS,K.MJBPAS                                                
         MVI   K.MJBPTYP,MJBPTYPQ                                               
         MVI   K.MJBPSUB,MJBPSUBQ                                               
         MVC   K.MJBPCPY,CUXCPY                                                 
         MVC   K.MJBPACT,QS_ACCT                                                
         MVC   CSVKEY1,K.MJBPAS                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     SJINT46                                                          
*                                                                               
SJINT44  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
*                                                                               
SJINT46  CLC   K.MJBPAS(MJBPSUBJ-MJBPAS),CSVKEY1                                
         JNE   SJINT38                                                          
         OI    DS_IND1,DS_ISUBJ    Set a sub job has been found                 
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,K.MJBPSUBJ),DS_SUBJI,          +        
               DS_SUBJM,LP_D                                                    
         MVC   DS_ALEV1,SPACES     split ACCOUNT BY LEVEL                       
         MVC   DS_ALEV2,SPACES                                                  
         MVC   DS_ALEV3,SPACES                                                  
         MVC   DS_ALEV4,SPACES                                                  
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         MVC   DS_ALEV1(0),K.MJBPSUBJ                                           
         EX    RE,*-6                                                           
*                                                                               
         OC    LDGAL2,LDGAL2                                                    
         JZ    SJINT48                                                          
         LLC   R1,LDGAL1                                                        
         LA    RE,K.MJBPSUBJ(R1)                                                
         LLC   RF,LDGAL2                                                        
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         MVC   DS_ALEV2(0),0(RE)                                                
         EX    RF,*-6                                                           
*                                                                               
         OC    LDGAL3,LDGAL3                                                    
         JZ    SJINT48                                                          
         LLC   R1,LDGAL2                                                        
         LA    RE,K.MJBPSUBJ(R1)                                                
         LA    RF,L'ACTKACT                                                     
         SR    RF,R1                                                            
         CHI   RF,6                                                             
         JNH   *+8                                                              
         LA    RF,6                                                             
         SHI   RF,1                                                             
         MVC   DS_ALEV3(0),0(RE)                                                
         EX    RF,*-6                                                           
*                                                                               
SJINT48  MVC   DS_CPJES(L'ESTKCLI),DS_ALEV1                                     
         MVC   DS_CPJES+L'ESTKCLI(L'ESTKPRO),DS_ALEV2                           
*&&UK*&& MVC   DS_CPJES+L'ESTKCLI+L'ESTKPRO(L'ESTKJOB),DS_ALEV3                 
*&&US                                                                           
         MVI   DS_CPJES+L'ESTKCLI+L'ESTKPRO,0                                   
         MVC   DS_CPJES+L'ESTKCLI+L'ESTKPRO+1(L'ESTKJOB),DS_ALEV3               
*&&                                                                             
         MVC   DS_CPJEV(L'EVEKCLI),DS_ALEV1                                     
         MVC   DS_CPJEV+L'EVEKCLI(L'EVEKPRO),DS_ALEV2                           
         MVC   DS_CPJEV+L'EVEKCLI+L'EVEKPRO(L'EVEKJOB),DS_ALEV3                 
         GOTOR LP_AAWMP,DMCB,(L'DS_CPJES,DS_CPJES),DS_SUESI,DS_SUESM,  +        
               LP_D                                                             
         GOTOR LP_AAWMP,DMCB,(L'DS_CPJEV,DS_CPJEV),DS_SUEVI,DS_SUEVM,  +        
               LP_D                                                             
         J     SJINT44                                                          
         DROP  K                                                                
* Finish by adding master to estimate read list                                 
SJINT50  MVC   DS_ALEV1,SPACES     split ACCOUNT BY LEVEL                       
         MVC   DS_ALEV2,SPACES                                                  
         MVC   DS_ALEV3,SPACES                                                  
         MVC   DS_ALEV4,SPACES                                                  
         LLC   RE,LDGAL1                                                        
         AHI   RE,-1                                                            
         MVC   DS_ALEV1(0),QS_ACCT                                              
         EX    RE,*-6                                                           
*                                                                               
         OC    LDGAL2,LDGAL2                                                    
         JZ    SJINT60                                                          
         LLC   R1,LDGAL1                                                        
         LA    RE,QS_ACCT(R1)                                                   
         XR    RF,RF                                                            
         IC    RF,LDGAL2                                                        
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         MVC   DS_ALEV2(0),0(RE)                                                
         EX    RF,*-6                                                           
*                                                                               
         OC    LDGAL3,LDGAL3                                                    
         JZ    SJINT60                                                          
         LLC   R1,LDGAL2                                                        
         LA    RE,QS_ACCT(R1)                                                   
         LA    RF,L'ACTKACT                                                     
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         MVC   DS_ALEV3(0),0(RE)                                                
         EX    RF,*-6                                                           
*                                                                               
SJINT60  MVC   DS_CPJES(L'ESTKCLI),DS_ALEV1                                     
         MVC   DS_CPJES+L'ESTKCLI(L'ESTKPRO),DS_ALEV2                           
*&&UK*&& MVC   DS_CPJES+L'ESTKCLI+L'ESTKPRO(L'ESTKJOB),DS_ALEV3                 
*&&US                                                                           
         MVI   DS_CPJES+L'ESTKCLI+L'ESTKPRO,0                                   
         MVC   DS_CPJES+L'ESTKCLI+L'ESTKPRO+1(L'ESTKJOB),DS_ALEV3               
*&&                                                                             
         MVC   DS_CPJEV(L'EVEKCLI),DS_ALEV1                                     
         MVC   DS_CPJEV+L'EVEKCLI(L'EVEKPRO),DS_ALEV2                           
         MVC   DS_CPJEV+L'EVEKCLI+L'EVEKPRO(L'EVEKJOB),DS_ALEV3                 
         GOTOR LP_AAWMP,DMCB,(L'DS_CPJES,DS_CPJES),DS_SUESI,DS_SUESM,  +        
               LP_D                                                             
         GOTOR LP_AAWMP,DMCB,(L'DS_CPJEV,DS_CPJEV),DS_SUEVI,DS_SUEVM,  +        
               LP_D                                                             
*                                                                               
         GOTOR GETOPTM                                                          
         GOTOR GETTUP                                                           
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
********************************************************************            
* Set commission rate/comm amount for current record               *            
*                                                                  *            
********************************************************************            
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING WCS_D,R4                                                         
SETALL   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SETALL*'                                                      
                                                                                
         CLC   WCS_WCOD,BILWC      No commission or VAT on billed               
         JE    SETALLX                                                          
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB         Yes - Getopt block will still                
                                                                                
SALL015  CLC   WCS_WCOD,LASTWC     same w/c as before?                          
         JNE   SALL020                                                          
         ZAP   WCS_COMR,LASTCOMR   contain the commission rate                  
         MVC   WCS_VATR,LASTWCTX   Saved tax rate                               
         J     SALL060                                                          
*                                                                               
SALL020  MVC   LASTWC,WCS_WCOD    get commission                                
         GOTOR GETOPTW                                                          
         ZAP   WCS_COMR,GOAGYCOM                                                
         ZAP   LASTCOMR,GOAGYCOM                                                
         MVC   BYTE2,GOTAXCOD                                                   
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      germany may have internal commission         
         JNE   SALL030               rate, which can override                   
         CLI   GOSELWC,C'Z'                                                     
         JH    SALL030                                                          
         OC    GOPROVPC,GOPROVPC                                                
         JZ    SALL030                                                          
         ZAP   WCS_COMR,GOPROVPC                                                
         ZAP   LASTCOMR,GOPROVPC                                                
         DROP  R3                                                               
*                                                                               
         USING VTCD,R3                                                          
SALL030  LAY   R3,VATCNBLK         build VATICAN call block                     
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUXCPY                                                    
         MVC   VTCOFFC,DS_JBOFF                                                 
         MVC   VTCCOMF,ACOMFACS                                                 
         MVC   VTCINVD,DS_TODP                                                  
         MVC   VTCTYPE,BYTE2                                                    
         MVC   VTCCPYS1,SCPYEL+CPYSTAT1-CPYELD                                  
         MVC   VTCSGOPO,LDGAOP        Should be SG off position                 
                                                                                
         GOTO1 VATICAN,VTCD                                                     
         JE    SALL040                                                          
         MVC   ROUERRV,VTCERR                                                   
         J     SETALLN                                                          
                                                                                
SALL040  TM    VTCINDS,VTCINA         tax is not applicable                     
         JZ    SALL045                                                          
         MVC   ROUERRV,=AL2(AE$VRNDE) VAT rates not defined for date            
         J     SETALLN                                                          
                                                                                
SALL045  MVC   LASTWCTX,VTCRATE                                                 
         MVC   WCS_VATR,VTCRATE                                                 
         DROP  R3                                                               
*&&                                                                             
         GOTOR GETOPTM              Reset account get opt settings              
                                                                                
*                                                                               
* Calculate commission                                                          
*                                                                               
SALL060  CLC   WCS_WCOD,BILWC       No commission on billed                     
         JE    SETALLX                                                          
         CLI   WCS_APST,WCS_ASFA    Only want commission on approved            
         JNE   SETALLX                                                          
*                                                                               
         ZAP   MYPL16,WCS_DBA                                                   
         AP    MYPL16,WCS_CRA                                                   
         MP    MYPL16,WCS_COMR                                                  
         SRP   MYPL16,64-6,5     Rate is 4 decimal places                       
         AP    WCS_COM,MYPL16                                                   
SETALLX  J     EXITY                                                            
*                                                                               
SETALLN  J     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*  USE PRORATA TO GET BILLED VALUES AT T/X LEVEL                                
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING WCS_D,R4                                                         
SETPRAT  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SETPRAT'                                                      
         USING PRORATAD,R3                                                      
         CLC   WCS_WCOD,BILWC      Bill t/x had actuals                         
         JE    SETPRAX                                                          
*                                                                               
         USING GOBLOCKD,RE         tinker with GOBLOCK for PRORATA              
         L     RE,AGOBLOCB                                                      
         ZAP   MYPL16,GOAGYCOM     restore later                                
         ZAP   GOAGYCOM,PZERO                                                   
         DROP  RE                                                               
*                                                                               
         L     R3,AJOBLOCK         point to cleared PRORATAD area               
         LR    R0,R3                                                            
         LA    R1,JBLOCKL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 VPRORATA,DMCB,IOADDR,AGOBLOCB,ACOMFACS,0,PRORATAD,0              
*                                                                               
         USING GOBLOCKD,RE         restore GOBLOCK, save rebuilding             
         L     RE,AGOBLOCB                                                      
         ZAP   GOAGYCOM,MYPL16                                                  
         DROP  RE                                                               
*                                                                               
         MVI   WCS_BLST,3          Mark unbilled                                
         TM    PG$STAT,PG$FULLB+PG$PARTB      billed?                           
         JZ    SETPRAX                                                          
                                                                                
SETPRA9  DS    0H                                                               
         AP    WCS_ALA,PA$NETBL    Net billed (billed allocation)               
         AP    WCS_COB,PA$COMBL    Commission billed                            
         AP    WCS_FCAL,PB$NETBL   Net billed in currency                       
         AP    WCS_COMF,PB$COMBL   Commission billed in currency                
         MVI   WCS_BLST,2          Fully billed                                 
         TM    PG$STAT,PG$FULLB                                                 
         JNZ   SETPRAX                                                          
         MVI   WCS_BLST,1          Partly                                       
                                                                                
SETPRAX  J     EXITY                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* process PTAELS on a transaction, save bill numbers                            
***********************************************************************         
PROBIL   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*PROBIL*'                                                      
*                                                                               
         LAY   R4,EX_BUFF                                                       
         USING WCS_D,R4                                                         
         XC    WCS_ACUR,WCS_ACUR                                                
         USING PTAELD,R3                                                        
         LR    R3,R2               A(TRANSACTION)                               
         AHI   R3,TRNRFST-TRNRECD                                               
*                                                                               
PROBIL20 LLC   R0,PTALN            find PTA elements on transaction             
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         JE    PROBILX             End of elements                              
*                                                                               
PROBIL30 CLI   PTAEL,PTAELQ        filter PTA element (may need fixes)          
         JNE   PROBIL20                                                         
         CLI   PTATYPE,PTATRAL                                                  
         JNE   PROBIL20                                                         
*&&US                                                                           
         TM    PTASTAT1,PTASREVS                                                
         JZ    *+8                                                              
         MVI   WCS_IREV,YESQ                                                    
*&&                                                                             
         TM    PTASTAT1,PTASPEND+PTASREVD                                       
         JNZ   PROBIL20                                                         
         OC    WCS_ACUR,WCS_ACUR                                                
         JNZ   PROBIL40                                                         
         MVC   WCS_ACUR,PTACUR     Billing currency                             
         J     PROBIL50                                                         
                                                                                
PROBIL40 CLC   WCS_ACUR,PTACUR     Is billing currency the same                 
         JE    PROBIL50                                                         
         MVC   WCS_ACUR,=C'*  '    No set mixed                                 
*                                                                               
PROBIL50 GOTOR ADDPTBN                                                          
         J     PROBIL20                                                         
*                                                                               
PROBILX  J     EXITY                                                            
         DROP  R3,R4                                                            
***********************************************************************         
* ADD PTAEL BILL NUMBER TO WCS_D REC                                            
* ENTRY : R3 IS PTAEL, R4 IS WCS_D REC                                          
***********************************************************************         
ADDPTBN  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*ADDPTBN'                                                      
         USING PTAELD,R3                                                        
         USING WCS_D,R4                                                         
*                                                                               
         LA    R2,WCS_TERM                                                      
         USING WCS_ELET,R2                                                      
APTBN10  CLI   WCS_ELET,0         Find end of current rec                       
         JE    APTBN30                                                          
         CLI   WCS_ELET,WCS_ELBQ                                                
         JNE   APTBN20                                                          
*        CLC   PTARBLNO,WCS_ELED   SKIP DUPES                                   
*        JE    EXITY                                                            
APTBN20  LLC   R0,WCS_ELEL                                                      
         AR    R2,R0                                                            
         J     APTBN10                                                          
*                                                                               
APTBN30  MVC   WCS_ELED(L'PTARBLNO),PTARBLNO    BUILD NEW ELEM                  
         MVI   WCS_ELET,WCS_ELBQ                                                
         MVI   WCS_ELEL,L'PTARBLNO+2                                            
         LA    R2,L'PTARBLNO(R2)                                                
         LA    R2,2(R2)                                                         
         MVI   WCS_ELET,0          SET NEW TERMINATOR                           
         LA    R2,1(R2)                                                         
         SR    R2,R4               WORK OUT NEW LENGTH OF REC                   
         STH   R2,WCS_LEN                                                       
         LA    RE,L'EX_BUFF                                                     
         CR    RE,R2                                                            
         JH    EXITY                                                            
         DC    H'0'                EX_BUFF TOO SMALL                            
         DROP  R2                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* Read estimate list from transactions for bill                                 
***********************************************************************         
RDESTL   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*RDESTL*'                                                      
*                                                                               
         LAY   R4,EX_BUFF          TSAR IO AREA FOR TRAN REC                    
         USING WCS_D,R4                                                         
         L     R3,AIO2             TSAR IO AREA FOR BILL REC                    
B        USING WCS_D,R3                                                         
*                                                                               
         L     R0,AIO2                                                          
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               Clear IO2                                    
*                                                                               
         LAY   RE,EX_BUFF                                                       
         LH    RF,WCS_LEN                                                       
         CHI   RF,IOLENQ                                                        
         JH    *+2                 Bill rec too big                             
         L     R0,AIO2                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               Copy bill rec to AIO2                        
*                                                                               
         CLC   B.WCS_ESTG,SPACES   ANY ESTIMATE ON BILL                         
         JNH   RDESTL04                                                         
         GOTOR ADDEST,B.WCS_ESTG                                                
*                                                                               
RDESTL04 LAY   R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     RDESTL12                                                         
*                                                                               
RDESTL08 LAY   R0,EX_BUFF          Set A(Returned record)                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
RDESTL12 TM    TSARERRS,TSEEOF                                                  
         JNZ   RDESTLX                                                          
*                                                                               
*      Filter by record type                                                    
*                                                                               
         CLI   WCS_TYPE,WCS_TRN                                                 
         JE    *+12                transaction recs (incl order t/x)            
         CLI   WCS_TYPE,WCS_FBAD                                                
         JNE   RDESTL08            Don't want this                              
         CLC   WCS_WCOD,BILWC                                                   
         JE    RDESTL08            Don't want billing                           
*                                                                               
         CLC   WCS_ESTG,SPACES     ANY ESTIMATE ON TRANSACTION                  
         JNH   RDESTL08                                                         
*                                                                               
         LA    RF,WCS_TERM                                                      
         USING WCS_ELET,RF                                                      
RDESTL20 CLI   WCS_ELET,0          Find bill in transaction rec                 
         JE    RDESTL08                                                         
         CLI   WCS_ELET,WCS_ELBQ   Test bill element                            
         JNE   RDESTL24                                                         
         CLC   WCS_ELED(L'PTARBLNO),B.WCS_LREF                                  
         JE    RDESTL30                                                         
                                                                                
RDESTL24 LLC   R0,WCS_ELEL                                                      
         AR    RF,R0                                                            
         J     RDESTL20                                                         
*                                                                               
RDESTL30 GOTOR ADDEST,WCS_ESTG                                                  
         J     RDESTL08                                                         
*                                                                               
RDESTLX  DS    0H                                                               
         MVC   WCS_KEY(WCS_TKYL),B.WCS_KEY RESTORE KEY                          
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
*                                                                               
         L     RE,AIO2                                                          
         LHI   RF,IOLENQ                                                        
         LAY   R0,EX_BUFF                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               Copy bill rec back to EXT_BUFF               
         J     EXITY                                                            
         DROP  B,R4,RF                                                          
         SPACE 2                                                                
***********************************************************************         
* ADD ESTIMATE NUMBER TO WC 99 BILL IN WCS_D REC                                
* ENTRY : R1 IS ESTIMATE NUMBER, R3 IS WCS_D REC                                
***********************************************************************         
ADDEST   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*ADDEST*'                                                      
         USING WCS_D,R3                                                         
*                                                                               
         LA    R2,WCS_TERM                                                      
         USING WCS_ELET,R2                                                      
ADDEST02 CLI   WCS_ELET,0          Find end of current rec                      
         JE    ADDEST06                                                         
         CLI   WCS_ELET,WCS_ELEQ   Test estimates                               
         JNE   ADDEST04                                                         
         CLC   WCS_ELED(L'WCS_ESTG),0(R1)                                       
         JE    EXITY               Skip duplicate                               
*                                                                               
ADDEST04 LLC   R0,WCS_ELEL                                                      
         AR    R2,R0                                                            
         J     ADDEST02                                                         
*                                                                               
ADDEST06 MVI   WCS_ELET,WCS_ELEQ   BUILD NEW ELEM                               
         MVI   WCS_ELEL,L'WCS_ESTG+2                                            
         MVC   WCS_ELED(L'WCS_ESTG),0(R1)                                       
         LA    R2,L'WCS_ESTG+2(,R2)                                             
         MVI   WCS_ELET,0          SET NEW TERMINATOR                           
         LA    R2,1(R2)                                                         
         SR    R2,R3               WORK OUT NEW LENGTH OF REC                   
         STH   R2,WCS_LEN                                                       
         LHI   RE,IOLENQ                                                        
         CR    RE,R2                                                            
         JH    EXITY                                                            
         DC    H'0'                EX_BUFF TOO SMALL                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROLL A TRANSACTION INTO A SUMMARY RECORD                                      
*                                                                               
* AIO2 FOR THE SUMMARY RECORD                                                   
* EX_BUFF FOR THE TRANSACTION RECORD                                            
***********************************************************************         
         SPACE 1                                                                
SUMTRN   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SUMTRN*'                                                      
         USING WCS_D,R4                                                         
STR      USING WCS_D,R2                                                         
         L     R2,AIO2                                                          
         LAY   R4,EX_BUFF                                                       
         LA    RF,WCS_SPNS                                                      
         LA    RE,WCS_SPNQ            NUMBER OF ACCUMS TO ROLL                  
         LA    R1,STR.WCS_SPNS                                                  
SUMTRN10 AP    0(L'WCS_SPNS,R1),0(L'WCS_SPNS,RF)                                
         LA    RF,L'WCS_SPNS(RF)                                                
         LA    R1,L'WCS_SPNS(R1)                                                
         JCT   RE,SUMTRN10                                                      
*                                                                               
         CLI   WCS_TYPE,WCS_TRN                                                 
         JNE   SUMTRNX                                                          
         ZAP   STR.WCS_COMR,WCS_COMR                                            
         MVC   STR.WCS_VATR,WCS_VATR                                            
         MVI   STR.WCS_BTYP,X'FF'      marker                                   
*                                                                               
SUMTRNX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIOBUF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIOBUF'                                                      
                                                                                
         XC    TSAROBUF(TSPNEWL),TSAROBUF                                       
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR                                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,ONEK                                                          
         OC    T.TSBUFFL,T.TSBUFFL                                              
         JNZ   *+8                                                              
         STCM  R0,3,T.TSBUFFL                                                   
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ACOMFACS                                                
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
**********************************************************************          
* Build subclient/product/job account code                           *          
**********************************************************************          
                                                                                
BLDSCPJ  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDSCPJ'                                                      
                                                                                
         USING WCS_D,R4                                                         
         LAY   R4,EX_BUFF                                                       
         MVC   DS_ACCT,SPACES                                                   
         CLC   DS_SCLI,SPACES      Any client code?                             
         JNH   EXITY                                                            
         LA    R1,DS_ACCT                                                       
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),DS_SCLI                                                  
         EX    RF,0(RE)                                                         
         CLC   DS_SPRO,SPACES      Any product code?                            
         JNH   EXITY                                                            
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         LLC   RE,PPROLEN                                                       
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),DS_SPRO                                                  
         EX    RE,0(RF)                                                         
         CLC   DS_SJOB,SPACES      Any job code?                                
         JNH   EXITY                                                            
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RF,PJOBLEN          RF=job length                                
         LLC   RE,PPROLEN          RE=product length                            
         SR    RF,RE                                                            
         CHI   RF,6                Job code should never be longer              
         JNH   *+8                              than 6 characters               
         LA    RF,6                                                             
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),DS_SJOB     Extract job code                             
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Add a record to optimisation buffer                                 *         
*                                                                     *         
* Ntry   - R1 points to caller's OB_D                                           
***********************************************************************         
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Get a record from optimisation buffer                               *         
*                                                                     *         
* Ntry   - R1 points to caller's OB_D                                           
* Exit   - CC=Low if record not found in buffer                                 
*        CC=Equal if record found and is not posted with an error     *         
*           - record is returned in caller's OB_D                     *         
*        CC=High if record found and is posted with an error (set in  *         
*           ROUERRV)                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
                                                                                
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITL               Not found/EOF                                
         DROP  T                                                                
                                                                                
         MVC   P.OB_D(OB_LNQ),W.OB_D                                            
                                                                                
         MVC   ROUERRV,P.OB_ERROR  Set/test record in error                     
         OC    ROUERRV,ROUERRV                                                  
         JNZ   EXITH                                                            
         J     EXITY                                                            
         DROP  W,P                                                              
         EJECT                                                                  
*BLOCK****************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   SET MORE TO COME                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'X'                                                             
*                                                                               
FILES2   DS    0C                  ** File Info **                              
         DC    C'MEDIA  '                                                       
         DC    C'U'                                                             
MEDDIR   DC    C'MEDDIR '                                                       
         DC    C'U'                                                             
MEDFIL   DC    C'MEDFIL '                                                       
         DC    C'U'                                                             
         DC    C'MEDRCV '                                                       
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
APPRVDQ  EQU   C'A'                APPROVED                                     
REJECTDQ EQU   C'R'                REJECTED                                     
AWAITAPQ EQU   C'M'                AWAITING APPROVAL                            
NOTREQDQ EQU   C'N'                NOT REQUIRED                                 
NOTAPPLQ EQU   C'*'                NOT APPLICABLE                               
*                                                                               
GAPSMAX  EQU   5000                Max number of entries                        
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
*                                                                               
WORKLEN  DC    AL2(((WORKL+7)/8)*8)     a(offline save area)                    
*              I think this is rounded up to nearest doubleword??               
*                                                                               
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
ACSD#    DC    AL2(A#ACSD)         ACCOUNT SUMMARY DOWNLOAD                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
*                                                                               
WCONLIT  DC    C'Workcode/office summary only'                                  
XDRFTLIT DC    C'Exclude draft transactions'                                    
EXPTLIT  DC    C'Export'                                                        
ORDLLIT  DC    C'Order list'                                                    
ESTLLIT  DC    C'Estimate list'                                                 
PAYINFO  DC    C'Payment info'                                                  
PRODLIT  DC    C'PRODUCTION  '                                                  
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
FFS      DC    X'FFFFFFFFFFFFFFFF'                                              
PZERO    DC    P'0'                                                             
PMONE    DC    P'-1'                                                            
LARE     DC    X'41E0'                                                          
LAREADDR DC    S(0)                                                             
ONENUL   DC    C'1N'                                                            
ORDWC    DC    C'**'               SJ order tx workcode                         
BILWC    DC    C'99'               Billing workcode                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
*                                                                               
**********************************************************************          
* TABLE OF EXPENSE VIEWS TO CHECK  - USED IN KEYDRIVER               *          
**********************************************************************          
*                                                                               
EXPTYTAB DC    AL1(EXJPCLI1)       list of expense types to read for            
         DC    AL1(EXJPCBL1)                         (GETEXP)                   
         DC    AL1(EXJPCNB1)                                                    
         DC    X'FF'                                                            
EXPTYTBL EQU   *-EXPTYTAB                                                       
EXPTTB#  EQU   EXPTYTBL-1/L'EXPTYTAB                                            
         EJECT                                                                  
*                                                                               
ASTERS   DC    16C'*'                                                           
                                                                                
         EJECT                                                                  
**********************************************************************          
*Keydriver macros*****************************************************          
**********************************************************************          
*                                                                               
ESTKEYT  LKKEY H,ESTKEY,SAVED     ** ESTIMATE RECORDS **                        
         LKKEY LIT,ESTKTYP,ESTKTYPQ                                             
         LKKEY LIT,ESTKSUB,ESTKSUBQ                                             
         LKKEY SIN,ESTKCPY,AGENCY                                               
         LKKEY LIT,ESTKREM,0                                                    
         LKKEY WMP,ESTKSJ,DS_SUESA                                              
         LKKEY ALL,ESTKLNO                                                      
         LKKEY ALL,ESTKSEQ                                                      
         LKKEY E                                                                
*                                                                               
EVEKEYT  LKKEY H,EVEKEY,SAVED     ** OLD ESTIMATE RECORDS **                    
         LKKEY LIT,EVEKTYP,EVEKTYPQ                                             
         LKKEY LIT,EVEKSUB,EVEKSUBQ                                             
         LKKEY SIN,EVEKCPY,AGENCY                                               
         LKKEY SIN,EVEKUNT,QS_UNIT                                              
         LKKEY SIN,EVEKLDG,QS_LDGR                                              
         LKKEY WMP,EVEKSJ,DS_SUEVA                                              
         LKKEY LIT,EVEKTYPE,EVEKTREV                                            
         LKKEY ALL,EVEKVER                                                      
         LKKEY LIT,EVEKWC,0        IGNORE TIME ESTIMS                           
         LKKEY ALL,EVEKSWC                                                      
         LKKEY ALL,EVEKSEQ                                                      
         LKKEY ALL,EVEKREM                                                      
         LKKEY E                                                                
*                                                                               
TRNKEYT  LKKEY H,TRNKEY,SAVED     ** TRANS'N KEY DRIVER (WORKCODES) **          
         LKKEY SIN,TRNKCPY,AGENCY                                               
         LKKEY SIN,TRNKUNT,QS_UNIT                                              
         LKKEY SIN,TRNKLDG,QS_LDGR                                              
         LKKEY WMP,TRNKACT,DS_SUBJA                                             
         LKKEY WMP,TRNKWORK,QS_AWORK        WORKCODE/OFFICE FILTERS             
         LKKEY ALL,TRNKCULC                                                     
         LKKEY ALL,TRNKDATE                                                     
         LKKEY ALL,TRNKREF                                                      
         LKKEY ALL,TRNKSBR                                                      
         LKKEY E                                                                
*                                                                               
ORNKEYT  LKKEY H,TRNKEY,SAVED     ** TRANS'N KEY DRIVER (OFFICES) **            
         LKKEY SIN,TRNKCPY,AGENCY                                               
         LKKEY SIN,TRNKUNT,QS_UNIT                                              
         LKKEY SIN,TRNKLDG,QS_LDGR                                              
         LKKEY WMP,TRNKACT,DS_SUBJA                                             
         LKKEY WMP,TRNKWORK,DS_AROFF        OFFICE FILTERS                      
         LKKEY ALL,TRNKCULC                                                     
         LKKEY ALL,TRNKDATE                                                     
         LKKEY ALL,TRNKREF                                                      
         LKKEY ALL,TRNKSBR                                                      
         LKKEY E                                                                
*                                                                               
ATRKEYT  LKKEY H,TRNKEY,SAVED     ** TRANS'N KEY DRIVER **                      
         LKKEY SIN,TRNKCPY,AGENCY                                               
         LKKEY SIN,TRNKUNT,QS_UNIT                                              
         LKKEY SIN,TRNKLDG,QS_LDGR                                              
         LKKEY WMP,TRNKACT,DS_SUBJA                                             
         LKKEY RNG,TRNKWORK,DS_TRWCS        WORKCODE VALID RANGE                
         LKKEY ALL,TRNKCULC                                                     
         LKKEY ALL,TRNKDATE                                                     
         LKKEY ALL,TRNKREF                                                      
         LKKEY ALL,TRNKSBR                                                      
         LKKEY E                                                                
*                                                                               
OTRKEYT  LKKEY H,TRNKEY,SAVED     ** ORDER TRANSACTION - BY W/C **              
         LKKEY SIN,TRNKCPY,AGENCY                                               
         LKKEY SIN,TRNKUNT,QS_UNIT                                              
         LKKEY SIN,TRNKLDG,QS_LDGR                                              
         LKKEY WMP,TRNKACT,DS_SUBJA                                             
         LKKEY SIN,TRNKWORK,DS_OTXWC       WORKCODE FOR ORDER TX                
         LKKEY ALL,TRNKCULC                        (VARIES BY LEDGER)           
         LKKEY ALL,TRNKDATE                                                     
         LKKEY ALL,TRNKREF                                                      
         LKKEY ALL,TRNKSBR                                                      
         LKKEY E                                                                
*                                                                               
TIMKEYT  LKKEY H,TSJPAS,SAVED     ** TIMESHEET RECORDS **                       
         LKKEY LIT,TSJPTYP,TSJPTYPQ           PASSIVE - DO GETREC!              
         LKKEY LIT,TSJPSUB,TSJPSUBQ                                             
         LKKEY SIN,TSJPCPY,AGENCY                                               
         LKKEY LIT,TSJPREM,0                                                    
         LKKEY LIT,TSJPVIEW,TSJPSJAQ                                            
         LKKEY SIN,TSJPCOFF,DS_JBOFF                                            
         LKKEY WMP,TSJPACT,DS_SUBJA                                             
         LKKEY ALL,TSJPMED                                                      
         LKKEY ALL,TSJPPEDT                                                     
         LKKEY ALL,TSJPPODS                   ADD OFFICE FILTER                 
         LKKEY ALL,TSJPKDA                                                      
         LKKEY ALL,TSJPKSTA                                                     
         LKKEY ALL,TSJPSBR                                                      
         LKKEY E                                                                
*                                                                               
EXJKEYT  LKKEY H,EXJPAS,SAVED      ** EXPENSE KEY DRIVER  **                    
         LKKEY LIT,EXJPTYP,EXJPTYPQ         Passive - do getrec!                
         LKKEY LIT,EXJPSUB,EXJPSUBQ                                             
         LKKEY SIN,EXJPCPY,AGENCY                                               
         LKKEY LIT,EXJPREM,0                                                    
         LKKEY WMP,EXJPVIEW,DS_EXTYA        Views array                         
         LKKEY SIN,EXJPCOFF,DS_JBOFF                                            
         LKKEY WMP,EXJPCPJ,DS_SUBJA                                             
         LKKEY ALL,EXJPMED                                                      
         LKKEY ALL,EXJPDATE                                                     
         LKKEY ALL,EXJPPID                                                      
         LKKEY ALL,EXJPNUM                                                      
         LKKEY ALL,EXJPSEQ                                                      
         LKKEY ALL,EXJPSRC                                                      
         LKKEY ALL,EXJPKSTA                                                     
         LKKEY E                                                                
*                                                                               
OSJKEYT  LKKEY H,OSJPAS,SAVED     ** ORDER KEY DRIVER **                        
         LKKEY LIT,OSJPTYP,OSJPTYPQ                                             
         LKKEY LIT,OSJPSUB,OSJPSUBQ                                             
         LKKEY SIN,OSJPCPY,AGENCY                                               
         LKKEY LIT,OSJPREM,0                                                    
         LKKEY WMP,OSJPACT,DS_SUBJA                                             
         LKKEY ALL,OSJPMEM               MEMO AND NON-MEMO                      
         LKKEY ALL,OSJPORD                                                      
         LKKEY E                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         ORG                                                                    
                                                                                
*&&US                                                                           
LCL#LIT  DC    C'Live Claim Number'                                             
*&&                                                                             
LOV#LIT  DC    C'Override limit list'                                           
*                                                                               
LVALUES  DS    0D                                                               
         DC    A(FLTRAND)                                                       
         DC    A(FLTRANF)                                                       
         DC    A(FLTESTD)                                                       
         DC    A(FLTMKD)                                                        
         DC    A(FLTUORD)                                                       
         DC    A(FLTEXPD)                                                       
LVALUESL EQU   *-LVALUES                                                        
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#TIME,L'AC@TIME                                                
         DCDDL AC#MAT,L'AC@MAT                                                  
         DCDDL AC#TOTAL,L'AC@TOTAL                                              
         DCDDL AC#SBJBT,L'AC@SBJBT                                              
         DCDDL AC#MSJBT,L'AC@MSJBT                                              
         DCDDL AC#MJBT,L'AC@MJBT                                                
         DCDDL AC#PREBL,L'AC@PREBL                                              
DCDICTLX DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*BLOCK*****************************************************************         
* storage area equ's. numbers are indexes into list of storage block            
* addresses. Addresses point at a series of 15 1K blocks (hence 2               
* assigned for an IO area).                                                     
* blocks 1 and 2 are reserved for saved/working storage, 15 for TWA?            
***********************************************************************         
B#TRN    EQU   3                   IO2 - transaction                            
B#EST    EQU   3                   IO2 - ESTIMATE record                        
B#EXC    EQU   5                   IO3 - Expense claim                          
B#ORD    EQU   5                   IO3 - Order record                           
B#TIM    EQU   5                   IO3 - Time record                            
B#BUFREC EQU   7                   EX_BUFF - BUFFERED EXPENSE RECORD            
B#GOXBLK EQU   8                   GOXBLOCK                                     
B#COBLCK EQU   11                      - COST PROFILE AREA                      
ACOSTPR  EQU   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)                                 
B#SVRDEF EQU   12                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   13                      - LP_D                                   
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
ATOTALS  DS    A                   A(total lines in AIO2 buffer)                
*                                                                               
WVALUES  DS    0X                                                               
ADCONS   DS    0A                                                               
AFLTRAND DS    A                                                                
AFLTRANF DS    A                                                                
AFLTESTD DS    A                                                                
AFLTMKD  DS    A                                                                
AFLTUORD DS    A                                                                
AFLTEXPD DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
*                                                                               
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
*                                                                               
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
GBUFAREA DS    XL(OB_LNQ)          Buffer area block                            
*                                                                               
DSDICTL  DS    0C                                                               
AC@TIME  DS    CL20                                                             
AC@MAT   DS    CL20                                                             
AC@TOTAL DS    CL30                                                             
AC@SBJBT DS    CL30                                                             
AC@MSJBT DS    CL30                                                             
AC@MJBT  DS    CL30                                                             
AC@PREBL DS    CL30                                                             
*                                                                               
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
*                                                                               
SAVEVAR  DS    0F                                                               
MYPL16   DS    PL16                Packed arithmetic work area                  
MYPL8    DS    PL8                 Packed arithmetic work area                  
SAVEKEY1 DS    CL64                save for keydrivers                          
SAVEKEY2 DS    CL64                                                             
SAVEKEY3 DS    CL64                                                             
SAVEKEY4 DS    CL64                                                             
SAVERF   DS    F                   SAVED REGISTER RF                            
SAVETNUM DS    XL(L'TSRNUM)                                                     
VATCNBLK DS    (VTCLNQ)X           Varican block                                
SEQCTR   DS    XL2                 Sequence counter for tsar                    
LASTWC   DS    XL(L'TRNKWORK)      Last workcode record read for desc.          
LASTWCTX DS    XL(L'VTCRATE)       Vat rate for last workcode                   
LASTCOMR DS    PL6                 Commission rate                              
*                                                                               
RTIMTUPT DS    XL(TMSTTABL)        R time settings from GETOPT/GETCAP           
BTIMTUPT DS    XL(TMSTTABL)        B time settings from GETOPT/GETCAP           
TMSTNMB  EQU   (*-RTIMTUPT)/TMSTTABL When doing estimate checking               
NTIMTUPT DS    XL(TMSTTABL)        N time settings from GETOPT/GETCAP           
TMSTNMN  EQU   (*-RTIMTUPT)/TMSTTABL                                            
*                                                                               
STSARKEY DS    XL(WCS_TKYL)        Key length                                   
ATSRERRS DS    XL1                 Error area for buffer                        
MYBYTE   DS    XL1                 Byte for use in routines                     
         EJECT                                                                  
*BLOCK*****************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
*    This is the standard form, laid out to match exactly to the                
*    mapping tables for each request                                            
***********************************************************************         
                                                                                
QVALUES  DS    0X                 ** REQUEST VALUES **                          
* Search call values                                                            
QS_TYPE  DS    CL1                 Summary type                                 
QS_TWOSQ EQU   C'1'                - Workcode/office summary                    
QS_TACCQ EQU   C'2'                - Accounting transaction detail              
QS_TBILQ EQU   C'3'                - Billing Detail                             
QS_UNIT  DS    CL1                 Unit code                                    
QS_LDGR  DS    CL1                 Ledger code                                  
QS_ACCT  DS    CL12                Account code                                 
QS_WCIND DS    XL1                 Workcode array indicator                     
QS_AWORK DS    AL3                 Workcode array address                       
QS_OFIND DS    XL1                 Office Array indicator                       
QS_AOFFC DS    AL3                 Office Array address                         
QS_ESTC  DS    CL1                 Estimate checking                            
QS_ICON  DS    CL1                 Include contingency                          
QS_HRS   DS    CL1                 Include hours                                
QS_REVRS DS    CL1                 Include reversals (default is no)            
QS_STACT DS    XL2                 Activity start date                          
QS_ENACT DS    XL2                 Activity end date                            
QS_OWCON DS    CL1                 Workcode/office summary only                 
QS_XDRFT DS    CL1                 Exclude drafts (default is no)               
QS_EXPRT DS    CL1                 Export                                       
QS_ORDL  DS    CL1                 Order list request                           
QS_ESTL  DS    CL1                 Estimate list request                        
QS_PAYI  DS    CL1                 Payment info request                         
QVALUESL EQU   *-QVALUES                                                        
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
DS_TOTBL DS    PL8                 Total billed                                 
DS_ACCT  DS    CL(L'ACTKACT)       Working account code                         
DS_SACCT DS    CL(L'ACTKACT)       Saved working account code                   
DS_EXULA DS    CL(L'ACTKULA)       Expense account code                         
DS_CPJES DS    CL(L'ESTKSJ)        Client prod job for mcs estimates            
DS_CPJEV DS    CL(L'EVEKSJ)        Client prod job for non mcs estimate         
DS_ALEV1 DS    CL(L'ACTKACT)       Level 1 account code/client                  
DS_ALEV2 DS    CL(L'ACTKACT)       Level 2 account code/product                 
DS_ALEV3 DS    CL(L'ACTKACT)       Level 3 account code/job                     
DS_ALEV4 DS    CL(L'ACTKACT)       Level 4 account code                         
DS_OTXWC DS    CL2                 Order t/x 'wordcode' (ledger based)          
DS_FFTWC DS    CL2                 Memo order t/x FFTEL workcode                
DS_WCLST DS    CL2                 Last WC record read                          
DS_WDESC DS    CL15                W/C description for DS_WCLST                 
DS_WCEST DS    CL1                 W/C estimate check                           
DS_WINEX DS    CL1                 Internal/external workcode                   
DS_CALST DS    CL12                Last contra record read                      
DS_CTRN  DS    CL36                Contra a/c name (DS_CALST)                   
DS_MEDDS DS    CL(L'PMDDESC)       Media description                            
DS_SRACT DS    CL(L'ACTKULA)       Debtor account including unit/ledger         
DS_JBOFF DS    CL(L'TRNKWORK)      Office code from job a/c                     
DS_PRPID DS    CL2                 Last PID passed to GETPID                    
DS_BLPID DS    CL8                 8 byte PIDCODE for DS_PRPID                  
DS_PRUID DS    CL2                 Last UID passed to GETUSR                    
DS_BLUID DS    CL10                Translated user id                           
DS_PRPIN DS    CL8                 Last PID we read names for                   
DS_PIDFN DS    CL16                Names for DS_PRPIN                           
DS_PIDMN DS    CL16                                                             
DS_PIDLN DS    CL58                                                             
DS_TRWCS DS    XL(L'TRNKWORK)      Start/end values for tracsaction             
DS_TRWCE DS    XL(L'TRNKWORK)           Workcodes (For keydrivers)              
DS_SCLI  DS    CL6                 SubClient code                               
DS_SPRO  DS    CL6                 SubProduct code                              
DS_SJOB  DS    CL8                 Subjob code                                  
DS_IND1  DS    XL1                 Indicator                                    
DS_ISUBJ EQU   X'80'               Job has sub jobs present                     
DS_IMSTJ EQU   X'40'               Job is marked as master job                  
DS_IEOB  EQU   X'20'               Set end of buffer                            
DS_ITOT  EQU   X'10'               Set total work code line required            
DS_IEST  EQU   X'04'               Set to skip this estimate                    
*                                                                               
DS_EXTYI DS    XL1                 Expense type array indicator                 
DS_EXTYA DS    AL3                 Expense type array address                   
DS_EXTYM EQU   10                  Expense type array MAX                       
*                                                                               
* Sub job array contains master so we read for master and sub jobs              
DS_SUBJI DS    XL1                 Sub job array indicator                      
DS_SUBJA DS    AL3                 Sub job array address                        
*&&UK                                                                           
DS_SUBJM EQU   300                 Sub job array maximum                        
*&&                                                                             
*&&US                                                                           
DS_SUBJM EQU   200                 Sub job array maximum                        
*&&                                                                             
*                                                                               
DS_SUESI DS    XL1                 Sub jb array for estimates indicator         
DS_SUESA DS    AL3                 Sub jb array address                         
*&&UK                                                                           
DS_SUESM EQU   300                 Sub jb array maximum                         
*&&                                                                             
*&&US                                                                           
DS_SUESM EQU   200                 Sub jb array maximum                         
*&&                                                                             
*                                                                               
DS_SUEVI DS    XL1                 Sub jb array for estimates indicator         
DS_SUEVA DS    AL3                 Sub jb array address                         
*&&UK                                                                           
DS_SUEVM EQU   300                 Sub jb array maximum                         
*&&                                                                             
*&&US                                                                           
DS_SUEVM EQU   200                 Sub jb array maximum                         
*&&                                                                             
*                                                                               
DS_ROFFI DS    XL1                 Office Array indicator for reading           
DS_AROFF DS    AL3                 Office Array address for reading             
OFFMAXQ  EQU   170                                                              
*                                                                               
DS_SORM  DS    XL1                 Master or sub job                            
DS_PROLD DS    CL1                 Ledger is production?                        
DS_ISDBT DS    CL1                 T/X is debit Y/N                             
DS_TAMT  DS    PL6                 Transaction amount (DB+CR)                   
DS_EGLO  DS    CL(L'EMDGNO)        Estimate global number                       
DS_ELOC  DS    XL1                          Local number                        
DS_EDSC  DS    CL(L'ENMNAME)                Name/description                    
DS_ESTDT DS    XL3                          Date                                
DS_ECUR  DS    CL3                          Currency code                       
DS_EXRT  DS    XL7                          Exchange rate                       
DS_RPRE  DS    XL1                 Requisition prefix                           
DS_RSUF  DS    XL1                             suffix                           
DS_ODAT  DS    XL3                 Order date                                   
DS_OCTRA DS    XL(L'TRNKULC)       Order contra                                 
DS_OREQN DS    XL8                 Order requisition number                     
DS_EXPT  DS    XL(L'WCS_EXPT)      Expense type                                 
DS_GPST  DS    XL(L'EMDGSTAT)      Gap status                                   
DS_ESTHS DS    XL1                 Saved highest subm. estimate number          
DS_ESTHI DS    XL1                 Current estimate/highest approved            
DS_ESTS  DS    XL1                 estimate status                              
DS_TODC  DS    XL2                 Today compressed format                      
DS_TODP  DS    XL3                 Today packed format                          
DS_GINEL DS    CL1                 Ginel element on tx                          
DS_ORST  DS    CL1                 Order matched status                         
DS_ORAU  DS    CL(L'ORDAUTH)       Order authoriser                             
DS_ORIN  DS    CL1                 Order invoice Y/N                            
DS_APST  DS    XL1                 Approval status                              
DS_TYPE  DS    CL1                 Order type                                   
DS_ORA   DS    PL6                 Order amount                                 
         ORG   DS_ORA                                                           
DS_AEST  DS    PL6                 Estimate amount                              
         ORG                                                                    
DS_OIN   DS    PL6                 Invoiced amount                              
DS_FORA  DS    PL6                 Foreign currency order amount                
DS_TOIN  DS    PL6                 Total invoiced amount                        
DS_TORA  DS    PL6                 Total order amount                           
DS_ORDN  DS    CL100               Order name                                   
DS_PDESL DS    XL1                 Printed description length                   
DS_PDESC DS    CL240               Printed description                          
DS_MDESL DS    XL1                 Matching description length                  
DS_MDESC DS    CL120               Matching description                         
DS_JOPDC DS    XL2                 Job opened date compressed                   
DS_JOPDP DS    XL3                 Job opened date packed                       
DVALUESL EQU   *-DVALUES                                                        
*                                                                               
AGENCY   DS    XL(L'CUXCPY)        Company code                                 
USRID    DS    XL(L'CUUSER)        Connected user id                            
*                                                                               
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AALL     DS    AL3                 All value WMP entry                          
ANZR     DS    AL3                 Non-zero value WMP entry                     
                                                                                
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
CLI_COD  DS    CL6                 Client code                                  
CLI_NAM  DS    CL36                Client name                                  
PRD_COD  DS    CL6                 Product code                                 
PRD_NAM  DS    CL36                Product name                                 
JOB_COD  DS    CL6                 Job code                                     
JOB_NAM  DS    CL36                Job name                                     
                                                                                
OVALUESL EQU   *-OVALUES                                                        
*                                                                               
* Below array is mapped to DS_SUBJA, so it holds the current estimate,          
* total billed and indicator for pre billing for each job when you              
* have master and linked jobs                                                   
*                                                                               
*&&US                                                                           
DS_ESCEA DS    (DS_SUBJM)XL(JB_LNQ) Job array                                   
*&&                                                                             
*                                                                               
SAVELN1Q EQU   *-SAVEVAR                                                        
*                                                                               
* Buffer area needed is greater for EU as we have extended narratives           
* from invoice log                                                              
*                                                                               
*&&UK                                                                           
EX_BUFF  DS    XL2000              Buffer area for tsars - ALLOW 1500           
*&&                                   FOR EXTENDED NARRATIVES                   
*                                                                               
*&&US                                                                           
EX_BUFF  DS    XL1000              Buffer area for tsars -                      
*&&                                                                             
*                                                                               
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
                                                                                
***********************************************************************         
* overlay DSECTs                                                      *         
***********************************************************************         
***********************************************************************         
* Optimisation buffer record layout                                   *         
***********************************************************************         
         SPACE 1                                                                
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL64                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
         EJECT                                                                  
***********************************************************************         
* Job array layout                                                    *         
***********************************************************************         
         SPACE 1                                                                
JB_D     DSECT                                                                  
JB_ESTHI DS    XL1                 Highest apprd or submitted estimate          
JB_TOTAL DS    PL8                 Total allocated                              
JB_TOTBL DS    PL8                 Total billed                                 
JB_IND1  DS    XL1                 Indicator                                    
JB_IPRE  EQU   X'08'               Set job has prebilling to show               
JB_LNQ   EQU   *-JB_D                                                           
         EJECT                                                                  
***********************************************************************         
* Record buffer DSECT                                                 *         
***********************************************************************         
                                                                                
WCS_D    DSECT                                                                  
WCS_KEY  DS    0X                                                               
WCS_LEN  DS    XL2                 Length as recs are variable                  
WCS_KEY1 DS    0X                  Start of actual key                          
WCS_WCOD DS    CL2                 Work or office code                          
WCS_SORM DS    CL1                 Sub or master job                            
WCS_SUBJ EQU   1                   Sub job row                                  
WCS_MSTJ EQU   2                   Master job row                               
WCS_TOTJ EQU   3                   Total job row                                
WCS_SJAC DS    CL12                Sub or master account code                   
WCS_TYPE DS    CL1                 Entry type - record extracted from           
WCS_ORD  EQU   X'01'               Order (Memo/non-Memo/Req)                    
WCS_EST  EQU   X'02'               Estimate                                     
WCS_TRN  EQU   X'03'               Transactions (WCS_TTYP for detail)           
WCS_EXP  EQU   X'04'               Expense Claims                               
WCS_UORD EQU   X'05'               Unapproved Orders                            
WCS_TIME EQU   X'06'               Timesheets (TIMTSK)                          
WCS_SUM  EQU   X'07'               Summary record                               
WCS_FBAD EQU   X'08'               Flexibill Advance bill                       
WCS_ESTL EQU   X'09'               Estimate list                                
WCS_ORDL EQU   X'0A'               Order list                                   
WCS_CTRA DS    CL14                Contra ULA                                   
WCS_LREF DS    CL20                Long reference (FFTTINVN ETC)                
         ORG   WCS_LREF                                                         
WCS_TREF DS    0CL6                T/X reference                                
WCS_REQN DS    0CL8                requisition number                           
WCS_ENUM DS    XL1                 Estimate local number (hex)                  
         ORG                                                                    
WCS_DAT  DS    XL3                 DATE                                         
WCS_SREF DS    XL1                 SUBREF (TRNKSREF)                            
WCS_TTYP DS    CL1                 TRANSACTION TYPE                             
WCS_TNOT EQU   00                  Not a transaction record                     
WCS_TMOR EQU   01                  Memo order                                   
WCS_TORD EQU   02                  Non-Memo Order                               
WCS_TTRN EQU   03                  Transactions                                 
WCS_TMTR EQU   04                  Memo Transaction                             
WCS_TMRQ EQU   05                  Memo Requisition                             
WCS_TREQ EQU   06                  Non memo requisition                         
WCS_TBTI EQU   07                  Billable time                                
WCS_TMTI EQU   08                  Memo time                                    
WCS_TNTI EQU   09                  Non-Billable Time                            
WCS_TNEX EQU   10                  non-billable expense                         
WCS_TBEX EQU   11                  billable expense                             
WCS_SEQ  DS    XL2                 SEQUENCE NUMBER FOR UNIQUENESS               
WCS_KEYL EQU   *-WCS_KEY           Real key length                              
WCS_TKYL EQU   *-WCS_KEY1          TSAR Key length                              
WCS_RFST DS    0X                  START OF REC                                 
*                                                                               
* Summary values (use to roll up transactions for summary)                      
*                                                                               
WCS_SPNS DS    0PL6                Start of packed number fields                
WCS_AEST DS    PL6                 Current estimate amount                      
WCS_AEHR DS    PL6                 Current estimate hours                       
WCS_DBA  DS    PL6                 Debit amount                                 
WCS_CRA  DS    PL6                 Credit amount                                
WCS_ORA  DS    PL6                 Order amount                                 
WCS_HRS  DS    PL6                 Hours                                        
WCS_ALA  DS    PL6                 allocated amount                             
WCS_FCAL DS    PL6                 Foreign currency - allocated amount          
***_GRSS DS    PL6                 Gross amount                                 
WCS_NCOM DS    PL6                 Noncommissionable amount                     
WCS_CHT  DS    PL6                 Chargeable time/memo cost of hours           
WCS_TSR  DS    PL6                 Time sames                                   
WCS_COB  DS    PL6                 Commission billed                            
WCS_LBIL DS    PL6                 Left to bill                                 
WCS_LSPD DS    PL6                 Left to spend                                
WCS_LSPH DS    PL6                 Left to spend for header                     
WCS_CMTD DS    PL6                 Committed, actual costs and invoices         
WCS_ACTH DS    PL6                 Actuals in header                            
WCS_ORAH DS    PL6                 Orders in header                             
***_NTEX DS    PL6                 Net excluding write offs                     
WCS_SPNQ EQU   (*-WCS_SPNS)/L'WCS_SPNS    Number of summary accums              
* Transaction-level-only common costs                                           
WCS_COM  DS    PL6                 Commission amount                            
WCS_TCR  DS    PL6                 Time cost                                    
WCS_WROF DS    PL6                 Write off                                    
WCS_NBIL DS    PL6                 Non-billable/memo total                      
                                                                                
WCS_CURA DS    PL6                 Foreign currency amount                      
WCS_CURC DS    PL6                 Foreign currency credit amount               
WCS_CURD DS    PL6                 Foreign currency debit amount                
WCS_OIN  DS    PL6                 Invoiced order amount (order T/X)            
WCS_VAT  DS    PL6                 Bill VAT amount                              
WCS_COMR DS    PL6                 Commission rate                              
WCS_COMF DS    PL6                 Foreign commission amount                    
WCS_PAMT DS    PL6                 Payment amount                               
*  Billing specific:                                                            
WCS_BNET DS    PL6                 Bill Net amount                              
WCS_BTOT DS    PL6                 Bill Total                                   
WCS_BGRS DS    PL6                 Bill Gross (Payable)                         
WCS_BDAM DS    PL6                 Received from Debtor                         
WCS_BWAM DS    PL6                 Write-off Amount                             
WCS_BDNA DS    PL6                 Net amount from Debtor                       
WCS_BWNA DS    PL6                 Net write-off Amount                         
*  Estimate specific costs:                                                     
WCS_TPNQ EQU   (*-WCS_SPNS)/L'WCS_SPNS Number of accums(inc summary)            
*                                                                               
* NON-MONETARY VALUES                                                           
WCS_BTYP DS    XL1                 Batch type                                   
WCS_BLTY DS    XL1                 Bill type                                    
         ORG   WCS_BLTY                                                         
WCS_OTYP DS    CL1                 Order type                                   
WCS_ILOG DS    CL7                 INVOICE LOG NUMBER                           
WCS_EXPT DS    CL3                 EXPENSE/ESTIMATE TYPE CODE                   
WCS_APST DS    XL1                 APPROVAL STATUS                              
WCS_ASIP EQU   C'1'                IN PROGRESS                                  
WCS_ASSB EQU   C'2'                SUBMITTED                                    
WCS_ASPA EQU   C'3'                PART APPROVED                                
WCS_ASFA EQU   C'4'                FULLY APPROVED                               
WCS_ASRJ EQU   C'5'                REJECTED                                     
WCS_ASDL EQU   C'6'                DELETED                                      
WCS_BLST DS    XL1                 BILLING STATUS                               
WCS_ORST DS    XL1                 ORDER MATCHING STATUS                        
WCS_NMCH EQU   X'1'                NO MATCHING                                  
WCS_PMCH EQU   X'2'                PART MATCHED                                 
WCS_FMCH EQU   X'3'                FULLY MATCHED                                
WCS_CLLD EQU   X'4'                CANCELLED - UNMATCHED AND CLOSED             
WCS_IREF DS    CL9                 INTERNAL REF                                 
WCS_ONUM DS    CL6                 ORDER NUM                                    
WCS_ESTG DS    CL6                 ESTIMAGE GLOBAL NUM (from exp/time)          
WCS_CURR DS    CL3                 F/CURRENCY CODE                              
WCS_ACUR DS    CL3                 F/CURRENCY CODE FOR ALLOCATED/BILLS          
WCS_PREF DS    CL6                 Payment reference                            
WCS_PDAT DS    XL2                 Payment date                                 
WCS_CURX DS    CL14                CURR EXCH RATE RULE                          
WCS_VATR DS    XL(L'VTCRATE)       VAT rate                                     
WCS_GPST DS    XL(L'EMDGSTAT)      GAP STATUS (ESTIMATE/ORDER)                  
WCS_CREF DS    CL16                Campaign reference                           
WCS_CLR1 DS    CL70                Client reference 1                           
WCS_CLR2 DS    CL70                Client reference 2                           
*&&US                                                                           
         ORG   WCS_CLR1                                                         
WCS_BLDS DS    CL(L'TRNBLTYP)      Bill type description                        
*&&                                                                             
         ORG   WCS_CLR1                                                         
WCS_ORDN DS    CL100               Order name                                   
WCS_GDRC DS    CL1                 Order goods received                         
         ORG                                                                    
WCS_MEDC DS    CL1                 Media code                                   
WCS_CLIC DS    CL6                 Client code                                  
WCS_PROC DS    CL6                 Product code                                 
WCS_JOBC DS    CL6                 Job code                                     
WCS_CAMC DS    CL3                 Campaign code                                
WCS_OULA DS    0CL14               Other source unit ledger account             
WCS_OUNT DS    CL1                 Other source unit                            
WCS_OLDG DS    CL1                 Other source ledger                          
WCS_OACT DS    CL12                Other source account                         
WCS_DUED DS    XL2                 Due date                                     
WCS_BREF DS    CL6                 Batch reference                              
WCS_MOA  DS    XL3                 MOA                                          
WCS_IREV DS    CL1                 Is reversed                                  
WCS_IHLD DS    CL1                 Is held                                      
WCS_IQRD DS    CL1                 Is queried                                   
WCS_ICOM DS    CL1                 Is commissionable                            
WCS_ACDT DS    XL2                 Activity date                                
WCS_UQID DS    XL4                 Unique id                                    
WCS_ATYP DS    XL1                 Allocation type - see RALELD                 
WCS_ADT  DS    XL3                 Allocation date                              
WCS_DPDT DS    XL3                 Allocation deposit date                      
WCS_AREF DS    CL6                 Allocation reference                         
WCS_AULA DS    CL14                Allocation unit ledger account               
WCS_IMEM DS    CL1                 Foreign currency is memo                     
WCS_PID  DS    XL2                 Biller PID/Order creator PID                 
* BILLING SPECIFIC:                                                             
WCS_BDST DS    XL1                 Debtor status of bill                        
WCS_BDSF EQU   X'01'                -FULLY PAID                                 
WCS_BDSP EQU   X'02'                -PART PAID                                  
WCS_BDSO EQU   X'03'                -OUTSTANDING                                
WCS_BPDT DS    PL3                 Date of last payment                         
WCS_BUID DS    XL2                 User ID                                      
WCS_CTRN DS    CL36                                                             
* estimate specific values                                                      
WCS_ESTS DS    XL1                 Estimate status                              
WCS_ESCA EQU   1                   Client approved                              
WCS_ESCS EQU   2                   Client submitted                             
WCS_ESIA EQU   3                   Internally approved                          
WCS_ESIS EQU   4                   Internally submitted                         
WCS_ESPG EQU   5                   In progress                                  
WCS_ESRJ EQU   6                   Rejected                                     
WCS_ESDL EQU   7                   Deleted                                      
WCS_ESDT DS    XL3                 Estimate date                                
WCS_ESTD DS    CL50                Estimate Description                         
         ORG   WCS_ESTD                                                         
WCS_ORAU DS    CL(L'ORDAUTH)       Order authoriser from presto                 
         ORG                                                                    
*                                                                               
WCS_TERM DS    0XL1                 TERMINATOR BYTE                             
WCS_DLNQ EQU   *-WCS_D             LENGTH OF RECORD w/o bill nums/NARRS         
*                                                                               
WCS_ELET DS    XL1                           -TYPE   0=EOR                      
WCS_ELBQ EQU   1                                     BILL NUMBERS               
WCS_ELRQ EQU   2                                     NARRATIVES                 
WCS_ELEQ EQU   3                                     ESTIMATES                  
WCS_ELEL DS    XL1                           -LENGTH                            
WCS_ELED DS    0X                            -DATA                              
         EJECT                                                                  
***********************************************************************         
* included books                                                      *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
EVERECD  DSECT                                                                  
         ORG   EVEKCLI                                                          
EVEKSJ   DS    0CL18                                                            
         ORG                                                                    
* MEDIA RECORDS                                                                 
*PREFIX=M_                                                                      
         PRINT OFF                                                              
       ++INCLUDE MEFILAGY                                                       
       ++INCLUDE MEFILMED                                                       
*PREFIX=                                                                        
         PRINT ON                                                               
                                                                                
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACBRA30   06/24/20'                                      
         END                                                                    
