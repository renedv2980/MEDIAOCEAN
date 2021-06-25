*          DATA SET ACBRA27    AT LEVEL 067 AS OF 02/03/21                      
*PHASE T62427A                                                                  
                                                                                
ACBRA27  TITLE '- BrandOcean Timesheet Display/initial Download Server'         
                                                                                
***********************************************************************         
* Level change comments                                               *         
* ---------------------                                               *         
* NSHE 001 05OCT07 New application version                            *         
* MPEN 002 06APR09 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT        *         
* NSHE 006 19SEP09 Relink for change in TIMELD for media approvals    *         
* MPEN 007 26OCT09 <LO01-9452> New profiles                           *         
* MPEN 008 28OCT09 <LO01-9472> Pass prof allow new time if prev not sb*         
*          02NOV09 <LO01-9493> Amend FTA and pass new setting on timi *         
* SMAN 009 05FEB10 <BR30563L> Ignore future location lines            *         
* NSHE 010 12MAY10 <PR000224> Send new dialogue row profile           *         
* MPEN 011 09AUG10 <BR16423D> Check lock date on initialise           *         
* NRAK 012 07OCT10 <UKCR00029> DON'T RETURN QLOCED IF TSD=OLD         *         
* MPEN 013 29DEC10 <BR17044D> Fix code for LVL 11                     *         
* MPEN 014 04JAN10 <BR38624L> Remove code to show error message       *         
* NSHE 015 15NOV10 Fix for back up approvers and change to grey out   *         
* SMAN 016 28APR11 <BR41356L> Ignore adjustment ind. if CTS set to Y  *         
* MPEN 016 15DEC10 <PR001254> Show job locked status                  *         
* NSHE 030 21OCT11 <PR002306> New calendar download for res managment *         
* NSHE 031 03APR12 Resource management - error on lock TS             *         
* MPEN 032 18MAY12 <BR49808L> Fix for calendar lookup if no end date  *         
* NSHE 033 25JUN12 <UKCR34322> Show future time flag for display time *         
* NSHE 034 03SEP12 Change to IO routine for auto switching system     *         
* NRAK     06FEB12 <BR53921L> If previous time = 0 edit hours treat as*         
*                     submitted for PTA testing                       *         
* NSHE 035 05AUG13 Change initial call for BrandOcean replacement     *         
* NRAK 036 22OCT13 Fix Initial call - ND=Y for office code            *         
* YNGX 037 27NOV13 <OT77792L> Bug fix: CLEAR ALOCEL IN ROUTINE PRSDTL *         
* NSHE 038 29OCT13 DSRD-217 Don't send blank lines for Rodick         *         
* NSHE     28NOV13 DSRD-190 Bring back rejection comments for rejected*         
* NSHE     13FEB14 DSRD-1176 Set approved date and PID                *         
* NSHE     20MAR14 DSRD-1727 Pass user is a back up approver for time *         
* MPEN     20MAR14 DSRD-1745 Secure timesheet display                 *         
* MPEN     28MAR14 DSRD-1856 Return 1R level lengths                  *         
* NSHE 039 02APR14 DSRD-1934 Skip security check for Resource man     *         
* NSHE     08APR14 DSRD-1975 Bug advising overdue when you are not    *         
* NSHE     08APR14 DSRD-1935 bug display status to viewer             *         
* NSHE 040 01MAY14 Copy US fixes for calendar record                  *         
* NSHE     01MAY14 Remove some fields from the client download        *         
* NSHE     07MAY14 Change io routine in client look up                *         
* MPEN 041 13JUN14 DSRD-3014 Skip clients locked from time            *         
* NSHE 042 18JUN14 DSRD-2999 Split time initial call for Aura         *         
* NSHE 043 02OCT14 DSRD-4535 Check length of element for client recs  *         
* NSHE 044 30DEC14 DSRD-5587 Fix to copy from previous timesheet      *         
* MPEN 045 06JAN14 OT81953L Fix dump/loop in GETAGN in Aura           *         
* NSHE 046 05FEB14 DSRD-5955 Stop GroupM users accessing time         *         
* NSHE 047 24Apr15 DSBO-1406 Amend SETFAC call                        *         
* NSHE 048 05May15 DSRD-6982 Read approval rights for back up approver*         
* NSHE     05May15 DSRD-6993 check manager search for limit list      *         
* TKLU 049 27Jan15 <RD010160>  Performance improvements (GETOPT)      *         
* NSHE 050 04May16 DSRD-11053 Support empty timesheet                 *         
* MPEN 051 01Jul16 DSRD-11140 MF changes for federated url            *         
* NSHE     27Jul16 DSRD-12032 Return closed status for job time lines *         
* MPEN 052 16Nov16 DSRD-14069 Return agency currency and d.p.         *         
* NSHE 053 10Jul17 DSRD-16147 No longer return client list in initial *         
* NSHE 054 05Mar18 DSRD-18379 Deal with fiscal start month correctly  *         
* MPEN 055 10May18 DSRD-18847 Handle locked 1N accounts               *         
* NSHE 056 11Oct18 DSRD-20089 Return default account level for input  *         
* MPEN 057 23Oct18 DSRD-20447 Relink for DPAPASD changes              *         
* MPEN 058 23Jan19 DSRD-21354 User preference for new timesheet       *         
* YNGX 059 25Mar19 DSRD-21829 Add a new field to get period by date   *         
* MPEN     28Mar19 DSRD-22108 Fix for overdue t/s                     *         
* MPEN     24Apr19 DSRD-22197 Fix for t/s display after reset         *         
* MPEN 060 06Jun19 DSRD-22633 Fix for time widget                     *         
* MPEN     29Apr19 DSRD-22248 Return timeoff id                       *         
* MPEN 061 26Jun19 DSRD-22750 Don't check daily edit hours for overdue*         
* MPEN 062 30Aug19 DSRD-23698 Fix for overdue indicator               *         
* MPEN 063 04Dec19 DSRD-24607 Prevent editing lines in mobile         *         
* SGAV 064 02Jul20 DSRD-26554 New Cost Prof to confirm overtime apprvl*         
* NSHE 065 05Aug20 <DSRD-27173> Clean up URL logic                    *         
* MPEN     11Aug20 <DSRD-27177> Return day narrative                  *         
* YNGX 066 24Sep20 <DSRD-27594> Restrict access T/S based on status   *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,WORKERKEY=ACBO,ABENDLIST=FAILS,FACS=FACS,+        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#LP_D,LP_D,                                            +        
               B#TIMREC,TIMRECD,                                       +        
               B#CLIREC,ACTRECD,                                       +        
               B#ORD,ORDRECD,                                          +        
               B#CAL,CASRECD,                                          +        
               B#EST,ESTRECD,                                          +        
               B#SCMREC,SCMRECD,                                       +        
               B#TWAD,TWAD,                                            +        
               B#SVRDEF,SVRDEF,                                        +        
               B#PTASK,PKSRECD,                                        +        
               B#GOBLK,GOBLOCKD,                                       +        
               B#GOXBLK,GOXBLKD,                                       +        
               B#GOBBLK,GOBBLKD,                                       +        
               B#COBLCK,COBLOCKD)                                               
                                                                                
TSTACLK  EQU   C'A'                                                             
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO27**,CLEAR=YES,RR=RE                                       
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
         B     INIT06                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)              BLOCK #1         
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
INIT06   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
         MVC   ATWA,LP_ATWA        OFFLINE TWA AREA SET BY RUNNER               
                                                                                
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
         L     R7,ATWA             R7=A(ON/OFFLINE TWA)                         
         USING TWAD,R7                                                          
                                                                                
         MVI   TWAMODE,0                                                        
                                                                                
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
         JZ    RUNSTR02            NO                                           
         LHI   R0,TWAMAX                                                        
         GETMAIN R,LV=(0)          ACQUIRE STORAGE FOR OFFLINE TIA              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ATIA             SET A(TIA)                                   
         L     RF,AMASTC                                                        
         MVC   VACCEMU,MCVACEMU-MASTD(RF)                                       
         L     RF,ACOMFACS         YES - LOAD FACILITIES OVERLAYS               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
         J     RUNSTR04                                                         
                                                                                
RUNSTR02 L     RF,AACCFACS                                                      
         MVC   VACCEMU,X_AACCEMU-X_ACCFACSD(RF)                                 
                                                                                
         LHI   R1,BRO#GENR                                                      
         CLC   LP_QMAPN,=AL2(A#TINI)   time initial call                        
         JNE   *+8                                                              
         LHI   R1,BRO#TIME                                                      
         GOTOR (#SETFAC,ASETFAC)                                                
                                                                                
RUNSTR04 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR06 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#TIMREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                 
         MVC   LP_BLKS+((B#ORD-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         MVC   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)(L'LP_BLKS),ACOBLOCK             
         MVC   LP_BLKS+((B#GOBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBLOCB              
         MVC   LP_BLKS+((B#GOBBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBBLCK             
         MVC   LP_BLKS+((B#GOXBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOXBLCK             
         MVC   LP_BLKS+((B#PTASK-1)*L'LP_BLKS)(L'LP_BLKS),AIO7                  
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
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
         SPACE 1                                                                
PRCWRK   LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    RUNI,RUNI                                                        
         ICM   RF,15,AMASTC        TEST RUNNING OFFLINE                         
         BZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
PRCWRK02 DS    0H                                                               
*                                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2ETIM                                                    
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         MVC   TI_INRN,TEMP2                                                    
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         MVC   USRID,CUUSER                                                     
         MVC   GLOBID,CPYUID                                                    
         TM    CPYSTATC,CPYSROFF OFFICES FOR RESOURCES                          
         JNZ   RUNREQ2                                                          
         MVC   USRID,GLOBID        NO ALWAYS USE GLOBAL ID                      
                                                                                
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
                                                                                
         GOTOR INIBUF                                                           
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
*&&UK                                                                           
         CLI   CUTSYS,X'73'                                                     
         JNE   RUNREQ4                                                          
         MVC   D_TODYP,=X'B40101'  Set this date for                            
         CLC   CUAALF,=C'BA'       QAROD*, QFROD*, QMROD*                       
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'BB'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'BC'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'AD'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'AA'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'AB'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'AC'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'CA'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'CB'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'CC'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'A1'                                                    
         JE    RUNREQ4                                                          
         CLC   CUAALF,=C'Y9'                                                    
         JE    RUNREQ4                                                          
         MVC   D_TODYP,=X'B60101'  Set this date for                            
         CLC   CUAALF,=C'16'       German Aura logon                            
         JE    RUNREQ4                                                          
         MVC   D_TODYP,=X'A60701'                                               
RUNREQ3  GOTOR VDATCON,DMCB,(1,D_TODYP),(2,D_TODYC)                             
         J     RUNREQ6                                                          
*&&                                                                             
                                                                                
RUNREQ4  GOTOR VDATCON,DMCB,(5,0),(1,D_TODYP)                                   
         GOTOR (RF),(R1),,(2,D_TODYC)                                           
RUNREQ6  GOTOR (RF),(R1),,(3,D_TODYB)                                           
         GOTOR (RF),(R1),,(0,D_TODYF)                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
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
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
**********************************************************************          
* TIMESHEET INITIAL DOWNLOAD                                         *          
**********************************************************************          
                                                                                
REQTIMI  LKREQ H,A#TINI,OUTTMI,NEXTREQ=REQTIMD                                  
Initial  LKREQ F,01,(D,B#SAVED,QINITIAL),CHAR,OLEN=L'QINITIAL,         +        
               MAXLEN=L'QINITIAL,TEXT=(*,INITLIT),COL=*                         
IOCTS    LKREQ F,02,(D,B#SAVED,QCOCTS),CHAR,OLEN=L'QCOCTS,             +        
               MAXLEN=L'QCOCTS,TEXT=(*,CTSOLIT),COL=*                           
IOTSD    LKREQ F,03,(D,B#SAVED,QCOTSD),CHAR,OLEN=L'QCOTSD,             +        
               MAXLEN=L'QCOTSD,TEXT=(*,ODFTSDI),COL=*                           
         LKREQ E                                                                
                                                                                
OUTTMI   LKOUT H                                                                
                                                                                
GENINI   LKOUT R,A#GINI            General initial                              
Array    LKOUT C,A#GINI,(A,ARYGINI),FILTROUT=TSTRODK1                           
         LKOUT E                                                                
                                                                                
OUTOVTM1 LKOUT R,A#OVTM            Overdue timesheets                           
Array    LKOUT C,A#OVTM,(A,ARYOVTM),FILTROUT=TSTRODK1                           
Prout    LKOUT P,,RESINI                                                        
         LKOUT E                                                                
                                                                                
NARNAR   LKOUT R,A#NARR            Narrative record                             
Array    LKOUT C,A#NARR,(A,ARYSCM),FILTROUT=TSTRODK2                            
         LKOUT E                                                                
                                                                                
NCLLST   LKOUT R,A#ACTS            Non-client list                              
Array    LKOUT C,A#ACTS,(A,ARY1NAC),FILTROUT=TSTRODK2                           
         LKOUT E                                                                
                                                                                
TIMINI   LKOUT R,R#TINI            Time initial                                 
Array    LKOUT C,1,(A,ARYINI),FILTROUT=TST1ST                                   
         LKOUT E                                                                
                                                                                
TIMHDR1  LKOUT R,R#TIMHDR          Time header                                  
Array    LKOUT C,R#TIMHDR,(A,ARYHDR),FILTROUT=TSTQENDD                          
         LKOUT E                                                                
                                                                                
TIMROW1  LKOUT R,R#TIMTIM          Timeline information - real time             
Array    LKOUT C,R#TIMTIM,(A,ARYTLN)                                            
         LKOUT E                                                                
*                              **  Second call - PREV timelines **              
TIMROW1B LKOUT R,R#TIMTIM          Timeline information - from prev             
PROUT    LKOUT P,,SETPRWJ                                                       
Array    LKOUT C,R#TIMTIM,(A,ARYTLN),FILTROUT=TSTWIDG                           
         LKOUT E                                                                
                                                                                
TIMROW2  LKOUT R,R#TIMTIM          Timeline information - holiday               
Array    LKOUT C,R#TIMTIM,(A,ARYHOL),FILTROUT=TSTPRE2                           
         LKOUT E                                                                
                                                                                
TIMROW3  LKOUT R,R#TIMTIM          Timeline information - template              
Array    LKOUT C,1,(A,ARYTMP),FILTROUT=TSTTEMP                                  
         LKOUT E                                                                
                                                                                
TIMROW4  LKOUT R,R#TIMTIM          Timeline info - from resources               
Array    LKOUT C,1,(A,ARYRES),FILTROUT=TSTTRES                                  
         LKOUT E                                                                
                                                                                
TIMROW5  LKOUT R,R#TIMTIM          Timeline information - blank lines           
Array    LKOUT C,1,(A,ARYBLNK),FILTROUT=TSTPRRD                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYINI   LKOUT A,(R,TIMEINI),ROWNAME=COBLOCKD                                   
AgyCur   LKOUT C,1,(D,B#WORKD,AGYCURR),CHAR,LEN=L'AGYCURR,ND=Y                  
OfcInd   LKOUT C,2,(D,B#WORKD,OFFIND),CHAR,LEN=L'OFFIND,ND=Y                    
ForCur   LKOUT C,3,(D,B#SAVED,TI_FCUR),CHAR,LEN=L'TI_FCUR,ND=Y                  
Pers     LKOUT C,4,(D,B#SAVED,QPERSN),CHAR,LEN=L'QPERSN,ND=Y                    
PEndt    LKOUT C,5,(D,B#SAVED,QENDD),PDAT,LEN=L'QENDD,ND=Y                      
LocEnd   LKOUT C,6,(D,B#SAVED,QLOCED),PDAT,LEN=L'QLOCED,ND=Y                    
Hire     LKOUT C,7,(D,B#SAVED,TI_HIRE),PDAT,LEN=L'TI_HIRE,ND=Y                  
Term     LKOUT C,8,(D,B#SAVED,TI_TERM),PDAT,LEN=L'TI_TERM,ND=Y                  
TimeTyp  LKOUT C,9,COTYT,CHAR,ND=Y                                              
CliCde   LKOUT C,10,COCLC,CHAR,ND=Y                                             
ProCde   LKOUT C,11,COPRC,CHAR,ND=Y                                             
JobCde   LKOUT C,12,COJOC,CHAR,ND=Y                                             
Task     LKOUT C,13,COTSK,CHAR,ND=Y                                             
NCliCde  LKOUT C,14,CONCC,CHAR,ND=Y                                             
Total    LKOUT C,15,COTOT,CHAR,ND=Y                                             
CliNme   LKOUT C,16,COCNM,CHAR,ND=Y                                             
ProNme   LKOUT C,17,COPNM,CHAR,ND=Y                                             
JobNme   LKOUT C,18,COJNM,CHAR,ND=Y                                             
NCliNme  LKOUT C,19,CONCN,CHAR,ND=Y                                             
BillHrs  LKOUT C,20,COBHR,CHAR,ND=Y                                             
ChrgHrs  LKOUT C,21,COCHR,CHAR,ND=Y                                             
OrdNum   LKOUT C,22,COORD,CHAR,ND=Y                                             
IntRef   LKOUT C,23,COINR,CHAR,ND=Y                                             
OrdCol   LKOUT C,24,COORC,CHAR,ND=Y                                             
NBHrs    LKOUT C,25,CONBH,CHAR,ND=Y                                             
NCHrs    LKOUT C,26,CONCH,CHAR,ND=Y                                             
InRCol   LKOUT C,27,COIRC,CHAR,ND=Y                                             
InRNam   LKOUT C,28,(D,B#SAVED,TI_INRN),CHAR,LEN=L'TI_INRN,ND=Y                 
NoJob    LKOUT C,29,CONJB,CHAR,ND=Y                                             
FNarra   LKOUT C,30,COFNR,CHAR,ND=Y                                             
TarUti   LKOUT C,31,COTTU,CHAR,ND=Y                                             
EstCol   LKOUT C,32,COESC,CHAR,ND=Y                                             
BlnkLins LKOUT C,33,CONBL,SPAK,LEN=4,ND=Y                                       
Monday   LKOUT C,34,CODMO,CHAR,ND=Y                                             
Tuesday  LKOUT C,35,CODTU,CHAR,ND=Y                                             
Wedsday  LKOUT C,36,CODWE,CHAR,ND=Y                                             
Thuday   LKOUT C,37,CODTH,CHAR,ND=Y                                             
Friday   LKOUT C,38,CODFR,CHAR,ND=Y                                             
Satday   LKOUT C,39,CODSA,CHAR,ND=Y                                             
Sunday   LKOUT C,40,CODSU,CHAR,ND=Y                                             
EditHrs  LKOUT C,42,(D,B#SAVED,TI_EDTHR),CHAR,LEN=L'TI_EDTHR,ND=Y               
EstEdt   LKOUT C,43,COEST,CHAR,ND=Y                                             
Nudaft   LKOUT C,44,CONDA,SPAK,LEN=4,ND=Y                                       
Futbill  LKOUT C,45,(D,B#SAVED,TI_FUTB),CHAR,ND=Y                               
Futrtim  LKOUT C,46,(D,B#SAVED,TI_RTIME),CHAR,ND=Y                              
Funbclt  LKOUT C,47,(D,B#SAVED,TI_BCLIT),CHAR,ND=Y                              
Funbnct  LKOUT C,48,(D,B#SAVED,TI_BNCLI),CHAR,ND=Y                              
FutApp   LKOUT C,49,COFAP,CHAR,ND=Y                                             
Prevts   LKOUT C,50,(D,B#SAVED,TI_PRNS),CHAR,LEN=L'TI_PRNS,ND=Y                 
Nofuta   LKOUT C,51,COPTA,CHAR,ND=Y                                             
Dialog   LKOUT C,52,CODIA,CHAR,ND=Y                                             
ConSeq   LKOUT C,53,COACS,CHAR,ND=Y                                             
CoCts    LKOUT C,54,COCTS,CHAR,ND=Y                                             
CoTsd    LKOUT C,55,COTSD,CHAR,ND=Y                                             
         LKOUT E                                                                
                                                                                
TIMEINI  MVI   TI_FCUR,NOQ                                                      
         OI    MYBYTE,RUNITIM                                                   
         XC    QENDD,QENDD         Clear as used by overdue array prev          
         TM    CPYSTAT6,CPYSFMCR+CPYSFOCR                                       
         JZ    TINI002                                                          
         MVI   TI_FCUR,YESQ                                                     
TINI002  OC    CCTPID,CCTPID                                                    
         JNZ   TINI004                                                          
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         J     QERROR                                                           
*                                                                               
TINI004  GOTOR CSTPID,DMCB,CCTPID                                               
         JE    TINI006                                                          
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
*                                                                               
TINI006  GOTOR PRSDTL,DMCB,(X'FF',D_TODYP)                                      
         JNE   QERROR                                                           
*                                                                               
TINI008  L     R1,ALOCEL           retrieve office                              
         MVC   TH_1ROFF,LOCOFF-LOCELD(R1)                                       
         MVC   TH_DEPT,LOCDEPT-LOCELD(R1)                                       
         MVC   TH_SDPT,LOCSUB-LOCELD(R1)                                        
         MVC   QLOCED,LOCEND-LOCELD(R1)                                         
         MVC   QLOCST,LOCSTART-LOCELD(R1)                                       
         MVC   QLOCK,LOCLOCK-LOCELD(R1)                                         
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   *+8                                                              
         LA    RE,0                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   D1RACT(0),TH_1ROFF                                               
         AHI   RE,1                                                             
         LA    RF,D1RACT                                                        
         AR    RF,RE                                                            
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),TH_DEPT                                                  
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),TH_SDPT                                                  
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),QPERSN                                                   
         GOTOR CHKMCS                                                           
*                                                                               
         MVI   TI_EDTHR,C'N'                                                    
         TM    CPYSTAT9,CPYSEDHO                                                
         JZ    *+8                                                              
         MVI   TI_EDTHR,C'Y'                                                    
*                                                                               
         GOTOR (#CSTPRF,ACSTPRF),DMCB,D1RACT  get approvers profiles            
         L     R3,ACOBLOCK                                                      
         MVC   LP_ADATA,ACOBLOCK                                                
         USING COBLOCKD,R3                                                      
         OC    QCOCTS,QCOCTS                  Overriden COCTS?                  
         JNZ   *+10                                                             
         MVC   QCOCTS,COCTS                   Use COCTS from cost prof          
         OC    QCOTSD,QCOTSD                  Overriden COTSD?                  
         JNZ   *+10                                                             
         MVC   QCOTSD,COTSD                   Use COTSD from cost prof          
         MVI   TI_FUTA,C'N'                   FILL WITH N'S                     
         MVC   TI_FUTA+1(TI_FUTAL-1),TI_FUTA                                    
         LA    RE,COFTA                                                         
         XR    RF,RF                                                            
         LHI   RF,L'COFTA                                                       
*                                                                               
TINI09   CLI   0(RE),C','                                                       
         JE    TINI09A                                                          
         CLI   0(RE),C'B'                                                       
         JNE   *+8                                                              
         MVI   TI_FUTB,C'Y'                                                     
         CLI   0(RE),C'R'                                                       
         JNE   *+8                                                              
         MVI   TI_RTIME,C'Y'                                                    
         CLI   0(RE),C'C'                                                       
         JNE   *+8                                                              
         MVI   TI_BCLIT,C'Y'                                                    
         CLI   0(RE),C'N'                                                       
         JNE   *+8                                                              
         MVI   TI_BNCLI,C'Y'                                                    
TINI09A  AHI   RE,1                                                             
         JCT   RF,TINI09                                                        
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         CLI   QCOTSD,COOLDUN      Skip all this if doing new status            
         JE    TINI024                                                          
         CLI   QCOTSD,CONONE                                                    
         JE    TINIY                                                            
         L     R1,ALOCEL           retrieve office                              
         OC    LOCLOCK-LOCELD(L'LOCLOCK,R1),LOCLOCK-LOCELD(R1)                  
         JNZ   TINIY               if lock date don't display timesheet         
                                                                                
         MVC   DCDAT,DDATE                                                      
         MVC   DCDAT+1(1),BYTE1                                                 
*                                                                               
         CLC   DDATE+1(1),BYTE1                                                 
         JNL   TINI010                                                          
         XR    R1,R1                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
*                                                                               
TINI010  GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
*&&US*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'10'                             
*&&UK*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
         OC    QENDD,QENDD         Anything in QEND?                            
         JZ    TINI011                                                          
         CLC   DCEND,QENDD         Only update if QEND is higher                
         JNL   TINI011                                                          
         MVC   DCEND,QENDD                                                      
TINI011  GOTOR GTCAL                                                            
         JNE   QERROR                                                           
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R4                                                        
         L     R2,AIO3                                                          
         LA    R4,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
TINI012  CLI   TMPEL,0                                                          
         JNE   TINI014                                                          
         DC    H'0'                                                             
TINI014  CLI   TMPEL,TMPELQ                                                     
         JE    TINI018                                                          
                                                                                
TINI016  ST    R4,APREVPER        SAVE ADDRESS OF PREVIOUS PERIOD               
         IC    R0,TMPLN                                                         
         AR    R4,R0                                                            
         J     TINI012                                                          
                                                                                
TINI018  CLC   DDATE,TMPEND                                                     
         JH    TINI016                                                          
         CLC   DDATE,TMPSTART                                                   
         JL    TINI016                                                          
         CLI   QCOTSD,CONEWST                                                   
         JNE   TINI020                                                          
         MVC   QENDD,TMPEND       ONLY FOR NEW START DATE                       
         OC    QLOCED,QLOCED                                                    
         JZ    TINI020                                                          
         CLC   QLOCED,QENDD                                                     
         JNH   TINI020                                                          
         XC    QLOCED,QLOCED                                                    
         J     TINI020                                                          
*                                                                               
TINI020  L     R4,APREVPER         See if any time for previous period          
         CLC   TMPEND,TI_HIRE      Is previous period before start date         
         JL    TINI021                                                          
         XC    IOKEY,IOKEY                                                      
         USING TSWRECD,IOKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         XR    RE,RE                                                            
         ICM   RE,7,TMPEND                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND       2'S COMPLEMENT                                
         MVC   CSVKEY1(TSWKODS-TSWRECD),TSWRECD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         MVI   TI_PRNS,C'N'                                                     
         CLC   CSVKEY1(TSWKODS-TSWRECD),IOKEY                                   
         JNE   TINI022            prev timesheet not even started               
         OC    TSWKSTAT,TSWKSTAT  no status means not submitted                 
         JZ    TINI022                                                          
TINI021  MVI   TI_PRNS,C'Y'                                                     
TINI022  J     TINI050                                                          
         DROP  R2,R3,R4                                                         
*                                                                               
         USING COBLOCKD,R3                                                      
TINI024  L     R3,ACOBLOCK                                                      
         CLI   QCOTSD,COOLDUN      IS IT GET OLDEST UNSUBMITTED                 
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   WRKDATE,TI_HIRE                                                  
         XC    DDATE,DDATE                                                      
         LA    R4,BRATAB                                                        
         OC    0(BRATABX-BRATAB,R4),0(R4)                                       
         JZ    TINIY                       no BrO dates AT ALL                  
         SR    RE,RE               RE IS A(LOWEST LEVEL USEFUL ENTRY)           
*                                                                               
TINI026  CLI   0(R4),X'FF'         BRATABX                                      
         JE    TINI028                                                          
         OC    0(L'GDADATE+L'GDADATE2,R4),0(R4)                                 
         JZ    TINI028             BLANK ENTRY = EOT                            
         LR    RE,R4                                                            
         CLC   0(L'GDADATE,R4),L'GDADATE(R4)                                    
         JNE   *+6                 EQUAL DATES = INACTIVE                       
         SR    RE,RE                                                            
         LA    R4,L'GDADATE2+L'GDADATE(R4)                                      
         J     TINI026                                                          
*                                                                               
TINI028  LTR   RE,RE                                                            
         JZ    TINIY               NO RELEVANT DATES                            
         LR    R4,RE                                                            
*                                                                               
         CLC   0(3,R4),TI_HIRE      COMPARE WITH FIRST MCS DATE                 
         JNH   *+10                                                             
         MVC   WRKDATE,0(R4)                                                    
         CLC   WRKDATE,QLOCST       COMPARE WITH LOCATION START DATE            
         JNL   *+10                                                             
         MVC   WRKDATE,QLOCST                                                   
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         MVC   DCDAT,WRKDATE                                                    
         MVC   DCDAT+1(1),BYTE1                                                 
*                                                                               
         CLC   WRKDATE+1(1),BYTE1                                               
         JNL   TINI030                                                          
         XR    R1,R1                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
                                                                                
TINI030  GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
*&&UK*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
*&&US*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'10'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
*                                                                               
         OC    QENDD,QENDD         Anything in QEND?                            
         JZ    TINI031                                                          
         CLC   DCEND,QENDD         Only update if QEND is higher                
         JNL   TINI031                                                          
         MVC   DCEND,QENDD                                                      
TINI031  GOTOR GTCAL                                                            
         JNE   QERROR                                                           
*                                                                               
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
TINI032  CLI   TMPEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TMPEL,TMPELQ                                                     
         JE    TINI036                                                          
                                                                                
TINI034  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     TINI032                                                          
                                                                                
TINI036  CLC   WRKDATE,TMPEND                                                   
         JH    TINI034                                                          
         CLC   WRKDATE,TMPSTART                                                 
         JL    TINI034                                                          
TINI038  OC    QLOCK,QLOCK         Any lock date?                               
         JZ    TINI039                                                          
         CLC   TMPEND,QLOCK        Check against lock date                      
         JH    *+10                                                             
TINI039  MVC   QENDD,TMPEND                                                     
         CLC   QENDD,QLOCED                                                     
         JNL   *+10                                                             
         XC    QLOCED,QLOCED       web app always prefers loced                 
                                                                                
         GOTOR CHKLOA                                                           
         JNE   TINI042                                                          
         USING TSWRECD,R2                                                       
         LA    R2,IOKEY            Build start key for read                     
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         XR    RE,RE                                                            
         ICM   RE,7,TMPEND                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND                                                     
         MVC   CSVKEY1(TSWKODS-TSWRECD),TSWRECD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    TINI040                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
TINI040  CLC   CSVKEY1(TSWKODS-TSWRECD),TSWRECD                                 
         JNE   TINI050                                                          
         CLI   TSWKSTAT,0          IS IT SAVED TIME                             
         JE    TINI050             NO                                           
TINI042  XR    R0,R0                                                            
         IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         CLI   TMPEL,TMPELQ                                                     
         JE    TINI038                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(1,QENDD),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WRKDATE)                              
*                                                                               
         CLC   QENDD(1),WRKDATE    Are we still in the same year?               
         JNE   *+14                                                             
         MVC   QENDD,WRKDATE       Update QENDD with new date                   
         J     TINI045                                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,CPYSFST                                                       
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         MVC   DCDAT,WRKDATE                                                    
         MVC   DCDAT+1(1),BYTE1                                                 
*                                                                               
         CLC   WRKDATE+1(1),BYTE1                                               
         JNL   TINI044                                                          
         XR    R1,R1                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
*                                                                               
TINI044  GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
*&&US*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'10'                             
*&&UK*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
*                                                                               
TINI045  OC    QENDD,QENDD         Anything in QEND?                            
         JZ    TINI045A                                                         
         CLC   DCEND,QENDD         Only update if QEND is higher                
         JNL   TINI045A                                                         
         MVC   DCEND,QENDD                                                      
TINI045A GOTOR GTCAL                                                            
         JNE   QERROR                                                           
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
TINI046  CLI   TMPEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TMPEL,TMPELQ                                                     
         JE    TINI038                                                          
         IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     TINI046                                                          
*                                                                               
TINI050  DS    0H                                                               
*&&UK                                                                           
         OC    QENDD,QENDD         Anything in QEND?                            
         JZ    TINIY                                                            
         TM    CPYSTATD,CPYSRSTS   Restrict T/S by status                       
         JZ    TINIY                                                            
         CLC   CCTPID,DPPID        Is the user the connected person             
         JNE   TINIY               No                                           
         XC    IOKEY,IOKEY                                                      
         USING TSWRECD,IOKEY       Build start key for read                     
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         XR    RE,RE                                                            
         ICM   RE,7,QENDD                                                       
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND                                                     
         MVC   CSVKEY1(TSWKODS-TSWRECD),TSWRECD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   TINIY                                                            
         CLC   CSVKEY1(TSWKODS-TSWRECD),IOKEY                                   
         JNE   TINIY               Timesheet not found                          
         TM    TSWKSTAT,TIMSFAPP+TIMSPAPP                                       
         JZ    TINIY               Not fully/part approved T/S                  
         XC    QENDD,QENDD         Don't show Timesheet in Init call            
*&&                                                                             
TINIY    J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
* General initial call                                                          
                                                                                
ARYGINI  LKOUT A,(R,GENRINI),ROWNAME=SAVED                                      
Code     LKOUT C,1,OGICODE,CHAR,ND=Y                                            
List     LKOUT C,2,OGILIST,CHAR,ND=Y                                            
Nexp     LKOUT C,3,OGINEXP,CHAR,ND=Y                                            
URL      LKOUT C,4,OGIURL,CHAR,ND=Y                                             
Aguid    LKOUT C,5,OGIUID,CHAR,ND=Y                                             
Agname   LKOUT C,6,OGINAME,CHAR,ND=Y                                            
TimLmAp  LKOUT C,7,OGITIMAP,CHAR,ND=Y                                           
Clilen   LKOUT C,8,(D,B#WORKD,PCLILEN),LBIN,ND=Y                                
Prolen   LKOUT C,9,OPROLEN,LBIN,ND=Y                                            
Joblen   LKOUT C,10,OJOBLEN,LBIN,ND=Y                                           
TmBkUp   LKOUT C,11,OTIMBAK,CHAR,ND=Y                                           
Onerl1   LKOUT C,12,(D,B#WORKD,ONERL1L),LBIN,ND=Y                               
Onerl2   LKOUT C,13,OONRL2L,LBIN,ND=Y                                           
Onerl3   LKOUT C,14,OONRL3L,LBIN,ND=Y                                           
Onerl4   LKOUT C,15,OONRL4L,LBIN,ND=Y                                           
ISOCod   LKOUT C,23,OGIAGCUR,CHAR,ND=Y                                          
DecPlc   LKOUT C,24,OGIDEC,LBIN,ND=Y                                            
StMonth  LKOUT C,25,OSTMNTH,LBIN,ND=Y                                           
LtsCal   LKOUT C,26,OLTSCAL,HEXD,ND=Y                                           
Userid   LKOUT C,27,(D,B#WORKD,CUUSER),HEXD,ND=Y                                
PIN      LKOUT C,28,(D,B#WORKD,CCTPID),HEXD,ND=Y                                
MBTime   LKOUT C,29,OGIMBTIM,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
GENRINI  ST    R8,LP_ADATA                                                      
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         STC   RF,OPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         LLC   RE,PPROLEN                                                       
         SR    RF,RE                                                            
         STC   RF,OJOBLEN                                                       
*                                                                               
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE                                                            
         STC   RF,OONRL2L                                                       
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         STC   RF,OONRL3L                                                       
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         STC   RF,OONRL4L                                                       
*                                                                               
         MVI   OGINEXP,NOQ                                                      
         MVI   OGITIMAP,NOQ                                                     
         MVI   OGICODE,NOQ                                                      
         MVI   OTIMBAK,NOQ                                                      
                                                                                
         MVI   OGILIST,NOQ                                                      
         MVI   OGIMBTIM,NOQ                                                     
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'09'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,OSTMNTH                                                       
                                                                                
         TM    CPXSTAT6,CPX2LAEI                                                
         JZ    *+8                 CHECK COMP SETTING TO SEE WHETHER            
         MVI   OGINEXP,C'Y'        NEW EXPENSES WORKFLOW IN USE                 
         TM    CPYSTATB,CPYSBOMT                                                
         JZ    *+8                 CHECK COMP SETTING TO SEE WHETHER            
         MVI   OGIMBTIM,YESQ       MOBILE TIME IN USE                           
                                                                                
GINI02   OC    CCTPID,CCTPID                                                    
         JZ    GINI28                                                           
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            READ APPROVER RECORD                         
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,CCTPID                                                   
         MVI   PIDKSTYP,PIDKBAPQ                                                
         MVC   CSVKEY1,PIDKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     GINI08                                                           
                                                                                
GINI04   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
GINI08   CLC   CSVKEY1(PIDKPER-PIDRECD),PIDKEY                                  
         JNE   GINI10                                                           
         TM    PIDKAPPL,LIDLTIME                                                
         JZ    GINI04                                                           
         MVI   OTIMBAK,YESQ                                                     
                                                                                
         USING APPRECD,R2                                                       
GINI10   LA    R2,IOKEY            READ APPROVER RECORD                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,CCTPID                                                  
         MVC   CSVKEY1,APPKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   GINI28                                                           
         MVI   OGICODE,YESQ                                                     
         J     GINI14                                                           
GINI12   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         LA    R2,IOKEY                                                         
         CLC   CSVKEY1(APPKSEQ-APPRECD),APPKEY                                  
         JNE   GINI28                                                           
                                                                                
GINI14   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         XR    RF,RF                                                            
         LA    R3,APPRFST                                                       
         USING LIDELD,R3                                                        
GINI16   CLI   LIDEL,0                                                          
         JE    GINI12                                                           
         CLI   LIDEL,LIDELQ                                                     
         JNE   GINI24                                                           
                                                                                
GINI18   CLI   LIDTYPE,LIDTAP1R                                                 
         JNE   GINI24                                                           
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R1,LIDLN                                                         
         AR    R1,R3               R3=End of element                            
                                                                                
GINI20   TM    LIDAPDTY,LIDAPDTI   Is this entry for timesheets                 
         JNZ   GINI26              Yes                                          
                                                                                
GINI22   LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R1,R4               Check we haven't reached end of el           
         JH    GINI20              No - check next entry                        
GINI24   IC    RF,LIDLN                                                         
         AR    R3,RF                                                            
         J     GINI16                                                           
                                                                                
GINI26   MVI   OGITIMAP,YESQ       Set time line manager approver               
GINI28   MVI   OGILIST,NOQ         set Limit/Group List flag                    
         LA    R2,IOKEY                                                         
         USING LLSRECD,R2                                                       
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY1,LLSKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   GINI30                                                           
         CLC   LLSKEY(LLSKGRP-LLSRECD),CSVKEY1                                  
         JNE   GINI30                                                           
         MVI   OGILIST,C'G'                                                     
         CLC   LLSKGRP,SPACES                                                   
         JH    GINI30                                                           
         MVI   OGILIST,C'L'                                                     
                                                                                
GINI30   GOTOR GETURL,OGIURL       PULL URL                                     
         MVC   OGIUID,CPYLOGO      GET AGENCY USER ID                           
         GOTOR GETAGN,OGINAME      GET AGENCY NAME                              
*&&US                                                                           
         MVC   OGIAGCUR,=C'USD'    AgPASS ISO CUR CODE                          
         MVI   OGIDEC,2                           - DECIMAL POSITIONS           
*&&                                                                             
*&&UK                                                                           
         USING GCURD,R2            R3=A(KEY)                                    
         LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
         MVC   GCKCURU,AGYCURR                                                  
         MVC   CSVKEY1,GCKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   GINI36                                                           
         CLC   GCKEY(GCKCURX-GCURD),CSVKEY1                                     
         JNE   GINI36                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         AHI   R2,GCFIRST                                                       
         USING GCREL,R2                                                         
                                                                                
GINI32   CLI   GCREL,0                                                          
         JE    GINI36                                                           
         CLI   GCREL,GCRELQ                                                     
         JE    GINI34                                                           
         LLC   R0,GCRLEN                                                        
         AR    R2,R0                                                            
         J     GINI32                                                           
                                                                                
GINI34   MVC   OGIDEC,GCRDECP                                                   
         MVC   OGIAGCUR,AGYCURR                                                 
                                                                                
GINI36   DS    0H                                                               
*&&                                                                             
         J     EXITY                                                            
         DROP  R2                                                               
** Overdue timesheets *************************************************         
                                                                                
ARYOVTM  LKOUT A,(R,OVRTIM),ROWNAME=SAVED                                       
Ovrdue   LKOUT C,1,OOVERDUE,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
OVRTIM   ST    R8,LP_ADATA                                                      
         MVI   OOVERDUE,C'N'       Set default no overdue timesheets            
         XC    OVFLAG,OVFLAG                                                    
         OC    CCTPID,CCTPID       Any connected PID                            
         JZ    OVRTIMX             No - must be DDS                             
         MVC   HALF1,CCTPID                                                     
         OC    QPID,QPID           Were we passed a PID                         
         JZ    OVTIM05             No                                           
         MVC   TEMP2,QPID                                                       
         OC    TEMP2(L'QPID),SPACES                                             
         GOTOR (#GETPIN,AGETPIN)   (uses AIO1)                                  
         MVC   HALF1,TEMP2+50                                                   
         JE    OVTIM05                                                          
         MVC   LP_ERROR,=AL2(AE$IVPER)                                          
         J     QERROR                                                           
                                                                                
OVTIM05  GOTOR CSTPID,DMCB,HALF1                                                
         JE    OVTIM10                                                          
         MVC   LP_ERROR,=AL2(AE$PIDPR)                                          
         J     QERROR                                                           
*                                                                               
OVTIM10  GOTOR PRSDTL,DMCB,(X'00',D_TODYP) (Uses AIO1)                          
         JNE   QERROR                                                           
*                                                                               
         L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO4                                                          
         LA    R2,PERRFST-PERRECD(R2)                                           
         ST    R2,FULL2            Save start of elements                       
         USING LOCELD,R2                                                        
OVTIM15  CLI   LOCEL,0             End of record                                
         JE    OVRTIMX             Yes - exit                                   
         CLI   LOCEL,LOCELQ                                                     
         JE    OVTIM25                                                          
OVTIM20  L     R2,FULL2            Re-establish R2 as loop address              
         LLC   RE,LOCLN                                                         
         AR    R2,RE                                                            
         ST    R2,FULL2            Save off address of Current loop             
         J     OVTIM15                                                          
                                                                                
OVTIM25  LLC   RE,ONERL1L          For each location build 1R account           
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   D1RACT(0),LOCOFF                                                 
         MVC   D1ROFF,D1RACT                                                    
         OC    D1ROFF,SPACES                                                    
         AHI   RE,1                                                             
         LA    R3,D1RACT                                                        
         AR    R3,RE                                                            
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),LOCDEPT                                                  
         AHI   RF,1                                                             
         AR    R3,RF               R3=A(end of dept code on 1R account)         
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),LOCSUB                                                   
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),QPERSN                                                   
         OC    D1RACT,SPACES                                                    
         GOTOR CHKMCS           Get brandocean switch on and off dates          
         GOTOR CHKPLK           Check if person is locked already               
         JE    *+12                                                             
         MVI   OOVERDUE,C'Y'    Set as overdue                                  
         J     OVRTIMX                                                          
                                                                                
         GOTOR (#CSTPRF,ACSTPRF),DMCB,D1RACT  Get cost profile                  
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         ZAP   DUB1,CONDO                                                       
         CVB   R4,DUB1                                                          
         LNR   R4,R4                                                            
                                                                                
         MVC   WORK(L'D_TODYF),D_TODYF  Work out Overdue date                   
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R4)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DOVRDTE)                              
                                                                                
         XC    NTFY#(NTFYLNQ),NTFY#                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R1,24               Hours in a day                               
         CLC   CONCM,SPACES                                                     
         JE    OVTIM30                                                          
         OC    CONCM,CONCM         ANY NOTIFICATION NEEDED?                     
         JZ    OVTIM30                                                          
         ZAP   DUB1,CONCM                                                       
         JZ    OVTIM30                                                          
         CVB   RF,DUB1                                                          
         STCM  RF,3,NTFY#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,NTFYHRS                                                     
         MVC   LOCK#,NTFY#         use NOTIFY parms as LOCK defaults            
         MVC   LOCKHRS,NTFYHRS                                                  
*                                                                               
OVTIM30  CLC   CONCL,SPACES                                                     
         JE    OVTIM35                                                          
*&&US*&& OC    CONCL,CONCL         ANY NOTIFICATION NEEDED?                     
*&&US*&& JE    OVTIM35                                                          
         ZAP   DUB1,CONCL                                                       
         JZ    OVTIM35                                                          
         CVB   RF,DUB1                                                          
         STCM  RF,3,LOCK#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,LOCKHRS                                                     
                                                                                
OVTIM35  LA    R4,BRATAB                                                        
OVTIM40  OC    0(L'GDADATE*2,R4),0(R4)                                          
         JZ    OVTIM20                                                          
         CLC   0(L'GDADATE,R4),L'GDADATE(R4)                                    
         JNE   OVTIM50                                                          
OVTIM45  LA    R4,L'GDADATE2+L'GDADATE(R4)                                      
         J     OVTIM40                                                          
*                                                                               
OVTIM50  MVC   DDATE,0(R4)                                                      
         CLC   0(L'GDADATE,R4),LOCSTART  Brndocn on date after loc strt         
         JH    OVTIM55             Yes                                          
         MVC   DDATE,LOCSTART     Set location start date as start              
         OC    L'GDADATE(L'GDADATE,R4),L'GDADATE(R4) Any brndocn off            
         JZ    OVTIM60             No                                           
         CLC   L'GDADATE(L'GDADATE,R4),LOCSTART Brndocn off date before         
         JNH   OVTIM45                   location start date - get next         
         J     OVTIM60                                                          
                                                                                
OVTIM55  OC    LOCEND,LOCEND       Do we have an end date                       
         JZ    OVTIM60             No                                           
         CLC   0(L'GDADATE,R4),LOCEND   Brndocn on date after loc end           
         JH    OVTIM45             Yes - get next entry from table              
OVTIM60  MVC   QENDD,L'GDADATE(R4)  Set brandocean switch off date              
         OC    LOCEND,LOCEND       Any location end date                        
         JZ    OVTIM70             No                                           
         OC    L'GDADATE(L'GDADATE,R4),L'GDADATE(R4) Any bndocn off             
         JZ    OVTIM65             No - set location end date                   
         CLC   L'GDADATE(L'GDADATE,R4),LOCEND  Bndocn off date before           
         JL    OVTIM45                   location end date                      
OVTIM65  MVC   QENDD,LOCEND         No - set location end date as end           
                                                                                
OVTIM70  OC    QENDD,QENDD         Any end date so far                          
         JZ    OVTIM75             No                                           
         CLC   DOVRDTE,QENDD       Yes - check is overdue is sooner             
         JNL   *+10                No                                           
OVTIM75  MVC   QENDD,DOVRDTE       Yes - set overdue date as end date           
                                                                                
                                                                                
         OC    LOCLOCK,LOCLOCK     Check for timesheet lock date                
         JZ    OVTIM80             None present                                 
         CLC   LOCLOCK,QENDD       Check earlier than end date                  
         JH    *+10                No                                           
         MVC   QENDD,LOCLOCK       Yes - set end date as locked date            
OVTIM80  CLC   DDATE,QENDD         Ensure start date is lower than end          
         JNL   OVTIM20             No - go to next location                     
         GOTOR CALPRDS             Get period end dates into Genarea            
         JNE   QERROR                                                           
                                                                                
         USING PERTABD,R4                                                       
OVTIM85  L     R4,AIO5                                                          
OVTIM90  CLI   0(R4),0             Any dates left                               
         JE    OVTIM20             No - go to next location                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,PERENDT                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,DPERENDT                                                    
                                                                                
         USING TSWRECD,R3                                                       
OVTIM95  LA    R3,IOKEY            Build key for reading timesheets             
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN      Add person code                              
         MVC   TSWKEND,PERENDT                                                  
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TSWKODS(0),D1RACT    Add office dept sub dept                    
         OC    TSWKODS,SPACES                                                   
         MVC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD  Save key                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    OVTIM110                                                         
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
OVTIM100 LA    R3,IOKEY            Re-establish Key for IOSQ compare            
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
OVTIM105 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
OVTIM110 CLC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD                                 
         JNE   OVTIM115            Record not found                             
*                                                                               
         CLI   TSWKSTAT,0          Is it saved time                             
         JNE   OVTIM120            Ok - If not in progress                      
*                                                                               
OVTIM115 CLC   DPERENDT,DOVRDTE    Just check NDO profile                       
         JL    OVTIM999                                                         
         J     OVTIM120                                                         
*                                                                               
OVTIM120 LA    R4,PERLENQ(R4)      Bump to next period                          
         J     OVTIM90             Build key for next period                    
*                                                                               
OVTIM999 MVI   OOVERDUE,C'Y'       Set overdue timesheets                       
OVRTIMX  J     EXITY                                                            
         DROP  R4                                                               
                                                                                
** Narrative list *****************************************************         
                                                                                
ARYSCM   LKOUT A,(R,NXTSCM),MULTIROW=Y,ROWNAME=SCMRECD                          
                                                                                
SCCode   LKOUT C,1,SCMKCODE,CHAR                                                
SCSetV   LKOUT P,,SETSCM                                                        
SCDesc   LKOUT C,2,(D,B#SAVED,DNARRDS),CHAR,ND=Y                                
SCFilt   LKOUT C,3,(D,B#SAVED,DNARRFI),CHAR,ND=Y                                
SCGrup   LKOUT C,4,(D,B#SAVED,DNARRGR),CHAR,ND=Y                                
SCOffC   LKOUT C,6,(D,B#SAVED,DNARROF),CHAR,ND=Y                                
SCCliC   LKOUT C,7,(D,B#SAVED,DNARRCL),CHAR,ND=Y                                
SCAppl   LKOUT C,8,(D,B#SAVED,DNARRAP),CHAR,ND=Y                                
SCProC   LKOUT C,9,(D,B#SAVED,DNARRPR),CHAR,ND=Y                                
SCWrkC   LKOUT C,10,(D,B#SAVED,DNARRWC),CHAR,ND=Y                               
Array    LKOUT C,A#NARR,(A,ARYTXT)                                              
                                                                                
         LKOUT E                                                                
                                                                                
ARYTXT   LKOUT A,(D,B#SCMREC,SCMRFST),EOT=EOR,NEWEL=N,                 +        
               ROWID=(SCMEL,SCMELQ),ROWWIDTH=(V,SCMLN)                          
ITTXT    LKOUT C,5,SCMNARR,CHAR,LEN=V                                           
         LKOUT E                                                                
***********************************************************************         
* SCM record routines                                                 *         
***********************************************************************         
                                                                                
NXTSCM   CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXTSCM4                                                          
                                                                                
         OC    QNARCOD(QNARLEN),SPACES                                          
         MVC   QNARSTA,SPACES                                                   
         MVC   QNAREND,LSTNAR      Read for all                                 
NXTSCM4  GOTOR (#NXTREC,ANXTREC),DMCB,SCMKEYT,('B#SCMREC',0),          +        
               (0,SAVED),ASCMKF,ASCMRF                                          
         J     EXITY                                                            
         USING SCMRECD,R2                                                       
         USING COIELD,R3                                                        
SETSCM   L     R2,AIO2                                                          
         XC    DNARR(DNARRLQ),DNARR                                             
                                                                                
         XR    R0,R0                                                            
         LA    R3,SCMRFST                                                       
         XC    ELEMENT,ELEMENT                                                  
                                                                                
SETSCM02 CLI   COIEL,0                                                          
         JE    SETSCM06                                                         
         CLI   COIEL,COIELQ                                                     
         JE    SETSCM04                                                         
         IC    R0,COILN                                                         
         AR    R3,R0                                                            
         J     SETSCM02                                                         
                                                                                
SETSCM04 XR    R1,R1                                                            
         IC    R1,COILN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   ELEMENT(0),COIEL                                                 
                                                                                
SETSCM06 LA    R3,ELEMENT                                                       
                                                                                
         MVC   DNARRDS,COIDESC                                                  
         MVC   DNARRGR,COIGRP                                                   
         MVC   DNARRFI,COIFILT                                                  
                                                                                
         TM    SCMRSTAT,SCMKSCLP                                                
         JZ    SETSCM10                                                         
         MVC   DNARROF,COIOFFC                                                  
         MVC   DNARRCL,SCMRSCLI                                                 
         CLI   COILN,COILNXQ                                                    
         JL    SETSCM12                                                         
         MVC   DNARRPR,COIPROC                                                  
         J     SETSCM12                                                         
                                                                                
SETSCM10 MVC   DNARROF,SCMRSOFF                                                 
                                                                                
SETSCM12 DS    0H                                                               
*&&UK*&& MVC   DNARRAP,NONOS                                                    
*&&US*&& MVC   DNARRAP,YEYES       US doesn't have flist/afm narrative          
*                                  so no application specific filters           
SETSCM14 TM    SCMRSAPP,SCMKSRRQ                                                
         JZ    SETSCM16                                                         
         MVI   DNARRAP+0,YESQ                                                   
                                                                                
SETSCM16 TM    SCMRSAPP,SCMKSFBQ                                                
         JZ    SETSCM18                                                         
         MVI   DNARRAP+1,YESQ                                                   
                                                                                
SETSCM18 TM    SCMRSAPP,SCMKSESQ                                                
         JZ    SETSCM20                                                         
         MVI   DNARRAP+2,YESQ                                                   
                                                                                
SETSCM20 TM    SCMRSAPP,SCMKSORQ                                                
         JZ    SETSCM22                                                         
         MVI   DNARRAP+3,YESQ                                                   
                                                                                
SETSCM22 TM    SCMRSAPP,SCMKSTIQ                                                
         JZ    SETSCM24             bypass SETSCM24 for now                     
         MVI   DNARRAP+4,YESQ                                                   
                                                                                
SETSCM24 TM    SCMRSAPP,SCMKSEXQ                                                
         JZ    SETSCM26                                                         
         MVI   DNARRAP+5,YESQ                                                   
                                                                                
SETSCM26 CLI   COILN,COILNXQ                                                    
         JL    SETSCM28                                                         
         MVC   DNARRWC,COIWORK                                                  
                                                                                
SETSCM28 DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
                                                                                
** Non client list ****************************************************         
                                                                                
ARY1NAC  LKOUT A,(R,NXT1NAC),MULTIROW=Y,ROWNAME=ACTRECD                         
                                                                                
Code     LKOUT C,1,ACTKACT,CHAR,ND=Y                                            
Array    LKOUT C,2,(A,ARYCLNM)                                                  
Array    LKOUT C,4,(A,ARYCLAN)                                                  
Array    LKOUT C,5,(A,ARYCLSN)                                                  
Draft    LKOUT C,6,ACTRSTAT,(R,EDTDRFT)                                         
Array    LKOUT C,12,(A,ARYRST)                                                  
ACLevel  LKOUT C,107,(D,B#SAVED,AC_LVL),CHAR,LEN=1,ND=Y                         
Unit     LKOUT C,122,ACTKUNT,CHAR,ND=Y                                          
Ledger   LKOUT C,123,ACTKLDG,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLNM  LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Name     LKOUT C,2,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLSN  LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(SNMEL,SNMELQ),ROWWIDTH=(V,SNMLN)                          
                                                                                
Name     LKOUT C,5,SNMNAME,CHAR                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLAN  LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(XNMEL,XNMELQ),ROWWIDTH=(V,XNMLN)                          
                                                                                
Name     LKOUT C,4,XNMSUBN,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYADR   LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(ADREL,ADRELQ),ROWWIDTH=(V,ADRLN)                          
Array    LKOUT C,11,(A,ARYADR1)                                                 
                                                                                
         LKOUT E                                                                
ARYADR1  LKOUT A,(*,ADRADD1),ROWNAME=ADREL,NROWS=*,ROWWIDTH=L'ADRADD1           
Addrss   LKOUT C,11,ADRADD1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYRST   LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,ROWID=(RSTELD,RSTELQ),   +        
               ROWWIDTH=(V,RSTLN)                                               
         LKOUT P,RSTELD,SETFLT                                                  
Locked   LKOUT C,13,RSTSTAT1,(R,EDTRSTL),ND=Y                                   
Closed   LKOUT C,14,RSTSTAT1,(R,EDTRSTC),ND=Y                                   
LockBil  LKOUT C,28,RSTLSTAT,(R,EDTLBIL),ND=Y                                   
1NFut    LKOUT C,29,RSTSTAT7,(R,EDT1NFT),ND=Y                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYLFLT  LKOUT A,(D,B#SAVED,AC_FLT1),NROWS=5,                          +        
               ROWNAME=AC_FLT1,ROWWIDTH=L'AC_FLT1                               
                                                                                
Flter    LKOUT C,12,AC_FLT1,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYAST   LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,ROWID=(ASTELD,ASTELQ),   +        
               ROWWIDTH=(V,ASTLN)                                               
                                                                                
Curr     LKOUT C,35,ASTCUR,CHAR,ND=Y                                            
FLang    LKOUT C,37,ASTSTAT1,(R,EDTASTFL),ND=Y                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYFFT   LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN)                          
                                                                                
PRout    LKOUT P,FFTTYPE,SETTTYPE                                               
TaxNum   LKOUT C,40,FFTDATA,CHAR,LEN=V,FILTROUT=TSTLTAXN,ND=Y                   
BSroll   LKOUT C,41,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFBSRN,ND=Y                   
VATcod   LKOUT C,42,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFVATC,ND=Y                   
Email    LKOUT C,43,FFTDATA,CHAR,LEN=V,FILTROUT=TSTFPEML,ND=Y                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYSCI   LKOUT A,(D,B#CLIREC,ACTRFST),EOT=EOR,                         +        
               ROWID=(SCIEL,SCIELQ),ROWWIDTH=(V,SCILN)                          
                                                                                
PRout    LKOUT P,SCITYPE,SETTTYPE                                               
CrLim    LKOUT C,44,SCIAMNT,SPAK,FILTROUT=TSTSCLIM,ND=Y                         
ICrLim   LKOUT C,45,SCIAMNT,SPAK,FILTROUT=TSTSICLM,ND=Y                         
                                                                                
         LKOUT E                                                                
                                                                                
NXT1NAC  CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXT1NA06                                                         
         XC    SAVEKEY,SAVEKEY                                                  
         OC    DA1NA,DA1NA         Test non client code list provided           
         JNZ   NXT1NA06            Yes                                          
         CLI   LL_DNCLA,LL_ALLQ    Do we have access to all if no list          
         JE    NXT1NA04                                                         
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
NXT1NA04 MVC   DA1NA,AALL          No - set all                                 
                                                                                
NXT1NA06 MVC   IOKEY,SAVEKEY                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,NCLKEYT,('B#CLIREC',0),          +        
               (0,SAVED),ANCLKF,ANCLRF                                          
         XC    BYTE3,BYTE3                                                      
         MVC   SAVEKEY,IOKEY                                                    
         MVI   AC_LVL,C'1'                                                      
         J     EXITY                                                            
                                                                                
TSTQENDD CLI   QINITIAL,QAPLINI                                                 
         JE    SETCCC                                                           
         CLI   QINITIAL,QTM2ND                                                  
         JE    SETCCC                                                           
         OC    QENDD,QENDD                                                      
         J     SETCCC                                                           
*                                                                               
TSTTINI  TM    MYBYTE,RUNITIM                                                   
         BZR   RE                                                               
         MVI   MYBYTE,0                                                         
         BR    RE                                                               
*                                                                               
TSTPDF   CLI   QPDF,YESQ                                                        
         BR    RE                                                               
*                                                                               
TSTRODK  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         BR    RE                                                               
*                                                                               
TSTRODK1 SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         BNER  RE                                                               
         CLI   QINITIAL,QAPLINI    If Aura check if * or 1 or blank             
         BER   RE                                                               
         OC    QINITIAL,QINITIAL                                                
         BZR   RE                                                               
         CLI   QINITIAL,QTM1ST                                                  
         BR    RE                                                               
*                                                                               
TSTRODK2 SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ          is connected                                
         BNER  RE                                                               
         CLI   QINITIAL,QAPLINI    If Aura check if * or 2 or blank             
         BER   RE                                                               
         OC    QINITIAL,QINITIAL                                                
         BZR   RE                                                               
         CLI   QINITIAL,QTM2ND                                                  
         BR    RE                                                               
*                                                                               
TST1ST   CLI   QINITIAL,QAPLINI                                                 
         BER   RE                                                               
         OC    QINITIAL,QINITIAL                                                
         BZR   RE                                                               
         CLI   QINITIAL,QTM1ST                                                  
         BR    RE                                                               
*                                                                               
TSTLTAXN CLI   WORK,FFTTAXNO   Test whether type is tax number                  
         BR    RE                                                               
                                                                                
TSTFBSRN CLI   WORK,FFTTBSRN   Test whether type is buildg soc roll             
         BR    RE                                                               
                                                                                
*&&UK                                                                           
TSTFPEML CLI   WORK,FFTTPEML   Test whether type is email                       
         BR    RE                                                               
*&&                                                                             
*&&US                                                                           
TSTFPEML CLI   WORK,FFTTEML    Test whether type is email                       
         BR    RE                                                               
*&&                                                                             
                                                                                
TSTFVATC CLI   WORK,FFTTVATC   Test whether type is vat code                    
         BR    RE                                                               
                                                                                
TSTSCLIM CLI   WORK,SCITCLIM   Test whether type is credit limit                
         BR    RE                                                               
                                                                                
TSTSICLM CLI   WORK,SCITICLM   Test whether type is int credit lim              
         BR    RE                                                               
                                                                                
         USING RSTELD,R1                                                        
SETFLT   L     R1,LP_AINP                                                       
         MVC   AC_FLT1,RSTFILT1                                                 
         MVC   AC_FLT2,RSTFILT2                                                 
         MVC   AC_FLT3,RSTFILT3                                                 
         MVC   AC_FLT4,RSTFILT4                                                 
         MVC   AC_FLT5,RSTFILT5                                                 
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
**********************************************************************          
* TIMESHEET DISPLAY DOWNLOAD                                         *          
**********************************************************************          
                                                                                
REQTIMD  LKREQ H,A#TIMDIS,OUTTMD,NEXTREQ=REQCALD                                
PersCde  LKREQ F,1,(D,B#SAVED,QPERSN),CHAR,OLEN=L'QPERSN,              +        
               MAXLEN=L'QPERSN,TEXT=AC#PRSN,COL=*                               
EndDte   LKREQ F,2,(D,B#SAVED,QENDD),PDAT,OLEN=L'QENDD,                +        
               TEXT=AC#RSPED,COL=*                                              
LocEnd   LKREQ F,3,(D,B#SAVED,QLOCED),PDAT,OLEN=L'QLOCED,              +        
               TEXT=AC#RSLED,COL=*                                              
Pdf      LKREQ F,4,(D,B#SAVED,QPDF),CHAR,OLEN=L'QPDF,                  +        
               MAXLEN=L'QPDF,TEXT=AC#PDF,COL=*                                  
All      LKREQ F,5,(D,B#SAVED,QALL),CHAR,OLEN=L'QALL,                  +        
               MAXLEN=L'QALL,TEXT=AC#VWALL,COL=*                                
Manager  LKREQ F,6,(D,B#SAVED,QMAN),CHAR,OLEN=L'QMAN,                  +        
               MAXLEN=L'QMAN,TEXT=(*,MANGLIT),COL=*                             
DOCTS    LKREQ F,7,(D,B#SAVED,QCOCTS),CHAR,OLEN=L'QCOCTS,              +        
               MAXLEN=L'QCOCTS,TEXT=(*,CTSOLIT),COL=*                           
TimDte   LKREQ F,8,(D,B#SAVED,QTIMDT),PDAT,OLEN=L'QTIMDT,              +        
               TEXT=AC#RSDTE,COL=*                                              
         LKREQ E                                                                
                                                                                
OUTTMD   LKOUT H                                                                
                                                                                
TIMHDR2  LKOUT R,R#TIMHDR          Time header                                  
Array    LKOUT C,R#TIMHDR,(A,ARYHDR)                                            
         LKOUT E                                                                
*                              **  First call - REAL time **                    
TIMROW10 LKOUT R,R#TIMTIM          Timeline information - real time             
Array    LKOUT C,R#TIMTIM,(A,ARYTLN)                                            
         LKOUT E                                                                
*                              **  Second call - PREV timelines **              
TIMROW1A LKOUT R,R#TIMTIM          Timeline information - from prev             
PROUT    LKOUT P,,SETPRWJ                                                       
Array    LKOUT C,R#TIMTIM,(A,ARYTLN),FILTROUT=TSTWIDG                           
         LKOUT E                                                                
                                                                                
TIMROW11 LKOUT R,R#TIMTIM          Timeline information - holidays              
Array    LKOUT C,R#TIMTIM,(A,ARYHOL),FILTROUT=TSTPRE2                           
         LKOUT E                                                                
                                                                                
TIMROW12 LKOUT R,R#TIMTIM          Timeline information - template              
Array    LKOUT C,R#TIMTIM,(A,ARYTMP),FILTROUT=TSTTEMP                           
         LKOUT E                                                                
                                                                                
TIMROW13 LKOUT R,R#TIMTIM          Timeline information - resources             
Array    LKOUT C,R#TIMTIM,(A,ARYRES),FILTROUT=TSTTRES                           
         LKOUT E                                                                
                                                                                
TIMROW14 LKOUT R,R#TIMTIM          Timeline information - blank lines           
Array    LKOUT C,R#TIMTIM,(A,ARYBLNK),FILTROUT=TSTPRRD                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYHDR   LKOUT A,(R,BLDHDR),ROWNAME=SAVED                                       
                                                                                
StatVw   LKOUT C,1,TH_STAVW,CHAR                                                
StatTs   LKOUT C,2,TH_STATS,CHAR                                                
LMPid    LKOUT C,3,(D,B#SAVED,TH_MANAP),(U,#EDTPID,$EDTPID),           +        
               LEN=L'SAPALPID                                                   
PRout    LKOUT P,,SETNAME                                                       
LMFNm    LKOUT C,4,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
LMMNm    LKOUT C,5,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LMLNm    LKOUT C,6,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                        
Indx     LKOUT C,7,TH_INDX,HEXD                                                 
Bill     LKOUT C,8,TH_BILL,CHAR                                                 
Chrg     LKOUT C,9,TH_CHRG,CHAR                                                 
Nbil     LKOUT C,10,TH_NBIL,CHAR                                                
DfltTy   LKOUT C,11,TH_DEFT,CHAR,ND=Y                                           
DfltWC   LKOUT C,12,TH_DEFWC,CHAR,ND=Y                                          
ProR     LKOUT C,13,TH_PROR,CHAR                                                
JobR     LKOUT C,14,TH_JOBR,CHAR                                                
MCSUsr   LKOUT C,15,TH_MCSU,CHAR                                                
OneROff  LKOUT C,16,TH_1ROFF,CHAR                                               
OneRDpt  LKOUT C,17,TH_DEPT,CHAR                                                
OneRSdp  LKOUT C,18,TH_SDPT,CHAR                                                
HrsRqd   LKOUT C,19,TH_RQHRS,SPAK,ND=Y                                          
PerdNm   LKOUT C,20,TH_PER#,LBIN                                                
LastRw   LKOUT C,21,TH_LSTRW,LBIN                                               
LMAprv   LKOUT C,22,TH_LMAP,CHAR                                                
Email    LKOUT C,23,TH_UEML,CHAR,ND=Y                                           
NmDays   LKOUT C,24,TH_NDAYS,LBIN                                               
OneROfN  LKOUT C,25,TH_1RON,CHAR,ND=Y                                           
OneRDpN  LKOUT C,26,TH_DEPN,CHAR,ND=Y                                           
OneRSdN  LKOUT C,27,TH_SDPN,CHAR,ND=Y                                           
TarUtil  LKOUT C,28,TH_TUPCT,SPAK,ND=Y                                          
DfltWCD  LKOUT C,29,TH_DEFWC,(R,EDTWCD)                                         
Prevts   LKOUT C,30,TH_PRNS,CHAR,ND=Y                                           
DTSprev  LKOUT C,31,TH_COPTA,CHAR,ND=Y                                          
FName    LKOUT C,33,TH_FNAME,CHAR,ND=Y                                          
MName    LKOUT C,34,TH_MNAME,CHAR,ND=Y                                          
LName    LKOUT C,35,TH_LNAME,CHAR,ND=Y                                          
Agname   LKOUT C,36,OGINAME,CHAR,ND=Y,FILTROUT=TSTPDF                           
AppDte   LKOUT C,37,TH_APDTE,PDAT,ND=Y,FILTROUT=TSTPDF                          
PerCde   LKOUT C,38,QPERSN,CHAR,ND=Y,FILTROUT=TSTPDF                            
ApPid    LKOUT C,39,(D,B#SAVED,TH_APPID),(U,#EDTPID,$EDTPID),          +        
               LEN=L'SAPALPID,ND=Y                                              
PRout    LKOUT P,,SETNAME                                                       
LMFNm    LKOUT C,40,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
LMMNm    LKOUT C,41,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LMLNm    LKOUT C,42,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                       
CTS      LKOUT C,43,(D,B#SAVED,QCOCTS),CHAR,ND=Y                                
DEF      LKOUT C,44,(D,B#COBLCK,CODEF),CHAR,ND=Y                                
OVT      LKOUT C,45,(D,B#COBLCK,COOVT),CHAR,ND=Y                                
Array    LKOUT C,32,(A,ARYDATES)                                                
Array    LKOUT C,33,(A,ARYREJC),FILTROUT=TSTTINI                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYDATES LKOUT A,(D,B#SAVED,TH_DATE),NROWS=(B#SAVED,TH_NDAYS),NEWEL=Y, +        
               ROWNAME=TH_DATE,ROWWIDTH=L'TH_DATE                               
                                                                                
Date     LKOUT C,32,TH_DATE,PDAT                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYREJC  LKOUT A,(R,NXTREJ),MULTIROW=Y,ROWNAME=REJ_D                            
Comments LKOUT C,1,REJ_DATA,CHAR,ND=Y,LEN=V                                     
PID      LKOUT C,2,REJ_PID,(U,#EDTPID,$EDTPID),ND=Y                             
PRout    LKOUT P,,SETNAME                                                       
FName    LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16                                
MName    LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                             
LName    LKOUT C,5,(D,B#WORKD,WORK2),CHAR,LEN=58                                
DComm    LKOUT C,6,REJ_DATE,PDAT,ND=Y                                           
TComm    LKOUT C,7,REJ_TIME,(R,EDTIME),LEN=6,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* BUILD HEADER RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
BLDHDR   ST    R8,LP_ADATA                                                      
         GOTOR GETAGN,OGINAME      Get agency name                              
*                                                                               
         OC    QTIMDT,QTIMDT       Any timesheet date passed                    
         JZ    BHDR001             No - OK                                      
         GOTOR SETPEND,DMCB,QTIMDT Set period end date                          
         JNE   QERROR                                                           
*                                                                               
BHDR001  LA    RF,QENDD                                                         
         OC    QLOCED,QLOCED                                                    
         JZ    *+8                                                              
         LA    RF,QLOCED                                                        
         GOTOR PRSDTL,DMCB,(X'00',(RF))                                         
         JNE   QERROR                                                           
         L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ALOCEL           retrieve office                              
         MVC   TH_1ROFF,LOCOFF-LOCELD(R1)                                       
         MVC   TH_DEPT,LOCDEPT-LOCELD(R1)                                       
         MVC   TH_SDPT,LOCSUB-LOCELD(R1)                                        
         MVC   QLOCED,LOCEND-LOCELD(R1)                                         
         MVC   QLOCST,LOCSTART-LOCELD(R1)                                       
*                                                                               
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   *+8                                                              
         LA    RE,0                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   D1RACT(0),TH_1ROFF                                               
         AHI   RE,1                                                             
         LA    RF,D1RACT                                                        
         AR    RF,RE                                                            
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),TH_DEPT                                                  
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),TH_SDPT                                                  
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   0(0,RF),QPERSN                                                   
                                                                                
         MVI   TH_ACCS,NOQ         Default user does not have access            
         CLC   CCTPID,DPPID        Check if user's timesheet                    
         JNE   *+12                                                             
         OI    DINDS2,DIIUSR       Set user connected (ok to view t/s)          
         J     BHDR006                                                          
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Resource         
         CHI   RF,XPRODIKQ          or mobile or rodick                         
         JNE   BHDR006             Skip security for anything but Aura          
*                              **  Else check to see if person has  **          
*                                  security to view this timesheet              
         CLI   QALL,YESQ                                                        
         JE    BHDR006             Security to view any t/s?                    
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
         CLI   QMAN,YESQ           Security to manager search                   
         JNE   BHDR002             No                                           
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QTIME',0)            
         JNE   BHDR002                                                          
         GOTOR CHKVIEW             Check if person has ability to view          
         JE    BHDR006                                                          
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
BHDR002  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTACC',GAPLPARM),('QTIME',0)            
         JNE   BHDR004                                                          
         GOTOR CHKVIEW             Check if person has ability to view          
         JE    BHDR006                                                          
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
BHDR004  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLBKAP',SPACES),('GAPLTACC',GAPLPARM),('QTIME',0)            
         JNE   BHDR008                                                          
         GOTOR CHKVIEW             Check if person has ability to view          
         JNE   BHDR008                                                          
*                                                                               
BHDR006  MVI   TH_ACCS,YESQ        set user right to see timesheet              
BHDR008  CLI   LP_ACCS,0           skip if no limit access                      
         JE    BHDR010                                                          
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,TH_1ROFF   move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    BHDR010                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     QERROR                                                           
         DROP  R1                                                               
                                                                                
BHDR010  OC    QLOCED,QLOCED                                                    
         JZ    BHDR020                                                          
         CLC   QLOCED,QENDD        is loc end date after period end dte         
         JH    BHDR020             no                                           
         GOTO1 VDATCON,DMCB,(1,QLOCED),(0,WORK)                                 
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
         L     R2,AIO1                                                          
         USING PERRECD,R2                                                       
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
BHDR012  CLI   LOCEL,0                                                          
         JE    BHDR022                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    BHDR016                                                          
BHDR014  SR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         J     BHDR012                                                          
*                                                                               
BHDR016  CLC   WORK(L'LOCSTART),LOCSTART                                        
         JL    BHDR014                                                          
         OC    LOCEND,LOCEND                                                    
         JZ    BHDR018                                                          
         CLC   WORK(L'LOCEND),LOCEND                                            
         JH    BHDR014                                                          
BHDR018  L     R1,ALOCEL                                                        
         CLC   LOCOFF(L'LOCOFF+L'LOCDEPT+L'LOCSUB),LOCOFF-LOCELD(R1)            
         JNE   BHDR022                                                          
         MVC   QLOCED,LOCEND                                                    
         J     BHDR010                                                          
                                                                                
BHDR020  MVC   QLOCED,QENDD       yes - set loc end date as period end          
                                                                                
BHDR022  GOTO1 VDATCON,DMCB,(1,QLOCST),(0,WORK)                                 
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
         L     R2,AIO1                                                          
         USING PERRECD,R2                                                       
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
BHDR024  CLI   LOCEL,0                                                          
         JE    BHDR032                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    BHDR028                                                          
BHDR026  SR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         J     BHDR024                                                          
*                                                                               
BHDR028  CLC   WORK(L'LOCSTART),LOCSTART                                        
         JL    BHDR026                                                          
         OC    LOCEND,LOCEND                                                    
         JZ    BHDR030                                                          
         CLC   WORK(L'LOCEND),LOCEND                                            
         JH    BHDR026                                                          
BHDR030  L     R1,ALOCEL                                                        
         CLC   LOCOFF(L'LOCOFF+L'LOCDEPT+L'LOCSUB),LOCOFF-LOCELD(R1)            
         JNE   BHDR032                                                          
         MVC   QLOCST,LOCSTART                                                  
         J     BHDR022                                                          
*                                                                               
BHDR032  GOTOR CHKMCS                                                           
*&&UK                                                                           
         CLI   TH_MCSU,YESQ        Are we a brandocean/aura user                
         JE    BHDR033             Yes                                          
         SR    RF,RF               No - check if Aura connected                 
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JNE   BHDR033             No - could be mobile or brandocean           
         CLC   =C'Y9',CUAALF       Is it GroupM old file                        
         JNE   BHDR033                                                          
         MVC   LP_ERROR,=AL2(AE$SECLK)  Yes give security lockout error         
         J     XERROR                                                           
*&&                                                                             
                                                                                
BHDR033  GOTOR GETMAP,D1RACT                                                    
         GOTOR (#CSTPRF,ACSTPRF),DMCB,D1RACT  get profile                       
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         OC    QCOCTS,QCOCTS                  Overriden COCTS?                  
         JNZ   *+10                                                             
         MVC   QCOCTS,COCTS                   Use COCTS from cost prof          
         MVC   TH_COPTA,COPTA      Prevent ts if prev not submitted             
         ZAP   DUB1,CONDO          Days for overdue                             
         ZAP   DUB2,CONBL          Number of blank lines                        
         CLI   CODTM,CONODEFL                                                   
         JE    BHDR034                                                          
         CLI   CODTM,COSETR                                                     
         JNE   *+8                                                              
         MVI   TH_DEFT,C'R'                                                     
         CLI   CODTM,COSETB                                                     
         JNE   *+8                                                              
         MVI   TH_DEFT,C'B'                                                     
         CLI   CODTM,COSETN                                                     
         JNE   *+8                                                              
         MVI   TH_DEFT,C'N'                                                     
                                                                                
BHDR034  LA    R3,COATM                                                         
         LA    R1,3                                                             
         MVC   TH_BILL(L'TH_BILL+L'TH_CHRG+L'TH_NBIL),=C'NNN'                   
BHDR036  CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   TH_BILL,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   TH_CHRG,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   TH_NBIL,C'Y'                                                     
         LA    R3,2(R3)                                                         
         JCT   R1,BHDR036                                                       
         DROP  R3                                                               
         CVB   R3,DUB2                                                          
         ST    R3,BLNKLNS                                                       
         CVB   R2,DUB1                                                          
         LNR   R2,R2                                                            
*                                                                               
         MVC   TEMP2(2),=C'1R'                                                  
         MVC   TEMP2+2(12),SPACES                                               
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP2+2(0),D1RACT                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   TH_1RON,TEMP2                                                    
*                                                                               
         MVC   TEMP2(2),=C'1R'                                                  
         MVC   TEMP2+2(12),SPACES                                               
         LLC   RE,ONERL2L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP2+2(0),D1RACT                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   TH_DEPN,TEMP2                                                    
*                                                                               
         MVC   TEMP2(2),=C'1R'                                                  
         MVC   TEMP2+2(12),SPACES                                               
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP2+2(0),D1RACT                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   TH_SDPN,TEMP2                                                    
*                                                                               
         MVC   WORK(L'D_TODYF),D_TODYF                                          
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R2)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DOVRDUED)                             
*                                                                               
BHDR038  MVC   DCEND,QENDD                                                      
         GOTO1 VDATCON,DMCB,(1,QENDD),(0,WORK+6)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK+6),WORK,F'-21'                            
         GOTO1 VDATCON,DMCB,(0,WORK),(1,DDATE)                                  
                                                                                
         GOTOR CALPRDS            and read periods into AIO5                    
         JNE   QERROR                                                           
                                                                                
BHDR040  SR    R4,R4                                                            
         IC    R4,NUMPRDS          HAVE WE ANY PERIODS                          
         LTR   R4,R4                                                            
         MVC   LP_ERROR,=AL2(AE$PERLT) PERSON LOCKED FROM TIMESHEETS            
         JZ    QERROR              NO                                           
         XC    LP_ERROR,LP_ERROR                                                
         GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
*                                                                               
         MVC   TEMP2(3),QENDD      and retrieve period number                   
         GOTOR RETCPFD                                                          
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$PERLT) PERSON LOCKED FROM TIMESHEETS            
         J     QERROR                                                           
                                                                                
         LA    RF,TEMP2                                                         
         USING PERTABD,RF                                                       
         XC    TH_DATE(TH_DATEQ),TH_DATE  Init Area                             
         MVC   TH_DATE,PERSTDT                                                  
BHDR042  CLC   QLOCST,PERSTDT                                                   
         JNH   *+10                                                             
         MVC   TH_DATE,QLOCST                                                   
         LA    R3,TH_DATE+L'TH_DATE                                             
         MVC   TH_PER#,PERNUM                                                   
         DROP  RF                                                               
         MVC   WORK(L'TH_DATE),TH_DATE                                          
         XR    R2,R2                                                            
         LA    R2,1(R2)                                                         
BHDR044  GOTO1 VDATCON,DMCB,(1,WORK),(0,WORK+12)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK+12),WORK+6,F'1'                           
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
         CLC   QLOCED,WORK       HAVE WE REACHED END DATE                       
         JL    BHDR046             YES                                          
         MVC   0(L'TH_DATE,R3),WORK                                             
         LA    R3,L'TH_DATE(R3)                                                 
         LA    R2,1(R2)                                                         
         J     BHDR044                                                          
*                                                                               
BHDR046  STCM  R2,3,TH_NDAYS                                                    
         MVI   TH_PRNS,C'Y'                                                     
         L     R4,APREVPER        Check previous period to check it's           
         USING PERTABD,R4         submitted                                     
         TM    PERSTAT,PERSMCSU   Are we a brandocean user for this             
         JZ    BHDR048                                      period              
         CLC   PERLOCE,TI_HIRE     Is period end date before hire date          
         JL    BHDR048             Yes                                          
         XC    IOKEY,IOKEY                                                      
         USING TSWRECD,IOKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         MVC   TSWKEND,PERENDT                                                  
         MVC   TSWKODS,PERODS                                                   
         MVC   CSVKEY1,TSWRECD                                                  
         MVI   TH_PRNS,C'N'                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   CSVKEY1(TSWKULC-TSWRECD),TSWKEY                                  
         JNE   BHDR048            prev timesheet not even started               
         OC    TSWKSTAT,TSWKSTAT  no status means not submitted                 
         JZ    BHDR048                                                          
         TM    TSWKSTAT,TIMSREJE  Rejected needs to be resubmitted              
         JNZ   BHDR048                                                          
         MVI   TH_PRNS,C'Y'                                                     
BHDR048  GOTOR CKHRS,TH_DATE                                                    
*&&UK                                                                           
         GOTOR CHKHIST                                                          
         JNE   QERROR                                                           
*&&                                                                             
         MVC   TEMP2(L'DPPID),DPPID                                             
         GOTOR (#GETPID,AGETPID)                                                
         JNE   BHDR050                                                          
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   BHDR050                                                          
         MVC   TH_FNAME,TEMP2                                                   
         MVC   TH_MNAME,TEMP2+32                                                
         MVC   TH_LNAME,WORK2                                                   
         MVI   BYTE4,SE#TIME                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,TH_1ROFF,BYTE4                            
         JNE   BHDR050             Check whether to send emails                 
         MVC   TH_UEML,APPEMAIL                                                 
*                                                                               
BHDR050  LA    RF,QENDD                                                         
         OC    QLOCED,QLOCED                                                    
         JZ    *+8                                                              
         LA    RF,QLOCED                                                        
         GOTOR GETAUD,DMCB,(0,(RF))                                             
         CLI   TH_ACCS,YESQ        Do we have access to view timesheet          
         JE    *+14                                                             
         MVC   LP_ERROR,=AL2(AE$NOSTS) No security to view t/s                  
         J     QERROR                                                           
                                                                                
         OI    DINDS2,DIINAUD      Set no audit found                           
         OC    TH_INDX,TH_INDX     If no index no audit                         
         JZ    BHDR052                                                          
         NI    DINDS2,X'FF'-DIINAUD Index existed so does audit                 
                                                                                
         USING TSWRECD,R2                                                       
BHDR052  LA    R2,IOKEY            Build start key for read                     
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         XR    RE,RE                                                            
         ICM   RE,7,QENDD                                                       
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND                                                     
         SR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TSWKODS(0),D1RACT                                                
         OC    TSWKODS,SPACES                                                   
         MVC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD                                 
         L     R1,=AL4(IOHID+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BHDR058                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   BHDR058                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
*                                                                               
BHDR054  MVC   IOKEY,CSVKEY4                                                    
         L     R1,=AL4(IOHID+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BHDR056                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   BHDR056                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
BHDR056  L     R1,=AL4(IOSQD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BHDR058                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   BHDR058                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     QERROR                                                           
                                                                                
BHDR058  CLC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD                                 
         JNE   BHDR060                                                          
         MVC   CSVKEY4,TSWRECD                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         USING TIMRECD,R4                                                       
         TM    TIMRSTAT,TIMSDELT   check master and not passive                 
         JNZ   BHDR054                                                          
         TM    DINDS2,DIINAUD      Did we find audit record                     
         JZ    BHDR062             Yes                                          
         MVI   TH_STAVW,TS#APPRO   No - must be old time                        
         MVI   TH_STATS,TS#APPRO   Set as fully approved                        
         J     BLDHDRX                                                          
                                                                                
BHDR060  OI    DINDS2,DIIPREV      Set search for previous periods              
         MVI   TH_STATS,TS#NSTRT                                                
         MVI   TH_STAVW,TS#NSTRT                                                
         CLC   QENDD,DOVRDUED      is it overdue                                
         JNL   BLDHDRX             no                                           
         MVI   TH_STAVW,TS#OVERD   yes                                          
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    BLDHDRX                                                          
         MVI   TH_STATS,TS#OVERD   If BrandOcean set as overdue                 
         J     BLDHDRX                                                          
                                                                                
BHDR062  GOTOR SETSSTA            Set TH_STATS                                  
         CLI   TH_STATS,TS#PROGR  If in progress TS could be overdue            
         JNE   BHDR064                                                          
         CLC   QENDD,DOVRDUED     Is it overdue                                 
         JNL   BHDR064            No                                            
         MVI   TH_STAVW,TS#OVERD  Yes                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    BLDHDRX                                                          
         MVI   TH_STATS,TS#OVERD  If BrandOcean set as overdue                  
         J     BLDHDRX                                                          
                                                                                
BHDR064  TM    DSTAT,TIMSFAPP     Is it fully approved                          
         JNZ   BHDR066            Appears the same to everyone                  
         CLI   DSTAT,0            Is it just saved                              
         JE    BHDR066            Appears the same to everyone                  
         CLC   CCTPID,DPPID       Is the user the connected person              
         JNE   BLDHDRX            No                                            
BHDR066  MVC   TH_STAVW,TH_STATS  Yes - viewing and TS status the same          
*                                                                               
BLDHDRX  DS    0H                                                               
*&&UK                                                                           
         TM    CPYSTATD,CPYSRSTS  Restrict T/S by status                        
         JZ    EXITY                                                            
         CLI   QMAN,YESQ          User has manager search on                    
         JE    EXITY                                                            
         CLC   CCTPID,DPPID       Is the user the connected person              
         JNE   EXITY              No                                            
         CLI   TH_STATS,TS#APPRO  Apporved timesheet?                           
         JE    *+12                                                             
         CLI   TH_STATS,TS#PAAPR  Part approved timesheet?                      
         JNE   EXITY                                                            
         MVC   LP_ERROR,=AL2(AE$TSDEN)                                          
         J     QERROR             Access to these T/S denied                    
*&&                                                                             
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Routine to return the name                                          *         
***********************************************************************         
         SPACE 1                                                                
SETNAME  GOTOR (#GETPIN,AGETPIN)                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Process a rejection comment from the buffer                         *         
***********************************************************************         
         SPACE 1                                                                
         USING REJ_D,RJ_BUFF                                                    
NXTREJ   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTR02                                                           
         XC    SAVREJ,SAVREJ                                                    
         LA    R0,RJ_BUFF                                                       
         LHI   R1,L'RJ_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         MVI   LP_RMODE,LP_RNEXT   SET TO READ NEXT NEXT TIME                   
         J     NXTR04                                                           
*                                                                               
NXTR02   LA    R0,RJ_BUFF                                                       
         LHI   R1,L'RJ_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
*                                                                               
NXTR04   TM    TSARERRS,TSEEOF     Finished buffer                              
         JNZ   NXTR06              Yes                                          
         CLC   SAVREJ,REJ_PID      Is this entry same as last                   
         JE    NXTR02              Yes - don't output it                        
         MVC   SAVREJ,REJ_PID      No - save new details                        
         LA    R0,REJ_D                                                         
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
NXTR06   MVI   LP_RMODE,LP_RLAST   FINISHED                                     
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYTLN   LKOUT A,(R,NXTTSW),MULTIROW=Y,ROWNAME,TIMERECD                         
                                                                                
PRout    LKOUT P,,CLRINPUT                                                      
Array    LKOUT C,100,(A,ARYDNAR3),FILTROUT=TSTDNARF Day narrative               
Array    LKOUT C,R#TIMTIM,(A,ARYTIMI)                                           
Array    LKOUT C,99,(A,ARYITM2),FILTROUT=TSTINPUT                               
Array    LKOUT C,100,(A,ARYDNAR2),FILTROUT=TSTINPUT Day narrative               
                                                                                
         LKOUT E                                                                
                                                                                
ARYTIMI  LKOUT A,(D,B#TIMREC,TIMRFST),NEWEL=B,EOT=EOR,                 +        
               ROWID=(TIMEL,TIMELQ),ROWWIDTH=(V,TIMLN),                +        
               SETFILT=(TIMSEQ,L'TIMSEQ)                                        
                                                                                
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
PRout    LKOUT P,TIMIND,SETTIND                                                 
TimTyp   LKOUT C,1,TIMTTYP,(R,EDTTYP),FILTROUT=TSTTINP,                +        
               SKIPCOLS=MGBSKIPS                                                
MGBSKIP  EQU   *                                                                
PRout    LKOUT P,TIMACC,BLNKCNT                                                 
PRout    LKOUT P,TIMSEQ,SETINPUT                                                
Cod1N    LKOUT C,2,TIMACC+2,CHAR,LEN=L'ACTKACT,FILTROUT=TSTE1NAC,      +        
               SKIPCOLS=1                                                       
Nam1N    LKOUT C,3,TIMACC,(R,EDTANM)                                            
CodCli   LKOUT C,4,TIMACC,(U,#EDTCLI,$EDTCLI),FILTROUT=TSTESJAC,       +        
               SKIPCOLS=NSJSKIPS                                                
NSJSKIP  EQU   *                                                                
NamCli   LKOUT C,5,TIMACC,(R,EDTCLN)                                            
CodPrd   LKOUT C,6,TIMACC,(U,#EDTPRD,$EDTPRD)                                   
NamPrd   LKOUT C,7,TIMACC,(R,EDTPRN)                                            
CodJob   LKOUT C,8,TIMACC,(U,#EDTJOB,$EDTJOB)                                   
NamJob   LKOUT C,9,TIMACC,(R,EDTJBN)                                            
Joblock  LKOUT C,10,(D,B#SAVED,TL_JOBL),CHAR,ND=Y                               
WC       LKOUT C,11,TIMTSK,CHAR                                                 
WCNam    LKOUT C,12,TIMTSK,(R,EDTWCD)                                           
PRout    LKOUT P,TIMACC,GETOPTM                                                 
FrcePr   LKOUT C,13,(D,B#SAVED,TL_FPT),CHAR,ND=Y                                
FrceJb   LKOUT C,14,(D,B#SAVED,TL_FJT),CHAR,ND=Y                                
NarBill  LKOUT C,15,(D,B#SAVED,TL_FNTB),CHAR,ND=Y                               
NarMemo  LKOUT C,16,(D,B#SAVED,TL_FNTR),CHAR,ND=Y                               
NarNBil  LKOUT C,17,(D,B#SAVED,TL_FNTN),CHAR,ND=Y                               
JobInpt  LKOUT C,18,(D,B#SAVED,TL_JOBT),CHAR,ND=Y                               
BilAlwd  LKOUT C,19,(D,B#SAVED,TL_BILA),CHAR,ND=Y                               
MemAlwd  LKOUT C,20,(D,B#SAVED,TL_CHGA),CHAR,ND=Y                               
NonAlwd  LKOUT C,21,(D,B#SAVED,TL_NONA),CHAR,ND=Y                               
DefType  LKOUT C,22,(D,B#SAVED,TL_TOT),CHAR,ND=Y                                
NSJSKIPS EQU   (*-NSJSKIP)/LX_COLSL                                             
                                                                                
AdjTyp   LKOUT C,23,TIMIND,(R,EDTADJ)                                           
Grey     LKOUT C,24,TIMELD,(R,EDTGRY)                                           
AppCod   LKOUT C,25,(D,B#SAVED,TL_PIDC),CHAR,ND=Y                               
FirstNam LKOUT C,26,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,27,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,28,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                       
Array    LKOUT C,29,(A,ARYNAR),FILTROUT=TSTCNAR  Narrative                      
Array    LKOUT C,50,(A,ARYOHRS),FILTROUT=TSTNAUD Pre BrndOcn time               
Array    LKOUT C,30,(A,ARYORD),FILTROUT=TSTPREVR Order                          
Array    LKOUT C,32,(A,ARYINT)                   Internal Reference             
Array    LKOUT C,36,(A,ARYEST),FILTROUT=TSTPREVR Estimate                       
LocBl    LKOUT C,39,(D,B#SAVED,TL_LOCBI),CHAR,ND=Y                              
Futal    LKOUT C,40,(D,B#SAVED,TL_FUTAL),CHAR,ND=Y                              
Office   LKOUT C,41,(D,B#SAVED,TL_OFF),CHAR,ND=Y                                
OffNam   LKOUT C,42,(D,B#SAVED,TL_OFF),(R,EDTOFN),ND=Y                          
JobClos  LKOUT C,43,(D,B#SAVED,TL_JOBC),CHAR,ND=Y                               
1Nlock   LKOUT C,44,(D,B#SAVED,TL_TLOC),CHAR,ND=Y                               
Tioid    LKOUT C,45,(A,ARYTIO),FILTROUT=TSTPREVR Timeoff id                     
RealT    LKOUT C,46,TIMELD,(R,EDTREAL),ND=Y      Real timeline                  
Array    LKOUT C,32,(A,ARYTIM),FILTROUT=TSTPREVR Time date/hours                
Array    LKOUT C,99,(A,ARYITM),FILTROUT=TSTPREVR Materials                      
Array    LKOUT C,100,(A,ARYDNAR),FILTROUT=TSTPREVR Day narrative                
MGBSKIPS EQU   (*-MGBSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYNAR   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Narr     LKOUT C,29,TIMNARR,CHAR,LEN=V,ND=Y,FILTROUT=TSTTNAR                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYOHRS  LKOUT A,(R,BLDOHRS),NROWS=1                                            
                                                                                
Row      LKOUT C,33,(D,B#SAVED,TL_TROW),LBIN                                    
array    LKOUT C,50,(A,ARYHRS2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYHRS2  LKOUT A,(D,B#SAVED,TL_HRS),NROWS=(B#SAVED,TH_NDAYS),          +        
               ROWWIDTH=L'TL_HRS,ROWNAME=TL_HRS                                 
                                                                                
Hours    LKOUT C,50,TL_HRS,SPAK                                                 
                                                                                
         LKOUT E                                                                
                                                                                
BLDOHRS  LA    R2,TL_HRS                                                        
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         L     R3,LP_AINP                                                       
         USING TIMELD,R3                                                        
         ZAP   0(L'TL_HRS,R2),TIMHRS                                            
         TM    DINDS2,DIIPREV                                                   
         JZ    *+10                                                             
         ZAP   0(L'TL_HRS,R2),PZERO                                             
         LA    R2,L'TL_HRS(R2)                                                  
         LH    RF,TH_NDAYS                                                      
         SHI   RF,1                                                             
         CHI   RF,0                                                             
         JE    BOHRS04                                                          
BOHRS02  ZAP   0(L'TL_HRS,R2),PZERO                                             
         LA    R2,L'TMTHRS(R2)                                                  
         JCT   RF,BOHRS02                                                       
                                                                                
BOHRS04  ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
ARYORD   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Order    LKOUT C,30,TIMOORDR,FILTROUT=TSTTORD,CHAR,SKIPCOLS=2                   
PRout    LKOUT P,TIMOORDR,SETQORD                                               
Array    LKOUT C,31,(A,ARYORN)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYINT   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
IntRef   LKOUT C,32,TIMJINRF,FILTROUT=TSTTINT,CHAR                              
                                                                                
         LKOUT E                                                                
                                                                                
ARYEST   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
EstNum   LKOUT C,36,TIMSESNM,FILTROUT=TSTTEST,CHAR,SKIPCOLS=2                   
PRout    LKOUT P,TIMSESNM,SETQEST                                               
Array    LKOUT C,37,(A,ARYESN)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYTIO   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Timeoi   LKOUT C,45,TIMFIDN,CHAR,LEN=V,FILTROUT=TSTTIOF                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYTIM   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ)                    
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
PRout    LKOUT P,TIMELD,SETHROW#                                                
Row      LKOUT C,33,TIMEIDNO,LBIN,FILTROUT=TSTTTIM,SKIPCOLS=TIMSKIPS            
TIMSKIP  EQU   *                                                                
LstItm   LKOUT C,34,TIMELD,(R,EDTLITM)                                          
PRout    LKOUT P,TIMEPST1,SETWIDG                                               
CliApp   LKOUT C,35,TIMEPST1,(R,EDTAPST)                                        
Array    LKOUT C,50,(A,ARYHRS)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYHRS   LKOUT A,(*,TIMEDAY),ROWNAME=TIMELD,NROWS=*,ROWWIDTH=L'TIMEDAY          
Hours    LKOUT C,50,TIMEHRS,SPAK                                                
                                                                                
TIMSKIPS EQU   (*-TIMSKIP)/LX_COLSL                                             
         LKOUT E                                                                
                                                                                
ARYITM   LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ),NEWEL=B            
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Code     LKOUT C,1,TIMINUM,CHAR,FILTROUT=TSTTITM,SKIPCOLS=ITMSKIPS              
ITMSKIP  EQU   *                                                                
Seqnce   LKOUT C,2,TIMISEQ,HEXD                                                 
ItmRow   LKOUT C,3,TIMIIIDN,LBIN                                                
ItmDsc   LKOUT C,4,TIMISEQ,(R,EDTITMD)                                          
ItmPrc   LKOUT C,5,TIMIPRCE,SPAK                                                
ItmMult  LKOUT C,6,TIMIMULT,SPAK                                                
ItmTot   LKOUT C,7,TIMITOT,SPAK                                                 
ItmOvrd  LKOUT C,8,TIMIIND,(R,EDTOVR)                                           
ItmFlag  LKOUT C,9,(D,B#SAVED,TL_IFLAG),CHAR                                    
ItmText  LKOUT C,10,TIMITEXT,CHAR,LEN=V                                         
ItmUnit  LKOUT C,11,(D,B#SAVED,TL_IUNIT),CHAR                                   
ITMSKIPS EQU   (*-ITMSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYITM2  LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),NEWEL=B                                       
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Code     LKOUT C,1,TIMINUM,CHAR,FILTROUT=TSTTITM,SKIPCOLS=IT2SKIPS              
IT2SKIP  EQU   *                                                                
Seqnce   LKOUT C,2,TIMISEQ,HEXD                                                 
ItmRow   LKOUT C,3,TIMIIIDN,LBIN                                                
ItmDsc   LKOUT C,4,TIMISEQ,(R,EDTITMD)                                          
ItmPrc   LKOUT C,5,TIMIPRCE,SPAK                                                
ItmMult  LKOUT C,6,TIMIMULT,SPAK                                                
ItmTot   LKOUT C,7,TIMITOT,SPAK                                                 
ItmOvrd  LKOUT C,8,TIMIIND,(R,EDTOVR)                                           
ItmFlag  LKOUT C,9,(D,B#SAVED,TL_IFLAG),CHAR                                    
ItmText  LKOUT C,10,TIMITEXT,CHAR,LEN=V                                         
ItmUnit  LKOUT C,11,(D,B#SAVED,TL_IUNIT),CHAR                                   
IT2SKIPS EQU   (*-IT2SKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
*                                                                               
ARYDNAR  LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),TESTFILT=(TIMSEQ,L'TIMSEQ),NEWEL=B            
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Dndte    LKOUT C,1,TIMDTDT1,PDAT,FILTROUT=TSTDNAR,SKIPCOLS=NARSKIPS,   +        
               CHAR                                                             
NARSKIP  EQU   *                                                                
Row#     LKOUT C,2,TIMDIDNO,LBIN                                                
Seq      LKOUT C,3,TIMDNIDN,LBIN                                                
Dnarr    LKOUT C,4,TIMDNARR,CHAR,LEN=V                                          
NARSKIPS EQU   (*-NARSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
*                                                                               
ARYDNAR2 LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),NEWEL=B                                       
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
Dndte    LKOUT C,1,TIMDTDT1,PDAT,FILTROUT=TSTDNAR,SKIPCOLS=N2RSKIPS,   +        
               CHAR                                                             
N2RSKIP  EQU   *                                                                
Row#     LKOUT C,2,TIMDIDNO,LBIN                                                
Seq      LKOUT C,3,TIMDNIDN,LBIN                                                
Dnarr    LKOUT C,4,TIMDNARR,CHAR,LEN=V                                          
N2RSKIPS EQU   (*-N2RSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYDNAR3 LKOUT A,(D,B#TIMREC,TIMRFST),EOT=EOR,ROWID=(TIMELD,TIMELQ),   +        
               ROWWIDTH=(V,TIMLN),NEWEL=B                                       
PRout    LKOUT P,TIMETYP,SETTTYPE                                               
PRout    LKOUT P,TIMSEQ,SETDSEQ                                                 
Dndte    LKOUT C,1,TIMDTDT1,PDAT,FILTROUT=TSTDNSQ,SKIPCOLS=N3RSKIPS,   +        
               CHAR                                                             
N3RSKIP  EQU   *                                                                
Row#     LKOUT C,2,TIMDIDNO,LBIN                                                
Seq      LKOUT C,3,TIMDNIDN,LBIN                                                
Dnarr    LKOUT C,4,TIMDNARR,CHAR,LEN=V                                          
N3RSKIPS EQU   (*-N3RSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
***********************************************************************         
* Note this is called twice for time added via time widget            *         
* First call  = REAL time on timesheet (added by time widget)         *         
* Second call = Timelines from previous t/s or template               *         
***********************************************************************         
NXTTSW   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTTS08             No                                           
         MVC   TL_TROW,DV_HROW#    Then don't reset timeline row no.            
         TM    DINDS2,DIIPREV      Are we looking for previous periods          
         JZ    NXTTS20             No                                           
         CLI   QCOCTS,QCTYESQ      Are tsheets to be based on previous          
         JNE   NXTTS22             No                                           
         L     R4,AIO5             Yes - Look up period end dates               
         USING PERTABD,R4                                                       
NXTTS02  CLI   0(R4),0                                                          
         JE    NXTTS22                                                          
         XR    RF,RF                                                            
         ICM   RF,7,PERENDT                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,DPERENDT                                                    
         CLC   DPERENDT,QENDD                                                   
         JL    NXTTS06                                                          
NXTTS04  LA    R4,PERLENQ(R4)                                                   
         J     NXTTS02                                                          
*                                                                               
         USING TSWRECD,R2                                                       
NXTTS06  LA    R2,IOKEY            Build start key for read                     
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,QPERSN                                                   
         MVC   TSWKEND,PERENDT                                                  
         SR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TSWKODS(0),D1RACT                                                
         OC    TSWKODS,SPACES                                                   
         MVC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD                                 
         L     R1,=AL4(IOHID+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NXTTS12                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   NXTTS12                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     XERROR                                                           
                                                                                
NXTTS08  LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY4                                                    
         MVC   CSVKEY1,CSVKEY4                                                  
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NXTTS10                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   NXTTS10                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     XERROR                                                           
NXTTS10  L     R1,=AL4(IOSQD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NXTTS12                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   NXTTS12                                                          
         MVC   LP_ERROR,=AL2(AE$DSKER)                                          
         J     XERROR                                                           
                                                                                
NXTTS12  CLC   CSVKEY1(TSWKULC-TSWRECD),TSWRECD                                 
         JE    NXTTS16                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JE    NXTTS04             Yes - for previous period                    
         J     NXTTS22             No                                           
NXTTS16  MVC   CSVKEY4,TSWRECD                                                  
*                                                                               
* THERE IS GOOD REASON NOT TO TEST THE PASSIVE FOR DELETION                     
* THE UPDATE DELETES ALL PASSIVES AND READDS ALWAYS TEST PRIMARY RECORD         
*                                                                               
NXTTS18  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
NXTTS20  MVC   LP_ADATA,AIO2                                                    
         L     R2,AIO2             Yes - read through record and zero           
         USING TIMRECD,R2                                    hours              
         TM    TIMRSTAT,TIMSDELT   Don't want deleted records shown             
         JNZ   NXTTS08                                                          
         J     EXITY               No                                           
                                                                                
NXTTS22  MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
SETTTYPE L     R1,LP_AINP          Set type of time element                     
         MVC   WORK(L'TIMETYP),0(R1)                                            
         J     EXIT                                                             
                                                                                
SETDSEQ  L     R1,LP_AINP          Set sequence of narrative                    
         MVC   WORK+L'TIMETYP+L'TIMIND+L'TIMSEQ(L'TIMSEQ),0(R1)                 
         J     EXIT                                                             
                                                                                
RESINI   GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         J     EXIT                                                             
                                                                                
SETINPUT MVI   DINPUT,YESQ                                                      
         L     R1,LP_AINP                                                       
         MVC   WORK+L'TIMETYP+L'TIMIND(L'TIMSEQ),0(R1)                          
         J     EXIT                                                             
                                                                                
CLRINPUT MVI   DINPUT,NOQ                                                       
         MVI   DTEXT,NOQ                                                        
         L     R1,AIO2                                                          
         AHI   R1,TIMRFST-TIMRECD                                               
         USING TIMELD,R1                                                        
CLRI02   CLI   TIMEL,0                                                          
         JE    EXIT                                                             
         CLI   TIMEL,TIMELQ                                                     
         JNE   CLRI04                                                           
         CLI   TIMETYP,TIMEDNAR                                                 
         JE    CLRI06                                                           
         J     EXIT                                                             
CLRI04   LLC   RF,TIMLN                                                         
         AR    R1,RF                                                            
         J     CLRI02                                                           
                                                                                
CLRI06   MVI   DTEXT,YESQ                                                       
         J     EXIT                                                             
         DROP  R1                                                               
SETTIND  L     R1,LP_AINP                                                       
         MVC   WORK+L'TIMETYP(L'TIMIND),0(R1)                                   
         J     EXIT                                                             
                                                                                
SETWIDG  L     R1,LP_AINP                                                       
         NI    DINDS2,X'FF'-DIIWIDG                                             
         TM    0(R1),TIMEWIDG      Test whether widget time entry?              
         JZ    EXITY                                                            
         OI    DINDS2,DIIWIDG      Set widget time entry                        
         J     EXITY                                                            
                                                                                
         USING TIMELD,RF                                                        
SETHROW# L     RF,LP_AINP          Set highest row number                       
         CLI   TIMETYP,TIMETIME                                                 
         JNE   EXITY                                                            
         CLC   TIMEIDNO,DV_HROW#                                                
         JNH   EXITY                                                            
         MVC   DV_HROW#,TIMEIDNO  Set highest row number                        
         J     EXITY                                                            
         DROP  RF                                                               
                                                                                
SETPRWJ  TM    DINDS2,DIIWIDG      Is all time widget only?                     
         JZ    EXITY                                                            
         OI    DINDS2,DIIPREV      Set to return timeline from                  
         J     EXITY               previous period                              
                                                                                
BLNKCNT  L     R1,LP_AINP          For evey blank line in previous              
         CLC   0(L'ACTKULA,R1),SPACES     timesheet - reduce blankline          
         JNE   EXIT                       counter                               
         L     R3,BLNKLNS                                                       
         CHI   R3,0                ensure counter never goes negative           
         JE    EXIT                                                             
         SHI   R3,1                                                             
         ST    R3,BLNKLNS                                                       
         J     EXIT                                                             
                                                                                
TSTTINP  CLI   WORK,TIMEINP        Test if time input element                   
         BNER  RE                                                               
         TM    DINDS2,DIIPREV      Are we looking for previous periods          
         JZ    TSTTI04                                                          
         CLI   QCOCTS,QCTYESQ      Yes, is it copy t/s from prior?              
         JNE   TSTTI04                                                          
         TM    WORK+L'TIMETYP,TIMIADJ                                           
         JZ    TSTTI04                                                          
         LTR   RE,RE                                                            
         BR    RE                  Do not include greyed out rows               
TSTTI04  CR    RE,RE                                                            
TSTINPN  BR    RE                                                               
                                                                                
TSTTNAR  CLI   WORK,TIMENAR        Test if time narrative element               
         BR    RE                                                               
                                                                                
TSTTORD  CLI   WORK,TIMEORDR       Test if time order element                   
         BR    RE                                                               
                                                                                
TSTTINT  CLI   WORK,TIMEINRF       Test if time internal ref element            
         BR    RE                                                               
                                                                                
TSTTEST  CLI   WORK,TIMEEST        Test if time estimate element                
         BR    RE                                                               
                                                                                
TSTTIOF  CLI   WORK,TIMETOFF       Test if timeoff element                      
         BR    RE                                                               
                                                                                
TSTTTIM  CLI   WORK,TIMETIME       Test if time brandocean element              
         BR    RE                                                               
                                                                                
TSTTITM  CLI   WORK,TIMEITMS       Test if time item element                    
         BR    RE                                                               
                                                                                
TSTDNAR  CLI   WORK,TIMEDNAR       Test if day narrative element                
         BR    RE                                                               
                                                                                
TSTDNARF CLI   DTEXT,YESQ          Test if day narrative is first               
         BNER  RE                   timel on rec as it spans mult recs          
         TM    DINDS2,DIIPREV      Don't copy if based on previous ts           
         BR    RE                                                               
                                                                                
TSTDNSQ  CLI   WORK,TIMEDNAR       Test if day narrative element                
         BNER  RE                                                               
         CLC   WORK+L'TIMETYP+L'TIMIND(L'TIMSEQ),WORK+3 Does seq match          
         BR    RE                                                               
                                                                                
TSTESJAC L     R1,LP_AINP          Test if production ledger                    
         USING TIMELD,R1                                                        
         CLC   PRODUL,TIMACC                                                    
         BR    RE                                                               
                                                                                
TSTE1NAC L     R1,LP_AINP          Test if 1N ledger                            
         CLC   ONENUL,TIMACC                                                    
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
TSTNAUD  TM    DINDS2,DIINAUD                                                   
         JNZ   SETCCC                                                           
                                                                                
TSTPREV  TM    DINDS2,DIIPREV                                                   
         J     SETCCC                                                           
                                                                                
TSTPRE2  TM    DINDS2,DIIWIDG      Test all time is from widget?                
         JNZ   SETCCC              Then don't show line                         
         TM    DINDS2,DIIPREV                                                   
         J     SETCCC                                                           
                                                                                
TSTWIDG  TM    DINDS2,DIIWIDG      Test all time is from widget?                
         J     SETCCC                                                           
                                                                                
TSTINPUT CLI   DINPUT,NOQ                                                       
         BNER  RE                                                               
         TM    DINDS2,DIIPREV                                                   
         BR    RE                                                               
                                                                                
TSTPRRD  TM    DINDS2,DIIPREV      Returning previous t/s?                      
         JZ    SETCCC                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         J     SETCCC                                                           
                                                                                
TSTPREVR TM    DINDS2,DIIPREV                                                   
         BR    RE                                                               
                                                                                
TSTTEMP  TM    DINDS2,DIIPREV                                                   
         JZ    SETCCC                                                           
         CLI   QCOCTS,QCTST        Are tsheets to be based on template          
         BR    RE                                                               
                                                                                
TSTCNAR  TM    DINDS2,DIIPREV                                                   
         BZR   RE                                                               
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         CLI   COCNR,C'Y'          Are tsheets to be based on template          
         BR    RE                                                               
                                                                                
TSTTRES  TM    DINDS2,DIIPREV                                                   
         JZ    SETCCC                                                           
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         CLI   COURS,C'Y'          Are tsheets to be based on resources         
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Call GETOPT and get opt maint settings                              *         
***********************************************************************         
         SPACE 1                                                                
GETOPTM  NTR1  LABEL=NO,WORK=(RC,VIWORKL)                                       
         J     *+12                                                             
         DC    C'*GETOPTM'                                                      
                                                                                
         USING VIWORKD,RC                                                       
         GOTOR CLRWRK,VIWORKL      Clear work area                              
         L     R2,LP_AINP          For evey blank line in previous              
         USING OB_D,VIRBAREA                                                    
         MVI   OB_PTYP,OB_PTYPQ    Build key of buffer record                   
         MVI   OB_PSUB,OB_PSUBQ                                                 
         MVC   OB_PJOB,0(R2)                                                    
         MVC   OB_PWCD,L'TIMACC(R2)                                             
         GOTOR GETBUF,OB_D                                                      
         JH    EXITN                                                            
         JE    GETOPT04            If found have all values set                 
                                                                                
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
                                                                                
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
         JNH   GETOPT02            NO                                           
         CLC   0(0,R1),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   GOSELPRO(0),0(R1)                                                
         LLC   RE,PPROLEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNH   GETOPT02            NO                                           
         CLC   0(0,R1),SPACES      DO WE HAVE A JOB CODE                        
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   GOSELJOB(0),0(R1)                                                
         MVC   GOSELWC,L'TIMACC(R2)                                             
                                                                                
GETOPT02 GOTOR VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
*&&UK*&& MVC   OB_TTALL,GOTTALLW                                                
*&&UK*&& MVC   OB_TFNAR,GOTFNARR                                                
*&&UK*&& MVC   OB_TNOJB,GOTNOJOB                                                
                                                                                
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
         MVC   OB_FPT,GOFPT                                                     
         MVC   OB_FJT,GOFJT                                                     
         MVC   OB_TOT,GOTOT                                                     
*&&US*&& MVC   OB_TTALL,GOTTALLW                                                
*&&US*&& MVC   OB_TFNAR,GOTFNARR                                                
*&&US*&& MVC   OB_TNOJB,GOTNOJOB                                                
         GOTOR ADDBUF,OB_D         Add option buffer record                     
         DROP  R3                                                               
                                                                                
GETOPT04 LA    R3,OB_TTALL                                                      
         LA    R1,3                                                             
         MVC   TL_BILA(L'TL_BILA+L'TL_CHGA+L'TL_NONA),=C'NNN'                   
GETOPT06 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   TL_BILA,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   TL_CHGA,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   TL_NONA,C'Y'                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,GETOPT06                                                      
                                                                                
         MVC   TL_TOT,OB_TOT                                                    
*&&UK*&& MVC   TL_JOBT,OB_TNOJB                                                 
         MVC   TL_FPT,OB_FPT                                                    
         MVC   TL_FJT,OB_FJT                                                    
                                                                                
         LA    R3,OB_TFNAR                                                      
         LA    R1,3                                                             
         MVC   TL_FNTB(L'TL_FNTB+L'TL_FNTR+L'TL_FNTN),=C'NNN'                   
GETOPT08 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   TL_FNTB,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   TL_FNTR,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   TL_FNTN,C'Y'                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,GETOPT08                                                      
                                                                                
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
VIWORKD  DSECT                     ** VALINC local w/s **                       
VIRBAREA DS    XL(OB_LNQ)          GETOPT buffer record                         
         ORG   VIRBAREA                                                         
OB_PTYP  DS    X                   ** Buffer key **                             
OB_PTYPQ EQU   X'FF'               Buffer type                                  
OB_PSUB  DS    C                                                                
OB_PSUBQ EQU   C'O'                Buffer sub-type                              
OB_PJOB  DS    CL(L'ACTKULA)       Job code                                     
OB_PWCD  DS    CL(L'TIMTSK)        Work code                                    
         ORG   VIRBAREA+(OB_DATA-OB_D)                                          
OB_FPT   DS    CL(L'GOFPT)         Force product on time sheet                  
OB_FJT   DS    CL(L'GOFJT)         Force job on time sheet                      
OB_TOT   DS    CL(L'GOTOT)         Default type of time                         
OB_INACT DS    CL(L'GOICA)         Income account                               
OB_TFNAR DS    CL(L'GOTFNARR)      Force narrative for time types               
OB_TNOJB DS    CL(L'GOTNOJOB)      Job allowed                                  
OB_TTALL DS    CL(L'GOTTALLW)      Type of time allowed                         
         ORG                                                                    
VIWORKL  EQU   *-VIWORKD                                                        
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
                                                                                
ARYHOL   LKOUT A,(R,BLDHOLS),NROWS=1                                            
                                                                                
array    LKOUT C,R#TIMTIM,(A,ARYTTB)                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYTTB   LKOUT A,(I,B#WORKD,ATIA),NEWEL=B,EOT=EOR,                     +        
               ROWID=(TMTTEL,TMTTELQ),                                 +        
               ROWWIDTH=(V,TMTTLN)                                              
TimTyp   LKOUT C,1,TMTTYP,CHAR                                                  
Cod1N    LKOUT C,2,TMTACC,CHAR,LEN=L'ACTKACT,FILTROUT=TSTT1NAC,        +        
               SKIPCOLS=1                                                       
Nam1N    LKOUT C,3,TMTULA,(R,EDTANM)                                            
CodCli   LKOUT C,4,TMTULA,(U,#EDTCLI,$EDTCLI),FILTROUT=TSTTSJAC,       +        
               SKIPCOLS=MSJSKIPS                                                
MSJSKIP  EQU   *                                                                
NamCli   LKOUT C,5,TMTULA,(R,EDTCLN)                                            
CodPrd   LKOUT C,6,TMTULA,(U,#EDTPRD,$EDTPRD)                                   
NamPrd   LKOUT C,7,TMTULA,(R,EDTPRN)                                            
CodJob   LKOUT C,8,TMTULA,(U,#EDTJOB,$EDTJOB)                                   
NamJob   LKOUT C,9,TMTULA,(R,EDTJBN)                                            
WC       LKOUT C,11,TMTTSK,CHAR                                                 
WCNam    LKOUT C,12,TMTTSK,(R,EDTWCD)                                           
PRout    LKOUT P,TMTULA,GETOPTM                                                 
FrcePr   LKOUT C,13,(D,B#SAVED,TL_FPT),CHAR,ND=Y                                
FrceJb   LKOUT C,14,(D,B#SAVED,TL_FJT),CHAR,ND=Y                                
NarBill  LKOUT C,15,(D,B#SAVED,TL_FNTB),CHAR,ND=Y                               
NarMemo  LKOUT C,16,(D,B#SAVED,TL_FNTR),CHAR,ND=Y                               
NarNBil  LKOUT C,17,(D,B#SAVED,TL_FNTN),CHAR,ND=Y                               
JobInpt  LKOUT C,18,(D,B#SAVED,TL_JOBT),CHAR,ND=Y                               
BilAlwd  LKOUT C,19,(D,B#SAVED,TL_BILA),CHAR,ND=Y                               
MemAlwd  LKOUT C,20,(D,B#SAVED,TL_CHGA),CHAR,ND=Y                               
NonAlwd  LKOUT C,21,(D,B#SAVED,TL_NONA),CHAR,ND=Y                               
DefType  LKOUT C,22,(D,B#SAVED,TL_TOT),CHAR,ND=Y                                
MSJSKIPS EQU   (*-MSJSKIP)/LX_COLSL                                             
AppCod   LKOUT C,25,TMTCLIAP,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,26,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,27,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,28,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                       
Row      LKOUT C,33,TMTIDNO,LBIN                                                
LstItm   LKOUT C,34,TMTIIDN,LBIN                                                
CliApp   LKOUT C,35,TMTAPST,CHAR                                                
LocBl    LKOUT C,39,(D,B#SAVED,TL_LOCBI),CHAR,ND=Y                              
Futal    LKOUT C,40,(D,B#SAVED,TL_FUTAL),CHAR,ND=Y                              
Office   LKOUT C,41,TMTOFF,CHAR,ND=Y                                            
OffNam   LKOUT C,42,TMTOFF,(R,EDTOFN),ND=Y                                      
1Nlock   LKOUT C,44,(D,B#SAVED,TL_TLOC),CHAR,ND=Y                               
Tioid    LKOUT C,45,(A,ARYTIO),FILTROUT=TSTPREVR Timeoff id                     
RealT    LKOUT C,46,TMTTEL,(R,EDTREAL),ND=Y      Real timeline                  
Array    LKOUT C,50,(A,ARYHRS1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYHRS1  LKOUT A,(*,TMTHRS),ROWNAME=TMTTABD,NROWS=*,ROWWIDTH=L'TMTHRS           
Hours    LKOUT C,50,TMTHRS,SPAK                                                 
                                                                                
         LKOUT E                                                                
                                                                                
BLDHOLS  L     R2,ATIA                                                          
         USING TMTTABD,R2                                                       
         LR    R0,R2               Clear TIA                                    
         LHI   R1,TWAMAX                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR GETHOL                                                           
         XR    RF,RF                                                            
BHOL02   CLI   TMTTEL,TMTTELQ                                                   
         JNE   BHOL04                                                           
         IC    RF,TMTTLN                                                        
         AR    R2,RF                                                            
         J     BHOL02                                                           
*                                                                               
BHOL04   GOTOR GET1NAC                                                          
         L     R4,AELEAREA                                                      
BHOL06   CLI   0(R4),X'FF'                                                      
         JE    BHOL20                                                           
         USING TMTTABD,R2                                                       
         MVI   TMTTEL,TMTTELQ                                                   
         MVI   TMTTYP,C'N'                                                      
         MVC   TMTULA,0(R4)            '                                        
         MVC   TMTTSK,SPACES                                                    
         GOTOR GETCAP,TMTULA                                                    
         MVC   TMTCLIAP,TL_CLIAP                                                
         MVI   TMTGRY,C'Y'                                                      
         CLC   CCTPID,TL_CLIAP                                                  
         JE    BHOL08                                                           
         TM    DINDS2,DIILMGR                                                   
         JNZ   BHOL08                                                           
         TM    DINDS2,DIIUSR                                                    
         JZ    BHOL10                                                           
BHOL08   MVI   TMTGRY,C'N'                                                      
BHOL10   MVI   TMTAPST,C'N'                                                     
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         MVC   TMTIDNO,TL_TROW                                                  
         LA    R3,TMTHRS                                                        
         LH    RF,TH_NDAYS                                                      
BHOL12   ZAP   0(L'TMTHRS,R3),PZERO                                             
         LA    R3,L'TMTHRS(R3)                                                  
         JCT   RF,BHOL12                                                        
         SR    R3,R2                                                            
         STC   R3,TMTTLN                                                        
         AR    R2,R3                                                            
         LA    R4,L'ACTKULA(R4)                                                 
         J     BHOL06                                                           
*                                                                               
BHOL20   MVC   LP_ADATA,ATIA                                                    
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
TSTTSJAC L     R1,LP_AINP          Test if production ledger                    
         USING TMTTABD,R1                                                       
         CLC   PRODUL,TMTUNT                                                    
         BR    RE                                                               
                                                                                
TSTT1NAC L     R1,LP_AINP          Test if 1N ledger                            
         CLC   ONENUL,TMTUNT                                                    
         BNER  RE                                                               
         CLC   TMTACC,SPACES                                                    
         JNL   SETCCC                                                           
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
ARYTMP   LKOUT A,(R,BLDTMP),NROWS=1                                             
                                                                                
array    LKOUT C,R#TIMTIM,(A,ARYTTB)                                            
                                                                                
         LKOUT E                                                                
                                                                                
BLDTMP   L     R2,ATIA                                                          
         LR    R0,R2               Clear TIA                                    
         LHI   R1,TWAMAX                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR GETTEMP                                                          
         L     R3,AIO2                                                          
         USING TTMRECD,R3                                                       
         USING TMTTABD,R2                                                       
         LA    R3,TTMRFST                                                       
         USING LIDELD,R3                                                        
BTEMP02  CLI   LIDEL,0                                                          
         JE    BTEMPY                                                           
         CLI   LIDEL,LIDELQ                                                     
         JE    BTEMP06                                                          
BTEMP04  SR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R3,R0                                                            
         J     BTEMP02                                                          
*                                                                               
BTEMP06  LA    R4,LIDDATA                                                       
         XR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         STC   RE,ELEMLN                                                        
*                                                                               
BTEMP08  SHI   RE,(LIDDATA-LIDELD)+1                                            
         JM    BTEMP04             no codes on element                          
         CLI   LIDTYPE,LIDTNCLC    are we non client code                       
         JNE   BTEMP10             no                                           
*                                                                               
         MVC   TMTUNT(L'TMTUNT+L'TMTLDG),ONENUL                                 
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   TMTACC(0),0(R4)                                                  
         MVC   TEMP2(L'TMTUNT+L'TMTLDG+L'TMTACC),TMTUNT                         
         GOTOR CHK1AL              Check 1R account is locked                   
         JNE   BTEMP20                                                          
         MVI   TMTTYP,C'N'                                                      
         J     BTEMP12                                                          
*                                                                               
BTEMP10  CLI   LIDTYPE,LIDTCLIT    are we client/prod/job                       
         JNE   BTEMP04             no - get next element                        
         MVC   TMTUNT(L'TMTUNT+L'TMTLDG),PRODUL                                 
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         SHI   RF,L'LITTIMTY+L'LITTWRKC                                         
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   TMTACC(0),L'LITTIMTY+L'LITTWRKC(R4)                              
         AHI   RF,L'LITTIMTY+L'LITTWRKC                                         
         MVC   TMTTSK,L'LITTIMTY(R4)                                            
         MVC   TMTTYP,0(R4)                                                     
         CLC   TMTTSK,SPACES                                                    
         JH    *+10                                                             
         MVC   TMTTSK,TH_DEFWC                                                  
         GOTOR GETOFF,TMTULA                                                    
         MVC   TMTOFF,TL_OFF                                                    
*                                                                               
BTEMP12  GOTOR GETCAP,TMTULA                                                    
         MVC   TMTCLIAP,TL_CLIAP                                                
         MVI   TMTGRY,C'Y'                                                      
         CLC   CCTPID,TL_CLIAP                                                  
         JE    BTEMP14                                                          
         TM    DINDS2,DIILMGR                                                   
         JNZ   BTEMP14                                                          
         TM    DINDS2,DIIUSR                                                    
         JZ    BTEMP16                                                          
BTEMP14  MVI   TMTGRY,C'N'                                                      
*                                                                               
BTEMP16  MVI   TMTAPST,C'N'                                                     
         MVI   TMTTEL,TMTTELQ                                                   
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         MVC   TMTIDNO,TL_TROW                                                  
         LA    RE,TMTHRS                                                        
         LH    RF,TH_NDAYS                                                      
BTEMP18  ZAP   0(L'TMTHRS,RE),PZERO                                             
         LA    RE,L'TMTHRS(RE)                                                  
         JCT   RF,BTEMP18                                                       
         SR    RE,R2                                                            
         STC   RE,TMTTLN                                                        
         AR    R2,RE                                                            
*                                                                               
BTEMP20  XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         AR    R4,RF               R4=A(NEXT ENTRY ON ELEMENT)                  
         XR    RE,RE                                                            
         IC    RE,ELEMLN                                                        
         SR    RE,RF                                                            
         STC   RE,ELEMLN                                                        
         J     BTEMP08                                                          
                                                                                
BTEMPY   MVC   LP_ADATA,ATIA                                                    
         J     EXITY                                                            
                                                                                
ARYRES   LKOUT A,(R,BLDRES),NROWS=1                                             
                                                                                
array    LKOUT C,R#TIMTIM,(A,ARYTTB)                                            
                                                                                
         LKOUT E                                                                
                                                                                
BLDRES   L     R2,ATIA                                                          
         USING TMTTABD,R2                                                       
         LR    R0,R2               Clear TIA                                    
         LHI   R1,TWAMAX                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
BRES02   XC    DENDSTR,DENDSTR     SET RANGE READY FOR KEY DRIVER               
         MVI   DSTREND,X'FF'                                                    
         MVC   DSTREND+1(L'DSTREND-1),DSTREND                                   
         XR    RE,RE                                                            
         ICM   RE,7,QLOCED                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,DSTRSTR                                                     
         XR    RE,RE                                                            
         ICM   RE,7,TH_DATE                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,DENDEND                                                     
                                                                                
BRES04   MVC   IOKEY,SVPKSKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,PKSKEYT,('B#PTASK',0),           +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   BRESY                                                            
         MVC   SVPKSKEY,IOKEY                                                   
         LA    R3,SVPKSKEY                                                      
         USING PKSRECD,R3          R2=A(PERSONAL KEY STAGE KEY)                 
         TM    PKSKSTAT,RWKSDEAD                                                
         JNZ   BRES04                                                           
         OC    PKSKKYST,PKSKKYST                                                
         JNZ   BRES06                                                           
         MVC   TMTULA,DSHOLAC                                                   
         J     BRES08                                                           
*                                                                               
BRES06   MVC   TMTULA,PKSKUNT                                                   
         MVC   TMTTSK,TH_DEFWC                                                  
         GOTOR GETOFF,TMTULA                                                    
         MVC   TMTOFF,TL_OFF                                                    
*                                                                               
BRES08   GOTOR GETCAP,TMTULA                                                    
         MVC   TMTCLIAP,TL_CLIAP                                                
         MVI   TMTGRY,C'Y'                                                      
         CLC   CCTPID,TL_CLIAP                                                  
         JE    BRES10                                                           
         TM    DINDS2,DIILMGR                                                   
         JNZ   BRES10                                                           
         TM    DINDS2,DIIUSR                                                    
         JZ    BRES12                                                           
BRES10   MVI   TMTGRY,C'N'                                                      
*                                                                               
BRES12   MVI   TMTAPST,C'N'                                                     
         MVI   TMTTYP,C'N'                                                      
         CLI   TH_DEFT,C' '                                                     
         JNH   *+10                                                             
         MVC   TMTTYP,TH_DEFT                                                   
         MVI   TMTTEL,TMTTELQ                                                   
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         MVC   TMTIDNO,TL_TROW                                                  
         LA    RE,TMTHRS                                                        
         LH    RF,TH_NDAYS                                                      
BRES14   ZAP   0(L'TMTHRS,RE),PZERO                                             
         LA    RE,L'TMTHRS(RE)                                                  
         JCT   RF,BRES14                                                        
         SR    RE,R2                                                            
         STC   RE,TMTTLN                                                        
         AR    R2,RE                                                            
         J     BRES04                                                           
                                                                                
BRESY    MVC   LP_ADATA,ATIA                                                    
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
                                                                                
ARYBLNK  LKOUT A,(R,BLDBLNK),NROWS=1                                            
                                                                                
array    LKOUT C,R#TIMTIM,(A,ARYTTB)                                            
                                                                                
         LKOUT E                                                                
                                                                                
BLDBLNK  L     R2,ATIA                                                          
         L     R3,BLNKLNS                                                       
         LR    R0,R2               Clear TIA                                    
         LHI   R1,TWAMAX                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         OC    BLNKLNS,BLNKLNS                                                  
         JZ    BBLNK10                                                          
         USING TMTTABD,R2                                                       
BBLNK02  MVI   TMTTEL,TMTTELQ                                                   
         MVI   TMTTYP,C'N'                                                      
         CLI   TH_DEFT,C' '                                                     
         JNH   *+10                                                             
         MVC   TMTTYP,TH_DEFT                                                   
         MVC   TMTULA,SPACES                                                    
         MVC   TMTTSK,SPACES                                                    
         MVI   TMTGRY,C'Y'                                                      
         TM    DINDS2,DIILMGR                                                   
         JNZ   BBLNK04                                                          
         TM    DINDS2,DIIUSR                                                    
         JZ    BBLNK06                                                          
BBLNK04  MVI   TMTGRY,C'N'                                                      
BBLNK06  MVI   TMTAPST,C'N'                                                     
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         MVC   TMTIDNO,TL_TROW                                                  
         LA    RE,TMTHRS                                                        
         LH    RF,TH_NDAYS                                                      
BBLNK08  ZAP   0(L'TMTHRS,RE),PZERO                                             
         LA    RE,L'TMTHRS(RE)                                                  
         JCT   RF,BBLNK08                                                       
         SR    RE,R2                                                            
         STC   RE,TMTTLN                                                        
         AR    R2,RE                                                            
         JCT   R3,BBLNK02                                                       
                                                                                
BBLNK10  MVC   LP_ADATA,ATIA                                                    
         J     EXITY                                                            
                                                                                
SETQORD  L     R1,LP_AINP                                                       
         MVC   DORDNUM,0(R1)                                                    
         BR    RE                                                               
                                                                                
ARYORN   LKOUT A,(R,NXTORD),NROWS=1,ROWNAME=ORDRECD                             
                                                                                
Array    LKOUT C,31,(A,ARYORN1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYORN1  LKOUT A,(D,B#ORD,ORDRFST),EOT=EOR,                            +        
               ROWID=(ENMEL,ENMELQ),ROWWIDTH=(V,ENMLN)                          
                                                                                
Ordname  LKOUT C,31,ENMNAME,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET ORDER RECORD                                                    *         
***********************************************************************         
                                                                                
NXTORD   GOTOR (#NXTREC,ANXTREC),DMCB,ORDKEYT,('B#ORD',0),             +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
                                                                                
SETQEST  L     R1,LP_AINP                                                       
         MVC   DESTNUM,0(R1)                                                    
         BR    RE                                                               
                                                                                
ARYESN   LKOUT A,(R,NXTEST),NROWS=1,ROWNAME=ESTRECD                             
                                                                                
EstL#    LKOUT C,37,ESTKLNO,LBIN                                                
Array    LKOUT C,38,(A,ARYESN1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYESN1  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,                            +        
               ROWID=(ENMEL,ENMELQ),ROWWIDTH=(V,ENMLN)                          
                                                                                
EstName  LKOUT C,38,ENMNAME,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET ESTIMATE RECORD                                                 *         
***********************************************************************         
                                                                                
NXTEST   MVC   IOKEY,SVEGNKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,EGNKEYT,('B#EST',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         MVC   SVEGNKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         USING EGNPASD,R2                                                       
         MVC   IODA,EGNPDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ADATA,AIO3                                                    
         J     EXITY                                                            
                                                                                
**********************************************************************          
* CALENDAR DOWNLOAD                                                  *          
**********************************************************************          
                                                                                
REQCALD  LKREQ H,A#CALD,OUTCALD,NEXTREQ=REQUESTX                                
                                                                                
Dummy    LKREQ F,01,(D,B#SAVED,QINITIAL),CHAR,OLEN=L'QINITIAL,         +        
               MAXLEN=L'QINITIAL,TEXT=(*,CALDWLIT),COL=*                        
         LKREQ E                                                                
                                                                                
OUTCALD  LKOUT H                                                                
                                                                                
CALHDR2  LKOUT R,R#CALD            Calendar record                              
Array    LKOUT C,R#CALD,(A,ARYCAL)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCAL   LKOUT A,(R,NXTCAL),MULTIROW=Y,ROWNAME=CASRECD                          
PRout    LKOUT P,CASKEMOA,SETDATE                                               
EnMOA    LKOUT C,1,(D,B#WORKD,WORK),PDAT                                        
PRout    LKOUT P,CASKSMOA,SETDATE                                               
StMOA    LKOUT C,2,(D,B#WORKD,WORK),PDAT                                        
Office   LKOUT C,3,CASKOFC,CHAR                                                 
                                                                                
Array    LKOUT C,1,(A,ARYPER)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYPER   LKOUT A,(D,B#CAL,CASRFST),NEWEL=B,EOT=EOR,                    +        
               ROWID=(TMPEL,TMPELQ),ROWWIDTH=(V,TMPLN)                          
PRout    LKOUT P,TMPMTH,SETDATE                                                 
PrMnth   LKOUT C,1,(D,B#WORKD,WORK),PDAT                                        
PrNum    LKOUT C,2,TMPNUMB,LBIN                                                 
StDate   LKOUT C,3,TMPSTART,PDAT                                                
EnDate   LKOUT C,4,TMPEND,PDAT                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read for calendar records                                           *         
***********************************************************************         
                                                                                
NXTCAL   GOTOR (#NXTREC,ANXTREC),DMCB,CALKEYT,('B#CAL',0),             +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
                                                                                
SETDATE  L     R1,LP_AINP          Convert YYMM to YYMMDD                       
         MVC   WORK(L'CASKEMOA),0(R1)                                           
         MVI   WORK+L'CASKEMOA,X'01'                                            
         J     EXIT                                                             
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
***********************************************************************         
* Edit time type code                                                 *         
***********************************************************************         
         SPACE 1                                                                
EDTTYP   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),TIMTCB                                                     
         JNE   EDTTYP02                                                         
         MVI   0(R4),C'B'                                                       
         J     EDTTYP10                                                         
                                                                                
EDTTYP02 CLI   0(R2),TIMTCR                                                     
         JNE   EDTTYP04                                                         
         MVI   0(R4),C'R'                                                       
         J     EDTTYP10                                                         
EDTTYP04 CLI   0(R2),TIMTEM                                                     
         JNE   EDTTYP06                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Only show empty row for Aura                 
         CHI   RF,XPRODIKQ          otherwise treat as non-billable             
         JNE   EDTTYP06                                                         
         MVI   0(R4),C'E'                                                       
         J     EDTTYP10                                                         
EDTTYP06 MVI   0(R4),C'N'                                                       
                                                                                
EDTTYP10 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit Account name                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTANM   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'TRNKULA),0(R2)                                           
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit Client name                                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTCLN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         AHI   RE,1                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   TEMP2(0),0(R2)                                                   
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDIT PRODUCT NAME - PARM 1 UNIT/LEDGER/ACCOUNT                      *         
***********************************************************************         
         SPACE 1                                                                
EDTPRN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PCLILEN                                                       
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         AHI   RE,L'ACTKUNT+L'ACTKLDG                                           
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         JNH   EXITY               NO                                           
         CLC   0(0,RE),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         AHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP2(0),0(R2)                                                   
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDIT JOB NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                       *         
***********************************************************************         
         SPACE 1                                                                
EDTJBN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         IC    RF,PJOBLEN                                                       
         SR    RF,RE               RF=LENGTH OF JOB CODE                        
         AHI   RE,1                RE=DISPLACEMENT TO JOB CODE                  
         AR    RE,R2                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         JNH   EXITY               NO EXIT                                      
         CLC   0(0,RE),SPACES      DO WE HAVE A JOB CODE                        
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDIT TIME                                                           *         
***********************************************************************         
         SPACE 1                                                                
EDTIME   LM    R2,R4,LP_AINP                                                    
         UNPK  DUB,0(L'REJ_TIME,R2)                                             
         OI    DUB+7,C'0'                                                       
         MVC   0(6,R4),DUB+2                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate account code                                               *         
*                                                                     *         
* Ntry:- R1=A(ULCliProJob)                                            *         
***********************************************************************         
                                                                                
VALACT   NTR1  LABEL=NO,WORK=(RC,VAWORKL)                                       
         J     *+12                                                             
         DC    C'*VALACT*'                                                      
                                                                                
         USING VAWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VAWORKL      Clear work area                              
         USING OB_D,VARBAREA                                                    
         MVC   OB_KEY(L'ACTKULA),TEMP2                                          
         GOTOR GETBUF,OB_D                                                      
         JL    VALACT02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         MVC   TL_LOCBI(OB_LENQ),OB_LOCBI                                       
         J     VALACTY                                                          
                                                                                
VALACT02 GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
         MVI   OB_JOBL,NOQ                                                      
         MVI   OB_JOBC,NOQ                                                      
         MVI   OB_LOCBI,NOQ                                                     
         MVI   OB_FUTAL,NOQ                                                     
         MVI   OB_TLOC,NOQ                                                      
         L     R2,AIO3                                                          
         TM    ACTRSTAT-ACTRECD(R2),ACTSLOCK                                    
         JZ    *+8                                                              
         MVI   OB_JOBL,YESQ        Job locked status                            
         TM    ACTRSTAT-ACTRECD(R2),ACTSCLOS                                    
         JZ    *+8                                                              
         MVI   OB_JOBC,YESQ        job closed status                            
         GOTOR GETELA,RSTELQ                                                    
         JNE   VALACT06                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALACT06                                                         
         TM    RSTLSTAT,RSTLSBIQ   LOCKED FROM BILLING                          
         JZ    *+8                                                              
         MVI   OB_LOCBI,YESQ                                                    
         TM    RSTSTAT7,RSTSFUTM   FUTURE TIME ALLOWED?                         
         JZ    *+8                                                              
         MVI   OB_FUTAL,YESQ                                                    
         TM    RSTLSTAT,RSTLSTIQ   LOCKED FROM T/S?                             
         JZ    *+8                                                              
         MVI   OB_TLOC,YESQ                                                     
         DROP  R1                                                               
*                                                                               
VALACT06 MVC   TL_LOCBI(OB_LENQ),OB_LOCBI                                       
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALACTY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VAWORKD  DSECT                     ** VALACT local w/s **                       
VARBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VARBAREA+(OB_OTHER-OB_D)                                         
OB_LOCBI DS    XL1                 Locked from billing                          
OB_FUTAL DS    XL1                 Future time allowed                          
OB_JOBL  DS    XL1                 job is locked                                
OB_JOBC  DS    XL1                 job is closed                                
OB_TLOC  DS    XL1                 lock from t/s                                
OB_LENQ  EQU   *-OB_LOCBI          length of extra buffer data                  
         ORG                                                                    
VAWORKL  EQU   *-VAWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit office name                                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTOFN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'TRNOFFC,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'TRNOFFC),0(R2)                                           
         GOTOR VALOFF,TEMP2                                                     
         JNE   EXITY                                                            
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate office code and get name                                   *         
*                                                                     *         
* Ntry:- R1=A(office code)                                            *         
***********************************************************************         
                                                                                
VALOFF   NTR1  LABEL=NO,WORK=(RC,VOWORKL)                                       
         J     *+12                                                             
         DC    C'*VALOFF*'                                                      
                                                                                
         USING VOWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VOWORKL      Clear work area                              
         MVC   VOOFFCOD,0(R2)      Save office code                             
         USING OB_D,VORBAREA                                                    
         MVI   OB_OTYP,OB_OTYPQ    Build key of buffer record                   
         MVI   OB_OSUB,OB_OSUBQ                                                 
         MVC   OB_OFFC(L'TRNOFFC),VOOFFCOD                                      
         GOTOR GETBUF,OB_D                                                      
         JL    VALOFF02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         J     VALOFFY                                                          
                                                                                
VALOFF02 TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   VALOFF04                                                         
                                                                                
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'TRNOFFC),VOOFFCOD                                        
         GOTOR (#GETOFN,AGETOFN)                                                
         MVC   OB_NAME,TEMP2                                                    
         J     VALOFF10                                                         
*                                                                               
K        USING OFFRECD,IOKEY                                                    
VALOFF04 MVC   K.OFFKEY,SPACES                                                  
         MVI   K.OFFKTYP,OFFKTYPQ                                               
         MVC   K.OFFKCPY,CUXCPY                                                 
         MVC   K.OFFKOFF,VOOFFCOD                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   OB_NAME,SPACES                                                   
         GOTOR GETELA,NAMELQ       Locate name element on record                
         JNE   VALOFFN                                                          
         USING NAMELD,R1                                                        
         CLI   NAMLN,NAMLN1Q       Test good length                             
         JNH   VALOFFN                                                          
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         CHI   RF,L'NAMEREC-1                                                   
         JNH   *+8                                                              
         LHI   RF,L'NAMEREC-1                                                   
         BASR  RE,0                                                             
         MVC   OB_NAME(0),NAMEREC   Move name to output area                    
         EX    RF,0(RE)                                                         
                                                                                
VALOFF10 MVC   TEMP2,OB_NAME                                                    
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALOFFY  J     EXITY                                                            
VALOFFN  J     EXITN                                                            
                                                                                
         DROP  R1,RC,K                                                          
                                                                                
VOWORKD  DSECT                     ** VALOFF local w/s **                       
VOOFFCOD DS    CL(L'TRNOFFC)       Office code                                  
VORBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VORBAREA                                                         
OB_OTYP  DS    X                   ** Buffer key **                             
OB_OTYPQ EQU   X'FF'               Buffer type                                  
OB_OSUB  DS    C                                                                
OB_OSUBQ EQU   C'A'                Buffer sub-type                              
OB_OFFC  DS    CL(L'TRNOFFC)       Office code                                  
         ORG   VORBAREA+(OB_DATA-OB_D)                                          
         ORG                                                                    
VOWORKL  EQU   *-VOWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* EDIT WORDCODE DESCIRPTION - PARM 1 IS WORDCODE                      *         
***********************************************************************         
         SPACE 1                                                                
EDTWCD   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'WCOKWRK,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2(L'WCOKWRK),0(R2)                                           
         GOTOR VALWCD,TEMP2                                                     
         MVC   0(L'WCODESC,R4),TEMP2                                            
         LHI   RE,L'WCODESC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate workcode                                                   *         
*                                                                     *         
* Ntry:- R1=A(WC)                                                     *         
***********************************************************************         
                                                                                
VALWCD   NTR1  LABEL=NO,WORK=(RC,VAWORKL)                                       
         J     *+12                                                             
         DC    C'*VALWCD*'                                                      
                                                                                
         USING VWWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VWWORKL      Clear work area                              
         USING OB_D,VWRBAREA                                                    
         MVC   OB_KEY(L'WCOKWRK),0(R2)                                          
         GOTOR GETBUF,OB_D                                                      
         JL    VALWCD02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         J     VALWCDY                                                          
                                                                                
VALWCD02 GOTOR (#GETWCD,AGETWCD)                                                
         MVC   OB_NAME(L'WCODESC),TEMP2                                         
VALWCD06 GOTOR ADDBUF,OB_D                                                      
                                                                                
VALWCDY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VWWORKD  DSECT                     ** VALWCD local w/s **                       
VWRBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG                                                                    
VWWORKL  EQU   *-VWWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit time adjustment type                                           *         
***********************************************************************         
                                                                                
EDTADJ   LM    R2,R4,LP_AINP                                                    
         TM    0(R2),TIMIADJ       Is time an adjustment/transfer               
         JZ    EDTADJ02            No                                           
         MVI   0(R4),C'A'          Yes - set A for adjustment                   
         J     EDTADJ06                                                         
                                                                                
EDTADJ02 TM    0(R2),TIMIWO        Is time a write off                          
         JZ    EDTADJ04            No                                           
         MVI   0(R4),C'W'          Yes - W for write off                        
         J     EDTADJ06                                                         
EDTADJ04 XC    LP_OLEN,LP_OLEN     No adjustments found - zero output           
                                                                                
EDTADJ06 LHI   R3,1                Set length as 1 for adjustments              
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit time grey line out                                             *         
***********************************************************************         
                                                                                
         USING TIMELD,R2                                                        
EDTGRY   LM    R2,R4,LP_AINP                                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether mobile                         
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   EDTGRY04                                                         
*                                                                               
         MVI   0(R4),C'Y'                                                       
*                                                                               
         LR    RF,R2                                                            
T        USING TIMELD,RF                                                        
EDTGRY02 LLC   R0,T.TIMLN                                                       
         AR    RF,R0                                                            
         CLI   T.TIMEL,0                                                        
         JE    EDTGRY04                                                         
         CLI   T.TIMEL,TIMELQ                                                   
         JNE   EDTGRY02                                                         
         CLC   T.TIMSEQ,TIMSEQ      Check sequence matches                      
         JNE   EDTGRY02                                                         
         CLI   T.TIMETYP,TIMETOFF   If row is from timeoff, disable             
         JE    EDTGRY16             for mobile                                  
         J     EDTGRY02                                                         
                                                                                
EDTGRY04 MVI   0(R4),C'N'                                                       
         TM    DINDS2,DIIPREV      Are we copying previous timesheet            
         JNZ   EDTGRY08            Yes - then no need to grey lines             
         MVI   0(R4),C'Y'          Default to grey out line                     
         TM    DINDS2,DIILMGR      Is the connected the line manager            
         JNZ   EDTGRY06            Yes                                          
         TM    DINDS2,DIIUSR       Is the connected the user                    
         JZ    EDTGRY08            No                                           
EDTGRY06 TM    TIMIND,TIMISADJ+TIMISWO IS THIS TIME ADJUSTED  Any adjst         
         JNZ   EDTGRY08            Yes                                          
         MVI   0(R4),C'N'          No - then set they can see lines             
                                                                                
EDTGRY08 MVC   TL_OFF,TIMOFF                                                    
         TM    CPYSTAT1,CPYSOROE Is agency on offices                           
         JNZ   EDTGRY10            Yes                                          
         MVC   TL_OFF,SPACES       No - clear offices to spaces                 
EDTGRY10 MVC   TL_MED,TIMMED                                                    
         GOTOR GETCAP,TIMACC       Get client/1n approvers                      
         MVC   TEMP2,SPACES                                                     
         XC    TL_PIDC,TL_PIDC                                                  
         OC    TL_CLIAP,TL_CLIAP                                                
         JZ    EDTGRY16                                                         
         MVC   TEMP2(L'SA0KNUM),TL_CLIAP                                        
         GOTOR (#GETPID,AGETPID)                                                
         JNE   EDTGRY14                                                         
         MVC   TL_PIDC,TEMP2                                                    
         GOTOR (#GETPIN,AGETPIN)                                                
         CLC   TL_CLIAP,CCTPID     Is the connected user an approver            
         JE    EDTGRY14            Yes                                          
         LA    R0,TH_BAMAX         Max Backup Approvers                         
         LA    R1,TL_CLIBA         Search backups approvers for match           
EDTGRY12 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    EDTGRY16                                                         
         CLC   CCTPID,0(R1)        Is user a back up client approver?           
         JE    EDTGRY14            Yes                                          
         LA    R1,L'PIDNO(R1)      Check other entries                          
         JCT   R0,EDTGRY12                                                      
         J     EDTGRY16                                                         
*                                                                               
EDTGRY14 MVI   0(R4),C'N'          Set No to grey out line                      
EDTGRY16 LHI   R3,1                Set length as 1                              
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit last row used for materials                                    *         
***********************************************************************         
                                                                                
EDTLITM  LM    R2,R4,LP_AINP                                                    
         USING TIMELD,R2                                                        
         XC    TL_MROW,TL_MROW                                                  
         L     R3,AELEAREA         BUFFER OF HIGHEST MAT NUM FOR EACH           
EDTLITM2 CLI   0(R3),X'FF'                              ROW                     
         JE    EDTLITM6            NO MATS FOR ROW                              
         CLC   TIMEIDNO,0(R3)                                                   
         JE    EDTLITM4                                                         
         LA    R3,L'TIMEIDNO+L'STCTMROW(R3)                                     
         J     EDTLITM2                                                         
*                                                                               
EDTLITM4 DS    0H                  MATCHING BUFFER ENTRY                        
         MVC   TL_MROW,L'TIMEIDNO(R3)                                           
         DROP  R4                                                               
*                                                                               
EDTLITM6 CURED (B2,TL_MROW),(4,0(R4)),0,ZERO=YES,ALIGN=LEFT                     
         XC    TL_MROW,TL_MROW                                                  
         LHI   R3,4                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit time approval status                                           *         
***********************************************************************         
                                                                                
EDTAPST  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'N'                                                       
         TM    0(R2),TIMESAPR                                                   
         JZ    EDTAPST2                                                         
         MVI   0(R4),C'Y'                                                       
         J     EDTAPST4                                                         
                                                                                
EDTAPST2 TM    0(R2),TIMESREJ                                                   
         JZ    EDTAPST4                                                         
         MVI   0(R4),C'R'                                                       
                                                                                
EDTAPST4 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit item description                                               *         
***********************************************************************         
                                                                                
EDTITMD  LM    R2,R4,LP_AINP                                                    
         XC    LP_OLEN,LP_OLEN     Set zero output                              
         MVI   TL_IFLAG,C' '                                                    
         MVC   TL_IUNIT,SPACES                                                  
         OC    0(L'TIMISEQ,R2),0(R2)  Have we got a sequence number             
         JZ    EXITY               No                                           
ITM      USING PASRECD,IOKEY                                                    
         XC    ITM.PASKEY,ITM.PASKEY                                            
         MVI   ITM.PASKTYP,PASKTYPQ                                             
         MVI   ITM.PASKSUB,PASKSQ                                               
         MVC   ITM.PASKCPY,CUXCPY                                               
         MVC   ITM.PASKSEQ,0(R2)                                                
         MVC   CSVKEY3,ITM.PASKEY                                               
         MVI   TL_IFLAG,C'F'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    EITMD02                                                          
         DC    H'0'                                                             
EITMD02  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    EITMD04                                                          
         DC    H'0'                                                             
         DROP  ITM                                                              
EITMD04  L     R2,AIO3                                                          
         USING ARTRECD,R2                                                       
         TM    ARTRSTA,ARTKNOPQ                                                 
         JZ    *+8                                                              
         MVI   TL_IFLAG,C'D'                                                    
         TM    ARTRSTA,ARTKFLXQ                                                 
         JZ    *+8                                                              
         MVI   TL_IFLAG,C'V'                                                    
         LA    R2,ARTRFST                                                       
         USING NAMELD,R2                                                        
         SR    R0,R0                                                            
EITMD06  CLI   NAMEL,0                                                          
         JE    EITMD14                                                          
         CLI   NAMEL,NAMELQ                                                     
         JE    EITMD10                                                          
         CLI   NAMEL,AFDELQ                                                     
         JE    EITMD12                                                          
EITMD08  IC    R0,NAMLN                                                         
         AR    R2,R0                                                            
         J     EITMD06                                                          
*                                                                               
EITMD10  SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         JM    EITMD08                                                          
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,R4),NAMEREC                                                  
         AHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EITMD08                                                          
*                                                                               
         USING AFDELD,R2                                                        
EITMD12  CLI   AFDLN,AFDLNQ                                                     
         JNH   EITMD08                                                          
         MVC   TL_IUNIT,AFDUNIT                                                 
         J     EITMD08                                                          
*                                                                               
EITMD14  J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit materials price override                                       *         
***********************************************************************         
                                                                                
EDTOVR   LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'N'                                                       
         TM    0(R2),TIMISPOV                                                   
         JZ    EDTOVR02                                                         
         MVI   0(R4),C'Y'                                                       
                                                                                
EDTOVR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit draft account                                                  *         
***********************************************************************         
                                                                                
EDTDRFT  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'N'          No                                           
         TM    0(R2),ACTSDRFT      Is account draft                             
         JZ    *+8                                                              
         MVI   0(R4),C'Y'          Yes                                          
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit account locked                                                 *         
***********************************************************************         
                                                                                
EDTRSTL  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTSACIL      Is account locked                            
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'          Yes                                          
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit account closed                                                 *         
***********************************************************************         
                                                                                
EDTRSTC  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTSACIC      Is account closed                            
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'          Yes                                          
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit balance sheet account                                          *         
***********************************************************************         
                                                                                
EDTBSHE  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTSLABS                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit profit and loss account                                        *         
***********************************************************************         
                                                                                
EDTPNL   LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTSLAPL                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit locked from billing                                            *         
***********************************************************************         
                                                                                
EDTLBIL  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTLSBIQ                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit future time allowed                                            *         
***********************************************************************         
                                                                                
EDT1NFT  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),RSTSFUTM                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* Edit foreign language user                                          *         
***********************************************************************         
                                                                                
EDTASTFL LM    R2,R4,LP_AINP                                                    
         TM    0(R2),ASTISFOR                                                   
         JZ    EXITY                                                            
         MVI   0(R4),C'Y'                                                       
         LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit whether this is a real timeline or not                         *         
***********************************************************************         
                                                                                
EDTREAL  L     R4,LP_AOUT                                                       
         MVI   0(R4),YESQ                                                       
         TM    DINDS2,DIIPREV                                                   
         JZ    EXITY                                                            
         MVI   0(R4),NOQ                                                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Get approver and back-up approver PID for time                      *         
***********************************************************************         
                                                                                
GETMAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETMAP*'                                                      
                                                                                
         LR    R2,R1                                                            
P        USING ACTKACT,R2                                                       
         XC    TH_MANAP,TH_MANAP                                                
         XC    TH_MANBA,TH_MANBA                                                
                                                                                
MN       USING DPAPASD,IOKEY                                                    
         XC    MN.DPAPAS,MN.DPAPAS                                              
         MVI   MN.DPAPTYP,DPAPTYPQ                                              
         MVI   MN.DPAPSUB,DPAPSUBQ                                              
         MVI   MN.DPAPAPPL,DPAPATIM                                             
         MVC   MN.DPAPCPY,CUXCPY                                                
         ZAP   MN.DPAPXVAL,PZERO                                                
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETMAP02 MVC   MN.DPAP1RAC,SPACES                                               
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   MN.DPAP1RAC(0),P.ACTKACT                                         
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GETMAP06                                                         
         DC    H'0'                                                             
                                                                                
GETMAP04 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GETMAP06                                                         
         DC    H'0'                                                             
                                                                                
GETMAP06 CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JNE   GETMAP08                                                         
         MVC   TH_MANAP,MN.DPAPPIDB                                             
         J     GETMAP10                                                         
                                                                                
GETMAP08 MVC   MN.DPAPAS,IOKEYSAV  Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETMAP02         Do for number of 1R levels                   
         J     GETMAPN                                                          
         DROP  MN,P                                                             
                                                                                
GETMAP10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETMAP20 CLI   LIDEL,0             Test end of record                           
         JE    EXITY                                                            
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETMAP24                                                         
GETMAP22 IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GETMAP20                                                         
                                                                                
GETMAP24 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETMAP22                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
         LA    RF,TH_MANBA                                                      
         XR    RE,RE                                                            
                                                                                
GETMAP26 TM    LIDLAPPL,LIDLTIME   Is this entry for timesheets                 
         JZ    GETMAP28            No                                           
         MVC   0(L'PIDNO,RF),LIDLPID  Yes - Get back up approver PID            
         LA    RF,L'PIDNO(RF)                                                   
         AHI   RE,1                                                             
GETMAP28 CHI   RE,TH_BAMAX         Did we fill up the whole table?              
         JE    EXITY                                                            
                                                                                
         LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETMAP26            No - check next entry                        
         STCM  RE,3,TH_NUMBA       Yes - store number of back ups               
         J     EXITY                                                            
                                                                                
GETMAPN  MVC   LP_ERROR,=AL2(AE$INAPP)                                          
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Locate an element in record pointed to by IOADDR                    *         
*                                                                     *         
* Ntry:- R1=Element code                                              *         
* Exit:- R1=A(Element) and CC=Equal if element found                  *         
*        R1=0 and CC=Not equal if element not found                   *         
***********************************************************************         
                                                                                
GETELA   LR    RF,R1               RF=Element code to search for                
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
GETELA02 CLI   0(R1),0             Test end of record                           
         JNE   *+10                                                             
         SR    R1,R1               Yes - clear pointer address                  
         CR    RE,R1               Set CC to not equal                          
         BR    RE                                                               
         CLM   RF,1,0(R1)          Test correct element                         
         BER   RE                  Yes - exit with CC equal                     
         IC    R0,1(R1)            No - bump to next element on record          
         AR    R1,R0                                                            
         J     GETELA02                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE COSTING PID                                                *         
* P1 - BINARY PID                                                     *         
***********************************************************************         
         SPACE 1                                                                
CSTPID   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CSTPID*'                                                      
         L     R3,0(R1)                                                         
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,0(R3)                                                    
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   CSVKEY1,IOKEY                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    CSTPID02                                                         
         DC    H'0'                                                             
                                                                                
CSTPID02 CLC   CSVKEY1(PIDKPER-PIDRECD),IOKEY                                   
         JNE   EXITN                                                            
                                                                                
CSTPID04 MVC   QPERSN,PIDKPER                                                   
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PERSON DETAIL AS ACCORDING TO DATE PASSED                           *         
* P1 BYTE 1   'FF' DATE WAS CALCULATED                                *         
* P1 BYTE 2-4 A(DATE)                                                 *         
***********************************************************************         
         SPACE 1                                                                
PRSDTL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*PERDTL*'                                                    
         L     R3,0(R1)                                                         
         MVC   DDATE,0(R3)                                                      
         MVC   BYTE1,0(R1)                                                      
         XC    WRK2DTE,WRK2DTE                                                  
         XC    BYTE2,BYTE2                                                      
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,QPERSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PRSDTL02                                                         
         MVC   LP_ERROR,=AL2(AE$IVPER)                                          
         J     EXITN                                                            
                                                                                
PRSDTL02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    PRSDTL04                                                         
         DC    H'0'                Fatal error                                  
                                                                                
PRSDTL04 L     R2,AIO1             R2=A(IO area 1)                              
         LA    R2,PERRFST                                                       
         USING EMPELD,R2                                                        
         XC    ALOCEL,ALOCEL                                                    
         XR    R0,R0                                                            
                                                                                
PRSDTL06 CLI   EMPEL,0                                                          
         JE    PRSDTL80                                                         
         CLI   EMPEL,EMPELQ                                                     
         JE    PRSDTL40                                                         
         CLI   EMPEL,LOCELQ                                                     
         JE    PRSDTL60                                                         
         CLI   EMPEL,PIDELQ                                                     
         JE    PRSDTL70                                                         
                                                                                
PRSDTL08 IC    R0,EMPLN                                                         
         AR    R2,R0                                                            
         J     PRSDTL06                                                         
                                                                                
PRSDTL40 CLC   EMPHIR,EMPTRM                                                    
         JNE   PRSDTL42                                                         
         MVC   LP_ERROR,=AL2(AE$TDBHD)                                          
         J     EXITN                                                            
PRSDTL42 MVC   TI_HIRE,EMPHIR                                                   
         MVC   TI_TERM,EMPTRM                                                   
         CLC   DDATE,EMPHIR        VALIDATE EMPEL                               
         JNL   PRSDTL45                                                         
         CLI   BYTE1,0             WAS START DATE CALCULATED?                   
         JE    PRSDTL44                                                         
         MVC   DDATE,EMPHIR                                                     
         J     PRSDTL45                                                         
PRSDTL44 MVC   LP_ERROR,=AL2(AE$OHIRE)                                          
         J     EXITN                                                            
PRSDTL45 OC    EMPTRM,EMPTRM                                                    
         JZ    PRSDTL50                                                         
         CLC   DDATE,EMPTRM      VALIDATE EMPEL                                 
         JNH   PRSDTL50                                                         
         MVC   LP_ERROR,=AL2(AE$OHIRE)                                          
         J     EXITN                                                            
PRSDTL50 DS    0H                                                               
         J     PRSDTL08                                                         
                                                                                
         USING LOCELD,R2                                                        
         USING OFFALD,R1                                                        
PRSDTL60 CLI   BYTE1,0             Was start date calculated?                   
         JE    PRSDTL64            No                                           
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,LOCOFF     move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   PRSDTL08            not a valid location for this logon          
         OC    WRK2DTE,WRK2DTE     have we previous date                        
         JZ    PRSDTL62            no - save the locel                          
         OC    LOCEND,LOCEND       Most recent location?                        
         JZ    PRSDTL63            Yes                                          
         CLC   WRK2DTE,LOCEND      yes - check if location is newer             
         JNL   PRSDTL08            no                                           
PRSDTL62 MVC   WRK2DTE,LOCEND      yes - save end date and locel                
PRSDTL63 ST    R2,ALOCEL                                                        
PRSDTL64 CLC   DDATE,LOCSTART      find location for current date               
         JL    PRSDTL08                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    PRSDTL68                                                         
         CLC   DDATE,LOCEND                                                     
         JH    PRSDTL08                                                         
PRSDTL68 ST    R2,ALOCEL                                                        
         MVI   BYTE2,1                                                          
         J     PRSDTL08                                                         
                                                                                
         USING PIDELD,R2                                                        
PRSDTL70 MVC   DPPID,PIDNO         EXTRACT PID                                  
         J     PRSDTL08                                                         
                                                                                
PRSDTL80 OC    ALOCEL,ALOCEL       ENSURE THIS IS SET                           
         JNZ   PRSDTL85                                                         
         MVC   LP_ERROR,=AL2(AE$IVLDT)                                          
         J     EXITN                                                            
                                                                                
PRSDTL85 CLI   BYTE2,1                                                          
         JE    PRSDTL90                                                         
         OC    WRK2DTE,WRK2DTE     HAVE WE GOT ANY DATE?                        
         JZ    PRSDTL90                                                         
         MVC   DDATE,WRK2DTE                                                    
PRSDTL90 OC    DPPID,DPPID                                                      
         JNZ   PRSDTL95                                                         
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         J     EXITN                                                            
                                                                                
PRSDTL95 J     EXITY                                                            
                                                                                
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK MCS TIMESHEET USER                                            *         
* ON NTRY PARM1 BYTE 1-3 ADDRESS OF 1R ACCOUNT                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CHKMCS   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKMCS*'                                                    
         LA    R4,BRATAB                                                        
         XC    0(BRATABX-BRATAB,R4),0(R4)                                       
         MVI   TH_PROR,C'N'                                                     
         MVI   TH_JOBR,C'N'                                                     
         MVI   TH_MCSU,C'N'                                                     
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         GOTOR (#GETACN,AGETACN)                                                
         GOTOR CHKE5EL                                                          
         LA    R2,ONERL1L                                                       
         LA    R3,4                                                             
CKMCS02  XR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SHI   RE,1                                                             
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),D1RACT                              
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         GOTOR CHKE5EL                                                          
         AHI   R2,1                                                             
         BCT   R3,CKMCS02                                                       
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK E5 ELEMENT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CHKE5EL  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKE5L*'                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R3,ACTRFST                                                       
         USING GDAELD,R3                                                        
         SR    R0,R0                                                            
CKE5L002 CLI   GDAEL,0                                                          
         BE    CKE5L030                                                         
         CLI   GDAEL,GDAELQ                                                     
         BE    CKE5L006                                                         
         OC    QENDD,QENDD                                                      
         BZ    CKE5L004                                                         
         CLI   GDAEL,RSTELQ                                                     
         BE    CKE5L020                                                         
CKE5L004 IC    R0,GDALN                                                         
         AR    R3,R0                                                            
         B     CKE5L002                                                         
*                                                                               
CKE5L006 CLI   GDATYPE,GDATMCST                                                 
         BNE   CKE5L004                                                         
         OC    QENDD,QENDD         ANY END DATE                                 
         BZ    CKE5L012                                                         
         CLC   QENDD,GDADATE       CHECK SWITCH ON BEFORE END DATE              
         BL    CKE5L004                                                         
         OC    GDADATE2,GDADATE2   ANY SWITCH OFF DATE?                         
         BZ    CKE5L008                                                         
         CLC   QENDD,GDADATE2      CHECK SWITCH OFF IS AFTER END DATE           
         BNL   CKE5L010                                                         
CKE5L008 MVI   TH_MCSU,C'Y'                                                     
         B     CKE5L012                                                         
*                                                                               
CKE5L010 MVI   TH_MCSU,C'N'                                                     
*                                                                               
CKE5L012 LA    R5,BRATAB           FILL OUT BRATAB                              
         LHI   R0,MAXBRAQ                                                       
CKE5L014 OC    0(L'GDADATE2,R5),0(R5)                                           
         JZ    CKE5L016                                                         
         CLC   0(L'GDADATE,R5),GDADATE                                          
         JE    CKE5L016                                                         
         LA    R5,L'GDADATE2+L'GDADATE(R5)                                      
         J     CKE5L014                                                         
*                                                                               
CKE5L016 MVC   0(L'GDADATE,R5),GDADATE                                          
         MVC   L'GDADATE(L'GDADATE2,R5),GDADATE2                                
         J     CKE5L004                                                         
*                                                                               
         USING RSTELD,R3                                                        
CKE5L020 CLI   RSTLN,RSTLN2Q                                                    
         JNH   CKE5L004                                                         
         CLI   TH_PROR,C'Y'                                                     
         JE    CKE5L022                                                         
         TM    RSTSTAT5,RSTSPROD                                                
         JZ    *+8                                                              
         MVI   TH_PROR,C'Y'                                                     
CKE5L022 CLI   TH_JOBR,C'Y'                                                     
         JE    CKE5L024                                                         
         TM    RSTSTAT5,RSTSPRJB                                                
         JZ    *+8                                                              
         MVI   TH_JOBR,C'Y'                                                     
CKE5L024 CLC   RSTDFTSK,SPACES     DON'T OVERWRITE IF LOWER LEVEL BLANK         
         JNH   CKE5L004                                                         
         MVC   TH_DEFWC,RSTDFTSK                                                
         J     CKE5L004                                                         
*                                                                               
CKE5L030 XIT1  ,                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Check If Person is Locked                                           *         
*       1R Account is in AIO3 from CHKMCS call                        *         
***********************************************************************         
         SPACE 1                                                                
CHKPLK   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*CHKPLK*'                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R3,ACTRFST                                                       
         USING RSTELD,R3                                                        
         SR    R0,R0                                                            
CKPLK002 CLI   RSTEL,0                                                          
         JE    CKPLKX                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    CKPLK006                                                         
CKPLK004 IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     CKPLK002                                                         
*                                                                               
CKPLK006 TM    RSTSTAT7,RSTLCKTS                                                
         JO    EXITN                                                            
*                                                                               
CKPLKX   J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Get current's system URL (hard coded)                               *         
***********************************************************************         
         SPACE  1                                                               
                                                                                
GETURL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GETURL*'                                                      
         LR    R2,R1                                                            
*                                                                               
         MVC   0(L'OGIURL,R2),SPACES                                            
*                                                                               
         LA    R3,AGYURL           PULL URL FROM TABLE                          
         USING AGYURLD,R3                                                       
GETURL02 CLI   AGUSE,0             LIVE OR TEST                                 
         JE    GETURL04            LIVE                                         
         CLC   AGUSE,LP_SENO       TEST - CHECK SE NUMBER                       
         JE    GETURL08                                                         
         J     GETURL06                                                         
                                                                                
GETURL04 CLI   AGUAA,0             END OF TABLE, USE DEFAULT                    
         JE    GETURL08                                                         
         CLC   AGUAA,CUAALF        MATCH ON AGENCY ALPHA                        
         JE    GETURL08                                                         
                                                                                
GETURL06 AHI   R3,AGYURLQ          NO MATCH, NEXT ENTRY                         
         J     GETURL02                                                         
                                                                                
GETURL08 DS    0H                                                               
                                                                                
         LA    RF,AGUHTT                                                        
         LHI   RE,L'AGUHTT-1                                                    
         CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   GETURL10                                                         
         TM    CPXSTATA,CPXFEDAT   Is federated auth in use?                    
         JZ    GETURL10                                                         
         LA    RF,HTTP             Always http for federated urls               
         LHI   RE,L'HTTP-1                                                      
*                                                                               
GETURL10 BASR  R1,0                                                             
         MVC   0(0,R2),0(RF)                                                    
         EX    RE,0(R1)                                                         
         AHI   R2,L'AGUHTT                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         TM    CPXSTATA,CPXFEDAT   Is federated auth in use?                    
         JNZ   GETURL12                                                         
         CLC   AGUENV,SPACES       Any environment?                             
         JNH   GETURL12                                                         
         MVC   0(L'AGUENV,R2),AGUENV                                            
         AHI   R2,L'AGUENV                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
*                                                                               
GETURL12 CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   GETURL14                                                         
         TM    CPXSTATA,CPXFEDAT                                                
         JZ    GETURL14            Is federated auth in use?                    
         MVC   0(L'AGUFEDP,R2),AGUFEDP                                          
         AHI   R2,L'AGUFEDP                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'FEDURL,R2),FEDURL                                            
         AHI   R2,L'FEDURL                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'AGUFEDS,R2),AGUFEDS                                          
         AHI   R2,L'AGUFEDS                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
GETURL14 MVC   0(L'AGUURL2,R2),AGUURL2  Aura                                    
         J     EXITY                                                            
         DROP  R3                                                               
***********************************************************************         
* Get agency name                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,IOKEY                                                    
GETAGN   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GETAGN*'                                                      
         LR    R2,R1                                                            
         MVC   0(L'OGINAME,R2),SPACES                                           
         L     R3,AIO8                                                          
         LA    R3,CPYRFST-CPYRECD(R3)                                           
*                                                                               
         USING NAMELD,R3                                                        
GETAGN02 CLI   NAMEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                MUST HAVE NAMELD                             
         CLI   NAMEL,NAMELQ                                                     
         JE    GETAGN04                                                         
         LLC   R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     GETAGN02                                                         
*                                                                               
GETAGN04 LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         BASR  R1,0                                                             
         MVC   0(0,R2),NAMEREC                                                  
         EX    RF,0(R1)                                                         
         J     EXITY                                                            
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
                                                                                
***********************************************************************         
* Set limit access values from Company extra element                  *         
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
                                                                                
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
***********************************************************************         
*   Key filter routine for Non Client record                          *         
***********************************************************************         
         SPACE 1                                                                
NCLKF    NTR1                                                                   
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         CLC   ACTKACT,SPACES                                                   
         JNH   EXITN                                                            
         TM    ACTKSTAT,ACTSCLOS                                                
         JNZ   EXITN                                                            
         TM    ACTKSTAT,ACTSLOCK                                                
         JNZ   EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
*   Key filter routine for Non Client record                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R2                                                        
NCLRF    NTR1                                                                   
         L     R2,AIO2                                                          
         AHI   R2,ACTRFST-ACTRECD                                               
*                                                                               
NCLR02   CLI   RSTEL,0                                                          
         JE    EXITY                                                            
         CLI   RSTEL,RSTELQ                                                     
         JE    NCLR06                                                           
NCLR04   LLC   R0,RSTLN                                                         
         AR    R2,R0                                                            
         J     NCLR02                                                           
*                                                                               
NCLR06   CLI   RSTLN,RSTLN3Q       Is element long enough?                      
         JL    NCLR04                                                           
         TM    RSTLSTAT,RSTLSTIQ   Test whether locked from t/s                 
         JNZ   EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
*   Key filter routine for Client record                              *         
***********************************************************************         
         SPACE 1                                                                
CLIKF    NTR1                                                                   
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         CLC   ACTKACT,SPACES                                                   
         JNH   EXITN                                                            
         LLC   RF,PCLILEN                                                       
         LA    RE,ACTKACT                                                       
         AR    RE,RF                                                            
         CLI   0(RE),X'40'                                                      
         JH    EXITN                                                            
         TM    ACTKSTAT,ACTSCLOS                                                
         JNZ   EXITN                                                            
         TM    ACTKSTAT,ACTSLOCK                                                
         JNZ   EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* Record filter routine for Client record                             *         
***********************************************************************         
         SPACE 1                                                                
CLIRF    NTR1                                                                   
         L     R1,AIO2                                                          
         USING ACTRECD,R1                                                       
         LA    R2,ACTRFST                                                       
         LR    R4,R2                                                            
*                                                                               
         USING RSTELD,R2                                                        
CLIRF02  CLI   RSTEL,0                                                          
         JE    EXITY                                                            
         CLI   RSTEL,PPRELQ                                                     
         JE    CLIRF10                                                          
         CLI   RSTEL,RSTELQ                                                     
         JE    CLIRF08                                                          
CLIRF06  LLC   R0,RSTLN                                                         
         AR    R2,R0                                                            
         J     CLIRF02                                                          
*                                                                               
CLIRF08  CLI   RSTLN,RSTLN3Q      Make sure element is big enough               
         JL    CLIRF06                                                          
         TM    RSTSTAT5,RSTSNOTS  Skip                                          
         JNZ   EXITN                                                            
         J     CLIRF06                                                          
*                                                                               
         USING PPRELD,R2                                                        
CLIRF10  CLC   PPRGAOFF,SPACES                                                  
         JNH   CLIRF06                                                          
         USING LW_D,R3                                                          
         XR    R3,R3               Point to list in WMP                         
         ICM   R3,7,DAOFF                                                       
         JZ    CLIRF18                                                          
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        Number of entries                            
         JZ    CLIRF06                                                          
         TM    X#LLIND,X#CLIOF     Are we in filter clients by office           
         JNZ   CLIRF14                                      mode                
         LA    R3,LW_DATA2         Start of list                                
CLIRF12  CLC   PPRGAOFF,0(R3)                                                   
         JE    CLIRF18                                                          
         AHI   R3,L'PPRGAOFF                                                    
         JCT   R0,CLIRF12                                                       
         J     EXITN                                                            
*                                                                               
CLIRF14  LA    R3,LW_DATA2         Start of list                                
CLIRF16  CLC   PPRGAOFF,0(R3)                                                   
         JE    EXITN                                                            
         AHI   R3,L'PPRGAOFF                                                    
         JCT   R0,CLIRF16                                                       
                                                                                
         USING OFFALD,R1                                                        
CLIRF18  L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,PPRGAOFF   validate current office                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    CLIRF06                                                          
         DROP  R1                                                               
                                                                                
         USING LIDELD,R4                                                        
CLIRF20  CLI   LIDEL,0                                                          
         JE    EXITN               No valid office code found                   
         CLI   LIDEL,LIDELQ                                                     
         JNE   CLIRF22                                                          
         CLI   LIDTYPE,LIDTPOFC      Product office codes                       
         JE    CLIRF24                                                          
CLIRF22  LLC   RF,LIDLN                                                         
         AR    R4,RF                                                            
         J     CLIRF20                                                          
CLIRF24  XR    R0,R0                                                            
         LA    RF,LIDDATA-LIDELD                                                
         LLC   R1,LIDLN                                                         
         SR    R1,RF               R1=Length of LIDDATA                         
         LLC   RF,LIDITLN                                                       
         DR    R0,RF               R1=No. of office codes in LIDDATA            
         STC   R1,BYTE3                                                         
         LR    R3,R1                                                            
         LA    R4,LIDDATA                                                       
         USING OFFALD,R1                                                        
CLIRF26  L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(R4)                                                   
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    CLIRF06                                                          
         LA    R4,L'TRNOFFC(R4)                                                 
         JCT   R3,CLIRF26                                                       
         J     EXITN                                                            
         DROP  R1,R2,R4                                                         
***********************************************************************         
* Key filter routine for SCMRECD                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCMRECD,R2                                                       
SCMKF    NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         TM    SCMKSTAT,SCMKSLCK   exlude locked                                
         JNZ   EXITN                                                            
*&&US*&& TM    SCMKSTAT,SCMKSABO   Is this available to BrandO?                 
*&&US*&& JNO   EXITN               SKIP                                         
                                                                                
         TM    SCMKSTAT,SCMKSCLP   Skip if no office set                        
         JNZ   SCMKF02                                                          
                                                                                
         CLC   SCMKSOFF,SPACES     Office check                                 
         JNH   SCMKF02                                                          
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,SCMKSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   EXITN                                                            
         DROP  R1                                                               
                                                                                
SCMKF02  DS    0H                                                               
*&&UK                                                                           
         TM    SCMKSAPP,SCMKSTIQ                                                
         JZ    EXITN                                                            
*&&                                                                             
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Record filter routine for SCMRECD ***                                       
                                                                                
         USING SCMRECD,R1                                                       
         USING COIELD,R2                                                        
SCMRF    NTR1                                                                   
         LA    R2,SCMRFST                                                       
         XC    ELEMENT,ELEMENT                                                  
         XR    R0,R0                                                            
                                                                                
SCMRF02  CLI   COIEL,0                                                          
         JE    SCMRF06                                                          
         CLI   COIEL,COIELQ                                                     
         JE    SCMRF04                                                          
         IC    R0,COILN                                                         
         AR    R2,R0                                                            
         J     SCMRF02                                                          
                                                                                
SCMRF04  XR    RE,RE                                                            
         IC    RE,COILN                                                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   ELEMENT(0),COIEL                                                 
                                                                                
SCMRF06  LA    R2,ELEMENT                                                       
                                                                                
         TM    COISTAT,COISLOCK    (lock filter again)                          
         JNZ   EXITN                                                            
                                                                                
SCMRF16  DS    0H                                                               
         J     EXITY                                                            
         DROP  R1,R2                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* Get limit list data                                                 *         
* Exit: ELEMENT (LIDTMED,LIDTEXPD,LIDTWKCD)                           *         
***********************************************************************         
         SPACE 1                                                                
GETLMT   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETLMT*'                                                      
*                                                                               
         XR    R6,R6                                                            
         IC    R6,PCLILEN                                                       
         SHI   R6,1                                                             
         MVC   TEMP(L'ACTKACT),SPACES                                           
         OC    CCTPID,CCTPID                                                    
         JZ    GETLMTN                                                          
*                                                                               
         USING LLSRECD,R2          Read list PID record for this person         
         LA    R2,IOKEY                                                         
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY3,LLSKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEY(LLSKGRP-LLSRECD),CSVKEY3                                   
         JNE   GETLMTN                                                          
         MVI   LL_DACS,LL_ALLQ     Set default access                           
         MVC   LL_DACS+L'LL_DACS(LL_DACLQ-L'LL_DACS),LL_DACS                    
         OI    X#LLIND,X#LLINI                                                  
         J     GETLMT04                                                         
GETLMT02 LA    R2,IOKEY                                                         
         CLC   IOKEY(LLSKGRP-LLSRECD),CSVKEY3                                   
         JE    GETLMT03                                                         
         MVC   CSVKEY3,LLSKEY          Update Save key as well                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
GETLMT03 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
                                                                                
         LA    R1,LLSKGRP-LLSRECD  Assume its a limlist record                  
         CLI   LLSKSUB,LLSKSUBQ                                                 
         JE    *+8                                                              
         LA    R1,GLSKSEQ-GLSRECD  No - Its a group record                      
         AHI   R1,-1                                                            
         BASR  R3,0                                                             
         EX    R1,8(R3)                                                         
         JNE   GETLMTY                                                          
         CLC   IOKEY(0),CSVKEY3                                                 
                                                                                
GETLMT04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
*                                                                               
         L     R2,AIO1                                                          
         MVC   IOKEY(L'LLSKEY),0(R2)   update IOKEY                             
         LA    R2,LLSRFST                                                       
         USING LIDELD,R2                                                        
*                                                                               
GETLMT10 CLI   LIDEL,0                                                          
         JE    GETLMT02                                                         
         CLI   LIDEL,RSTELQ                                                     
         JE    GETLMT56                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   GETLMT12                                                         
         CLI   LIDTYPE,LIDTCPJL                                                 
         JE    GETLMT14                                                         
         CLI   LIDTYPE,LIDTNCLL                                                 
         JE    GETLMT14                                                         
*                                                                               
GETLMT12 XR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         AR    R2,RE                                                            
         J     GETLMT10            Get next                                     
*                                                                               
GETLMT14 DS    0H                                                               
*&&UK                                                                           
         CLI   LIDTYPE,LIDTCPJL                                                 
         JNE   *+8                                                              
         MVI   LL_DCLIA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTNCLL    non client code list                         
         JNE   *+8                                                              
         MVI   LL_DNCLA,LL_LISTQ                                                
*&&                                                                             
*                                                                               
GETLMT28 LA    R3,LIDDATA                                                       
MED      USING LIDDATA,R3                                                       
GETLMT30 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   GETLMT12                                                         
                                                                                
         TM    MED.LIDLAPPL,LIDLTIME                                            
         JZ    GETLMT54                                                         
                                                                                
GETLMT46 DS    0H                                                               
*&&US                                                                           
         CLI   LIDTYPE,LIDTCPJL                                                 
         JNE   *+8                                                              
         MVI   LL_DCLIA,LL_LISTQ                                                
         CLI   LIDTYPE,LIDTNCLL    non client code list                         
         JNE   *+8                                                              
         MVI   LL_DNCLA,LL_LISTQ                                                
*&&                                                                             
         DROP  MED                                                              
*                                                                               
         CLI   LIDTYPE,LIDTNCLL    non client code list                         
         JNE   GETLMT50                                                         
NC       USING LIDDATA,R3                                                       
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,NC.LIDLACT),D1NAIND,D1NAMAXQ,  +        
               LP_D                                                             
         J     GETLMT54                                                         
         DROP  NC                                                               
                                                                                
SJ       USING LIDDATA,R3                                                       
GETLMT50 BASR  RF,0                                                             
         EX    R6,4(RF)                                                         
         MVC   TEMP(0),SJ.LIDLACT                                               
         CLI   TEMP,C' '                                                        
         JNH   GETLMT52                                                         
         GOTOR LP_AAWMP,DMCB,(L'ACTKACT,TEMP),DCLIIND,DCLIMAXQ,        +        
               LP_D                                                             
         J     GETLMT54                                                         
                                                                                
GETLMT52 GOTOR LP_AAWMP,DMCB,(L'LIDLOFF,SJ.LIDLOFF),DOFFIND,DOFFMAXQ,  +        
               LP_D                                                             
GETLMT54 LA    R3,LIDLLN1Q(R3)                                                  
         CLI   LIDTYPE,LIDTCPJL                                                 
         JNE   GETLMT30                                                         
         LA    R3,L'LIDLOFF(R3)                                                 
         J     GETLMT30                                                         
         DROP  SJ,R2                                                            
                                                                                
         USING RSTELD,R2                                                        
GETLMT56 CLI   RSTLN,RSTLN3Q                                                    
         JL    GETLMT12                                                         
         TM    RSTACST1,RSTAJOBS                                                
         JZ    *+8                                                              
         MVI   LL_DCLIA,LL_NONEQ                                                
         TM    RSTACST1,RSTA1NAC   Non-client limit list                        
         JZ    *+8                                                              
         MVI   LL_DNCLA,LL_NONEQ                                                
         J     GETLMT12                                                         
                                                                                
GETLMTY  J     EXITY                                                            
                                                                                
GETLMTN  J     EXITN                                                            
         EJECT                                                                  
         DROP  R2                                                               
***********************************************************************         
* GET CALENDAR                                                        *         
***********************************************************************         
         DS    0H                                                               
GTCAL    NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*GTCAL**'                                                    
*        OC    QENDD,QENDD         Anything in QEND?                            
*        JZ    GETCAL02                                                         
*        CLC   DCEND,QENDD          Only update if QEND is higher               
*        JNL   GETCAL02                                                         
*        MVC   DCEND,QENDD                                                      
                                                                                
         USING CASRECD,R2                                                       
GETCAL02 LA    R2,IOKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASPTYP,CASPTYPQ                                                 
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,CUXCPY                                                   
         MVC   CASPEDTE,DCEND                                                   
         MVC   CSVKEY1,CASKEY                                                   
                                                                                
         L     R1,=AL4(IOHI+IODIR+IO3)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETCAL06                                                         
         DC    H'0'                                                             
                                                                                
GETCAL05 L     R1,=AL4(IOSQD+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETCAL06                                                         
         DC    H'0'                                                             
                                                                                
GETCAL06 CLC   CSVKEY1(CASPEDTE-CASKEY),IOKEY                                   
         JE    GETCAL10                                                         
         MVC   LP_ERROR,=AL2(AE$NOCAL)                                          
         J     EXITN                                                            
                                                                                
GETCAL10 CLC   CASPSDTE,DCEND    Test starts after request end                  
         JH    GETCAL05                                                         
         CLC   CASPEDTE,DCEND    Test ends before request end                   
         JL    GETCAL05                                                         
                                                                                
         CLC   CASPSDTE,WRKDATE    Is WRKDATE valid for this calendar?          
         JNH   *+10                Yes - Continue                               
         MVC   WRKDATE,DCEND       No - Update with current date                
                                                                                
         MVC   CASPOFC,TH_1ROFF                                                 
                                                                                
         L     R1,=AL4(IORD+IODIR+IO3)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETCAL20                                                         
                                                                                
         MVC   CASKEY,CSVKEY1                                                   
                                                                                
GETCAL20 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    EXITY                                                            
         DC    H'0'                FATAL ERROR                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Calendar periods retrieval                                          *         
*   This routine is used to build a list of time periods using the    *         
*   company calendar                                                  *         
*   If you came from a routine looking for a particular person it     *         
*   will also add period location information for that person into    *         
*   the period table                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALPRDS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CALPRDS'                                                      
                                                                                
         USING PERTABD,R4                                                       
         L     R4,AIO5             clear first entry of output area             
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO5                                        
                                                                                
         XC    NUMPRDS,NUMPRDS                                                  
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
                                                                                
         MVC   DCDAT,DDATE                                                      
         MVC   DCDAT+1(1),BYTE1                                                 
                                                                                
         CLC   DDATE+1(1),BYTE1                                                 
         BNL   CALP002                                                          
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
                                                                                
CALP002  GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'10'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
                                                                                
CALP004  GOTOR GTCAL                                                            
         BNE   CALPRDSN                                                         
         L     R0,AIO1             copy calendar to IO1                         
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         MVI   BYTE1,0                                                          
         L     R2,AIO1                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
CALP006  CLI   TMPEL,0                                                          
         BE    CALP062                                                          
         CLI   TMPEL,TMPELQ                                                     
         BE    CALP010                                                          
         CLI   TMPEL,TMRELQ                                                     
         BNE   CALP008                                                          
         ST    R3,FULL1                                                         
                                                                                
CALP008  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         B     CALP006                                                          
                                                                                
CALP010  CLC   TMPSTART,DDATE      IS START DATE LOWER THAN PERIOD DATE         
         BNL   CALP012             NO - CHECK END DATE                          
         CLC   TMPEND,DDATE        IS START DATE LOWER THAN PERIOD END          
         BL    CALP008             NO - NOT INTERESTED                          
         B     CALP028                                                          
CALP012  CLC   TMPEND,QENDD        IS PERIOD END DATE LOWER THAN END            
         BNH   CALP028             YES                                          
         CLC   TMPSTART,QENDD      IS PERIOD START DATE HIGHER THAN END         
         BH    CALP008             YES - NOT INTERESTED                         
*                                                                               
CALP028  L     R1,AIO4             yes get details of person                    
         USING PERRECD,R1                                                       
         LA    R1,PERRFST                                                       
         USING LOCELD,R1                                                        
CALP030  CLI   LOCEL,0                                                          
         BE    CALP060                                                          
         CLI   LOCEL,LOCELQ                                                     
         BE    CALP034                                                          
CALP032  XR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R1,R0                                                            
         B     CALP030                                                          
                                                                                
CALP034  OC    LOCLOCK,LOCLOCK     HAVE WE GOT A TIMESHEET LOCK DATE            
         BZ    CALP038                                                          
         OC    LOCEND,LOCEND                                                    
         BZ    CALP036                                                          
         CLC   LOCLOCK,LOCEND                                                   
         BNL   CALP038                                                          
CALP036  CLC   TMPSTART,LOCLOCK    PERIOD AFTER LOCK DATE NO GOOD               
         BH    CALP032             GET NEXT ELEMENT                             
CALP038  OC    LOCEND,LOCEND       HAVE WE GOT A LOCATION END DATE              
         BZ    *+14                NO                                           
         CLC   TMPSTART,LOCEND     PERIOD START DATE AFTER END DATE             
         BH    CALP032             YES - NO GOOD GET NEXT ELEMENT               
         CLC   TMPSTART,LOCSTART   PERIOD START DATE BEFORE LOC START           
         BNL   CALP040             NO                                           
         CLC   LOCSTART,TMPEND     YES CHECK NOT LOA                            
         BH    CALP032                                                          
         MVC   PERLOCS,LOCSTART                                                 
                                                                                
CALP040  MVC   PERLOCE,=X'FFFFFF'                                               
         OC    LOCEND,LOCEND       LOCATION END DATE EXISTS                     
         BZ    CALP042             NO                                           
         CLC   TMPEND,LOCEND       PERIOD END DATE BEFORE LOCATION END          
         BNH   CALP042             YES                                          
         MVI   BYTE1,1             NO- MUST BE SPLIT LOCATION                   
         MVC   PERLOCE,LOCEND                                                   
*                                                                               
CALP042  LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    CPYSTAT4,CPYSOFF2                                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   PERODS(0),LOCOFF                                                 
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    R2,PERODS                                                        
         AR    R2,RE                                                            
                                                                                
         LR    R0,RE                                                            
         XR    RF,RF                                                            
         IC    RF,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,R2),LOCDEPT                                                  
         EX    RF,*-6                                                           
         LA    R2,1(RF,R2)                                                      
         IC    R0,ONERL2L                                                       
         IC    RF,ONERL3L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,R2),LOCSUB                                                   
         EX    RF,*-6                                                           
         OC    PERODS,SPACES                                                    
         DROP  R1                                                               
                                                                                
         LA    R5,BRATAB                                                        
CALP050  OC    0(L'GDADATE,R5),0(R5)                                            
         BZ    CALP058                                                          
         CLC   TMPEND,0(R5)                                                     
         BL    CALP056                                                          
         OC    L'GDADATE(L'GDADATE,R5),L'GDADATE(R5)                            
         BZ    CALP052                                                          
         CLC   TMPEND,L'GDADATE(R5)                                             
         BNL   CALP054                                                          
CALP052  OI    PERSTAT,PERSMCSU                                                 
         B     CALP056                                                          
*                                                                               
CALP054  NI    PERSTAT,X'FF'-PERSMCSU                                           
CALP056  LA    R5,L'GDADATE+L'GDADATE2(R5)                                      
         B     CALP050                                                          
*                                                                               
CALP058  MVC   PERNUM,TMPNUMB                                                   
         MVC   PERSTDT,TMPSTART                                                 
         XR    RF,RF                                                            
         ICM   RF,7,TMPEND                                                      
         LNR   RF,RF                                                            
         STCM  RF,7,PERENDT                                                     
         LA    R4,PERLENQ(R4)                                                   
         XR    RE,RE                                                            
         IC    RE,NUMPRDS                                                       
         AHI   RE,1                                                             
         STC   RE,NUMPRDS                                                       
*                                                                               
CALP060  MVI   0(R4),0                                                          
         B     CALP008                                                          
*                                                                               
CALP062  DS    0H                                                               
         L     R3,FULL1                                                         
         USING TMRELD,R3                                                        
         CLC   QENDD(2),TMREND                                                  
         BNH   CALPRDSY                                                         
         DROP  R3                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
         GOTO1 VDATCON,DMCB,(1,DCEND),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
         B     CALP004                                                          
*                                                                               
CALPRDSY J     EXITY                                                            
                                                                                
CALPRDSN J     EXITN                                                            
         DROP  R2,R4                                                            
***********************************************************************         
* Set period end date                                                 *         
*   This routine is used to set period end date from a timesheet date *         
***********************************************************************         
         SPACE 1                                                                
SETPEND  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SETPEND'                                                      
                                                                                
         L     R3,0(R1)                                                         
         MVC   DDATE,0(R3)         Timesheet Date                               
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
                                                                                
         MVC   DCDAT,DDATE                                                      
         MVC   DCDAT+1(1),BYTE1                                                 
                                                                                
         CLC   DDATE+1(1),BYTE1                                                 
         BNL   SETP002                                                          
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
                                                                                
SETP002  GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
*&&US*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'10'                             
*&&UK*&& GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCEND)                                
                                                                                
         GOTOR GTCAL                                                            
         BNE   SETPENDN                                                         
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
SETP004  CLI   TMPEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                CAN'T FIND PERIOD END DATE                   
         CLI   TMPEL,TMPELQ                                                     
         JE    *+14                                                             
SETP006  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     SETP004                                                          
                                                                                
         CLC   DDATE,TMPEND                                                     
         JH    SETP006                                                          
         CLC   DDATE,TMPSTART                                                   
         JL    SETP006                                                          
         MVC   QENDD,TMPEND       SET PERIOD END DATE                           
*                                                                               
SETPENDY J     EXITY                                                            
                                                                                
SETPENDN J     EXITN                                                            
         DROP  R2,R3                                                            
***********************************************************************         
* RETRIEVE COST PERIOD ENTRY FROM END DATE                            *         
***********************************************************************         
         SPACE 1                                                                
RETCPFD  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*RETCPFD'                                                      
                                                                                
         L     R2,AIO5                                                          
         USING PERTABD,R2                                                       
                                                                                
RETCPFD2 CLI   0(R2),0                                                          
         JE    EXITN                                                            
         XR    RE,RE                                                            
         ICM   RE,7,PERENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,WRKDATE                                                     
         CLC   QENDD,WRKDATE                                                    
         JE    RETCPFD4                                                         
         AHI   R2,PERLENQ                                                       
         J     RETCPFD2                                                         
                                                                                
RETCPFD4 MVC   TEMP2(PERLENQ),0(R2)                                             
         AHI   R2,PERLENQ                                                       
         ST    R2,APREVPER          set previous period for pta profile         
* If this period has edit hours recs, but 0 in period, go further back          
RETCPF15 DS    0H                                                               
         TM    PERSTAT,PERSMCSU     Brandocean user for this period             
         JZ    RETCPF35             No, stick with this entry                   
         XC    PREVDAYS(PREVDAYL),PREVDAYS  Init Area                           
         MVC   PREVDAYS,PERSTDT                                                 
         CLC   PERLOCS,PERSTDT                                                  
         JNH   *+10                                                             
         MVC   PREVDAYS,PERLOCS                                                 
         LA    R3,PREVDAYS+L'PREVDAYS                                           
         MVC   WORK(L'PREVDAYS),PREVDAYS                                        
         XR    R4,R4                                                            
         LA    R4,1(R4)                                                         
         SR    R6,R6                                                            
         ICM   R6,7,PERLOCE                                                     
         CLC   PERLOCE,=X'FFFFFF'                                               
         JNE   RETCPF20                                                         
         ICM   R6,7,PERENDT                                                     
         LNR   R6,R6                                                            
RETCPF20 GOTO1 VDATCON,DMCB,(1,WORK),(0,WORK+12)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK+12),WORK+6,F'1'                           
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
         CLM   R6,7,WORK       Have we reached end date                         
         JL    RETCPF30            Yes                                          
         MVC   0(L'PREVDAYS,R3),WORK                                            
         LA    R3,L'PREVDAYS(R3)                                                
         LA    R4,1(R4)                                                         
         J     RETCPF20                                                         
*                                                                               
RETCPF30 GOTOR CKHRS,PREVDAYS                                                   
         CLI   BYTE2,0                                                          
         JNE   RETCPF35             Edit hours not used                         
         CP    TH_RQHRS,PZERO       Does prev period have hours?                
         JH    RETCPF35             yep - finish                                
         AHI   R2,PERLENQ                                                       
         ST    R2,APREVPER                                                      
         TM    PERSTAT,PERSMCSU     Brandocean user for this period             
         JZ    RETCPF35             No, stick with this entry                   
         J     RETCPF15                                                         
RETCPF35 XC    TH_RQHRS,TH_RQHRS    Make sure this isn't used                   
         J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* GET EDIT HOURS AMOUNT FOR TIMESHEET PERIOD                          *         
* Entry: R1 = list of days to check                                   *         
* Returns: AMOUNT OF HOURS IN TH_RQHRS                                *         
*          byte2=0 if edit hours records found                        *         
***********************************************************************         
         SPACE 1                                                                
CKHRS    NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CKHRS**'                                                    
         LR    R2,R1                                                            
         ST    R2,FULL1         save for later                                  
DAYS     USING TH_DATE,R2                                                       
         ZAP   TH_RQHRS,PZERO                                                   
         MVI   BYTE2,EDTKSDAY   Set to read daily time                          
CKHRS00  OI    DINDS1,DIIOFF+DIIDPT+DIISDP+DIIPER                               
         USING EDTRECD,R4                                                       
         LA    R4,IOKEY                                                         
CKHRS02  MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,CUXCPY                                                   
         TM    DINDS1,DIIOFF                                                    
         BZ    *+14                                                             
         MVC   EDTKOFC,TH_1ROFF                                                 
         NI    DINDS1,X'FF'-DIIOFF                                              
         TM    DINDS1,DIIDPT                                                    
         BZ    *+18                                                             
         MVC   EDTKDPT,TH_DEPT                                                  
         NI    DINDS1,X'FF'-DIIDPT                                              
         OI    DINDS1,DIIOFF                                                    
         TM    DINDS1,DIISDP                                                    
         BZ    *+18                                                             
         MVC   EDTKSBD,TH_SDPT                                                  
         NI    DINDS1,X'FF'-DIISDP                                              
         OI    DINDS1,DIIDPT                                                    
         TM    DINDS1,DIIPER                                                    
         BZ    *+18                                                             
         MVC   EDTKPER,QPERSN                                                   
         NI    DINDS1,X'FF'-DIIPER                                              
         OI    DINDS1,DIISDP                                                    
         XC    EDTKSEQ,EDTKSEQ                                                  
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   DCDAT,DAYS.TH_DATE  was qendd                                    
         MVC   DCDAT+1(1),BYTE1                                                 
         CLC   DAYS.TH_DATE+1(1),BYTE1   request mth vs fiscal month            
         BNL   CKHRS04                 (edt key years are fiscal years)         
*                                                                               
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
*                                                                               
CKHRS04  MVC   EDTKYR,DCDAT                                                     
         MVC   EDTKKSTA,BYTE2      Set Type of Time                             
         MVC   CSVKEY2,EDTKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    CKHRS06                                                          
         MVC   IOKEY,CSVKEY2                                                    
         CLC   EDTKOFC,SPACES      have we read lowest level                    
         BE    CKHRS38             yes - exit                                   
         B     CKHRS02                                                          
*                                                                               
CKHRS06  CLC   EDTKSTDT,DAYS.TH_DATE                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   EDTKENDT,DAYS.TH_DATE If date earlier we need to look at         
         BNL   CKHRS07                     next year edit hour rec              
                                                                                
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'+1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
         B     CKHRS04                                                          
*                                                                               
CKHRS07  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   BYTE2,0             Set to record found                          
                                                                                
         CLI   EDTKKSTA,EDTKSDAY   Daily or period edit hours?                  
         JNE   CKHRS30             Period edit hours                            
         LA    R3,TOTDAYS                                                       
CKHRS08  CLC   0(L'TH_DATE,R2),SPACES                                           
         BNH   CKHRS38                                                          
         ZAP   DUB,PZERO                                                        
         GOTO1 VDATCON,DMCB,(1,0(R2)),(0,WORK)                                  
         GOTO1 VGETDAY,DMCB,WORK,DUB1                                           
         CLC   DUB1(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,DAYTAB                                                        
CKHRS10  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RF),0(R1)                                                    
         BE    CKHRS18                                                          
         LA    RF,DAYTABL(RF)                                                   
         B     CKHRS10                                                          
*                                                                               
CKHRS18  MVC   BYTE1,1(RF)                                                      
CKHRS20  L     R4,AIO3                                                          
         LA    R4,EDTRFST                                                       
         USING DEDELD,R4                                                        
CKHRS22  CLI   DEDEL,0                                                          
         BE    CKHRS28                                                          
         CLI   DEDEL,FFRELQ                                                     
         BNE   CKHRS23                                                          
         USING FFRELD,R4                                                        
         CLI   FFRTYPE,FFRTPCT     Is it a percentage?                          
         BNE   CKHRS26                                                          
         ZAP   TH_TUPCT,FFRPCT                                                  
         USING DEDELD,R4                                                        
CKHRS23  CLI   DEDEL,DEDELQ                                                     
         BNE   CKHRS26                                                          
         CLI   DEDLN,DEDLN1Q                                                    
         BNE   CKHRS24                                                          
         CLC   BYTE1,DEDIND                                                     
         BNE   CKHRS26                                                          
         ZAP   DUB,DEDHRS                                                       
         B     CKHRS26                                                          
*                                                                               
CKHRS24  CLC   DEDDATE,DAYS.TH_DATE                                             
         BNE   CKHRS26                                                          
         ZAP   DUB,DEDHRS                                                       
         B     CKHRS28                                                          
*                                                                               
CKHRS26  SR    R0,R0                                                            
         IC    R0,DEDLN                                                         
         AR    R4,R0                                                            
         B     CKHRS22                                                          
*                                                                               
CKHRS28  AP    TH_RQHRS,DUB                                                     
         LA    R2,L'TH_DATE(R2)                                                 
         BCT   R3,CKHRS08                                                       
         J     CKHRS38                                                          
* Period record                                                                 
CKHRS30  L     R4,AIO3                                                          
         USING EDTRECD,R4                                                       
         LA    R4,EDTRFST                                                       
CKHRS31  CLI   DAYS.TH_DATE+L'TH_DATE,0    Find end date in date block          
         JE    CKHRS32                                                          
         LA    R2,L'TH_DATE(R2)                                                 
         J     CKHRS31                                                          
CKHRS32  CLI   0(R4),0                                                          
         BE    CKHRS38                                                          
         CLI   0(R4),SHRELQ        STANDARD HOURS YEAR ELEMENT                  
         BE    CKHRS36                                                          
CKHRS34  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CKHRS32                                                          
*                                                                               
         USING SHRELD,R4                                                        
CKHRS36  CLC   DAYS.TH_DATE,SHRSTART      Find right bucket                     
         BL    CKHRS34                                                          
         CLC   DAYS.TH_DATE,SHREND                                              
         BH    CKHRS34                                                          
         ZAP   TH_RQHRS,SHRHOURS                                                
         B     CKHRS34                                                          
         DROP  R4                                                               
*                                                                               
CKHRS38  DS    0H                                                               
*&&US                                                                           
         OC    BYTE2,BYTE2         No - may need to read period                 
         BZ    CKHRS40             Already done so                              
         CLI   BYTE2,EDTKSPER      Did we already read periods?                 
         BE    CKHRS40             Yes - We are done.                           
         MVI   BYTE2,EDTKSPER      Set to read period                           
         B     CKHRS00                                                          
*&&                                                                             
                                                                                
CKHRS40  J     EXITY                                                            
                                                                                
DAYTAB   DS    0X                                                               
         DC    AL1(1,DEDIMON)                                                   
DAYTABL  EQU   *-DAYTAB                                                         
         DC    AL1(2,DEDITUE)                                                   
         DC    AL1(3,DEDIWED)                                                   
         DC    AL1(4,DEDITHU)                                                   
         DC    AL1(5,DEDIFRI)                                                   
         DC    AL1(6,DEDISAT)                                                   
         DC    AL1(7,DEDISUN)                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK HISTORY RECORDS EXIST                                         *         
***********************************************************************         
         SPACE 1                                                                
CHKHIST  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKHIS*'                                                    
         L     R3,ABLOCK                                                        
         USING X_RATED,R3          Look up cost rate                            
         XC    0(X_RATEDQ,R3),0(R3)                                             
         MVC   X_RATCTRY,CUCTRY                                                 
         MVC   X_RAT1RACT,D1RACT                                                
         MVC   X_RAT1ROFF,TH_1ROFF Person office                                
         MVC   X_RAT1RDPT,SPACES                                                
         MVC   X_RAT1RDPT,TH_DEPT                                               
         MVC   X_RAT1RSUB,SPACES                                                
         MVC   X_RAT1RSUB,TH_SDPT                                               
         MVC   X_RAT1RPER,QPERSN                                                
         MVC   X_RATCRDTE,QENDD                                                 
         OC    QLOCED,QLOCED       do we have location end date                 
         JZ    CHKH020                                                          
         CLC   QENDD,QLOCED        Compare period end with loc end date         
         JL    CHKH020             and take whichever is lower                  
         MVC   X_RATCRDTE,QLOCED                                                
CHKH020  MVC   X_RDATAMGR,VDATAMGR                                              
         MVC   X_RCOMFACS,ACOMFACS                                              
         MVC   X_RCASHVAL,VCASHVAL                                              
         MVC   X_RAT1CMPY,CUXCPY                                                
         OI    X_RATSTAT,X_RATSCOST  get cost rate only                         
                                                                                
         GOTOR VGETRTE,X_RATED                                                  
         TM    X_RATSTAT2,X_RATEFNDC   was cost rate found                      
         JNZ   EXITY                                                            
         TM    X_RATSTAT2,X_RATEHIST   was history record found                 
         JNZ   EXITY                                                            
         MVC   LP_ERROR,=AL2(AE$00558)                                          
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* GET AUDIT DETAILS                                                   *         
* ON NTRY PARM1 BYTE 1 TYPE OF QUERY                                  *         
*                   0/1=GET ALL ELS (TIMEL AND STCEL)                 *         
*                   2=GET APPROVER TIMELS ONLY                        *         
*         PARM1 BYTE 1-3 ADDRESS OF DATE                              *         
* ON EXIT ELEARAEA HOLD ARRAY OF HIGHEST MAT NUM BY ROW NUMBER        *         
***********************************************************************         
         SPACE 1                                                                
GETAUD   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETAUD*'                                                    
                                                                                
         L     R4,AELEAREA                                                      
         ST    R4,ANXTELAR                                                      
         MVI   0(R4),X'FF'                                                      
         MVC   BYTE1,0(R1)                                                      
         L     R2,0(R1)                                                         
         ICM   R1,7,0(R2)                                                       
         LNR   R1,R1                                                            
         STCM  R1,7,DDATE                                                       
         XC    TH_INDX,TH_INDX                                                  
AUD      USING AUDRECD,IOKEY                                                    
         XC    AUD.AUDKEY,AUD.AUDKEY                                            
         MVI   AUD.AUDKTYP,AUDKTYPQ                                             
         MVI   AUD.AUDKSUB,AUDKSUBQ                                             
         MVI   AUD.AUDKAUDT,AUDKTIME                                            
         MVC   AUD.AUDKCPY,CUXCPY                                               
         MVC   AUD.AUDKPEDT,DDATE                                               
         MVC   AUD.AUDKPIDB,DPPID                                               
         MVC   CSVKEY2,AUD.AUDKEY                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         B     GTAUD15                                                          
*                             MANUALLY READ NEXT RECORD AS GTAUDTIM             
GTAUD10  MVC   AUD.AUDKEY,CSVKEY2     OFTEN BREAKS READSEQ BY CALLING           
         LLC   RE,AUD.AUDKSEQ         GETOFF AND GETCAP                         
         LA    RE,1(RE)                                                         
         STC   RE,AUD.AUDKSEQ                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
*                                                                               
GTAUD15  CLC   CSVKEY2(AUDKSEQ-AUDKEY),AUD.AUDKEY DO ANY RECORDS EXIST          
         JNE   EXITY               NO                                           
*                                                                               
         MVC   TH_INDX,AUD.AUDKINDX                                             
         MVC   DSTAT,AUD.AUDKSTAT                                               
*                                                                               
GTAUD20  MVC   CSVKEY2,AUD.AUDKEY                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  AUD                                                              
*                                                                               
         L     R3,AIO4                                                          
         USING AUDRECD,R3                                                       
GTAUD25  LA    R2,AUDRFST                                                       
         USING STCELD,R2                                                        
GTAUD30  CLI   STCEL,0                                                          
         JE    GTAUD10                                                          
         CLI   BYTE1,2                                                          
         JE    GTAUD32             JUST TIMELS                                  
         CLI   STCEL,STCELQ        ELSE STCELS TOO                              
         JNE   GTAUD32                                                          
         CLI   STCIND,STCITIME                                                  
         JNE   GTAUD32                                                          
         GOTOR GTAUDSTC                                                         
         J     GTAUD92                                                          
*                                                                               
         USING TIMELD,R2                                                        
GTAUD32  CLI   TIMEL,TIMELQ                                                     
         JNE   GTAUD92                                                          
         CLI   TIMETYP,TIMEARIN                                                 
         JNE   GTAUD34                                                          
         GOTOR GTAUDTIM                                                         
         J     GTAUD92                                                          
*                                                                               
GTAUD34  CLI   TIMETYP,TIMERJAP                                                 
         JNE   GTAUD92                                                          
         TM    TIMRRSTA,TIMRRREJ                                                
         JZ    GTAUD92                                                          
         GOTOR GTAUDRJC                                                         
         J     GTAUD92                                                          
*                                                                               
GTAUD92  LLC   RE,TIMLN            Get next element on record                   
         AR    R2,RE                                                            
         J     GTAUD30                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* STCELS - MAINTAIN HIGHEST EVER ROW NUMBER (CANNOT RE-ADD OLD ROW NO)          
*        - MAINTIAN BUFFER HIGHEST MAT ROW NUMBER PER TIME ROW.                 
* ENTRY:R2=STCEL (STCITIME TYPE)                                                
* EXIT : ELEAREA UPDATED FROM MAT ROW NUMBER, IF ANY                            
***********************************************************************         
GTAUDSTC NTR1                                                                   
*                                                                               
         USING STCELD,R2                                                        
         SR    RF,RF                                                            
         ICM   RF,3,STCTROW                                                     
         CH    RF,TH_LSTRW                                                      
         JNH   GTASTC10                                                         
         STCM  RF,3,TH_LSTRW       UPDATE HIGHEST EVER ROW NUMBER               
*                                                                               
GTASTC10 CLI   STCTTYP,STCTMRAM    Look for materials                           
         JE    GTASTC12                                                         
         CLI   STCTTYP,STCTMRDL                                                 
         JE    GTASTC12                                                         
         CLI   STCTTYP,STCTMRAD                                                 
         JE    GTASTC12                                                         
         CLI   STCTTYP,STCTTIMS    Or status changes                            
         JNE   EXITY                                                            
         TM    STCDTTO,TIMSFAPP    Has the timesheet become approved            
         JZ    EXITY               No                                           
         MVC   TH_APDTE,STCTDTE    Approved date                                
         MVC   TH_APPID,STCTPID    Approver who approved                        
         J     EXITY                                                            
                                                                                
GTASTC12 CLC   STCTMROW,SPACES                                                  
         JE    EXITY               BLANK LINES ARE SPACES, DUNNO WHY            
*                                         LINE 16448 SEEMS UNLIKELY             
         L     R4,AELEAREA                                                      
*                                                                               
GTASTC15 DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         JE    GTASTC50            NO ENTRY FOR THIS IDNO                       
*                                                                               
         CLC   STCTROW,0(R4)      CHECK TIMELINE NUMBER MATCHES                 
         JNE   GTASTC20                                                         
         SR    RF,RF               YES, UPDATE                                  
         ICM   RF,3,STCTMROW                                                    
         CH    RF,L'STCTROW(R4)    MAT ROW NUM HIGHEST FOUND?                   
         JNH   EXITY               NO                                           
         STCM  RF,3,L'STCTROW(R4) YES                                           
         J     EXITY                                                            
*                                                                               
GTASTC20 DS    0H                                                               
         LA    R4,L'STCTROW+L'STCTMROW(R4)                                      
         J     GTASTC15                                                         
*  ADD ENTRY, CHECK NO OVERFLOW                                                 
GTASTC50 DS    0H                                                               
         LA    RF,L'STCTROW+L'STCTMROW+1(R4)                                    
         L     R5,=AL4(LELEAREA)                                                
         A     R5,AELEAREA       A(END OF BUFFER)                               
         CR    R5,RF                                                            
         JH    *+6                                                              
         DC    H'0'                ELEAREA FULL - TOO MUCH DATA                 
         MVC   0(L'STCTROW,R4),STCTROW                                          
         MVC   L'STCTROW(L'STCTMROW,R4),STCTMROW                                
         MVI   L'STCTROW+L'STCTMROW(R4),X'FF'                                   
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TIMELS - MAINTAIN LINE MANAGER APPROVAL STATUS TH_LMAP                        
*        - MAINTAIN CLIENT APPROVAL STATUS       TH_CLIAP                       
*        - MAINTIAN STATUS TO VIEWER             TH_STAVW                       
* ENTRY:R2=TIMEL (TIMEARIN TYPE)                                                
***********************************************************************         
GTAUDTIM NTR1                                                                   
*                                                                               
         USING TIMELD,R2                                                        
         OC    TIMAIDNO,TIMAIDNO                                                
         JNZ   GTATIM50                                                         
*                                                                               
* LINE MANAGER STATUS ELEMENT - set line approval status                        
         MVI   TH_LMAP,C'N'                                                     
         TM    TIMASTAT,TIMASAPR   is time approved/rejected                    
         JZ    *+8                                                              
         MVI   TH_LMAP,C'Y'                                                     
         TM    TIMASTAT,TIMASREJ                                                
         JZ    *+8                                                              
         MVI   TH_LMAP,C'R'                                                     
         CLC   TH_MANAP,CCTPID                                                  
         JE    GTATIM15                                                         
*                                                                               
         LA    R1,TH_MANBA                                                      
         LA    R0,TH_BAMAX         Max Backup Approvers                         
GTATIM10 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    EXITY                                                            
         CLC   CCTPID,0(R1)        Is line manager a back up approver?          
         JE    GTATIM15                                                         
         LA    R1,L'PIDNO(R1)                                                   
         JCT   R0,GTATIM10                                                      
         J     EXITY               NO                                           
*                                                                               
* set status to viewer                                                          
GTATIM15 OI    DINDS2,DIILMGR                                                   
         TM    DSTAT,TIMSAWAP                                                   
         JZ    GTATIM20                                                         
         MVI   TH_STAVW,TS#AWAIT                                                
         CLC   TH_MANAP,CCTPID     Main approver                                
         JE    EXITY               YES                                          
         MVI   TH_STAVW,TS#AWBAP   No - must be back up                         
         J     EXITY                                                            
*                                                                               
GTATIM20 TM    DSTAT,TIMSMAAP                                                   
         JZ    GTATIM25                                                         
         CLI   TH_STAVW,TS#AWAIT   Deal with awaiting approval from             
         JE    EXITY                      client level                          
         MVI   TH_STAVW,TS#APPRO                                                
         J     EXITY                                                            
*                                                                               
GTATIM25 TM    DSTAT,TIMSSUBM                                                   
         JZ    GTATIM30                                                         
         CLI   TH_STAVW,TS#AWAIT   Deal with awaiting approval from             
         JE    EXITY                          client level                      
         MVI   TH_STAVW,TS#SUBMT                                                
         J     EXITY                                                            
*                                                                               
GTATIM30 TM    DSTAT,TIMSREJE                                                   
         JZ    EXITY                                                            
         CLI   TH_STAVW,TS#AWAIT   Deal with awaiting approval from             
         JE    EXITY                          client level                      
         CLI   TH_LMAP,C'R'                                                     
         JNE   EXITY                                                            
         MVI   TH_STAVW,TS#REJEC                                                
         J     EXITY                                                            
*                                                                               
* ROW - CHECK CLIENT APPROVAL STATUS                                            
GTATIM50 DS    0H                                                               
*        MVC   ANYACCNT,TIMAPULA                                                
         SR    RF,RF               KEEP ROW HIGH-WATER MARK UP TO DATE          
         ICM   RF,3,TIMAIDNO                                                    
         CH    RF,TH_LSTRW                                                      
         JNH   GTATIM55                                                         
         STCM  RF,3,TH_LSTRW                                                    
*                                                                               
GTATIM55 CLI   DSTAT,0             IS IT SAVED                                  
         JE    EXITY               THEN NO APPROVAL POSSIBLE                    
*                                                                               
*        CLC   TH_MANAP,CCTPID                                                  
*        JE    GTATIM65                                                         
*                                                                               
         GOTOR GETOFF,TIMAPULA                                                  
         GOTOR GETCAP,TIMAPULA     CHECK TO SEE IF NEW CLIENT MANAGER           
         JNE   EXITY                         OR BACK UP APPROVER                
         CLC   TL_CLIAP,CCTPID     YES - COMPARE WITH CONNECTED USER            
         JE    GTATIM65                                                         
         TM    DINDS2,DIILMGR                                                   
         JNZ   EXITY               ALREADY HAVE MANAGER STATUS                  
*                                                                               
         LA    R0,TH_BAMAX         Max Backup Approvers                         
         LA    R1,TL_CLIBA         Search backups approvers for match           
GTATIM60 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    EXITY                                                            
         CLC   CCTPID,0(R1)        Is user a back up client approver?           
         JE    GTATIM65            Yes                                          
         LA    R1,L'PIDNO(R1)      Check other entries                          
         JCT   R0,GTATIM60                                                      
         J     EXITY                                                            
*                                                                               
GTATIM65 MVI   TH_ACCS,YESQ        Set connected user has access                
         TM    TIMASTAT,TIMASAPR   Is time approved                             
         JZ    GTATIM70                                                         
         CLI   TH_STAVW,TS#AWAIT   Or is it awaiting line manager               
         JE    EXITY                                        approval            
         MVI   TH_STAVW,TS#APPRO   Time approved                                
         J     EXITY                                                            
*                                                                               
GTATIM70 MVI   TH_STAVW,TS#AWAIT                                                
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TIMELS - MAINTAIN LINE MANAGER APPROVAL STATUS TH_LMAP                        
*        - MAINTAIN CLIENT APPROVAL STATUS       TH_CLIAP                       
*        - MAINTIAN STATUS TO VIEWER             TH_STAVW                       
* ENTRY:R2=TIMEL (TIMEARIN TYPE)                                                
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R2           R2=(TIMELD)                                  
         USING REJ_D,RJ_BUFF                                                    
GTAUDRJC NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*GTAUDR*'                                                    
         MVC   REJ_ROW,TIMRIDNO                                                 
         MVC   REJ_PID,TIMRRPID                                                 
         MVC   REJ_DATE,TIMRRDTE                                                
         MVC   REJ_TIME,TIMRRTME                                                
         LLC   RF,TIMLN                                                         
         SHI   RF,TIMRLN1Q+1                                                    
         BASR  R1,0                                                             
         MVC   REJ_DATA(0),TIMREJAP                                             
         EX    RF,0(R1)            Comments                                     
         BASR  R1,0                                                             
         OC    REJ_DATA(0),TIMREJAP                                             
         EX    RF,0(R1)                                                         
         AHI   RF,2                 +2 TO BE SAFE                               
         AHI   RF,REJ_RLEN                                                      
         STH   RF,REJ_LEN                                                       
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK LOCATION EXISTS FOR PERIOD                                    *         
* ON EXIT EQUAL OR NOT EQUAL                                          *         
***********************************************************************         
         SPACE 1                                                                
CHKLOA   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*CHKLOA*'                                                    
         USING PERRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
         USING TMPELD,R3                                                        
         XR    R0,R0                                                            
CHKLOA02 CLI   LOCEL,0                                                          
         JE    CHKLOAN                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    CHKLOA06                                                         
CHKLOA04 IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         J     CHKLOA02                                                         
*                                                                               
CHKLOA06  CLC  TMPSTART,LOCSTART                                                
          JNL  CHKLOA08                                                         
          CLC  TMPEND,LOCSTART                                                  
          JL   CHKLOA04                                                         
          J    CHKLOA10                                                         
*                                                                               
CHKLOA08  OC   LOCEND,LOCEND                                                    
          JZ   CHKLOA10                                                         
          CLC  TMPSTART,LOCEND                                                  
          JH   CHKLOA04                                                         
*                                                                               
CHKLOA10 LA    R4,BRATAB                                                        
CHKLOA12 OC    0(L'GDADATE,R4),0(R4)                                            
         JZ    CHKLOAY                                                          
         CLC   TMPSTART,0(R4)                                                   
         JL    CHKLOAN                                                          
         OC    L'GDADATE(L'GDADATE,R4),L'GDADATE(R4)                            
         JZ    CHKLOAY                                                          
         CLC   TMPSTART,L'GDADATE(R4)                                           
         JL    CHKLOAY                                                          
         LA    R4,L'GDADATE2+L'GDADATE(R4)                                      
         J     CHKLOA12                                                         
                                                                                
CHKLOAY  J     EXITY                                                            
CHKLOAN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* SET STATUS OF TIMESHEET FROM AUDRECD                                *         
***********************************************************************         
         SPACE 1                                                                
SETSSTA  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SETSSTA'                                                      
                                                                                
         MVC   TH_STATS,ASTERS                                                  
         MVC   BYTE1,DSTAT                                                      
         NI    BYTE1,FF-TRNSARCH                                                
                                                                                
         LA    RE,SETSTAB                                                       
SETSSTA1 CLI   0(RE),X'FF'                                                      
         BE    SETSSTA3                                                         
         MVC   BYTE2,BYTE1                                                      
         NC    BYTE2,0(RE)                                                      
         OC    BYTE2,BYTE2                                                      
         BNZ   SETSSTA2                                                         
         AHI   RE,2                                                             
         B     SETSSTA1                                                         
                                                                                
SETSSTA2 MVC   TH_STATS,1(RE)                                                   
         B     SETSSTA4                                                         
SETSSTA3 MVI   TH_STATS,TS#PROGR                                                
                                                                                
SETSSTA4 J     EXITY                                                            
                                                                                
SETSTAB  DC    AL1(TIMSFAPP,TS#APPRO)                                           
         DC    AL1(TIMSPAPP,TS#PAAPR)                                           
         DC    AL1(TIMSSUBM,TS#SUBMT)                                           
         DC    AL1(TIMSREJE,TS#REJEC)                                           
         DC    AL1(TAPSSAVQ,TS#PROGR)                                           
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* GET HOLIDAYS FROM EDIT HOURS RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
GETHOL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETHOL*'                                                    
         MVI   DINDS1,0                                                         
         L     R6,ATIA                                                          
         USING TMTTABD,R6                                                       
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),ONENUL                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(L'COPHA),COPHA                         
         MVC   DPHOLAC,TEMP2                                                    
         OC    DPHOLAC,SPACES                                                   
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),ONENUL                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(L'COSHA),COSHA                         
         MVC   DSHOLAC,TEMP2                                                    
         OC    DSHOLAC,SPACES                                                   
         MVC   ANYACCNT,DPHOLAC                                                 
*                                                                               
         CLI   COPHL,COSLVOFF                                                   
         BNE   *+8                                                              
         OI    DINDS1,DIIOFF                                                    
         CLI   COPHL,COSLVDPT                                                   
         BNE   *+8                                                              
         OI    DINDS1,DIIDPT+DIIOFF                                             
         CLI   COPHL,COSLVSDT                                                   
         BNE   *+8                                                              
         OI    DINDS1,DIISDP+DIIDPT+DIIOFF                                      
         CLI   COPHL,COSLVPER                                                   
         BNE   *+8                                                              
GTHOL02  OI    DINDS1,DIIPER+DIIOFF+DIIDPT+DIISDP                               
                                                                                
         LA    R4,IOKEY                                                         
         USING EDTRECD,R4                                                       
GTHOL04  MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,CUXCPY                                                   
         TM    DINDS1,DIIOFF                                                    
         BZ    *+10                                                             
         MVC   EDTKOFC,TH_1ROFF                                                 
         TM    DINDS1,DIIDPT                                                    
         BZ    *+10                                                             
         MVC   EDTKDPT,TH_DEPT                                                  
         TM    DINDS1,DIISDP                                                    
         BZ    *+10                                                             
         MVC   EDTKSBD,TH_SDPT                                                  
         TM    DINDS1,DIIPER                                                    
         BZ    *+10                                                             
         MVC   EDTKPER,QPERSN                                                   
         XC    EDTKSEQ,EDTKSEQ                                                  
                                                                                
         XR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   DCDAT,QENDD                                                      
         MVC   DCDAT+1(1),BYTE1                                                 
         CLC   QENDD+1(1),BYTE1                                                 
         BNL   GTHOL06                                                          
                                                                                
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
                                                                                
GTHOL06  MVC   EDTKYR,DCDAT                                                     
         MVI   EDTKKSTA,EDTKSDAY   set daily time                               
         MVC   CSVKEY2,EDTKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         BE    GTHOL08                                                          
         B     GTHOL44             exit                                         
*                                                                               
GTHOL08  CLC   EDTKSTDT,TH_DATE                                                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   EDTKENDT,TH_DATE                                                 
         BNL   GTHOL10                                                          
         GOTO1 VDATCON,DMCB,(1,DCDAT),(0,WORK)                                  
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'+1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DCDAT)                                
         B     GTHOL06                                                          
*                                                                               
GTHOL10  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
GTHOL12  LA    R2,TH_DATE                                                       
         LA    R7,TMTHRS                                                        
         LH    R3,TH_NDAYS                                                      
GTHOL14  ZAP   0(L'TMTHRS,R7),PZERO                                             
         GOTO1 VDATCON,DMCB,(1,0(R2)),(0,WORK)                                  
         GOTO1 VGETDAY,DMCB,WORK,DUB1                                           
         CLC   DUB1(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
GTHOL16  L     R4,AIO2                                                          
         LA    R4,EDTRFST                                                       
         USING DEDELD,R4                                                        
GTHOL18  CLI   DEDEL,0                                                          
         BE    GTHOL28                                                          
         CLI   DEDEL,DEDELQ                                                     
         BNE   GTHOL26                                                          
         CLI   DEDLN,DEDLN1Q                                                    
         BE    GTHOL26                                                          
*                                                                               
GTHOL20  CLC   DEDDATE,0(R2)                                                    
         BNE   GTHOL26                                                          
         TM    DINDS1,DIISHOL                                                   
         BNZ   GTHOL22                                                          
         TM    DEDSTAT,DEDSPHOL                                                 
         BNZ   GTHOL24                                                          
         B     GTHOL28                                                          
GTHOL22  TM    DEDSTAT,DEDSSHOL                                                 
         BZ    GTHOL28                                                          
GTHOL24  ZAP   0(L'TMTHRS,R7),DEDHRS                                            
         B     GTHOL28                                                          
*                                                                               
GTHOL26  SR    R0,R0                                                            
         IC    R0,DEDLN                                                         
         AR    R4,R0                                                            
         B     GTHOL18                                                          
*                                                                               
GTHOL28  LA    R2,L'TH_DATE(R2)                                                 
         LA    R7,L'TMTHRS(R7)                                                  
         BCT   R3,GTHOL14                                                       
*                                                                               
GTHOL30  LA    R3,TMTHRS                                                        
         LH    RF,TH_NDAYS                                                      
GTHOL32  CP    0(L'TMTHRS,R3),PZERO                                             
         BNE   GTHOL34                                                          
         LA    R3,L'TMTHRS(R3)                                                  
         BCT   RF,GTHOL32                                                       
         B     GTHOL44                                                          
*                                                                               
GTHOL34  MVI   TMTAPST,C'N'                                                     
         MVI   TMTTEL,TMTTELQ                                                   
         MVI   TMTTYP,C'N'                                                      
         LH    RF,TL_TROW                                                       
         AHI   RF,1                                                             
         STH   RF,TL_TROW                                                       
         MVC   TMTIDNO,TL_TROW                                                  
         SR    R7,R6                                                            
         STC   R7,TMTTLN                                                        
         MVC   TMTULA,ANYACCNT                                                  
*                                                                               
GTHOL36  GOTOR GETCAP,ANYACCNT                                                  
         BNE   GTHOL38                                                          
         MVC   TMTCLIAP,TL_CLIAP                                                
*                                                                               
GTHOL38  MVI   TMTGRY,C'Y'                                                      
         CLC   CCTPID,TL_CLIAP                                                  
         BE    GTHOL40                                                          
         TM    DINDS2,DIILMGR                                                   
         BNZ   GTHOL40                                                          
         TM    DINDS2,DIIUSR                                                    
         BZ    GTHOL42                                                          
GTHOL40  MVI   TMTGRY,C'N'                                                      
*                                                                               
GTHOL42  AR    R6,R7                                                            
*                                                                               
GTHOL44  TM    DINDS1,DIISHOL      Have we read for staff holidays              
         BNZ   GTHOL46             Yes - we have finished                       
         OI    DINDS1,DIISHOL      No - set to read for it                      
         MVC   ANYACCNT,DSHOLAC    set account code for staff holiday           
         TM    DINDS1,DIIPER+DIIOFF+DIIDPT+DIISDP  Are we at staff lvel         
         BO    GTHOL12             Yes - so reuse record in IO area             
         B     GTHOL02             No                                           
                                                                                
GTHOL46  J     EXITY                                                            
         DROP  R4,R6,RB                                                         
         EJECT                                                                  
***********************************************************************         
* Establish office and media code from account                        *         
***********************************************************************         
                                                                                
GETOFF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETOFF*'                                                      
                                                                                
         LR    R7,R1                                                            
P        USING ACTKULA,R7                                                       
         MVC   TL_OFF,SPACES                                                    
         MVC   TL_MED,SPACES                                                    
         CLC   PRODUL,P.ACTKUNT    Test production ledger                       
         JNE   EXITY                                                            
                                                                                
         USING ACTRECD,R2                                                       
GETOFF02 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),P.ACTKULA                           
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),P.ACTKACT                                             
         EX    RF,0(RE)                                                         
         L     R1,=AL4(IORDD+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETOFF04                                                         
         CLI   IOERR,IOEDEL                                                     
         JE    GETOFF04                                                         
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN                                                            
                                                                                
GETOFF04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
GETOFF06 CLI   PPREL,0                                                          
         JE    GETOFF12                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    GETOFF10                                                         
                                                                                
GETOFF08 LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     GETOFF06                                                         
                                                                                
GETOFF10 MVC   TL_OFF,PPRGAOFF                                                  
         OC    TL_OFF,SPACES                                                    
         J     GETOFF08                                                         
                                                                                
GETOFF12 LLC   RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,P.ACTKACT(RE)                                                 
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JNH   EXITY                                                            
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),P.ACTKACT                                             
         EX    RF,0(RE)                                                         
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETOFF14                                                         
         CLI   IOERR,IOEDEL                                                     
         JE    GETOFF14                                                         
         MVC   LP_ERROR,=AL2(AE$ACTNF)                                          
         J     EXITN                                                            
                                                                                
GETOFF14 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
GETOFF16 CLI   PPREL,0                                                          
         JE    GETOFF22                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    GETOFF20                                                         
                                                                                
GETOFF18 LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     GETOFF16                                                         
                                                                                
GETOFF20 CLI   PPRGAOFF,X'40'                                                   
         JNH   GETOFF18                                                         
         MVC   TL_OFF,PPRGAOFF                                                  
         OC    TL_OFF,SPACES                                                    
         J     GETOFF18                                                         
*                                                                               
GETOFF22 LLC   RF,PPROLEN                                                       
         LA    R1,P.ACTKACT(RF)                                                 
         CLI   0(R1),C' '                                                       
         JNH   EXITY                                                            
         MVC   TL_MED,0(R1)                                                     
         J     EXITY                                                            
                                                                                
         DROP  P,R2                                                             
***********************************************************************         
* Establish approver and back-up approver PID for time                *         
***********************************************************************         
                                                                                
GETCAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETCAP*'                                                      
                                                                                
         LR    R7,R1                                                            
P        USING ACTKULA,R7                                                       
         XC    TL_CLIAP,TL_CLIAP   Client approver                              
         XC    TL_CLIBA,TL_CLIBA   Client back up approver                      
         CLC   PRODUL,P.ACTKUNT    Test production ledger                       
         JNE   GETCAP34                                                         
                                                                                
         LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY       Find job, product or client approver         
GETCAP02 XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVI   SJ.JOBPAPPL,JOBPATIM                                             
         MVC   SJ.JOBPCOFF,SPACES                                               
         MVC   SJ.JOBPCMED,SPACES                                               
                                                                                
         TM    APRSTAT,APRJOB      Are we looking at job level                  
         JZ    GETCAP04                                                         
         LLC   RF,PPROLEN                                                       
         LA    RF,P.ACTKACT(RF)                                                 
         CLI   0(RF),X'40'         Have we got a job                            
         JNH   GETCAP14            No                                           
         LLC   RF,PJOBLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
         J     GETCAP08                                                         
GETCAP04 TM    APRSTAT,APRPRO                                                   
         JZ    GETCAP06                                                         
         LLC   RF,PCLILEN                                                       
         LA    RF,P.ACTKACT(RF)                                                 
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   GETCAP14            No                                           
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
         J     GETCAP08                                                         
GETCAP06 TM    APRSTAT,APRCLI                                                   
         JZ    GETCAP08                                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
GETCAP08 TM    APRSTAT,APRMED                                                   
         JZ    GETCAP10                                                         
         CLC   TL_MED,SPACES                                                    
         JNH   GETCAP14                                                         
         MVC   SJ.JOBPCMED,TL_MED                                               
                                                                                
GETCAP10 TM    APRSTAT,APROFF                                                   
         JZ    GETCAP12                                                         
         CLC   TL_OFF,SPACES                                                    
         JNH   GETCAP14                                                         
         MVC   SJ.JOBPCOFF,TL_OFF                                               
                                                                                
GETCAP12 OC    SJ.JOBPCODE,SPACES  make sure all spaces before read             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),IOKEYSAV                             
         JE    GETCAP18                                                         
GETCAP14 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   GETCAP02                                                         
         J     GETCAPN                                                          
                                                                                
GETCAP18 MVC   TL_CLIAP,SJ.JOBPPIDB                                             
         DROP  SJ                                                               
         J     GETCAP40                                                         
                                                                                
NC       USING NCTPASD,IOKEY       Non-client time approver                     
GETCAP34 XC    NC.NCTPAS,NC.NCTPAS                                              
         MVI   NC.NCTPTYP,NCTPTYPQ                                              
         MVI   NC.NCTPSUB,NCTPSUBQ                                              
         MVC   NC.NCTPCPY,CUXCPY                                                
         MVI   NC.NCTPAPPL,NCTPATIM                                             
         MVC   NC.NCTPNCC,P.ACTKACT                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GETCAP38                                                         
         DC    H'0'                                                             
                                                                                
GETCAP36 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GETCAP38                                                         
         DC    H'0'                                                             
                                                                                
GETCAP38 CLC   NC.NCTPAS(NCTPPIDB-NCTPASD),IOKEYSAV                             
         JNE   GETCAPN                                                          
         MVC   TL_CLIAP,NC.NCTPPIDB                                             
         DROP  NC,P                                                             
                                                                                
GETCAP40 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETCAP42 CLI   LIDEL,0             Test end of record                           
         JE    EXITY                                                            
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETCAP46                                                         
GETCAP44 IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GETCAP42                                                         
                                                                                
GETCAP46 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETCAP44                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
         LA    RF,TL_CLIBA                                                      
         XR    RE,RE                                                            
GETCAP48 TM    LIDLAPPL,LIDLTIME   Is this entry for timesheets                 
         JZ    GETCAP50            No                                           
         MVC   0(L'PIDNO,RF),LIDLPID  Yes - Get back up approver PID            
         LA    RF,L'PIDNO(RF)                                                   
         AHI   RE,1                                                             
                                                                                
GETCAP50 CHI   RE,TH_BAMAX         Did we fill up the whole table?              
         JE    EXITY                                                            
                                                                                
         LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETCAP48            No - check next entry                        
         J     EXITY               Yes - finish                                 
                                                                                
GETCAPN  J     EXITN                                                            
         DROP  R1,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Check whether 1N account is locked                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
CHK1AL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CHK1AL*'                                                      
         GOTOR VALACT,TEMP2                                                     
         CLI   TL_TLOC,YESQ        LOCKED FROM T/S?                             
         BE    EXITN                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET A LIST OF 1N ACCOUNTS WHICH HAVE FILTER 2 SET AS T              *         
***********************************************************************         
         SPACE 1                                                                
GET1NAC  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GET1NA*'                                                      
         USING ACTRECD,R2                                                       
         L     R4,AELEAREA                                                      
         MVI   0(R4),X'FF'                                                      
         L     R5,=AL4(LELEAREA-L'ACTKULA)                                      
         AR    R5,R4                                                            
GET1N02  LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),ONENUL                              
         MVC   CSVKEY1,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    GET1N20                                                          
         DC    H'0'                                                             
*                                                                               
GET1N10  DS    0H                                                               
         LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    GET1N20                                                          
         DC    H'0'                                                             
*                                                                               
GET1N20  CLC   ACTKEY(ACTKACT-ACTKEY),CSVKEY1                                   
         BNE   GET1N80                                                          
*                                                                               
GET1N30  CLC   ACTKEY+L'ACTKCULA(L'ACTKEY-L'ACTKCULA),SPACES                    
         BNE   GET1N10                                                          
         TM    ACTKSTAT,ACTSABLP                                                
         BZ    GET1N10                                                          
         TM    ACTKSTAT,ACTSLOCK                                                
         BNZ   GET1N10                                                          
         CLI   ACTKSAF2,C'T'                                                    
         BNE   GET1N10                                                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         AHI   R3,ACTRFST-ACTRECD                                               
         USING RSTELD,R3                                                        
GET1N40  CLI   RSTEL,0                                                          
         BE    GET1N60                                                          
         CLI   RSTEL,RSTELQ                                                     
         BE    GET1N50                                                          
GET1N45  LLC   R0,RSTLN                                                         
         AR    R3,R0                                                            
         B     GET1N40                                                          
*                                                                               
GET1N50  CLI   RSTLN,RSTLN3Q       Exclude 1N accs locked from t/s              
         JL    GET1N45                                                          
         TM    RSTLSTAT,RSTLSTIQ   LOCKED FROM T/S?                             
         JNZ   GET1N10                                                          
*                                                                               
GET1N60  MVC   0(L'ACTKULA,R4),ACTKULA                                          
         MVI   L'ACTKULA(R4),X'FF'                                              
         LA    R4,L'ACTKULA(R4)                                                 
         CR    R4,R5                                                            
         BNH   GET1N10                                                          
         DC    H'0'                TOO MUCH DATA FOR ELEAREA                    
*                                                                               
GET1N80  J     EXIT                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET TIME TEMPLATE RECORD INTO AIO2                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
GETTEMP  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETTEMP'                                                    
MN       USING TTMRECD,IOKEY                                                    
         XC    MN.TTMKEY,MN.TTMKEY                                              
         MVI   MN.TTMKTYP,TTMKTYPQ                                              
         MVI   MN.TTMKSUB,TTMKSUBQ                                              
         MVC   MN.TTMKCPY,CUXCPY                                                
         MVC   MN.TTMK1RAC,D1RACT                                               
         MVC   CSVKEY3,MN.TTMKEY                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    GTEMP04                                                          
         DC    H'0'                                                             
*                                                                               
GTEMP04  CLC   MN.TTMKEY(TTMKCODE-TTMRECD),CSVKEY3                              
         BE    GTEMP20                                                          
*                                                                               
GTEMP06  XC    MN.TTMKEY,MN.TTMKEY                                              
         MVI   MN.TTMKTYP,TTMKTYPQ                                              
         MVI   MN.TTMKSUB,TTMKSUBQ                                              
         MVC   MN.TTMKCPY,CUXCPY                                                
         SR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SHI   RE,1                                                             
         MVC   MN.TTMK1RAC(0),D1RACT                                            
         EX    RE,*-6                                                           
         OC    MN.TTMK1RAC,SPACES                                               
         MVC   CSVKEY3,MN.TTMKEY                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    GTEMP08                                                          
         DC    H'0'                                                             
*                                                                               
GTEMP08  CLC   MN.TTMKEY(TTMKCODE-TTMRECD),CSVKEY3                              
         BE    GTEMP20                                                          
*                                                                               
         XC    MN.TTMKEY,MN.TTMKEY                                              
         MVI   MN.TTMKTYP,TTMKTYPQ                                              
         MVI   MN.TTMKSUB,TTMKSUBQ                                              
         MVC   MN.TTMKCPY,CUXCPY                                                
         SR    RE,RE                                                            
         IC    RE,ONERL2L                                                       
         SHI   RE,1                                                             
         MVC   MN.TTMK1RAC(0),D1RACT                                            
         EX    RE,*-6                                                           
         OC    MN.TTMK1RAC,SPACES                                               
         MVC   CSVKEY3,MN.TTMKEY                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    GTEMP10                                                          
         DC    H'0'                                                             
*                                                                               
GTEMP10  CLC   MN.TTMKEY(TTMKCODE-TTMRECD),CSVKEY3                              
         BE    GTEMP20                                                          
*                                                                               
         XC    MN.TTMKEY,MN.TTMKEY                                              
         MVI   MN.TTMKTYP,TTMKTYPQ                                              
         MVI   MN.TTMKSUB,TTMKSUBQ                                              
         MVC   MN.TTMKCPY,CUXCPY                                                
         SR    RE,RE                                                            
         IC    RE,ONERL1L                                                       
         SHI   RE,1                                                             
         MVC   MN.TTMK1RAC(0),D1RACT                                            
         EX    RE,*-6                                                           
         OC    MN.TTMK1RAC,SPACES                                               
         MVC   CSVKEY3,MN.TTMKEY                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    GTEMP12                                                          
         DC    H'0'                                                             
*                                                                               
GTEMP12  CLC   MN.TTMKEY(TTMKCODE-TTMRECD),CSVKEY3                              
         BNE   GTEMPY                                                           
*                                                                               
GTEMP20  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GTEMPY   J     EXITY                                                            
         DROP  MN,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIBUF*'                                                      
                                                                                
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
***********************************************************************         
* Add a record to optimisation buffer                                 *         
*                                                                     *         
* Ntry:- R1 points to caller's OB_D                                   *         
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
* Ntry:- R1 points to caller's OB_D                                   *         
* Exit:- CC=Low if record not found in buffer                         *         
*        CC=Equal if record found and is not posted with an error     *         
*           - record is returned in caller's OB_D                     *         
*        CC=High if record found and is posted with an error (set in  *         
*           ROUERRV)                                                  *         
***********************************************************************         
                                                                                
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
***********************************************************************         
* Interface to TSAR                                                   *         
*                                                                     *         
***********************************************************************         
                                                                                
GOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFREJ*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
TB       USING TSARD,TSARRECS      R3=A(TSAR block)                             
         LA    R1,RJ_BUFF                                                       
         ST    R1,TB.TSAREC        Address of record buffer area                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TB.TSARD(TSPNEWL),TB.TSARD                                       
         MVC   TB.TSACTN,0(R2)                                                  
         MVC   TB.TSACOM,ACOMFACS                                               
         LHI   R0,2*ONEK                                                        
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL      Set require 1MB off-line                    
         MVI   TB.TSRECI,TSRXTN+TSRMINB1+TSRVAR                                 
         MVI   TB.TSKEYL,REJ_KEYL   Set key length                              
         MVI   TB.TSINDS,TSINODSK   Set no disk writes (save/restore)           
         MVI   TB.TSIND2,TSI2MANY                                               
         LHI   R0,L'RJ_BUFF                                                     
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
         JNE   GOTSARN                                                          
GOTSARY  J     EXITY                                                            
         XIT1                                                                   
                                                                                
GOTSARN  J     EXITN                                                            
         XIT1                                                                   
**********************************************************************          
* Check if the connected user can view the timesheet                 *          
**********************************************************************          
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
CHKVIEW  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CHKVIE*'                                                      
*                                                                               
         LA    R4,GAPAREA          point to main/office entry                   
         XC    GAPAREA,GAPAREA                                                  
         MVI   GAPTDAT1,GAPTT1Q                                                 
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         MVC   GAPTCODE(0),D1RACT                                               
         EX    RF,*-6                                                           
*                                                                               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CHKVW04                                                          
*                                                                               
CHKVW02  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
CHKVW04  TM    ATSRERRS,TSEEOF     LOOK FOR A MAIN ENTRY MATCH                  
         JNZ   CHKVWN                                                           
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   CHKVWN                                                           
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         CLC   GAPTCODE(0),D1RACT                                               
         EX    RF,*-6                                                           
         JNE   CHKVW02                                                          
         TM    GAPTSTA,GAPTSMQ                                                  
         JZ    CHKVW02                                                          
*                                                                               
CHKVW06  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF            AND THEN CHECK FOR EXCEPTIONS                
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   CHKVWY                                                           
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   CHKVWY                                                           
         TM    GAPTSTA,GAPTSMQ     NEXT MAIN ENTRY, WE'RE OK                    
         JO    CHKVWY                                                           
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         CLC   GAPTCODE(0),D1RACT                                               
         EX    RF,*-6                                                           
         JNE   CHKVW06             SOME OTHER EXCEPTION, CARRY ON               
         J     CHKVWN                                                           
*                                                                               
CHKVWY   J     EXITY                                                            
*                                                                               
CHKVWN   J     EXITN                                                            
         EJECT                                                                  
**********************************************************************          
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
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
TIMIDL#  DC    AL2(A#TINI)         TIMESHEET INITIAL DOWNLOAD                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
TIMDDL#  DC    AL2(A#TIMDIS)       TIMESHEET DISPLAY DOWNLOAD                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
APRTAB   DS    0XL1                                                             
         DC    B'11111000'         Office/client/product/job/media              
         DC    B'11011000'         Office/client/product/media                  
         DC    B'10011000'         Office/client/media                          
         DC    B'10010000'         Client/media                                 
         DC    B'00011000'         Office/media                                 
         DC    B'00010000'         Media                                        
         DC    B'11001000'         Office/client/product                        
         DC    B'10001000'         Office/client                                
         DC    B'10000000'         Client                                       
         DC    B'00001000'         Office                                       
         DC    X'FF'                                                            
                                                                                
MANGLIT  DC    C'Manager search'                                                
                                                                                
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
PZERO    DC    P'0'                                                             
ONENUL   DC    C'1N'                                                            
LSTNAR   DC    C'999999'                                                        
                                                                                
NONOS    DC    C'NNNNNN'                                                        
YEYES    DC    C'YYYYYY'                                                        
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
         EJECT                                                                  
                                                                                
ASTERS   DC    16C'*'                                                           
                                                                                
         EJECT                                                                  
*                                                                               
PKSKEYT  LKKEY H,PKSKEY,SAVED      ** PERSONAL KEYSTAGE KEY DRIVER **           
         LKKEY LIT,PKSKTYP,PKSKTYPQ                                             
         LKKEY LIT,PKSKSUB,PKSKSUBQ                                             
         LKKEY SIN,PKSKCPY,AGENCY                                               
         LKKEY LIT,PKSKREM,0                                                    
         LKKEY SIN,PKSKPIDB,DPPID                                               
         LKKEY RNG,PKSKEDT,DENDRNG                                              
         LKKEY RNG,PKSKSDT,DSTRRNG                                              
         LKKEY ALL,PKSKCODE                                                     
         LKKEY ALL,PKSKUNT                                                      
         LKKEY ALL,PKSKLDG                                                      
         LKKEY ALL,PKSKACT                                                      
         LKKEY ALL,PKSKKYST                                                     
         LKKEY ALL,PKSKOUT                                                      
         LKKEY ALL,PKSKAMPM                                                     
         LKKEY E                                                                
                                                                                
ORDKEYT  LKKEY H,ORDKEY            ** ORDER KEY DRIVER **                       
         LKKEY LIT,ORDKTYP,ORDKTYPQ                                             
         LKKEY SIN,ORDKCPY,AGENCY                                               
         LKKEY SIN,ORDKORD,DORDNUM                                              
         LKKEY LIT,ORDKSEQ,0                                                    
         LKKEY LIT,ORDKSP2,0                                                    
         LKKEY E                                                                
                                                                                
EGNKEYT  LKKEY H,EGNPAS            ** ESTAIMATE NUMBER KEY DRIVER **            
         LKKEY LIT,EGNPTYP,EGNPTYPQ                                             
         LKKEY LIT,EGNPSUB,EGNPSUBQ                                             
         LKKEY SIN,EGNPCPY,AGENCY                                               
         LKKEY LIT,EGNPREM,0                                                    
         LKKEY SIN,EGNPNUM,DESTNUM                                              
         LKKEY ALL,EGNPCLI                                                      
         LKKEY ALL,EGNPPRO                                                      
         LKKEY ALL,EGNPJOB                                                      
         LKKEY ALL,EGNPLNO                                                      
         LKKEY E                                                                
                                                                                
SJAKEYT  LKKEY H,ACTKEY            ** SJ ACCOUNT RECORD DRIVER **               
         LKKEY SIN,ACTKCPY,AGENCY                                               
         LKKEY LIT,ACTKUNT,C'S'                                                 
         LKKEY LIT,ACTKLDG,C'J'                                                 
         LKKEY WMP,ACTKACT,DACLI                                                
         LKKEY LIT,ACTREST,X'40'                                                
         LKKEY E                                                                
                                                                                
NCLKEYT  LKKEY H,ACTKEY            ** 1N ACCOUNT RECORD DRIVER **               
         LKKEY SIN,ACTKCPY,AGENCY                                               
         LKKEY LIT,ACTKUNT,C'1'                                                 
         LKKEY LIT,ACTKLDG,C'N'                                                 
         LKKEY WMP,ACTKACT,DA1NA                                                
         LKKEY LIT,ACTREST,X'40'                                                
         LKKEY E                                                                
                                                                                
SCMKEYT  LKKEY H,SCMKEY,SAVED      ** NARRATIVE RECORD KEY DRIVER **            
         LKKEY LIT,SCMKTYP,SCMKTYPQ                                             
         LKKEY SIN,SCMKCPY,AGENCY                                               
         LKKEY RNG,SCMKCODE,QNARCOD                                             
         LKKEY LIT,SCMKSPR,X'00'                                                
         LKKEY E                                                                
                                                                                
CALKEYT  LKKEY H,CASKEY            ** CALENDAR KEY DRIVER **                    
         LKKEY LIT,CASKTYP,CASKTYPQ                                             
         LKKEY LIT,CASKSUB,CASKSUBQ                                             
         LKKEY SIN,CASKCPY,AGENCY                                               
         LKKEY ALL,CASKEMOA                                                     
         LKKEY ALL,CASKSMOA                                                     
         LKKEY ALL,CASKOFC                                                      
         LKKEY ALL,CASKREM                                                      
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
                                                                                
INITLIT  DC    C'Initial'                                                       
CTSOLIT  DC    C'Create t/s from prior'                                         
ODFTSDI  DC    C'Override def t/s disp'                                         
CALDWLIT DC    C'Dummy calendar request field'                                  
HTTP     DC    C'HTTP://'                                                       
*                                                                               
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(NCLKF)                                                         
         DC    A(NCLRF)                                                         
         DC    A(CLIKF)                                                         
         DC    A(CLIRF)                                                         
         DC    A(SCMKF)                                                         
         DC    A(SCMRF)                                                         
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#RWKAD,L'AC@RWKAD                                              
         DCDDL AC#KDTAD,L'AC@KDTAD                                              
         DCDDL AC#KDTAM,L'AC@KDTAM                                              
         DCDDL AC#CMTAD,L'AC@CMTAD                                              
         DCDDL AC#CMTDL,L'AC@CMTDL                                              
         DCDDL AC#CMTAM,L'AC@CMTAM                                              
         DCDDL AC#KSTAD,L'AC@KSTAD                                              
         DCDDL AC#KSTDL,L'AC@KSTDL                                              
         DCDDL AC#STAAM,L'AC@STAAM                                              
         DCDDL AC#HLIDY,L'AC@HLIDY                                              
         DCDDL AC#OTHER,L'AC@OTHER                                              
         DCDDL AC#APPTM,L'AC@APPTM                                              
DCDICTLX DC    X'FF'                                                            
*                                                                               
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
                                                                                
BRATAB   DS    0X                                                               
         DC    (MAXBRAQ)XL3'00'                                                 
BRATABX  DC    X'FF'                                                            
MAXBRAQ  EQU   20                                                               
*                                                                               
CALDAYS  DC    XL160'00'           Days/Hours per period                        
         DC    X'FF'                                                            
       ++INCLUDE ACURLTAB                                                       
         EJECT                                                                  
B#TIMREC EQU   3                   IO2 - TIME RECORDS                           
B#SCMREC EQU   3                   IO2 - NARRATIVE RECORDS                      
B#CLIREC EQU   3                   IO2 - CLIENT RECORDS                         
ATIMREC  EQU   LP_BLKS+((B#TIMREC-1)*L'LP_BLKS)                                 
B#PTASK  EQU   4                       - PERSONAL TASK KEY                      
APKSREC  EQU   LP_BLKS+((B#PTASK-1)*L'LP_BLKS)                                  
B#ORD    EQU   5                   IO3 - ORDER RECORD                           
B#EST    EQU   5                   IO3 - ESTIMATE RECORD                        
B#CAL    EQU   5                   IO3 - CALENDAR RECORD                        
B#GOBLK  EQU   6                   GOBLOCKB                                     
B#GOXBLK EQU   7                   GOBBLCKA                                     
B#GOBBLK EQU   8                   GOXBLCKA                                     
AORDREC  EQU   LP_BLKS+((B#TIMREC-1)*L'LP_BLKS)                                 
B#COBLCK EQU   9                       - COST PROFILE AREA                      
ACOSTPR  EQU   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)                                 
B#SVRDEF EQU   10                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   11                      - LP_D                                   
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         EJECT                                                                  
SAVED    DSECT                                                                  
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
ANCLKF   DS    A                   A(NCLKF)                                     
ANCLRF   DS    A                   A(NCLRF)                                     
ACLIKF   DS    A                   A(CLIKF)                                     
ACLIRF   DS    A                   A(CLIRF)                                     
ASCMKF   DS    A                   A(SCMKF)                                     
ASCMRF   DS    A                   A(SCMRF)                                     
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
VACCEMU  DS    A                   A(ACCEMU)                                    
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
ALOCEL   DS    A                   A(LOCEL)                                     
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAP#LLDL EQU   MAPI1                                                            
MAPSLLDL EQU   X'80'               LIM LIST DOWNLOAD                            
MAP#ONAL EQU   MAPI1                                                            
MAPSONAL EQU   X'40'               ONA LIST DOWNLOAD                            
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
RUNI     DS    0XL(L'RUNI1)                                                     
RUNI1    DS    X                   ** RUN INDICATOR BYTE1 1 **                  
RUN#MEDI EQU   RUNI1                                                            
RUNSMEDI EQU   X'80'               MEDIA LIDELD INIT                            
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN VALUES                     
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSARRECS DS    XL(TSPXTNL)                                                      
RJ_BUFF  DS    XL256                                                            
                                                                                
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPLPARM DS    XL1                                                              
ATSRERRS DS    XL1                 Error area for buffer                        
                                                                                
DSDICTL  DS    0C                                                               
AC@RWKAD DS    CL30                                                             
AC@KDTAD DS    CL30                                                             
AC@KDTAM DS    CL30                                                             
AC@CMTAD DS    CL30                                                             
AC@CMTDL DS    CL30                                                             
AC@CMTAM DS    CL30                                                             
AC@KSTAD DS    CL30                                                             
AC@KSTDL DS    CL30                                                             
AC@STAAM DS    CL30                                                             
AC@HLIDY DS    CL20                                                             
AC@OTHER DS    CL20                                                             
AC@APPTM DS    CL20                                                             
                                                                                
XLOCAL   DS    0D                  Local storage                                
BLNKLNS  DS    F                   Number of blank lines required               
ANXTELAR DS    A                   Address of next space on elearea             
APREVPER DS    A                   Address of previous period                   
NUMPRDS  DS    XL1                 Number of periods in table (AIO5)            
ELEMLN   DS    XL1                 Length of element                            
SVPKSKEY DS    XL64                Saved personal keystage key                  
SVEGNKEY DS    XL64                Saved estimate number key                    
SAVEKEY  DS    XL64                Saved key                                    
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
SAVEVAR  DS    0X                                                               
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
QNARCOD  DS    0CL6                                                             
QNARSTA  DS    CL6                                                              
QNAREND  DS    CL6                                                              
QNARLEN  EQU   *-QNARCOD                                                        
                                                                                
QINITIAL DS    CL1                 Initial                                      
QAPLINI  EQU   C'*'                Module initial call                          
QTM1ST   EQU   C'1'                Time 1st initial call                        
QTM2ND   EQU   C'2'                Time 2nd initial call                        
QCOCTS   DS    CL1                 Override create t/s from prior               
QCTYESQ  EQU   COOPTYQ             Yes                                          
QCTNOQ   EQU   COOPTNQ             No                                           
QCTST    EQU   COTEMPLA            Template                                     
QCOTSD   DS    CL1                 Override default timesheet                   
MYBYTE   DS    XL1                                                              
RUNITIM  EQU   X'40'               Time initial call                            
QLOCED   DS    PL(L'TIMKPEDT)      Location end date                            
QLOCST   DS    PL(L'TIMKPEDT)      Location start date                          
QLOCK    DS    PL(L'LOCLOCK)       Location lock date                           
QENDD    DS    PL(L'TIMKPEDT)      Period end date                              
QTIMDT   DS    PL(L'TIMKPEDT)      Timesheet date                               
QPERSN   DS    CL(L'TSWKPER)       Accounting Person code                       
QPID     DS    CL8                 PID character code                           
QPDF     DS    CL1                 Y/N PDF request - sends extra data           
QALL     DS    CL1                 Y/N Security to view all persons             
QMAN     DS    CL1                 Y/N Security to manager search               
         ORG   QVALUES+L'QVALUES                                                
QVALUESL EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
D_TODYP  DS    XL3                 Today's date                                 
D_TODYF  DS    CL6                                                              
D_TODYB  DS    XL3                                                              
D_TODYC  DS    XL2                                                              
                                                                                
DNARR    DS    0X                                                               
DNARRDS  DS    CL15                                                             
DNARROF  DS    CL2                                                              
DNARRCL  DS    CL5                                                              
DNARRPR  DS    CL2                                                              
DNARRWC  DS    CL2                                                              
DNARRAP  DS    CL6                                                              
DNARRFI  DS    CL4                                                              
DNARRGR  DS    CL1                                                              
DNARRLQ  EQU   *-DNARR                                                          
DORDNUM  DS    CL6                 Order number                                 
DESTNUM  DS    CL6                 Estimate number (global)                     
DCDAT    DS    PL(L'TRNKDATE)      Company start date                           
DCEND    DS    PL(L'TRNKDATE)      Company end date                             
DDATE    DS    PL(L'TRNKDATE)      Date for working with                        
DENDRNG  DS    0XL(L'PKSKEDT*2)    end date range                               
DENDSTR  DS    XL(L'PKSKEDT)       X'000000'                                    
DENDEND  DS    XL(L'PKSKEDT)       Start date                                   
DSTRRNG  DS    0XL(L'PKSKSDT*2)    start date range                             
DSTRSTR  DS    XL(L'PKSKSDT)       End date                                     
DSTREND  DS    XL(L'PKSKSDT)       X'FFFFFF'                                    
*                                                                               
D1NAIND  DS    X                                                                
DA1NA    DS    AL3                 Built Non Client Code in WMP                 
D1NAMAXQ EQU   200                                                              
                                                                                
DCLIIND  DS    X                                                                
DACLI    DS    AL3                 Built Client Code in WMP                     
DCLIMAXQ EQU   200                                                              
                                                                                
DCLIIND2 DS    X                                                                
DACLI2   DS    AL3                 Built Client Code in WMP                     
                                                                                
DOFFIND  DS    X                                                                
DAOFF    DS    AL3                 Built Office Code in WMP                     
DOFFMAXQ EQU   200                                                              
                                                                                
DTEXT    DS    CL1                 Y/N did we find day narrative first          
DINPUT   DS    CL1                 Y/N did we find input element                
D1RACT   DS    CL(L'ACTKACT)       1R account code                              
DPPID    DS    XL2                 PIN of users timesheet                       
*                                                                               
DINDS1   DS    XL1                 Edit Hours indicator                         
DIIOFF   EQU   X'80'               Office                                       
DIIDPT   EQU   X'40'               Department                                   
DIISDP   EQU   X'20'               Sub-Department                               
DIIPER   EQU   X'10'               Person                                       
DIISHOL  EQU   X'08'               Staff holiday                                
DINDS2   DS    XL1                 Time display indicator                       
DIILMGR  EQU   X'80'               Line manager connected                       
DIINAUD  EQU   X'40'               No audit record                              
DIIUSR   EQU   X'20'               User connected                               
DIIEMTY  EQU   X'10'               Empty timesheet for period                   
DIIPREV  EQU   X'08'               Previous timesheet used                      
DIIWIDG  EQU   X'04'               Timesheet is widget only                     
*                                                                               
DOVRDUED DS    PL(L'EMPTRM)        Overdue date                                 
*                                                                               
DV_HROW# DS    XL(L'TL_TROW)       Highest row number                           
*                                                                               
OVFLAG   DS    XL1                 Overdue time flag                            
OVRECFND EQU   X'40'               Found Record                                 
*                                                                               
DOVRDTE  DS    PL3                 OVERDUE DATE                                 
D1ROFF   DS    CL2                 OFFICE OF TIMESHEET                          
DNUMPRD  DS    XL1                 NUMBER OF PERIODS SAVED                      
*                                                                               
NTFY#    DS    XL2                 NUMBER OF DAYS  BEFORE NOTIFYING             
NTFYHRS  DS    XL2                 NUMBER OF HOURS BEFORE NOTIFYING             
NTFYDTE  DS    PL3                 DATE NOTIFICATIONS ARE SENT                  
LOCK#    DS    XL2                 NUMBER OF DAYS  BEFORE LOCKING               
LOCKHRS  DS    XL2                 NUMBER OF HOURS BEFORE LOCKING               
LOCKDTE  DS    PL3                 DATE LOCKS ARE SET                           
NTFYLNQ  EQU   *-NTFY#                                                          
*                                                                               
DSTAT    DS    XL1                 Status of timesheet                          
DPERENDT DS    XL3                 Period end date                              
DPHOLAC  DS    CL(L'ACTKULA)       Public holiday                               
DSHOLAC  DS    CL(L'ACTKULA)       Staff holiday                                
DVALUESL EQU   *-DVALUES                                                        
*                                                                               
AGENCY   DS    XL(L'CUXCPY)        Company code                                 
USRID    DS    XL(L'CUUSER)        Connected user id                            
GLOBID   DS    XL(L'CPYUID)        Global user id as defined on cpy rec         
*                                                                               
X#LLIND  DS    XL1                                                              
X#LLINI  EQU   X'80'               Limit list record found                      
X#CLIOF  EQU   X'40'               Read for clients filter by office            
X#ALCLI  EQU   X'20'               Read all clients                             
                                                                                
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
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
*                                                                               
SAVREJ   DS    XL(L'TIMRRPID+L'TIMRRDTE+L'TIMRRTME)                             
*                                                                               
WRKDATE  DS    PL(L'TRNKDATE)      Work date 1                                  
WRK2DTE  DS    PL(L'TRNKDATE)      Work date 2                                  
PREVDAYS DS    XL3                                                              
         DS    (TOTDAYS-1)XL(L'PREVDAYS)                                        
PREVDAYL EQU   *-PREVDAYS                                                       
*                                                                               
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
* OVERDUE TIMESHEET INITAL VALUES                                               
OOVERDUE DS    CL1                          Timesheets overdue                  
* GENERAL INITIAL VALUES                                                        
OGICODE  DS    CL1                 Approver record exists                       
OGILIST  DS    CL1                 Limit list record exists                     
OGINEXP  DS    CL1                 New expense workflow in use                  
OGIMBTIM DS    CL1                 Mobile time in use                           
OGIURL   DS    CL255               URL for agency                               
OGIUID   DS    CL8                 8 BYTE User ID                               
OGIAGCUR DS    CL3                 - Agency currency ISO code                   
OGIDEC   DS    XL1                 - Agency currency decimal places             
OGINAME  DS    CL36                Agency name                                  
OGITIMAP DS    CL1                 Timesheet line manager approver              
OPROLEN  DS    XL1                 Product length                               
OJOBLEN  DS    XL1                 Job length                                   
OTIMBAK  DS    XL1                 Time back up approver                        
OONRL2L  DS    XL1                 1R level 2                                   
OONRL3L  DS    XL1                 1R level 3                                   
OONRL4L  DS    XL1                 1R level 4                                   
OSTMNTH  DS    XL1                 Fiscal start month                           
OLTSCAL  DS    XL1                 Latest calendar year PWOS                    
                                                                                
* TIME INTIAL VALUES                                                            
TI_HIRE  DS    PL(L'EMPHIR)        Hire date                                    
TI_TERM  DS    PL(L'EMPTRM)        Termination date                             
TI_FCUR  DS    CL1                 Foreign currency indicator                   
TI_EDTHR DS    CL1                 Edit hours enforced                          
TI_INRN  DS    CL36                Internal reference name                      
TI_PRNS  DS    CL1                 Previous timesheets sub/not sub              
*                                                                               
TI_FUTA  DS    0X                  FUTURE TIME SETTINGS                         
TI_FUTB  DS    CL1                 future time billable                         
TI_RTIME DS    CL1                 future time r type                           
TI_BCLIT DS    CL1                 future time client non-bill                  
TI_BNCLI DS    CL1                 future time non-client non-bill              
TI_FUTAL EQU   *-TI_FUTA                                                        
                                                                                
         DS    0D                                                               
* TIME HEADER VALUES                                                            
TH_MANAP DS    XL(L'PIDNO)         Line manager approver PIN                    
TH_MANBA DS    XL(L'PIDNO)                                                      
         DS    (TH_BAMAX-1)XL(L'PIDNO) Line man back up approver PIN            
TH_BAMAX EQU   10                  maximum number of back up approvers          
TH_NUMBA DS    H                   Number of back up approvers                  
TH_LSTRW DS    XL(L'TIMEIDNO)      Last row number                              
TH_INDX  DS    XL(L'AUDKINDX)      Index                                        
TH_NDAYS DS    H                   Number of days                               
TH_DATE  DS    XL3                 Date                                         
         DS    (TOTDAYS-1)XL(L'TH_DATE)                                         
TH_DATEQ EQU   *-TH_DATE                                                        
TOTDAYS  EQU   31                  Max number of days                           
TH_RQHRS DS    PL4                 Required hours                               
TH_TUPCT DS    PL4                 Target Utilisation - percent                 
TH_STAVW DS    CL1                 Status to viewer                             
TH_STATS DS    CL1                 Status of timesheet                          
TH_JOBR  DS    CL1                 Job required                                 
TH_PROR  DS    CL1                 Product required                             
TH_DEFWC DS    CL2                 Default workcode                             
TH_BILL  DS    CL1                 Billable allowed                             
TH_CHRG  DS    CL1                 Chargeble allowed                            
TH_NBIL  DS    CL1                 Non billable allowed                         
TH_DEFT  DS    CL1                 Default type of time                         
TH_MCSU  DS    CL1                 Brandocean user                              
TH_PER#  DS    XL(L'TMPNUMB)       Period number                                
TH_UEML  DS    CL(L'APPEMAIL)      Users email address                          
TH_LMAP  DS    CL1                 Line manager approved                        
TH_PRNS  DS    CL1                 Previous timesheets sub/not sub              
TH_COPTA DS    CL1                 Disallow TS if previous not submittd         
TH_ACCS  DS    CL1                 Connected user has access via 1R             
TH_FNAME DS    CL16                First name                                   
TH_MNAME DS    CL16                Middle name                                  
TH_LNAME DS    CL58                Last name                                    
*                                                                               
TH_1ROFF DS    CL(L'LOCOFF)        1R office code                               
TH_DEPT  DS    CL6                 1R department code                           
TH_SDPT  DS    CL6                 1R sub-department code                       
TH_1RON  DS    CL36                1R office name                               
TH_DEPN  DS    CL36                1R department name                           
TH_SDPN  DS    CL36                1R sub-department name                       
*                                                                               
TH_APDTE DS    XL3                 Approved date                                
TH_APPID DS    XL2                 Who actually approved timesheet              
*                                                                               
         DS    0D                                                               
* TIME LINE VALUES                                                              
TL_HRS   DS    PL4                 Hours                                        
         DS    (TOTDAYS-1)XL(L'TL_HRS)                                          
*                                                                               
TL_CLIAP DS    XL(L'PIDNO)         Client manager approver PIN                  
TL_CLIBA DS    (TH_BAMAX)XL(L'PIDNO) Cli manager back up approver PIN           
TL_TROW  DS    XL(L'TIMEIDNO)      Time row number                              
TL_MROW  DS    XL(L'TIMEIDNO)      Materials row number                         
TL_IFLAG DS    CL1                 Materials flag                               
TL_PIDC  DS    CL8                 PID of approver                              
TL_BILA  DS    CL1                 Billable time allowed                        
TL_CHGA  DS    CL1                 Memo time allowed                            
TL_NONA  DS    CL1                 Non-billable time allowed                    
TL_FNTB  DS    CL1                 Force narrative on billable time             
TL_FNTR  DS    CL1                 Force narrative on memo time                 
TL_FNTN  DS    CL1                 Force narrative on non-billable              
TL_TOT   DS    CL1                 Default type of time                         
TL_JOBT  DS    CL1                 Job time allowed                             
TL_FPT   DS    CL1                 Force product time N, R, X, A                
TL_FJT   DS    CL1                 Force product time N, R, X, A                
TL_LOCBI DS    CL1                 Locked from billing                          
TL_FUTAL DS    CL1                 Future time allowed                          
TL_JOBL  DS    CL1                 Job locked status                            
TL_JOBC  DS    CL1                 Job closed status                            
TL_TLOC  DS    CL1                 Locked from t/s                              
TL_IUNIT DS    CL15                Units description                            
TL_OFF   DS    CL2                 Client office code                           
TL_MED   DS    CL1                 Media code                                   
*                                                                               
AC_FLTS  DS    0CL5                                                             
AC_FLT1  DS    CL1                                                              
AC_FLT2  DS    CL1                                                              
AC_FLT3  DS    CL1                                                              
AC_FLT4  DS    CL1                                                              
AC_FLT5  DS    CL1                                                              
AC_LVL   DS    CL1                                                              
*                                                                               
SAVEVARL EQU   *-SAVEVAR                                                        
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* included books and DSECTS                                           *         
***********************************************************************         
         SPACE 1                                                                
TMTTABD  DSECT                                                                  
TMTTEL   DS    XL1                 Identifier                                   
TMTTELQ  EQU   X'90'                                                            
TMTTLN   DS    XL1                 Length of table entry                        
TMTTYP   DS    CL1                 Time type                                    
TMTULA   DS    0CL14               Unit ledger account                          
TMTUNT   DS    CL1                 Unit                                         
TMTLDG   DS    CL1                 Ledger                                       
TMTACC   DS    CL12                Account                                      
TMTTSK   DS    CL2                 Task/workcode                                
TMTOFF   DS    CL2                 Client office                                
TMTMED   DS    CL1                 Media                                        
TMTGRY   DS    CL1                 Grey out line                                
TMTCLIAP DS    XL2                 Client approver PIN                          
TMTAPST  DS    CL1                 Approval status                              
TMTIDNO  DS    XL2                 Time row number                              
TMTIIDN  DS    XL2                 Material row number                          
TMTLN1Q  EQU   *-TMTTABD                                                        
TMTHRS   DS    PL4                 Hours                                        
                                                                                
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
         SPACE 1                                                                
***********************************************************************         
* Rejection comment audit buffer                                      *         
***********************************************************************         
         SPACE 1                                                                
REJ_D    DSECT                                                                  
REJ_LEN  DS    XL2                 Record length                                
REJ_KEY  DS    0X                                                               
REJ_PID  DS    XL(L'TIMRRPID)      PID of person                                
REJ_DATE DS    XL(L'TIMRRDTE)      Date                                         
REJ_TIME DS    XL(L'TIMRRTME)      Time                                         
REJ_ROW  DS    XL(L'TIMRIDNO)      Row number                                   
REJ_KEYL EQU   *-REJ_KEY                                                        
REJ_RLEN EQU   *-REJ_D             Basic record length                          
*                                                                               
REJ_DATA DS    0H                  COMMENTS                                     
REJ_LNQ  EQU   *-REJ_D                                                          
         SPACE 1                                                                
                                                                                
***********************************************************************         
* DAY TABLE DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
DAYTABD  DSECT                                                                  
DAY#     DS    XL1                 GETDAY Day #                                 
DAYEDT#  DS    XL1                 EDIT Hrs Day #                               
DAYTABLN EQU   *-DAYTABD                                                        
                                                                                
***********************************************************************         
* CALDAY TABLE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALDAYD  DSECT                                                                  
CALDAY#  DS    XL1                                                              
CALDAY   DS    PL3                                                              
CALHRS   DS    PL4                                                              
CALLNQ   EQU   *-CALDAYD                                                        
                                                                                
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
* Period table DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
PERTABD  DSECT                                                                  
PERENDT  DS    PL3                 Period end date 2's complement               
PERODS   DS    CL(L'TSWKODS)       office/dept/sub-dept for this period         
PERLOCE  DS    PL3                 location period end date                     
PERKEYQ  EQU   *-PERTABD                                                        
PERSTDT  DS    PL3                 Period start date                            
PERNUM   DS    XL1                 Period number                                
PERSTAT  DS    XL1                 Period status                                
PERSNFND EQU   X'80'               Not found                                    
PERSFOND EQU   X'40'               Found timesheet                              
PERSSKIP EQU   X'20'               skip this period as security lockout         
PERSREAD EQU   X'10'               Read for this period                         
PERSMCSU EQU   X'08'               MCS user for this period                     
PERSINVL EQU   X'04'               skip location as invalid for viewer          
PERLOCS  DS    PL3                 location period start date                   
PERLENQ  EQU   *-PERTABD                                                        
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
ACTRECD  DSECT                                                                  
         ORG   ACTKACT+L'ACTKACT                                                
ACTREST  DS    XL(L'ACTKEY-ACTKEND)                                             
         PRINT OFF                                                              
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
*&&UK                                                                           
         PRINT OFF                                                              
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACBRA27   02/03/21'                                      
         END                                                                    
