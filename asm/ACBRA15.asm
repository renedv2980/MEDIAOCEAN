*          DATA SET ACBRA15    AT LEVEL 041 AS OF 01/28/21                      
*PHASE T62415A                                                                  
*INCLUDE ACGETCRL                                                               
                                                                                
ACBRA15  TITLE '- BRA Expenses download Server'                                 
*                                                                               
                                                                                
***********************************************************************         
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 12MAY09 Rewrite DownLoads -                                          
* NRAK 002 07JAN10 relink to include CLDAPPID for UK                            
*      003                                                                      
* NRAK 004 15JAN10 <lo01-9471> don't need person to search                      
* NRAK 005 15APR10 <BR32535L> Bug in appr/date file reading                     
* NSHE 006 15JUN10 <UKCR00028266> Show future claims                            
* NSHE 007 05AUG10 Change how GAPLST is called                                  
* MPEN 008 15DEC10 <PR001254> Change all downloads to show job lock sta         
* NSHE 009 18MAY11 <PR001841> Return new mileage records                        
* JFOS 012 14NOV11 <PR001992> GAPLST changed parms/*&&UK mileage VAT            
* JFOS     29NOV11 <BR45886L> Allow for sequential DRT recs                     
* JFOS     09DEC11 <BR45888L> Ensure SAVEKEY4 set for PROCLL -> APRSTA          
* NSHE     12DEC11 Fix back up approver                                         
* YNGX     05JAN12 <BR46485L> BUG FIX RESTORING R1                              
* JFOS     03AUG12 <BR50581l> extend/ensure CA_COMMT doesn't overrun            
* MPEN     18APR12 <PR002889> Show breakdown of billable and billable           
*                             expense items in list/search                      
* NSHE     14AUG12 <BR51263L> Fix approver issue with 2 level                   
* JFOS 014 21AUG12 <BR51374L> Fix backup approver issue, WPP workflow           
* NRAK     18DEC12 <BR53176L> HANDLE -VE CLAIMS/WPP WORKFLOW                    
* YNGX 015 04SEP12 <BR51588L> Fix INCORRECT NAME ON EXPENSE CLAIM               
* NSHE 016 19JUL13 Change where SETFAC is set                                   
* JFOS 017 25SEP13 <DSBO-318> Fix backup approver issues, WPP workflow          
* NSHE 018 24SEP14 <DSRD-4448> Send xdata as HEX data with zero prefixs         
* NSHE 019 24Apr15 <DSBO-1406> Remove SETFAC routine                            
* NSHE 020 09Jun16 Don't return empty fields                                    
* NSHE     28Jun16 DSRD-12072 apply limit list and approver rules               
* NSHE 021 02Sep16 DSRD-13108 Show back up approver in display                  
* NSHE 022 28Oct16 DSRD-13860 Ensure job name isn't sent                        
* MPEN     03Nov16 DSRD-13894 Don't return approvers for del lines              
* NSHE 023 11Nov16 DSRD-13821 Return new type for claim history                 
* MPEN     17Nov16 DSRD-14117 Fix bad col skip                                  
* YNGX 024 28Nov16 DSRD-14211 Canadian GST/PST/Province changes                 
* MPEN     28Nov16 DSRD-14213 Canadian tax changes for audit                    
* MPEN     14Dec16 DSRD-14378 Fix for returning mileage when no mileage         
* MPEN 025 22JUN17 DSRD-16117 Fix for missing vehicle/engine/fuel name          
* NSHE 026 31JUL17 DSRD-14918 Fix back up approver for wpp workflow             
* RGUP 027 18Sep17 SPEC-16064 fix dont suppress zero amount.                    
* NSHE 028 25Jan18 DSRD-17599 Mileage down to 0.25 for NA                       
* NSHE     14Feb18 DSRD-18271 Date on mileage records is 1's complement         
* NSHE 029 29Aug18 DSRD-19479 Return 1R office override settings                
* MPEN 030 23Oct18 DSRD-20447 Relink for new DPAPASD                            
* MPEN 031 08Nov18 DSRD-20738 Expense currency and country                      
* NSHE 032 05Mar19 DSRD-21730 Fix office name missing                           
* NSHE     15Mar19 DSRD-22001 Fix bad audit response                            
* MPEN 034 08May19 DSRD-21614 Return extended MF reject comments                
* NSHE     13May19 DSRD-22017 Return currency rates on claim                    
* YNGX     24Jun19 DSRD-22931 Handle old claims when New VAT rules is           
*                             switched on.                                      
* NSHE 035 25Jul19 DSRD-23174 Remove check on VAT code and VAT acc              
* MPEN     31Jul19 DSRD-23340 Return agency currency as well                    
* YNGX 036 27Aug19 DSRD-23687 Fix bug in EDTVATN routine                        
* MPEN 037 28Nov19 DSRD-24629 Fix for the above                                 
* ABID 038 16MAY20 DSRD-25304 EXPENSES - M/F AND SERVICES TO SEND               
*                             PAYMENT INFO IN CLAIM DISP AND LIST CALLS         
* MPEN 039 08JUL20 DSRD-26551 Expense MF finance search                         
* MPEN 040 30NOV20 DSRD-27560 Expense report changes                            
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,+        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,B#TWA,TWAD,                               +        
               B#LP_D,LP_D,                                            +        
               B#EXPREC,EXCRECD,                                       +        
               B#VEHREC,VEHRECD,                                       +        
               B#FUEREC,FUERECD,                                       +        
               B#ENGREC,ENGRECD,                                       +        
               B#DTNREC,DTNRECD,                                       +        
               B#AUD,AUDRECD,                                          +        
               B#EST,ESTRECD,                                          +        
               B#BUFREC,CLM_D,                                         +        
               B#LMAPR,CH_LMAPD,                                       +        
               B#XDFREC,XDFRECD,                                       +        
               B#XDLREC,XDLRECD,                                       +        
               B#COBLCK,COBLOCKD)                                               
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO15**,R7,CLEAR=YES,RR=RE                                    
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
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE                                                   
         B     INIT03                                                           
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
RUNSTR   TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
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
                                                                                
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#EXPREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO6                 
         MVC   LP_BLKS+((B#XDFREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                 
         MVC   LP_BLKS+((B#AUD-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                    
         MVC   LP_BLKS+((B#EST-1)*L'LP_BLKS)(L'LP_BLKS),AIO5                    
         MVC   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)(L'LP_BLKS),ACOBLOCK             
         LA    R0,EX_BUFF                                                       
         ST    R0,LP_BLKS+((B#BUFREC-1)*L'LP_BLKS)                              
         LA    R0,CH_LMAP                                                       
         ST    R0,LP_BLKS+((B#LMAPR-1)*L'LP_BLKS)                               
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
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
PRCWRK   XC    QVALUES(QVALUESL),QVALUES                                        
         XC    DVALUES(DVALUESL),DVALUES                                        
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK02 DS    0H                                                               
*&&UK                                                                           
         CLI   LP_SENO,X'73'                                                    
         JNE   PRCWRK04                                                         
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JE    PRCWRK04                                                         
         MVC   TEMP(3),=X'A60701'                                               
         GOTOR VDATCON,DMCB,(1,TEMP),(2,CH_TODC)                                
         GOTOR (RF),(R1),,(1,CH_TODP)                                           
         J     EXITY                                                            
*&&                                                                             
PRCWRK04 GOTOR VDATCON,DMCB,(5,0),(2,CH_TODC)                                   
         GOTOR (RF),(R1),,(1,CH_TODP)                                           
         J     EXITY                                                            
                                                                                
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
                                                                                
RUNREQ04 XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
*                                                                               
         GOTOR INIOBUF                                                          
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQX                                                          
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
***********************************************************************         
*** Claim Search Download *********************************************         
***********************************************************************         
                                                                                
REQCLSD  LKREQ H,A#CLSD,OUTCLSD,NEXTREQ=REQCLAD                                 
                                                                                
View     LKREQ F,01,(D,B#SAVED,QS_CSVIE),CHAR,OLEN=L'QS_CSVIE,         X        
               MAXLEN=L'QS_CSVIE,TEXT=AC#VIEW,COL=*                             
FrDat    LKREQ F,02,(D,B#SAVED,QS_CSFDA),CDAT,OLEN=L'QS_CSFDA,         X        
               TEXT=AC#STRDT,COL=*                                              
ToDat    LKREQ F,03,(D,B#SAVED,QS_CSTDA),CDAT,OLEN=L'QS_CSTDA,         X        
               TEXT=AC#ENDDT,COL=*                                              
InPgr    LKREQ F,04,(D,B#SAVED,QS_INPRG),CHAR,OLEN=L'QS_INPRG,         +        
               MAXLEN=L'QS_INPRG,TEXT=AC#STT,COL=*                              
Submit   LKREQ F,05,(D,B#SAVED,QS_SUBMT),CHAR,OLEN=L'QS_SUBMT,         +        
               MAXLEN=L'QS_SUBMT,TEXT=AC#STT,COL=*                              
Apprvd   LKREQ F,06,(D,B#SAVED,QS_APPRD),CHAR,OLEN=L'QS_APPRD,         +        
               MAXLEN=L'QS_APPRD,TEXT=AC#STT,COL=*                              
Reject   LKREQ F,07,(D,B#SAVED,QS_REJED),CHAR,OLEN=L'QS_REJED,         +        
               MAXLEN=L'QS_REJED,TEXT=AC#STT,COL=*                              
Postd    LKREQ F,08,(D,B#SAVED,QS_POSTD),CHAR,OLEN=L'QS_POSTD,         +        
               MAXLEN=L'QS_POSTD,TEXT=AC#STT,COL=*                              
PartAp   LKREQ F,09,(D,B#SAVED,QS_PRTAP),CHAR,OLEN=L'QS_PRTAP,         +        
               MAXLEN=L'QS_PRTAP,TEXT=AC#STT,COL=*                              
*&&UK                                                                           
Claim    LKREQ F,10,(D,B#SAVED,QS_CSCNP),CHAR,OLEN=L'QS_CSCNP,         X        
               MAXLEN=L'QS_CSCNP,TEXT=AC#LCLNO,COL=*                            
*&&                                                                             
*&&US                                                                           
Claim    LKREQ F,10,(D,B#SAVED,QS_CSCNP),CHAR,OLEN=L'QS_CSCNP,         X        
               MAXLEN=L'QS_CSCNP,TEXT=(*,LCL#LIT),COL=*                         
*&&                                                                             
PerCd    LKREQ F,11,(D,B#SAVED,QS_CSPER),CHAR,OLEN=L'QS_CSPER,         X        
               MAXLEN=L'QS_CSPER,TEXT=AC#PRSN,COL=*                             
AppOv    LKREQ F,12,(D,B#SAVED,QS_OVAPP),CHAR,OLEN=L'QS_OVAPP,         X        
               MAXLEN=L'QS_OVAPP,TEXT=(*,LOV#LIT),COL=*                         
Office   LKREQ F,13,(D,B#SAVED,QS_OFFC),CHAR,OLEN=L'QS_OFFC,           X        
               MAXLEN=L'QS_OFFC,TEXT=AC#OFFC,COL=*                              
Dep      LKREQ F,14,(D,B#SAVED,QS_DEPC),CHAR,OLEN=L'QS_DEPC,           X        
               MAXLEN=L'QS_DEPC,TEXT=AC#DEPC                                    
Subd     LKREQ F,15,(D,B#SAVED,QS_SDPC),CHAR,OLEN=L'QS_SDPC,           X        
               MAXLEN=L'QS_SDPC,TEXT=AC#SUBDP                                   
Ety      LKREQ F,16,(D,B#SAVED,QS_ETYC),CHAR,OLEN=L'QS_ETYC,           X        
               MAXLEN=L'QS_ETYC,TEXT=AC#ETYPC                                   
Rcpt     LKREQ F,17,(D,B#SAVED,QS_RCPT),CHAR,OLEN=L'QS_RCPT,           X        
               MAXLEN=L'QS_RCPT,TEXT=AC#RCPTS                                   
Bill     LKREQ F,18,(D,B#SAVED,QS_BILL),CHAR,OLEN=L'QS_BILL,           X        
               MAXLEN=L'QS_BILL,TEXT=AC#BLB                                     
Curr     LKREQ F,19,(D,B#SAVED,QS_CURR),CHAR,OLEN=L'QS_CURR,           X        
               MAXLEN=L'QS_CURR,TEXT=AC#CURRC                                   
Vata     LKREQ F,20,(D,B#SAVED,QS_VATA),CHAR,OLEN=L'QS_VATA,           X        
               MAXLEN=L'QS_VATA,TEXT=AC#VATA                                    
Cliof    LKREQ F,21,(D,B#SAVED,QS_CLIO),CHAR,OLEN=L'QS_CLIO,           X        
               MAXLEN=L'QS_CLIO,TEXT=AC#RSOCC                                   
Cli      LKREQ F,22,(D,B#SAVED,QS_CLI),CHAR,OLEN=L'QS_CLI,             X        
               MAXLEN=L'QS_CLI,TEXT=AC#CLIC                                     
Pro      LKREQ F,23,(D,B#SAVED,QS_PRO),CHAR,OLEN=L'QS_PRO,             X        
               MAXLEN=L'QS_PRO,TEXT=AC#PROC                                     
Job      LKREQ F,24,(D,B#SAVED,QS_JOB),CHAR,OLEN=L'QS_JOB,             X        
               MAXLEN=L'QS_JOB,TEXT=AC#JOBC                                     
         LKREQ E                                                                
                                                                                
OUTCLSD  LKOUT H                                                                
                                                                                
OUTCLSDS LKOUT R,R#CLSD                                                         
Array    LKOUT C,1,(A,ARYCSRC),FILTROUT=TSTNFNR                                 
Array    LKOUT C,1,(A,ARYFSRC),FILTROUT=TSTFINR                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCSRC  LKOUT A,(R,NXTSRC),MULTIROW=Y,ROWNAME=CLM_D                            
Type     LKOUT C,01,CLM_TYPE,CHAR                                               
Number   LKOUT C,02,CLM_NUM,CHAR                                                
Descr    LKOUT C,03,CLM_DESC,CHAR                                               
Date     LKOUT C,04,CLM_DATE,CDAT                                               
Status   LKOUT C,05,CLM_STAT,(R,EDTSTAT)                                        
AprStat  LKOUT C,06,CLM_APST,CHAR,ND=Y                                          
FinRej   LKOUT C,07,CLM_STA2,(R,EDTFNRJ)                                        
Bhalf    LKOUT C,08,CLM_STA2,(R,EDTBHALF)                                       
Advanc   LKOUT C,09,CLM_CLST,(R,EDTCLDAD)                                       
ConUsr   LKOUT C,10,CLM_ABY,(R,EDTCON)                                          
Itms     LKOUT C,11,CLM_ITMS,SPAK                                               
Apprs    LKOUT C,12,CLM_APRS,SPAK                                               
Pid      LKOUT C,13,CLM_PIDB,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,14,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,16,(D,B#WORKD,WORK2),CHAR,LEN=58                               
UsrId    LKOUT C,17,CLM_UID,(U,#EDTUSR,$EDTUSR)                                 
Total    LKOUT C,18,CLM_TOT,SPAK                                                
Altn     LKOUT C,19,CLM_ALAM,SPAK                                               
IDnum    LKOUT C,20,CLM_ID,HEXD                                                 
AgyAdv   LKOUT C,21,CLM_ADVS,SPAK                                               
PRout    LKOUT P,CLM_1RAC,BLD1RAC                                               
Office   LKOUT C,22,(D,B#SAVED,CH_ULA),(U,#EDTOFF,$EDTOFF)                      
OffNam   LKOUT C,23,(D,B#SAVED,CH_ULA),(U,#EDTOFN,$EDTOFN)                      
Dept     LKOUT C,24,(D,B#SAVED,CH_ULA),(U,#EDTDPT,$EDTDPT)                      
DeptNm   LKOUT C,25,(D,B#SAVED,CH_ULA),(U,#EDTDPN,$EDTDPN)                      
SubDpt   LKOUT C,26,(D,B#SAVED,CH_ULA),(U,#EDTSUB,$EDTSUB)                      
SubNam   LKOUT C,27,(D,B#SAVED,CH_ULA),(U,#EDTSUN,$EDTSUN)                      
PerCode  LKOUT C,28,(D,B#SAVED,CH_ULA),(U,#EDTPER,$EDTPER)                      
BtchRf   LKOUT C,29,CLM_BREF,CHAR                                               
BtchNm   LKOUT C,30,CLM_BNAM,CHAR                                               
BtchDt   LKOUT C,31,CLM_BADD,CDAT                                               
BtchPr   LKOUT C,32,CLM_BPID,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,33,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,34,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,35,(D,B#WORKD,WORK2),CHAR,LEN=58                               
Apprdt   LKOUT C,36,CLM_APDT,PDAT                                               
ApprPID  LKOUT C,37,CLM_APPD,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,38,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,39,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,40,(D,B#WORKD,WORK2),CHAR,LEN=58                               
BatUId   LKOUT C,41,CLM_BUID,(U,#EDTUSR,$EDTUSR)                                
AprChs   LKOUT C,42,CLM_STA2,(R,EDTAPCH)                                        
Tbita    LKOUT C,43,CLM_BITM,SPAK                                               
Tnbita   LKOUT C,44,CLM_NBTM,SPAK                                               
*&&UK                                                                           
Ctry     LKOUT C,45,CLM_CTRY,LBIN,ND=Y                                          
Curr     LKOUT C,46,CLM_CURR,CHAR,ND=Y                                          
*&&                                                                             
PRout    LKOUT P,CLM_PFND,SETPFND                                               
Pymtot   LKOUT C,47,CLM_PTOT,SPAK,ND=Y,FILTROUT=TSTPFND,SKIPCOLS=2              
lstpref  LKOUT C,48,CLM_RPRF,CHAR,ND=Y                                          
Lstpdte  LKOUT C,49,CLM_RPDT,CDAT,ND=Y                                          
Pid      LKOUT C,50,CLM_SUBP,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
Subfn    LKOUT C,51,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
Submn    LKOUT C,52,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
Subln    LKOUT C,53,(D,B#WORKD,WORK2),CHAR,LEN=58                               
Subdt    LKOUT C,54,CLM_SUBD,PDAT,ND=Y                                          
*                                                                               
Array    LKOUT C,01,(A,ARYFCR)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYFCR   LKOUT A,(D,B#BUFREC,CLM_RFST),EOT=EOR,NEWEL=B,                +        
               ROWID=(CLM_FCD,CLM_FCIQ),ROWWIDTH=(V,CLM_FCLN)                   
                                                                                
FCCod    LKOUT C,01,CLM_FCCD,CHAR                                               
FCAmt    LKOUT C,02,CLM_FCAM,SPAK                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYFSRC  LKOUT A,(R,NXTSRC),MULTIROW=Y,ROWNAME=CLI_D                            
Type     LKOUT C,01,CLI_TYPE,CHAR                                               
Number   LKOUT C,02,CLI_NUM,CHAR                                                
*        LKOUT C,03                                                             
Date     LKOUT C,04,CLI_DATE,CDAT                                               
Status   LKOUT C,05,CLI_STAT,(R,EDTSTAT)                                        
AprStat  LKOUT C,06,CLI_APST,CHAR,ND=Y                                          
FinRej   LKOUT C,07,CLI_STA2,(R,EDTFNRJ)                                        
Bhalf    LKOUT C,08,CLI_STA2,(R,EDTBHALF)                                       
ConUsr   LKOUT C,10,CLI_ABY,(R,EDTCON)                                          
Pid      LKOUT C,13,CLI_PIDB,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,14,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,16,(D,B#WORKD,WORK2),CHAR,LEN=58                               
UsrId    LKOUT C,17,CLI_UID,(U,#EDTUSR,$EDTUSR)                                 
Office   LKOUT C,22,CLI_ULA,(U,#EDTOFF,$EDTOFF)                                 
OffNam   LKOUT C,23,CLI_ULA,(U,#EDTOFN,$EDTOFN)                                 
Dept     LKOUT C,24,CLI_ULA,(U,#EDTDPT,$EDTDPT)                                 
DeptNm   LKOUT C,25,CLI_ULA,(U,#EDTDPN,$EDTDPN)                                 
SubDpt   LKOUT C,26,CLI_ULA,(U,#EDTSUB,$EDTSUB)                                 
SubNam   LKOUT C,27,CLI_ULA,(U,#EDTSUN,$EDTSUN)                                 
PerCode  LKOUT C,28,CLI_ULA,(U,#EDTPER,$EDTPER)                                 
ApprPID  LKOUT C,37,CLI_LMPD,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,38,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,39,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,40,(D,B#WORKD,WORK2),CHAR,LEN=58                               
*&&UK                                                                           
Ctry     LKOUT C,45,CLI_CTRY,LBIN,ND=Y                                          
Curr     LKOUT C,46,CLI_CURR,CHAR,ND=Y                                          
*&&                                                                             
Pymtot   LKOUT C,47,CLI_IPTO,SPAK,ND=Y,FILTROUT=TSTPFND,               +        
               SKIPCOLS=SKIPPYS                                                 
SKIPPY   EQU   *                                                                
lstpref  LKOUT C,48,CLI_IPYR,CHAR,ND=Y                                          
Lstpdte  LKOUT C,49,CLI_IPYD,CDAT,ND=Y                                          
SKIPPYS  EQU   (*-SKIPPY)/LX_COLSL                                              
Iseq     LKOUT C,55,CLI_ISEQ,LBIN,ND=Y                                          
Idate    LKOUT C,56,CLI_IDTE,CDAT,ND=Y                                          
PRout    LKOUT P,CLI_IDTE,SETITDTE                                              
Ietype   LKOUT C,57,CLI_IETY,CHAR,ND=Y                                          
ExpCat   LKOUT C,58,CLI_IETY,(R,EDTETN)                                         
Ibill    LKOUT C,59,CLI_IBIL,CHAR,ND=Y                                          
Ircpt    LKOUT C,60,CLI_IRCP,CHAR,ND=Y                                          
Iexpa    LKOUT C,61,CLI_IEXP,CHAR,ND=Y,FILTROUT=TSTEXP,                +        
               SKIPCOLS=SKIPXPS                                                 
SKIPXP   EQU   *                                                                
Iexpn    LKOUT C,62,CLI_IEXP,(R,EDTANM)                                         
SKIPXPS  EQU   (*-SKIPXP)/LX_COLSL                                              
Ioff     LKOUT C,63,CLI_IOFF,CHAR,ND=Y                                          
Ivat     LKOUT C,64,CLI_IVTC,CHAR,ND=Y                                          
Ivatn    LKOUT C,65,CLI_IVTC,(R,EDTVATN),ND=Y                                   
Iamt     LKOUT C,66,CLI_IAMT,SPAK,ND=Y                                          
Iamt     LKOUT C,67,CLI_INET,SPAK,ND=Y                                          
Ivata    LKOUT C,68,CLI_IVMT,SPAK,ND=Y                                          
Ifamt    LKOUT C,69,CLI_IFMT,SPAK,ND=Y                                          
Ixrat    LKOUT C,70,CLI_IXRT,(R,EDTXRAT),ND=Y                                   
*&&US                                                                           
Ipsta    LKOUT C,71,CLI_IPSA,SPAK,ND=Y                                          
Iprvc    LKOUT C,72,CLI_IPRC,CHAR,ND=Y                                          
Iprvn    LKOUT C,73,CLI_IPRC,(R,SETPRVNM),ND=Y                                  
Ipstc    LKOUT C,74,CLI_IPST,CHAR,ND=Y                                          
Ipstn    LKOUT C,75,CLI_IPST,(R,EDTPSTN),ND=Y                                   
*&&                                                                             
Iwc      LKOUT C,76,CLI_IWRK,CHAR,ND=Y                                          
Iwcn     LKOUT C,77,CLI_IWRK,(R,EDTWCD),ND=Y                                    
Icli     LKOUT C,78,CLI_ISJU,(U,#EDTCLI,$EDTCLI),FILTROUT=TSTSJ,       +        
               SKIPCOLS=SKIPSJS                                                 
SKIPSJ   EQU   *                                                                
Iclin    LKOUT C,79,CLI_ISJU,(R,EDTCLN),ND=Y                                    
Ipro     LKOUT C,80,CLI_ISJU,(U,#EDTPRD,$EDTPRD),ND=Y                           
Ipron    LKOUT C,81,CLI_ISJU,(R,EDTPRN),ND=Y                                    
Ijob     LKOUT C,82,CLI_ISJU,(U,#EDTJOB,$EDTJOB),ND=Y                           
Ijnm     LKOUT C,83,CLI_ISJU,(R,EDTJBN),ND=Y                                    
SKIPSJS  EQU   (*-SKIPSJ)/LX_COLSL                                              
Ionar    LKOUT C,84,CLI_INAR,CHAR,ND=Y                                          
Ipayd    LKOUT C,85,CLI_IPYD,CDAT,ND=Y                                          
Ipayr    LKOUT C,86,CLI_IPYR,CHAR,ND=Y                                          
Ifnar    LKOUT C,87,CLI_FNAR,CHAR,ND=Y                                          
FCurdp   LKOUT C,88,(D,B#SAVED,CI_FCDP),LBIN,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read expense claim records for searching                            *         
***********************************************************************         
                                                                                
NXTSRC   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NLIS46              No                                           
                                                                                
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         MVC   CH_STADT,=XL2'0001' set dates up                                 
         MVC   CH_ENDDT,=XL2'FFFF'                                              
                                                                                
         CLC   QS_CSFDA,SPACES                                                  
         JNH   NSRC02                                                           
         MVC   CH_STADT,QS_CSFDA                                                
                                                                                
NSRC02   CLC   QS_CSTDA,SPACES                                                  
         JNH   NSRC04                                                           
         MVC   CH_ENDDT,QS_CSTDA                                                
                                                                                
NSRC04   XR    RE,RE               complement the dates                         
         ICM   RE,3,CH_STADT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_STADC                                                    
         XR    RE,RE                                                            
         ICM   RE,3,CH_ENDDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_ENDDC                                                    
                                                                                
NSRC06   OC    CCTPID,CCTPID       always check connected PID code              
         JNZ   NSRC08                                                           
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         J     QERROR                                                           
                                                                                
NSRC08   CLI   QS_CSVIE,QS_CSVBQ   If we have and back up approver              
         JE    NSRC30              ignore it                                    
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JE    *+12                                                             
         CLI   QS_CSVIE,QS_CSVFQ   If finance search check 1R and appr          
         JNE   NSRC09                                                           
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights           
         JE    NSRC09                                                           
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT9Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTSTA',GAPLPARM),('QEXPCLM',0)          
*                                                                               
NSRC09   CLC   QS_CSPER,SPACES     Have we got a person code                    
         JNH   NSRC30                                                           
         MVC   TEMP2(8),QS_CSPER                                                
         GOTOR (#ACCPFP,AACCPFP)                                                
         JE    NSRC10                                                           
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     QERROR                                                           
                                                                                
NSRC10   MVC   CH_PPID#,TEMP2+8                                                 
                                                                                
         CLI   QS_CSVIE,QS_CSVSQ   If staff search check limit list             
         JE    NSRC10A                                                          
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JE    NSRC10A                                                          
         CLI   QS_CSVIE,QS_CSVFQ                                                
         JNE   NSRC30                                                           
                                                                                
NSRC10A  CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights           
         JE    NSRC30                                                           
                                                                                
NSRC11   L     R2,AIO3                                                          
         USING PERRECD,R2                                                       
         LA    R3,PERRFST                                                       
         XR    R0,R0                                                            
         USING LOCELD,R3                                                        
                                                                                
NSRC12   CLI   LOCEL,0             End of record                                
         JE    NSRC18                                                           
         CLI   LOCEL,LOCELQ        Look for location elements                   
         JE    NSRC16                                                           
NSRC14   IC    R0,LOCLN                                                         
         AR    R3,R0                                                            
         J     NSRC12                                                           
                                                                                
NSRC16   LA    R1,CH_1RAC                                                       
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),LOCOFF                                                   
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),LOCDEPT                                                  
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),LOCSUB                                                   
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),QS_CSPER                                                 
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'CH_1RAC,CH_1RAC),DAC1RIND,DAC1RAXQ,    +        
               LP_D                                                             
         J     NSRC14                                                           
                                                                                
                                                                                
         USING LW_D,R2                                                          
NSRC18   XR    R2,R2               Point to list of 1R acs in WMP               
         ICM   R2,7,DAAC1R                                                      
         JZ    *+2                                                              
         XR    R0,R0               # of list e                                  
         ICM   R0,3,LW_NUMN                                                     
         LA    R3,LW_DATA2                                                      
                                                                                
         CLI   QS_CSVIE,QS_CSVFQ   Finance already read gaplst earlier          
         JE    NSRC20                                                           
         CLI   QS_CSVIE,QS_CSVRQ   Finance already read gaplst earlier          
         JE    NSRC20                                                           
                                                                                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
         JE    NSRC20                                                           
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
                                                                                
NSRC20   MVC   CH_1RAC,0(R3)                                                    
         LA    R1,CH_1RAC                                                       
         XC    GAPAREA2,GAPAREA2                                                
         GOTOR TT1RVGT             test against table                           
         JE    NSRC30                                                           
         AHI   R3,L'CH_1RAC        Go to next entry                             
         JCT   R0,NSRC20                                                        
         MVC   LP_ERROR,=AL2(AE$NACVC)  Error if no match                       
         J     QERROR                                                           
                                                                                
NSRC30   XC    CH_FSTAT,CH_FSTAT                                                
         LA    R1,CH_FSTAT                                                      
         LHI   RF,6                                                             
         LA    RE,STATAB                                                        
         USING STATABD,RE                                                       
         LA    R2,QS_INPRG                                                      
NSRC32   CLI   0(R2),YESQ                                                       
         JNE   NSRC34                                                           
         MVC   0(1,R1),STATRST                                                  
         LA    R1,1(R1)                                                         
NSRC34   LA    RE,STATABL(RE)                                                   
         LA    R2,1(R2)                                                         
         JCT   RF,NSRC32                                                        
         DROP  RE                                                               
                                                                                
NSRC36   CLI   QS_CSVIE,QS_CSVMQ   My claims?                                   
         JE    NSRC50              Yes                                          
         CLC   QS_CSPER,SPACES     Searching single person?                     
         JH    NSRC50              Yes, use 'my claims' logic                   
         CLI   QS_CSVIE,QS_CSVBQ   back up approver?                            
         JE    NSRC40              Yes                                          
         CLI   QS_CSVIE,QS_CSVFQ   Finance view                                 
         JE    NSRC48              Yes                                          
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view                          
         JE    NSRC48              Yes                                          
         GOTOR CLLAIM              *** by number ***                            
         JNE   QERROR                                                           
         J     NSRCX                                                            
*                                                                               
* BACK UP APPROVERS GET TO TRY OUT 1R FINANCE LIMACC AS WELL                    
*                                                                               
NSRC40   MVI   CH_FSTAT,EXCSSUBM                                                
         MVI   QL_CLVBY,QL_CLVSQ   Set status view for approvers                
         XC    MYBYTE,MYBYTE                                                    
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLBKAP',SPACES),('GAPLTSTA',GAPLPARM),('QEXPCLM',0)          
         JE    NSRC46                                                           
         MVC   LP_ERROR,FULL2                                                   
         J     QERROR                                                           
                                                                                
NSRC46   GOTOR CLLASS              *** approver by status ***                   
         J     NSRCX                                                            
                                                                                
NSRC48   GOTOR CLFASS              *** fin by status liml ovrride ***           
         J     NSRCX                                                            
*                                                                               
*  Single-person searching (Own/filtered approver/finance view)                 
*                                                                               
NSRC50   MVI   QL_CLOWN,QL_CLOMQ   Set to my view for PROCLL                    
         GOTOR CLLONE              *** own/status view ***                      
         JNE   QERROR                                                           
NSRCX    J     NLIS44                                                           
         EJECT                                                                  
***********************************************************************         
*** Claim AuditTrail download *****************************************         
***********************************************************************         
                                                                                
REQCLAD  LKREQ H,A#CLAD,OUTCLAD,NEXTREQ=REQCLLD                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,QD_CDTYP),CHAR,OLEN=L'QD_CDTYP,         +        
               MAXLEN=L'QD_CDTYP,TEXT=AC#TYPE,COL=*                             
Number   LKREQ F,02,(D,B#SAVED,QD_CDNUM),CHAR,OLEN=L'QD_CDNUM,         +        
               MAXLEN=L'QD_CDNUM,TEXT=AC#NUM,COL=*                              
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLAD  LKOUT H                                                                
                                                                                
OUTCLADS LKOUT R,R#CLAD                                                         
Array    LKOUT C,1,(A,ARYCAUD)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCAUD  LKOUT A,(R,NXTAUD),MULTIROW=Y,ROWNAME=AUDRECD                          
                                                                                
Array    LKOUT C,R#CLAD,(A,ARYSTC)                                              
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTC   LKOUT A,(D,B#AUD,AUDRFST),EOT=EOR,ROWID=(STCELD,STCELQ),      +        
               ROWWIDTH=(V,STCLN),NEWEL=B                                       
                                                                                
FrmSt    LKOUT C,01,STCDFR,CHAR,ND=Y                                            
ToSt     LKOUT C,02,STCDTO,CHAR,ND=Y                                            
Date     LKOUT C,03,STCDATE,PDAT,ND=Y                                           
PRout    LKOUT P,STCELD,PROCCOM                                                 
Time     LKOUT C,04,(D,B#SAVED,CA_TIME),CHAR,ND=Y                               
User     LKOUT C,05,STCUSER,(U,#EDTUSR,$EDTUSR),ND=Y                            
PidChr   LKOUT C,06,STCPERS,(U,#EDTPID,$EDTPID)                                 
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,07,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
LastNam  LKOUT C,08,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                          
PRout    LKOUT P,STCXTYP,SETTYPE                                                
Text     LKOUT C,09,STCCOMM,CHAR,LEN=V,ND=Y,FILTROUT=TESTCOM                    
Text     LKOUT C,09,STCXCOMT,CHAR,LEN=V,ND=Y,FILTROUT=TESTCOM2                  
Text     LKOUT C,09,(D,B#SAVED,CA_COMMT),CHAR,ND=Y,FILTROUT=TESTCOM3            
Type     LKOUT C,10,(D,B#SAVED,CA_TYP),CHAR,ND=Y,FILTROUT=TESTCOM4,    +        
               SKIPCOLS=2                                                       
Item     LKOUT C,11,(D,B#SAVED,CA_ITM),CHAR,ND=Y                                
Valu     LKOUT C,12,(D,B#SAVED,CA_VAL),CHAR,ND=Y                                
NewStyle LKOUT C,14,(D,B#SAVED,CA_NEW),CHAR,ND=Y                                
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
ItmNum   LKOUT C,16,STCXITEM,LBIN,ND=Y,FILTROUT=TESTIND,               +        
               SKIPCOLS=AUDSKIPS                                                
AUDSKIP  EQU   *                                                                
Type     LKOUT C,17,STCXTYP,LBIN,ND=Y                                           
XdCode   LKOUT C,18,STCXXDTC,(U,#EDTHEX,$EDTHEX),ND=Y,FILTROUT=TESTXDT,+        
               SKIPCOLS=4                                                       
PRout    LKOUT P,STCXXDTT,SETXDTYP                                              
XdDate   LKOUT C,19,STCXXDDT,PDAT,ND=Y,LEN=3,FILTROUT=TESTXDDT                  
XdAmnt   LKOUT C,19,STCXXDAT,SPAK,ND=Y,LEN=6,FILTROUT=TESTXDAM                  
XdData   LKOUT C,19,STCXXDCH,CHAR,ND=Y,LEN=V,FILTROUT=TESTXDCH                  
SupAcc   LKOUT C,20,STCXSULA,CHAR,ND=Y,FILTROUT=TESTITM,               +        
               SKIPCOLS=ITESKIPS                                                
ITESKIP  EQU   *                                                                
SupNam   LKOUT C,21,STCXSULA,(R,EDTANM)                                         
Etype    LKOUT C,22,STCXETYP,CHAR,ND=Y                                          
ExpCat   LKOUT C,23,STCXETYP,(R,EDTCAT)                                         
ExpNam   LKOUT C,24,(D,B#SAVED,CI_EXTNM),CHAR                                   
WC       LKOUT C,25,STCXWC,CHAR,ND=Y                                            
WCDesc   LKOUT C,26,STCXWC,(R,EDTWCD),ND=Y                                      
Amnt     LKOUT C,27,STCXAMNT,SPAK,ND=Y                                          
Cur      LKOUT C,28,STCXFCUR,CHAR,ND=Y                                          
FAmnt    LKOUT C,29,STCXFAMT,SPAK,ND=Y                                          
Narr     LKOUT C,30,STCXNARR,CHAR,ND=Y,LEN=V                                    
Est      LKOUT C,31,STCXEST#,CHAR,ND=Y                                          
PRout    LKOUT P,STCXEST#,SETEST                                                
Array    LKOUT C,32,(A,ARYESN),FILTROUT=TSTEST                                  
ExpAcc   LKOUT C,34,STCXXULA,CHAR,ND=Y                                          
ExpNam   LKOUT C,35,STCXXULA,(R,EDTANM)                                         
PRout    LKOUT P,STCXCPJ,BLDSJAC                                                
CliCod   LKOUT C,36,(D,B#SAVED,CI_SJULA),(U,#EDTCLI,$EDTCLI),          +        
               FILTROUT=TESTSJAC,SKIPCOLS=5                                     
CliNam   LKOUT C,37,(D,B#SAVED,CI_SJULA),(R,EDTCLN)                             
ProCod   LKOUT C,38,(D,B#SAVED,CI_SJULA),(U,#EDTPRD,$EDTPRD),          +        
               FILTROUT=TSTPROD,SKIPCOLS=3                                      
ProNam   LKOUT C,39,(D,B#SAVED,CI_SJULA),(R,EDTPRN)                             
JobCod   LKOUT C,40,(D,B#SAVED,CI_SJULA),(U,#EDTJOB,$EDTJOB),          +        
               FILTROUT=TSTJOB,SKIPCOLS=1                                       
JobNam   LKOUT C,41,(D,B#SAVED,CI_SJULA),(R,EDTJBN)                             
PRout    LKOUT P,STCXDATE,SETITDTE                                              
*&&UK                                                                           
Vat      LKOUT C,42,STCXVAT,CHAR,ND=Y                                           
VatName  LKOUT C,43,STCXVAT,(R,EDTVTNM),ND=Y                                    
*&&                                                                             
*&&US                                                                           
GST      LKOUT C,42,STCXGST,CHAR,ND=Y                                           
GSTName  LKOUT C,43,STCXGST,(R,EDTVATN),ND=Y                                    
*&&                                                                             
TwoP     LKOUT C,44,STCX2PA,CHAR,ND=Y                                           
PRout    LKOUT P,STCX2PA,BLD2PAC                                                
TwoPNm   LKOUT C,45,(D,B#SAVED,CI_2PULA),(R,EDTANM)                             
TwoD     LKOUT C,46,STCX2DA,CHAR,ND=Y                                           
PRout    LKOUT P,STCX2DA,BLD2DAC                                                
TwoDNm   LKOUT C,47,(D,B#SAVED,CI_2DULA),(R,EDTANM)                             
Miles    LKOUT C,48,STCXMILE,SPAK,ND=Y                                          
Date     LKOUT C,49,STCXDATE,CDAT,ND=Y                                          
Type     LKOUT C,50,STCXTYPE,CHAR,ND=Y                                          
Ref      LKOUT C,51,STCXREF,CHAR,ND=Y                                           
PRout    LKOUT P,STCELD,SETAUST                                                 
IsSel    LKOUT C,52,(D,B#SAVED,CI_SELTD),CHAR                                   
Recpt    LKOUT C,53,(D,B#SAVED,CI_RCPT),CHAR                                    
ITESKIPS EQU   (*-ITESKIP)/LX_COLSL                                             
Level    LKOUT C,54,STCXALVL,HEXD,ND=Y,FILTROUT=TESTAPRJ                        
VehCod   LKOUT C,55,STCXVEHC,CHAR,ND=Y                                          
VehNam   LKOUT C,56,STCXVEHC,(R,EDTVEH)                                         
FueCod   LKOUT C,57,STCXFUEL,CHAR,ND=Y                                          
FueNam   LKOUT C,58,STCXFUEL,(R,EDTFUEL)                                        
EngCod   LKOUT C,59,STCXENG,CHAR,ND=Y                                           
EngNam   LKOUT C,60,STCXENG,(R,EDTENG)                                          
Pass     LKOUT C,61,STCXPASS,LBIN,ND=Y                                          
Disclr   LKOUT C,62,(D,B#SAVED,CI_DISCL),CHAR,ND=Y                              
AUDSKIPS EQU   (*-AUDSKIP)/LX_COLSL                                             
PRout    LKOUT P,STCXTYP,SETTYPE                                                
ApLvl    LKOUT C,63,STCXAPLV,LBIN,ND=Y,FILTROUT=TESTAPR,SKIPCOLS=5              
ApPID    LKOUT C,64,STCXAPID,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,65,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,66,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,67,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                          
Applic   LKOUT C,68,STCXAPPL,LBIN,ND=Y                                          
*&&US                                                                           
Provin   LKOUT C,69,STCXPRV,CHAR,ND=Y                                           
Provnm   LKOUT C,70,STCXPRV,(R,SETPRVNM),ND=Y                                   
PRout    LKOUT P,STCXPRV,SETPRVC                                                
PSTCod   LKOUT C,71,STCXPST,CHAR,ND=Y                                           
PSTNam   LKOUT C,72,STCXPST,(R,EDTPSTN),ND=Y                                    
*&&                                                                             
Comsq    LKOUT C,73,STCXCSEQ,LBIN,ND=Y                                          
         LKOUT E                                                                
                                                                                
SETEST   L     R1,LP_AINP                                                       
         MVC   CI_ESTN,0(R1)                                                    
         BR    RE                                                               
                                                                                
PROCCOM  L     R4,LP_AINP                                                       
         USING STCELD,R4                                                        
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR PROCCMT                                                          
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
SETXDTYP L     R1,LP_AINP          Set type of cldel element                    
         MVC   CA_TYP,0(R1)                                                     
         J     EXIT                                                             
         USING STCELD,R2                                                        
SETAUST  L     R2,LP_AINP          Set audit status values                      
         MVI   CI_SELTD,NOQ                                                     
         MVI   CI_RCPT,NOQ                                                      
         MVI   CI_DISCL,NOQ                                                     
         TM    STCXISTA,STCXISEL      Is it selected                            
         JZ    *+8                                                              
         MVI   CI_SELTD,YESQ                                                    
         TM    STCXISTA,STCXIRCP      Has a receipt been included               
         JZ    *+8                                                              
         MVI   CI_RCPT,YESQ                                                     
         TM    STCXISTA,CIDMDIS       Has disclaimer been ticked                
         JZ    *+8                                                              
         MVI   CI_DISCL,YESQ                                                    
         GOTO1 VDATCON,DMCB,(2,STCXDATE),(1,QM_CDATE)                           
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
TESTXDDT CLI   CA_TYP,STCXTDT      Test xdata is date                           
         BR    RE                                                               
                                                                                
TESTXDAM CLI   CA_TYP,STCXTAMT     Test xdata is amount                         
         BR    RE                                                               
                                                                                
TESTSJAC CLC   PRODUL,CI_SJULA     Test production ledger                       
         BR    RE                                                               
                                                                                
TESTXDCH CLI   CA_TYP,STCXTCHR     Test xdata is character                      
         BER   RE                                                               
         CLI   CA_TYP,STCXTNUM     Test xdata is numerical                      
         BER   RE                                                               
         CLI   CA_TYP,STCXTYN      Test xdata is Yes/No                         
         BER   RE                                                               
         CLI   CA_TYP,STCXTDRP     Test xdata is dropdown list                  
         BR    RE                                                               
                                                                                
TESTAPRJ CLI   WORK,STCXAPP        Test status change is approval               
         BER   RE                                                               
         CLI   WORK,STCXCLMS       Test status change                           
         BER   RE                                                               
         CLI   WORK,STCXFCMT       Test finance comment                         
         BER   RE                                                               
         CLI   WORK,STCXREJ        Test status change is rejection              
         BR    RE                                                               
                                                                                
TESTAPR  CLI   WORK,STCXAPAD       Test approver added                          
         BER   RE                                                               
         CLI   WORK,STCXAPDL       Test approver deleted                        
         BR    RE                                                               
                                                                                
TESTXDT  CLI   WORK,STCXXDAM       Test status change is xdata amend            
         BER   RE                                                               
         CLI   WORK,STCXXDAD       Test status change is xdata add              
         BER   RE                                                               
         CLI   WORK,STCXXDDL       Test status change is xdata delete           
         BR    RE                                                               
                                                                                
TESTITM  CLI   WORK,STCXITAM       Test status change is item amend             
         BER   RE                                                               
         CLI   WORK,STCXITAD       Test status change is item add               
         BER   RE                                                               
         CLI   WORK,STCXITDL       Test status change is item delete            
         BR    RE                                                               
                                                                                
TESTCOM  CLI   BYTE1,0             Test comment is normal                       
         BR    RE                                                               
                                                                                
TESTCOM2 CLI   BYTE1,2             Test STCELD indicator                        
         BNER  RE                                                               
         CLI   WORK,STCXCLMS       Test status change is claim status           
         BER   RE                                                               
         CLI   WORK,STCXCMTA       Test claimant comments added                 
         BER   RE                                                               
         CLI   WORK,STCXCMTC       Test claimant comments changed               
         BER   RE                                                               
         CLI   WORK,STCXCMTD       Test claimant comments deleted               
         BER   RE                                                               
         CLI   WORK,STCXDESA       Test claim description added                 
         BER   RE                                                               
         CLI   WORK,STCXDESC       Test claim description changed               
         BER   RE                                                               
         CLI   WORK,STCXAPP        Test status change is approval               
         BER   RE                                                               
         CLI   WORK,STCXFCMT       Test finance comments for changes            
         BER   RE                                                               
         CLI   WORK,STCXREJ        Test status change is rejection              
         BR    RE                                                               
                                                                                
TESTCOM3 CLI   BYTE1,2             Test STCELD indicator                        
         BNER  RE                                                               
         CLI   WORK,STCXITAM       Test status change is item amend             
         BR    RE                                                               
                                                                                
TESTCOM4 CLI   BYTE1,1             Test comment is old style element            
         BR    RE                                                               
                                                                                
TESTIND  CLI   BYTE1,2             Test STCELD indicator                        
         BR    RE                                                               
***********************************************************************         
* Read expense claim audit records                                    *         
***********************************************************************         
                                                                                
NXTAUD   XC    CA_TYP(CA_LNQ),CA_TYP                                            
         XC    CA_COMMT,CA_COMMT                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTAUD02            No                                           
                                                                                
         XC    LDGAREA(LDGALNQ),LDGAREA  Get VAT ledger                         
         MVC   LDGAUL,=CL2'SG'                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   CH_SGOPO,LDGAOP                                                  
NXTAUD02 MVC   IOKEY,SAVEKEY2                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,AUDKEYT,('B#AUD',0),             +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
         MVC   SAVEKEY2,IOKEY                                                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
*** Claim List Download ***********************************************         
***********************************************************************         
                                                                                
REQCLLD  LKREQ H,A#CLLD,OUTCLLD,NEXTREQ=REQCLDD                                 
                                                                                
Owner    LKREQ F,01,(D,B#SAVED,QL_CLOWN),CHAR,OLEN=L'QL_CLOWN,         X        
               MAXLEN=L'QL_CLOWN,TEXT=AC#VWOW,COL=*                             
View     LKREQ F,02,(D,B#SAVED,QL_CLVBY),CHAR,OLEN=L'QL_CLVBY,         X        
               MAXLEN=L'QL_CLVBY,TEXT=AC#VWAL,COL=*                             
Type     LKREQ F,03,(D,B#SAVED,QL_CLTYP),CHAR,OLEN=L'QL_CLTYP,         X        
               MAXLEN=L'QL_CLTYP,TEXT=AC#TYPE,COL=*                             
FrDate   LKREQ F,04,(D,B#SAVED,QL_CLDFR),CDAT,OLEN=L'QL_CLDFR,         X        
               TEXT=AC#STRDT,COL=*                                              
EnDate   LKREQ F,05,(D,B#SAVED,QL_CLDTO),CDAT,OLEN=L'QL_CLDTO,         X        
               TEXT=AC#ENDDT,COL=*                                              
Status   LKREQ F,06,(D,B#SAVED,QL_CLSTA),CHAR,OLEN=L'QL_CLSTA,         X        
               MAXLEN=L'QL_CLSTA,TEXT=AC#STT,COL=*                              
GAOV     LKREQ F,07,(D,B#SAVED,QL_CLGAO),CHAR,OLEN=L'QL_CLGAO,         X        
               MAXLEN=L'QL_CLGAO,TEXT=AC#GAOV,COL=*                             
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLLD  LKOUT H                                                                
                                                                                
OUTCLLDS LKOUT R,R#CLLD                                                         
Array    LKOUT C,1,(A,ARYCLIS)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCLIS  LKOUT A,(R,NXTLIS),MULTIROW=Y,ROWNAME=CLM_D                            
Type     LKOUT C,01,CLM_TYPE,CHAR                                               
Number   LKOUT C,02,CLM_NUM,CHAR                                                
Descr    LKOUT C,03,CLM_DESC,CHAR                                               
Date     LKOUT C,04,CLM_DATE,CDAT                                               
Status   LKOUT C,05,CLM_STAT,(R,EDTSTAT)                                        
AprStat  LKOUT C,06,CLM_APST,CHAR,ND=Y                                          
FinRej   LKOUT C,07,CLM_STA2,(R,EDTFNRJ)                                        
Bhalf    LKOUT C,08,CLM_STA2,(R,EDTBHALF)                                       
Advanc   LKOUT C,09,CLM_CLST,(R,EDTCLDAD)                                       
ConUsr   LKOUT C,10,CLM_ABY,(R,EDTCON)                                          
Itms     LKOUT C,11,CLM_ITMS,SPAK                                               
Apprs    LKOUT C,12,CLM_APRS,SPAK                                               
Pid      LKOUT C,13,CLM_PIDB,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,14,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,16,(D,B#WORKD,WORK2),CHAR,LEN=58                               
UsrId    LKOUT C,17,CLM_UID,(U,#EDTUSR,$EDTUSR)                                 
Total    LKOUT C,18,CLM_TOT,SPAK                                                
Altn     LKOUT C,19,CLM_ALAM,SPAK                                               
IDnum    LKOUT C,20,CLM_ID,HEXD                                                 
AgyAdv   LKOUT C,21,CLM_ADVS,SPAK                                               
PRout    LKOUT P,CLM_1RAC,BLD1RAC                                               
Office   LKOUT C,22,(D,B#SAVED,CH_ULA),(U,#EDTOFF,$EDTOFF)                      
OffNam   LKOUT C,23,(D,B#SAVED,CH_ULA),(U,#EDTOFN,$EDTOFN)                      
Dept     LKOUT C,24,(D,B#SAVED,CH_ULA),(U,#EDTDPT,$EDTDPT)                      
DeptNm   LKOUT C,25,(D,B#SAVED,CH_ULA),(U,#EDTDPN,$EDTDPN)                      
SubDpt   LKOUT C,26,(D,B#SAVED,CH_ULA),(U,#EDTSUB,$EDTSUB)                      
SubNam   LKOUT C,27,(D,B#SAVED,CH_ULA),(U,#EDTSUN,$EDTSUN)                      
PerCode  LKOUT C,28,(D,B#SAVED,CH_ULA),(U,#EDTPER,$EDTPER)                      
BtchRf   LKOUT C,29,CLM_BREF,CHAR                                               
BtchNm   LKOUT C,30,CLM_BNAM,CHAR                                               
BtchDt   LKOUT C,31,CLM_BADD,CDAT                                               
BtchPr   LKOUT C,32,CLM_BPID,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,33,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,34,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,35,(D,B#WORKD,WORK2),CHAR,LEN=58                               
Apprdt   LKOUT C,36,CLM_APDT,PDAT                                               
ApprPID  LKOUT C,37,CLM_APPD,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,38,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
MidNam   LKOUT C,39,(D,B#WORKD,TEMP2+32),CHAR,LEN=16                            
LastNam  LKOUT C,40,(D,B#WORKD,WORK2),CHAR,LEN=58                               
BatUId   LKOUT C,41,CLM_BUID,(U,#EDTUSR,$EDTUSR)                                
AprChs   LKOUT C,42,CLM_STA2,(R,EDTAPCH)                                        
Tbita    LKOUT C,43,CLM_BITM,SPAK                                               
Tnbita   LKOUT C,44,CLM_NBTM,SPAK                                               
*&&UK                                                                           
Ctry     LKOUT C,45,CLM_CTRY,LBIN,ND=Y                                          
Curr     LKOUT C,46,CLM_CURR,CHAR,ND=Y                                          
*&&                                                                             
PRout    LKOUT P,CLM_PFND,SETPFND                                               
Pymtot   LKOUT C,47,CLM_PTOT,SPAK,ND=Y,FILTROUT=TSTPFND,SKIPCOLS=2              
lstpref  LKOUT C,48,CLM_RPRF,CHAR,ND=Y                                          
Lstpdte  LKOUT C,49,CLM_RPDT,CDAT,ND=Y                                          
*                                                                               
array    LKOUT C,01,(A,ARYFCR1)                                                 
         LKOUT E                                                                
                                                                                
ARYFCR1  LKOUT A,(D,B#BUFREC,CLM_RFST),EOT=EOR,NEWEL=B,                +        
               ROWID=(CLM_FCD,CLM_FCIQ),ROWWIDTH=(V,CLM_FCLN)                   
                                                                                
FCCod    LKOUT C,01,CLM_FCCD,CHAR                                               
FCAmt    LKOUT C,02,CLM_FCAM,SPAK                                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read expense claim records for listing                              *         
***********************************************************************         
                                                                                
NXTLIS   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NLIS46              No                                           
                                                                                
NLIS04   GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         MVC   CH_STADT,=XL2'0001' set dates up                                 
         MVC   CH_ENDDT,=XL2'FFFF'                                              
                                                                                
         CLC   QL_CLDFR,SPACES                                                  
         JNH   NLIS10                                                           
         MVC   CH_STADT,QL_CLDFR                                                
                                                                                
NLIS10   CLC   QL_CLDTO,SPACES                                                  
         JNH   NLIS12                                                           
         MVC   CH_ENDDT,QL_CLDTO                                                
                                                                                
NLIS12   XR    RE,RE               complement the dates                         
         ICM   RE,3,CH_STADT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_STADC                                                    
         XR    RE,RE                                                            
         ICM   RE,3,CH_ENDDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_ENDDC                                                    
                                                                                
         GOTOR SETFSTA,QL_CLSTA                                                 
         JE    NLIS14                                                           
                                                                                
         MVC   LP_ERROR,=AL2(AE$STAIN)                                          
         J     XERROR                                                           
                                                                                
NLIS14   OC    CCTPID,CCTPID       always check connected PID code              
         JNZ   NLIS16                                                           
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         J     XERROR                                                           
                                                                                
NLIS16   GOTOR SETPERD                                                          
                                                                                
NLIS18   CLI   QL_CLOWN,QL_CLOMQ   view own expenses?                           
         JE    NLIS20                                                           
                                                                                
         CLI   QL_CLOWN,QL_CLOFQ   finance approval view?                       
         JE    NLIS22                                                           
                                                                                
         CLI   QL_CLOWN,QL_CLOAQ   approvals view?                              
         JE    NLIS22                                                           
                                                                                
         DC    H'0'                invalid view                                 
                                                                                
NLIS20   GOTOR CLLONE              *** own/status view ***                      
         JE    NLIS44                                                           
         J     XERROR                                                           
                                                                                
NLIS22   MVI   BYTE1,GAPTT22Q      yes - set to read approvers                  
         MVI   BYTE2,GAPLTDTE      Set get approvers by date                    
         CLI   QL_CLVBY,QL_CLVSQ   Status view for approvers                    
         JNE   *+8                 No                                           
         MVI   BYTE2,GAPLTSTA      Yes - set get approvers by status            
         CLI   QL_CLOWN,QL_CLOFQ   finance approval view?                       
         JNE   NLIS24                                                           
         MVI   BYTE1,GAPTT9Q                                                    
                                                                                
NLIS24   GOTOR (#GAPLST,AGAPLST),DMCB,(BYTE1,TSARABUF),                +        
               ('GAPLAPPR',SPACES),(BYTE2,GAPLPARM),('QEXPCLM',0)               
                                                                                
NLIS26   CLI   QL_CLVBY,QL_CLVSQ   Status view for approvers                    
         JNE   NLIS34              No                                           
                                                                                
         CLI   QL_CLOWN,QL_CLOFQ   Finance approval view?                       
         JE    NLIS28              Yes                                          
         CLI   CH_FSTAT,EXCSFNTA   For 'approved' ...                           
         JE    NLIS30                                                           
         CLI   CH_FSTAT,EXCSREJE   For rejected                                 
         JE    NLIS30                                                           
NLIS28   CLI   CH_FSTAT,EXCSCOMP   For posted                                   
         JNE   NLIS32                                                           
                                                                                
NLIS30   GOTO1 VDATCON,DMCB,(2,CH_TODC),(0,TEMP2)                               
         GOTO1 VADDAY,PARM,(C'M',TEMP2),TEMP2+6,1                               
         GOTO1 VDATCON,DMCB,(0,TEMP2+6),(2,CH_ENDDT)                            
                                                                                
         GOTO1 VDATCON,DMCB,(2,CH_TODC),(0,TEMP2)                               
         ZAP   DUB,CH_EBV                                                       
         CVB   R3,DUB                                                           
         LNR   R3,R3               R3=number of months back                     
         GOTO1 VADDAY,PARM,(C'M',TEMP2),TEMP2+6,(R3)                            
         GOTO1 VDATCON,DMCB,(0,TEMP2+6),(2,CH_STADT)                            
                                                                                
         XR    RE,RE               new start date (profile dependant)           
         ICM   RE,3,CH_STADT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_STADC                                                    
         XR    RE,RE                                                            
         ICM   RE,3,CH_ENDDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,3,CH_ENDDC                                                    
                                                                                
NLIS32   GOTOR CLLASS              Looking for claims by status                 
         GOTOR CLLAPS              claims by approver + status                  
         J     NLIS44                                                           
                                                                                
NLIS34   GOTOR CLLADV              Appovals by date view                        
         GOTOR CLLAPS              claims by approver + status                  
         J     NLIS44                                                           
                                                                                
NLIS44   MVI   LP_RMODE,LP_RNEXT   NXTSRC JUMPS HERE! UGH                       
         LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NLIS48                                                           
                                                                                
NLIS46   LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
                                                                                
NLIS48   TM    TSARERRS,TSEEOF                                                  
         JNZ   NLIS100                                                          
         LA    R0,EX_BUFF                                                       
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
                                                                                
NLIS100  MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
*** Claim Display Download ********************************************         
***********************************************************************         
                                                                                
REQCLDD  LKREQ H,A#XDIS,OUTCLDD,NEXTREQ=REQCLMD                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,QD_CDTYP),CHAR,OLEN=L'QD_CDTYP,         X        
               MAXLEN=L'QD_CDTYP,TEXT=AC#TYPE,COL=*                             
Number   LKREQ F,02,(D,B#SAVED,QD_CDNUM),CHAR,OLEN=L'QD_CDNUM,         +        
               MAXLEN=L'QD_CDNUM,TEXT=AC#NUM,COL=*                              
View     LKREQ F,03,(D,B#SAVED,QD_CDVIW),CHAR,OLEN=L'QD_CDVIW,         +        
               MAXLEN=L'QD_CDVIW,TEXT=AC#VWOW,COL=*                             
Override LKREQ F,04,(D,B#SAVED,QD_CDOVR),CHAR,OLEN=L'QD_CDOVR,         +        
               MAXLEN=L'QD_CDOVR,TEXT=(*,OVERLIT),COL=*                         
Search   LKREQ F,05,(D,B#SAVED,QD_CDSRC),CHAR,OLEN=L'QD_CDSRC,         +        
               MAXLEN=L'QD_CDSRC,TEXT=(*,SRCHLIT),COL=*                         
PDF      LKREQ F,06,(D,B#SAVED,QD_CDPDF),CHAR,OLEN=L'QD_CDPDF,         +        
               MAXLEN=L'QD_CDPDF,TEXT=(*,XPDFLIT),COL=*                         
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLDD  LKOUT H                                                                
                                                                                
CLMHDR   LKOUT R,R#XDHDR                                                        
Array    LKOUT C,1,(A,ARYHDR)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
CLMTRL   LKOUT R,R#XDTRL                                                        
Prout    LKOUT P,,BLDTRL                                                        
FinDis   LKOUT C,01,(D,B#SAVED,CH_FNDIS),CHAR                                   
                                                                                
         LKOUT E                                                                
                                                                                
CLMAUD   LKOUT R,R#CLAD                                                         
Array    LKOUT C,1,(A,ARYCAUD),FILTROUT=TSTXAUD                                 
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYHDR   LKOUT A,(R,NXTEXP),MULTIROW=Y,ROWNAME=EXCRECD                          
Type     LKOUT C,01,EXCKTYPE,CHAR,FILTROUT=TSTHDR,SKIPCOLS=HDRSKIPS             
HDRSKIP  EQU   *                                                                
PRout    LKOUT P,,SETHDR                                                        
Number   LKOUT C,02,EXCKREF,CHAR                                                
Date     LKOUT C,03,(D,B#SAVED,CH_DATE),CDAT                                    
Status   LKOUT C,04,EXCRSTAT,(R,EDTSTAT)                                        
FinRej   LKOUT C,05,EXCRSTA2,(R,EDTFNRJ)                                        
Behalf   LKOUT C,06,EXCRSTA2,(R,EDTBHALF)                                       
Pid      LKOUT C,07,EXCKPIDB,(U,#EDTPID,$EDTPID)                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,08,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,09,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,10,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                          
PRout    LKOUT P,EXCK1RAC,BLD1RAC                                               
Office   LKOUT C,11,(D,B#SAVED,CH_ULA),(U,#EDTOFF,$EDTOFF)                      
OffNam   LKOUT C,12,(D,B#SAVED,CH_ULA),(U,#EDTOFN,$EDTOFN)                      
Dept     LKOUT C,13,(D,B#SAVED,CH_ULA),(U,#EDTDPT,$EDTDPT)                      
DeptNm   LKOUT C,14,(D,B#SAVED,CH_ULA),(U,#EDTDPN,$EDTDPN)                      
SubDpt   LKOUT C,15,(D,B#SAVED,CH_ULA),(U,#EDTSUB,$EDTSUB)                      
SubNam   LKOUT C,16,(D,B#SAVED,CH_ULA),(U,#EDTSUN,$EDTSUN)                      
PerCode  LKOUT C,17,(D,B#SAVED,CH_ULA),(U,#EDTPER,$EDTPER)                      
Itms     LKOUT C,19,EXCRNITM,SPAK                                               
Apprs    LKOUT C,20,EXCRNAPP,SPAK                                               
Array    LKOUT C,21,(A,ARYCLD)                                                  
Array    LKOUT C,35,(A,ARYCBI)                                                  
Array    LKOUT C,37,(A,ARYCLDC)                                                 
ApFrnt   LKOUT C,38,EXCRSTA2,(R,EDTCAE)                                         
MilRate  LKOUT C,39,(D,B#SAVED,CH_COMIL),CHAR                                   
U1ROfBl  LKOUT C,40,(D,B#SAVED,CH_1RBIL),CHAR                                   
U1ROfNb  LKOUT C,41,(D,B#SAVED,CH_1RNBL),CHAR                                   
PROUT    LKOUT P,,GETOFC                                                        
*&&UK                                                                           
Country  LKOUT C,42,(D,B#SAVED,D_CTRY),LBIN,ND=Y                                
Currency LKOUT C,43,(D,B#SAVED,D_CURR),CHAR,ND=Y                                
*&&                                                                             
Array    LKOUT C,1,(A,ARYCLDL)                                                  
Array    LKOUT C,1,(A,ARYLMA)                                                   
Array    LKOUT C,2,(A,ARYCLDF)                                                  
*&&UK                                                                           
Array    LKOUT C,R#CURR,(A,ARYCUR),FILTROUT=TSTMLTC                             
*&&                                                                             
HDRSKIPS EQU   (*-HDRSKIP)/LX_COLSL                                             
Array    LKOUT C,R#XDITM,(A,ARYEXPI)                                            
Array    LKOUT C,R#XDXDT,(A,ARYXDAT)                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLD   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CLDELD,CLDELQ),   +        
               ROWWIDTH=(V,CLDLN)                                               
PRout    LKOUT P,CLDTYPE,SETTYPE                                                
IsConUs  LKOUT C,21,CLDPIDAD,(R,EDTCON),FILTROUT=TSTTHDR,              +        
               SKIPCOLS=DETSKIPS                                                
DETSKIP  EQU   *                                                                
Desc     LKOUT C,22,CLDDESC,CHAR,LEN=V                                          
Advans   LKOUT C,23,CLDSTAT,(R,EDTCLDAD),ND=Y                                   
Supprs   LKOUT C,24,CLDSTAT,(R,EDTCLDZA),ND=Y                                   
UsrId    LKOUT C,25,CLDUID,(U,#EDTUSR,$EDTUSR),ND=Y                             
BUsrId   LKOUT C,26,CLDBUID,(U,#EDTUSR,$EDTUSR),ND=Y                            
Total    LKOUT C,27,CLDTAMT,SPAK                                                
PRout    LKOUT P,CLDTAMT,SETTOTL                                                
IDnum    LKOUT C,28,CLDIDN,HEXD                                                 
BtchRf   LKOUT C,29,CLDBREF,CHAR,ND=Y                                           
BtchDt   LKOUT C,30,CLDBADD,CDAT,ND=Y                                           
BtchPr   LKOUT C,31,CLDBABY,(U,#EDTPID,$EDTPID),ND=Y                            
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,32,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,33,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,34,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                          
DETSKIPS EQU   (*-DETSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYCBI   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CBIELD,CBIELQ),   +        
               ROWWIDTH=(V,CBILN)                                               
BtchNm   LKOUT C,35,CBIBNA,CHAR,ND=Y                                            
BtchCom  LKOUT C,36,CBIBCO,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLDC  LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CLDELD,CLDELQ),   +        
               ROWWIDTH=(V,CLDLN)                                               
PRout    LKOUT P,CLDTYPE,SETTYPE                                                
ClComt   LKOUT C,37,CLDCCMT,CHAR,LEN=V,FILTROUT=TSTTCMT                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLDL  LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CLDELD,CLDELQ),   +        
               ROWWIDTH=(V,CLDLN),NEWEL=B                                       
PRout    LKOUT P,CLDTYPE,SETTYPE                                                
Array    LKOUT C,1,(A,ARYLMAP),FILTROUT=TSTTLMA                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYLMAP  LKOUT A,(*,CLDLNTRY),ROWNAME=CLDEL,NROWS=*,ROWWIDTH=L'CLDLNTRY+        
               ,NEWEL=B                                                         
PRout    LKOUT P,CLDLSTAT,CHKFIN                                                
PRout    LKOUT P,CLDLNTRY,GETMAP                                                
                                                                                
         LKOUT E                                                                
ARYLMA   LKOUT A,(D,B#LMAPR,CH_LMVAL),NROWS=CH_MAXAP,                  +        
               ROWWIDTH=CH_LMAPL,NEWEL=B,ROWNAME=CH_LMAPD                       
                                                                                
LmStat   LKOUT C,1,CH_LMSTA,(R,EDTAPST),FILTROUT=TSTLMA,               +        
               SKIPCOLS=LMASKIPS                                                
LMASKIP  EQU   *                                                                
PID      LKOUT C,2,CH_LMPID,(U,#EDTPID,$EDTPID),ND=Y                            
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,5,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
Value    LKOUT C,6,CH_LMVAL,SPAK                                                
Level    LKOUT C,7,CH_LMLVL,LBIN                                                
Array    LKOUT C,5,(A,ARYLMB),FILTROUT=FLTAURA                                  
LMASKIPS EQU   (*-LMASKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYLMB   LKOUT A,(*,CH_LMBUP),NROWS=CH_MXBUQ,ROWWIDTH=L'CH_LMPID,      +        
               ROWNAME=CH_LMAPD,NEWEL=B                                         
PID      LKOUT C,1,CH_LMBUP,(U,#EDTPID,$EDTPID),ND=Y                            
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,2,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,3,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,4,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLDF  LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CLDELD,CLDELQ),   +        
               ROWWIDTH=(V,CLDLN),NEWEL=Y                                       
PRout    LKOUT P,CLDTYPE,SETTYPE                                                
Array    LKOUT C,1,(A,ARYFNAP),FILTROUT=TSTTFNA                                 
                                                                                
         LKOUT E                                                                
*&&UK                                                                           
ARYCUR   LKOUT A,(R,NXTCUR),MULTIROW=Y,ROWNAME=SAVED                            
                                                                                
ISOCod   LKOUT C,1,CL_CUR,CHAR,ND=Y                                             
Name     LKOUT C,2,CL_NAM,CHAR,ND=Y                                             
DecPlc   LKOUT C,3,CL_DEC,LBIN                                                  
Pos      LKOUT C,4,CL_POS,LBIN,ND=Y                                             
Rule     LKOUT C,5,CL_RUL,CHAR,ND=Y                                             
MinRte   LKOUT C,6,CL_MIN,HEXD,ND=Y                                             
MaxRte   LKOUT C,7,CL_MAX,HEXD,ND=Y                                             
Client   LKOUT C,8,CL_CLI,CHAR,ND=Y                                             
NoPro    LKOUT C,9,CL_NPC,CHAR,ND=Y                                             
                                                                                
         LKOUT E                                                                
*&&                                                                             
                                                                                
ARYFNAP  LKOUT A,(*,CLDLNTRY),ROWNAME=CLDEL,NROWS=*,ROWWIDTH=L'CLDLNTRY         
FnStat   LKOUT C,1,CLDFSTAT,(R,EDTAPST)                                         
PRout    LKOUT P,,GETFAP                                                        
PID      LKOUT C,2,(D,B#SAVED,CH_FNPID),(U,#EDTPID,$EDTPID),ND=Y                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,5,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
CUsrFin  LKOUT C,6,(D,B#SAVED,CH_USRFN),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYEXPI  LKOUT A,(D,B#EXPREC,EXCRFST),NEWEL=B,EOT=EOR,                 +        
               ROWID=(CIDEL,CIDELQ),ROWWIDTH=(V,CIDLN),                +        
               SETFILT=(CIDSEQ,L'CIDSEQ)                                        
                                                                                
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
ItmNum   LKOUT C,1,CIDSEQ,LBIN,FILTROUT=TSTTMAIN,                      +        
               SKIPCOLS=CLISKIPS                                                
CLISKIP  EQU   *                                                                
ItmDte   LKOUT C,2,CIDMDAT,CDAT                                                 
PRout    LKOUT P,CIDMDAT,SETITDTE                                               
ExpCat   LKOUT C,3,CIDMEXP,(R,EDTCAT)                                           
ExpTyp   LKOUT C,4,CIDMEXP,CHAR                                                 
ExpNam   LKOUT C,5,(D,B#SAVED,CI_EXTNM),CHAR                                    
ItmTyp   LKOUT C,6,CIDMTYP,CHAR,ND=Y                                            
PRout    LKOUT P,CIDMTYP,SETITYPE                                               
Receipt  LKOUT C,7,CIDMREC,CHAR,ND=Y                                            
Curr     LKOUT C,8,CIDMCUR,CHAR,ND=Y                                            
ExcRat   LKOUT C,9,CIDMRAT,(R,EDTXRAT),ND=Y                                     
PRout    LKOUT P,CIDMSTA,ITMSTAT                                                
ItmDel   LKOUT C,10,(D,B#SAVED,CI_ITMDL),CHAR,ND=Y                              
Selectd  LKOUT C,11,(D,B#SAVED,CI_SELTD),CHAR,ND=Y                              
NetInp   LKOUT C,12,(D,B#SAVED,CI_NETIN),CHAR,ND=Y                              
AgyInpt  LKOUT C,13,(D,B#SAVED,CI_AGYIN),CHAR,ND=Y                              
IntRef   LKOUT C,14,CIDMINT,CHAR,ND=Y                                           
Ref      LKOUT C,15,CIDMREF,CHAR,ND=Y                                           
Office   LKOUT C,16,CIDMOFF,CHAR,ND=Y                                           
OffNam   LKOUT C,17,CIDMOFF,(R,EDTOFN),ND=Y                                     
WC       LKOUT C,18,CIDMWCD,CHAR,ND=Y                                           
WCDesc   LKOUT C,19,CIDMWCD,(R,EDTWCD),ND=Y                                     
UntDist  LKOUT C,20,CIDMMIL,SPAK,ND=Y                                           
NumPass  LKOUT C,21,CIDMPASS,LBIN,ND=Y                                          
VehCod   LKOUT C,22,CIDMVEH,CHAR,ND=Y,FILTROUT=TSTVEH,SKIPCOLS=VHCSKIPS         
VHCSKIP  EQU   *                                                                
PRout    LKOUT P,CIDELD,SETDRT                                                  
DisAcumd LKOUT C,23,CIDMACDS,SPAK,ND=Y                                          
FuelCd   LKOUT C,24,CIDMFUEL,CHAR,ND=Y                                          
EnglCd   LKOUT C,25,CIDMENG,CHAR,ND=Y                                           
Disclmr  LKOUT C,26,(D,B#SAVED,CI_DISCL),CHAR,ND=Y                              
Array    LKOUT C,R#XVEHT,(A,ARYVEH)                                             
Array    LKOUT C,R#XENG,(A,ARYENG),FILTROUT=TSTQENG                             
Array    LKOUT C,R#XFUEL,(A,ARYFUEL),FILTROUT=TSTQFUEL                          
Array    LKOUT C,R#XDIST,(A,ARYDTN),FILTROUT=TSTQDTN                            
Array    LKOUT C,03,(A,ARYDRR),FILTROUT=TSTCOMPN                                
Array    LKOUT C,03,(A,ARYDRT),FILTROUT=TSTCOMPN                                
Array    LKOUT C,03,(A,ARYCIDM),FILTROUT=TSTCOMPY                               
PRout    LKOUT P,CIDMNVTA,SETNVT                                                
NVtDrt   LKOUT C,40,CIDMNVTA,SPAK                                               
PRout    LKOUT P,CIDMAMT,SETAMT                                                 
PRout    LKOUT P,CIDELD,SETACUM                                                 
VHCSKIPS EQU   (*-VHCSKIP)/LX_COLSL                                             
Amount   LKOUT C,41,CIDMAMT,SPAK                                                
CurAmt   LKOUT C,42,CIDMFCA,SPAK,PZERO=S                                        
NetAmt   LKOUT C,43,CIDMNET,SPAK,PZERO=S                                        
DscAmt   LKOUT C,44,CIDMDISC,SPAK,PZERO=S                                       
RateChg  LKOUT C,45,(D,B#SAVED,CM_RTCHG),CHAR,ND=Y                              
*&&US                                                                           
PrvCod   LKOUT C,46,CIDMPROV,CHAR,ND=Y                                          
PrvNam   LKOUT C,47,CIDMPROV,(R,SETPRVNM),ND=Y                                  
GstAmt   LKOUT C,48,CIDMGST,SPAK                                                
PstAmt   LKOUT C,49,CIDMPST,SPAK                                                
*&&                                                                             
Array    LKOUT C,60,(A,ARYACS)                                                  
SupItm   LKOUT C,71,CIDELD,(R,EDTSUP),FILTROUT=TSTADV                           
Array    LKOUT C,110,(A,ARYNAR)                                                 
Array    LKOUT C,111,(A,ARYONAR)                                                
FCurdp   LKOUT C,115,(D,B#SAVED,CI_FCDP),LBIN,ND=Y                              
Array    LKOUT C,03,(A,ARYAPP)                                                  
Array    LKOUT C,112,(A,ARYPAY)                                                 
Array    LKOUT C,A#XDFA,(A,ARYXDFH),FILTROUT=FLTAURA                            
                                                                                
CLISKIPS EQU   (*-CLISKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYCIDM  LKOUT A,(*,CIDMDNTY),ROWNAME=CIDELD,NROWS=*,                  +        
               ROWWIDTH=L'CIDMDNTY,NEWEL=B                                      
GrsRat   LKOUT C,01,CIDMDGRT,SPAK,PZERO=S                                       
VATRat   LKOUT C,02,CIDMDVRT,SPAK                                               
PasRat   LKOUT C,03,CIDMDPRT,SPAK,PZERO=S                                       
DstBnd   LKOUT C,04,CIDMDBND,CHAR,ND=Y                                          
DstBndNm LKOUT C,05,CIDMDBND,(R,EDTBNDN),ND=Y                                   
DstBndAm LKOUT C,06,(D,B#SAVED,CM_BNDAM),SPAK,PZERO=S                           
Dstnce   LKOUT C,07,CIDMDIST,SPAK,PZERO=S                                       
PRout    LKOUT P,CIDMDNTY,DISTOTS                                               
TotAmt   LKOUT C,08,(D,B#SAVED,CM_BNGAM),SPAK,PZERO=S                           
TotVAT   LKOUT C,09,(D,B#SAVED,CM_BNVTA),SPAK                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYACS   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CIDELD,CIDELQ),   +        
               ROWWIDTH=(V,CIDLN),TESTFILT=(CIDSEQ,L'CIDSEQ)                    
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
CliCod   LKOUT C,60,CIDSEQ,(U,#EDTCLI,$EDTCLI),                        +        
               FILTROUT=TSTTACNT,SKIPCOLS=ACTSKIPS                              
ACTSKIP  EQU   *                                                                
PRout    LKOUT P,CIDCCPJ,BLDSJAC                                                
CliNam   LKOUT C,61,(D,B#SAVED,CI_SJULA),(R,EDTCLN)                             
CliOff   LKOUT C,62,(D,B#SAVED,CI_SJOFF),CHAR,FILTROUT=TSTSJOFF,       +        
               SKIPCOLS=1                                                       
CliOfN   LKOUT C,63,(D,B#SAVED,CI_SJOFF),(R,EDTOFN)                             
ProCod   LKOUT C,64,(D,B#SAVED,CI_SJULA),(U,#EDTPRD,$EDTPRD),          +        
               FILTROUT=TSTPROD,SKIPCOLS=5                                      
ProNam   LKOUT C,65,(D,B#SAVED,CI_SJULA),(R,EDTPRN)                             
ProOff   LKOUT C,66,(D,B#SAVED,CI_SJOFF),CHAR,FILTROUT=TSTSJOFF,       +        
               SKIPCOLS=1                                                       
ProOfN   LKOUT C,67,(D,B#SAVED,CI_SJOFF),(R,EDTOFN)                             
JobCod   LKOUT C,68,(D,B#SAVED,CI_SJULA),(U,#EDTJOB,$EDTJOB),          +        
               FILTROUT=TSTJOB,SKIPCOLS=1                                       
JobNam   LKOUT C,69,(D,B#SAVED,CI_SJULA),(R,EDTJBN)                             
JobLckd  LKOUT C,70,(D,B#SAVED,CI_LOCK),CHAR,ND=Y                               
JobClos  LKOUT C,98,(D,B#SAVED,CI_CLOS),CHAR,ND=Y                               
JobExp   LKOUT C,99,(D,B#SAVED,CI_XJOB),CHAR,ND=Y                               
SupItm   LKOUT C,71,CIDELD,(R,EDTSUP)                                           
PRout    LKOUT P,CIDCCPJ,GETOPTM                                                
NoJob    LKOUT C,72,(D,B#SAVED,CI_NJBNB),CHAR,ND=Y                              
OnlyBil  LKOUT C,73,(D,B#SAVED,CI_BILO),CHAR,ND=Y                               
ExpAcc   LKOUT C,74,CIDCEXP,CHAR,FILTROUT=TSTEXPAC,SKIPCOLS=7                   
ExpAcNm  LKOUT C,75,CIDCEXP,(R,EDTANM)                                          
Exp2Da   LKOUT C,76,(D,B#SAVED,CI_2DAN),CHAR,ND=Y                               
Exp2Pa   LKOUT C,77,(D,B#SAVED,CI_2PAN),CHAR,ND=Y                               
ExpCli   LKOUT C,78,(D,B#SAVED,CI_CLAN),CHAR,ND=Y                               
ExpPro   LKOUT C,79,(D,B#SAVED,CI_PRAN),CHAR,ND=Y                               
ExpJob   LKOUT C,80,(D,B#SAVED,CI_JBAN),CHAR,ND=Y                               
ExpMile  LKOUT C,81,(D,B#SAVED,CI_MILE),CHAR,ND=Y                               
SupAcc   LKOUT C,82,CIDCSUP,CHAR,FILTROUT=TSTSUPAC,SKIPCOLS=2                   
SupNam   LKOUT C,83,CIDCSUP,(R,EDTANM)                                          
DscRat   LKOUT C,84,(D,B#SAVED,CI_DSCRT),LBIN                                   
TwoP     LKOUT C,85,CIDC2PA,CHAR,FILTROUT=TST2PAC,SKIPCOLS=2                    
PRout    LKOUT P,CIDC2PA,BLD2PAC                                                
TwoPNm   LKOUT C,86,(D,B#SAVED,CI_2PULA),(R,EDTANM)                             
TwoD     LKOUT C,87,CIDC2DA,CHAR,FILTROUT=TST2DAC,SKIPCOLS=3                    
PRout    LKOUT P,CIDC2DA,BLD2DAC                                                
TwoDCd   LKOUT C,88,CIDC2DA,(R,EDT2DC)                                          
TwoDNm   LKOUT C,89,(D,B#SAVED,CI_2DULA),(R,EDTANM)                             
VatCode  LKOUT C,90,CIDCVAT,(R,EDTVATC),ND=Y                                    
*&&UK                                                                           
VatAcCd  LKOUT C,90,CIDCVAC,(R,EDTVATC),ND=Y,FILTROUT=TESTNVAT                  
*&&                                                                             
VatName  LKOUT C,91,CIDCVAT,(R,EDTVATN),ND=Y                                    
*&&UK                                                                           
VatAcNm  LKOUT C,91,CIDCVAC,(R,EDTVATN),ND=Y,FILTROUT=TESTNVAT                  
*&&                                                                             
VatRate  LKOUT C,92,(D,B#SAVED,CI_VRATE),LBIN                                   
PRout    LKOUT P,CIDELD,SETQEST                                                 
ESTGLOB  LKOUT C,93,CIDCEGN,CHAR                                                
Array    LKOUT C,94,(A,ARYESN),FILTROUT=TSTEST                                  
IsClApp  LKOUT C,96,(D,B#SAVED,CI_CLAP),CHAR,ND=Y                               
IsClBup  LKOUT C,97,(D,B#SAVED,CI_CLBU),CHAR,ND=Y                               
*&&US                                                                           
PstCod   LKOUT C,100,CIDCPST,CHAR,ND=Y                                          
PstNam   LKOUT C,101,CIDCPST,(R,EDTPSTN),ND=Y                                   
PstRat   LKOUT C,102,(D,B#SAVED,CI_VRATE),LBIN                                  
*&&                                                                             
ACTSKIPS EQU   (*-ACTSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYNAR   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CIDELD,CIDELQ),   +        
               ROWWIDTH=(V,CIDLN),TESTFILT=(CIDSEQ,L'CIDSEQ)                    
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
Narr     LKOUT C,110,CIDNARR,LEN=V,CHAR,FILTROUT=TSTTNARR                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYONAR  LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CIDELD,CIDELQ),   +        
               ROWWIDTH=(V,CIDLN),TESTFILT=(CIDSEQ,L'CIDSEQ)                    
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
Narr     LKOUT C,111,CIDNARR,LEN=V,CHAR,FILTROUT=TSTTORNA                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYAPP   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CIDELD,CIDELQ),   +        
               ROWWIDTH=(V,CIDLN),TESTFILT=(CIDSEQ,L'CIDSEQ)                    
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
Array    LKOUT C,4,(A,ARYITAP),FILTROUT=TSTTAPPR                                
                                                                                
         LKOUT E                                                                
*                                                                               
ARYPAY   LKOUT A,(D,B#EXPREC,EXCRFST),EOT=EOR,ROWID=(CIDELD,CIDELQ),   +        
               ROWWIDTH=(V,CIDLN),TESTFILT=(CIDSEQ,L'CIDSEQ)                    
*                                                                               
PRout    LKOUT P,CIDTYPE,SETTYPE                                                
Pref     LKOUT C,112,CIDPYMTR,CHAR,ND=Y,FILTROUT=TSTTPYMT                       
Pdate    LKOUT C,113,CIDPYMDT,CDAT,ND=Y,FILTROUT=TSTTPYMT                       
Pamt     LKOUT C,114,CIDPYAMT,SPAK,ND=Y,FILTROUT=TSTTPYMT                       
*                                                                               
         LKOUT E                                                                
*                                                                               
ARYITAP  LKOUT A,(*,CIDANTRY),ROWNAME=CIDEL,NROWS=*,ROWWIDTH=L'CIDANTRY+        
               ,NEWEL=B                                                         
LmStat   LKOUT C,1,CIDASTAT,(R,EDTAPST)                                         
PID      LKOUT C,2,CIDAPID,(U,#EDTPID,$EDTPID),ND=Y                             
PRout    LKOUT P,CIDASTAT,CHKFIN                                                
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,5,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDAT  LKOUT A,(D,B#EXPREC,EXCRFST),NEWEL=B,EOT=EOR,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
PRout    LKOUT P,XDFXTYP,SETTYPE                                                
ItmNum   LKOUT C,01,XDFXPTC,LBIN                                                
Code     LKOUT C,02,XDFXPTX,(U,#EDTHEX,$EDTHEX),ND=Y                            
Type     LKOUT C,03,XDFXTYP,CHAR                                                
Data     LKOUT C,04,XDFXDTA,CHAR,LEN=V,FILTROUT=TSTXDFC                         
Date     LKOUT C,04,XDFXDTA,PDAT,LEN=3,FILTROUT=TSTXDFD                         
Amnt     LKOUT C,04,XDFXDTA,SPAK,LEN=6,FILTROUT=TSTXDFA                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYESN   LKOUT A,(R,NXTEST),NROWS=1,ROWNAME=ESTRECD                             
                                                                                
EstL#    LKOUT C,94,ESTKLNO,LBIN,ND=Y                                           
Array    LKOUT C,95,(A,ARYESN1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYESN1  LKOUT A,(D,B#EST,ESTRFST),EOT=EOR,                            +        
               ROWID=(ENMEL,ENMELQ),ROWWIDTH=(V,ENMLN)                          
EstName  LKOUT C,95,ENMNAME,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDFH  LKOUT A,(R,NXTXDF),MULTIROW=Y,ROWNAME=XDFRECD                          
                                                                                
PROUT    LKOUT P,XDFRECD,SETRCODE                                               
Array    LKOUT C,A#XDFA,(A,ARYXDF1)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDF1  LKOUT A,(D,B#XDFREC,XDFRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
Prout    LKOUT P,XDFELD,SETECODE                                                
Fieldcd  LKOUT C,1,(D,B#SAVED,CI_XDPTR),(U,#EDTHEX,$EDTHEX)                     
Field    LKOUT C,2,XDFCODE,CHAR                                                 
DataTyp  LKOUT C,3,XDFEDIT,CHAR                                                 
FieldLn  LKOUT C,4,XDFMXLN,LBIN,ND=Y,FILTROUT=FLTAURA                           
FieldLn  LKOUT C,4,XDFMXLN,LBIN,FILTROUT=FLTBRAND                               
FieldRq  LKOUT C,5,XDFELD,(R,EDTREQD)                                           
FieldAc  LKOUT C,6,XDFELD,(R,EDTACTV)                                           
FieldNm  LKOUT C,7,XDFNAME,CHAR,LEN=V                                           
Array    LKOUT C,8,(A,ARYXDLH)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDLH  LKOUT A,(R,NXTXDL),MULTIROW=Y,ROWNAME=XDLRECD                          
                                                                                
Array    LKOUT C,8,(A,ARYXDL)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDL   LKOUT A,(D,B#XDLREC,XDLRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
Deflt    LKOUT C,8,XDFELD,(R,EDTXLST)                                           
FieldNm  LKOUT C,9,XDFLDATA,CHAR,LEN=V                                          
FieldAct LKOUT C,10,XDFELD,(R,EDTACTL)                                          
                                                                                
         LKOUT E                                                                
                                                                                
                                                                                
*&&UK                                                                           
NXTCUR   ST    R8,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTCUR50                                                         
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
         MVC   X#LANG,CULANG                                                    
         CLI   X#LANG,0                                                         
         JH    *+8                                                              
         MVI   X#LANG,LANGEUK                                                   
                                                                                
         XC    CL_VALS(CL_LNQ),CL_VALS                                          
         USING GCURD,R2            R3=A(KEY)                                    
         LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
         MVC   GCKCURU,AGYCURR                                                  
         MVC   CSVKEY1,GCKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   NXTCUR08                                                         
         CLC   GCKEY(GCKCURX-GCURD),CSVKEY1                                     
         JNE   NXTCUR08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         AHI   R2,GCFIRST                                                       
         USING GCREL,R2                                                         
                                                                                
NXTCUR04 CLI   GCREL,0                                                          
         JE    NXTCUR08                                                         
         CLI   GCREL,GCRELQ                                                     
         JE    NXTCUR06                                                         
         LLC   R0,GCRLEN                                                        
         AR    R2,R0                                                            
         J     NXTCUR04                                                         
                                                                                
NXTCUR06 MVC   CURMIN,GCRMNEXC                                                  
         MVC   CURMAX,GCRMXEXC                                                  
         MVC   CL_DEC,GCRDECP                                                   
         MVC   CL_NAM,SPACES                                                    
         MVC   CL_CUR,AGYCURR                                                   
         MVI   BYTE1,1                                                          
         LLC   RE,GCRLEN                                                        
         SHI   RE,GCRLENQ+1                                                     
         LTR   RE,RE                                                            
         JM    NXTCUR08                                                         
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   CL_NAM(0),GCRNAME                                                
         DROP  R2                                                               
                                                                                
         USING CLRTABD,R3                                                       
NXTCUR08 L     R3,AIO3             Use AIO3-AIO8 for client levels              
         J     NXTCUR44                                                         
         XC    CLRTCUR(CURTABL),CLRTCUR                                         
         LHI   R0,((8+1-3)*IOLENQ)-(CLRTABL+1)     (12K should do)              
         MVI   CURIND,0                                                         
                                                                                
         USING GEXCD,R2                                                         
         LA    R2,IOKEY                                                         
         XC    GEKEY,GEKEY                                                      
         MVI   GEKREC,GEKRECQ                                                   
         MVC   GEKAGY,CUAALF                                                    
         MVI   GEKSYS,ACCSYSQ                                                   
         MVC   CSVKEY1,GEKEY                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   NXTCURN2                                                         
         J     NXTCUR14                                                         
                                                                                
NXTCUR12 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGND+IO1'                               
                                                                                
NXTCUR14 CLC   GEKEY(GEKCURF-GEKEY),CSVKEY1                                     
         JNE   NXTCUR24                                                         
         CLI   GEKACT,X'41'                                                     
         JNH   NXTCUR12                                                         
         CLC   GEKACT,EFFS                                                      
         JE    NXTCUR12                                                         
         CLC   GEKPEND,CH_TODC                                                  
         JL    NXTCUR12                                                         
         CLC   GEKPSTA,CH_TODC                                                  
         JH    NXTCUR12                                                         
         CLC   GEKCURF,D_CURR                                                   
         JNE   NXTCUR12                                                         
                                                                                
NXTCUR16 MVC   FULL1(3),GEKCURF                                                 
         CLC   GEKCURF,D_CURR                                                   
         JNE   NXTCUR18                                                         
         MVC   FULL1(3),GEKCURT                                                 
                                                                                
DUP      USING CLRTABD,RF                                                       
NXTCUR18 L     RF,AIO3             skip duplicate entries                       
                                                                                
NXTCUR20 CLI   DUP.CLRTCUR,0                                                    
         JE    NXTCUR22                                                         
         CLC   DUP.CLRTCUR,FULL1                                                
         JE    NXTCUR12                                                         
         AHI   RF,CLRTABL                                                       
         J     NXTCUR20                                                         
         DROP  DUP                                                              
                                                                                
NXTCUR22 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
                                                                                
         MVC   CLRTCUR,FULL1                                                    
         MVC   CLRTCLI,GEKACT                                                   
         AHI   R3,CLRTABL                                                       
         XC    CLRTCUR(CURTABL),CLRTCUR                                         
         SHI   R0,CLRTABL                                                       
         LTR   R0,R0               table size exceeded                          
         JNP   NXTCURN2                                                         
         J     NXTCUR12                                                         
         DROP  R2                                                               
                                                                                
NXTCUR24 L     R3,AIO3             all client data read - get details           
                                                                                
         USING GCURD,R2                                                         
NXTCUR26 LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
         MVC   GCKCURU,CLRTCUR                                                  
         MVC   CSVKEY1,GCKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   NXTCUR40                                                         
         CLC   GCKEY(GCKCURX-GCURD),CSVKEY1                                     
         JNE   NXTCUR40                                                         
         TM    GCDSTAT,GCDSPROD    Production currencies only                   
         JZ    NXTCUR40                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         AHI   R2,GCFIRST                                                       
         USING GCREL,R2                                                         
         MVI   BYTE3,NOQ                                                        
                                                                                
NXTCUR28 CLI   GCREL,0                                                          
         JE    NXTCUR36                                                         
         CLI   GCREL,GCRELQ                                                     
         JE    NXTCUR32                                                         
         CLI   GCREL,GDDELQ                                                     
         JE    NXTCUR34                                                         
                                                                                
NXTCUR30 LLC   R0,GCRLEN                                                        
         AR    R2,R0                                                            
         J     NXTCUR28                                                         
                                                                                
NXTCUR32 MVI   BYTE3,YESQ                                                       
         MVC   CLRTDPL,GCRDECP                                                  
         MVC   CLRTMIN,GCRMNEXC                                                 
         MVC   CLRTMAX,GCRMXEXC                                                 
         MVC   CLRTNAM,SPACES                                                   
         LLC   RE,GCRLEN                                                        
         SHI   RE,GCRLENQ+1                                                     
         LTR   RE,RE                                                            
         JM    NXTCUR40                                                         
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   CLRTNAM(0),GCRNAME                                               
         J     NXTCUR30                                                         
                                                                                
NXTCUR34 OC    GDDICT,GDDICT       DDBLDCUR.BLD5 for language - get             
         JZ    NXTCUR30            language name via DICTATE                    
         XC    TEMP,TEMP                                                        
         MVI   TEMP,DD#ESCL                                                     
         MVC   TEMP+1(L'GDDICT),GDDICT                                          
         MVI   TEMP+1+L'GDDICT,L'CURTLONG                                       
         LARL  RE,DDSTRNG                                                       
         ICM   RF,B'1111',0(RE)                                                 
         ICM   RF,B'0001',X#LANG                                                
         GOTOX VDICTATE,DMCB,(RF),TEMP                                          
         MVC   CLRTNAM,TEMP                                                     
         J     NXTCUR30                                                         
         DROP  R2                                                               
                                                                                
NXTCUR36 CLI   BYTE3,YESQ                                                       
         JNE   NXTCUR40                                                         
                                                                                
         USING EURKBLKD,R2                                                      
         L     R2,AIO1             read for rate                                
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,D_CURR                                                  
         MVC   EURKCUTO,CLRTCUR                                                 
         MVC   EURKALPH,CUAALF                                                  
         MVC   EURKDATE,CH_TODC                                                 
         MVC   EURKACT,CLRTCLI                                                  
         MVC   EURKAFAC,ACOMFACS                                                
         MVI   EURKTYPE,ACCQ                                                    
         TM    CPYSTAT6,CPYSFTXR                                                
         JZ    NXTCUR38                                                         
         OI    EURKTYPE,ALLOWFTQ+SWAPQ                                          
                                                                                
NXTCUR38 GOTO1 VEUREKA,DMCB,('GETQ',EURKBLKD)                                   
         CLI   0(R1),0                                                          
         JNE   NXTCUR40                                                         
         MVC   CLRTRUL,EURKRULE                                                 
         OI    CURIND,CURICLI                                                   
         J     NXTCUR42                                                         
                                                                                
NXTCUR40 MVI   CLRTCUR,FF          set to skip                                  
                                                                                
NXTCUR42 AHI   R3,CLRTABL          next entry                                   
         CLI   CLRTCUR,0                                                        
         JE    NXTCUR44                                                         
         J     NXTCUR26                                                         
         DROP  R3                                                               
                                                                                
         USING INFOD,R2                                                         
         USING LISTTABD,R3                                                      
NXTCUR44 L     R2,AIO1             USE IO1 TO BUILD PARAMETERS                  
         L     R3,AGENAREA         USE GENEAREA FOR OUTPUT                      
*                                                                               
         LR    R0,R3               clear data output area                       
         LHI   R1,GENAREAX-GENAREA-1                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   INFODAT,CH_TODC     - USE TODAY'S DATE FOR LIST                  
         MVC   INFOALP,CUAALF      - ALPHA ID                                   
         MVC   INFOCUR,AGYCURR     - AND AGENCY'S CURRENCY                      
         MVI   INFOFLG,0           - FT/DE RATE FLAG                            
         TM    CPYSTAT6,CPYSFTXR                                                
         JZ    *+8                                                              
         OI    INFOFLG,ALLOWFTQ    NOW CALL MODULE FOR LIST                     
         GOTO1 VGETCRL,DMCB,INFOD,LISTTABD,(X#LANG,ACOMFACS)                    
         CLI   0(R1),0             ANY ERRORS?                                  
         JE    NXTCUR46                                                         
         CLI   0(R1),EMPTYQ        IS IT AN EMPTY LIST?                         
         JNE   NXTCURN1                                                         
         XC    ACURLIST,ACURLIST                                                
         TM    CURIND,CURICLI      any client level rates?                      
         JNZ   NXTCUR48                                                         
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
                                                                                
NXTCUR46 OI    CURIND,CURICRL      GETCRL returned data                         
                                                                                
NXTCUR48 TM    CURIND,CURICLI      any client level found?                      
         JZ    NXTCUR62                                                         
         L     R3,AIO3                                                          
         ST    R3,ACURLIST                                                      
         OI    CURIND,CURIMOD                                                   
         MVI   LP_RMODE,LP_RNEXT                                                
         CLI   BYTE1,1                                                          
         JE    EXITY                                                            
*                                                                               
NXTCUR50 TM    CURIND,CURIOFC      Have we printed out office currency          
         JNZ   NXTCUR56                                                         
         OI    CURIND,CURIOFC                                                   
         CLC   D_CURR,AGYCURR      Do we have an office currency?               
         JE    NXTCUR56                                                         
*                                                                               
         XC    CL_VALS(CL_LNQ),CL_VALS                                          
         USING GCURD,R2            R3=A(KEY)                                    
         LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
                                                                                
         MVC   GCKCURU,D_CURR                                                   
         MVC   CSVKEY1,GCKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   NXTCUR56                                                         
         CLC   GCKEY(GCKCURX-GCURD),CSVKEY1                                     
         JNE   NXTCUR56                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         AHI   R2,GCFIRST                                                       
*                                                                               
         USING GCREL,R2                                                         
NXTCUR52 CLI   GCREL,0                                                          
         JE    NXTCUR44                                                         
         CLI   GCREL,GCRELQ                                                     
         JE    NXTCUR54                                                         
         LLC   R0,GCRLEN                                                        
         AR    R2,R0                                                            
         J     NXTCUR52                                                         
                                                                                
NXTCUR54 MVC   CURMIN,GCRMNEXC                                                  
         MVC   CURMAX,GCRMXEXC                                                  
         MVC   CL_DEC,GCRDECP                                                   
         MVC   CL_NAM,SPACES                                                    
         MVC   CL_CUR,D_CURR                                                    
         MVI   BYTE1,1                                                          
         LLC   RE,GCRLEN                                                        
         SHI   RE,GCRLENQ+1                                                     
         LTR   RE,RE                                                            
         JM    NXTCUR44                                                         
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   CL_NAM(0),GCRNAME                                                
         J     NXTCUR44                                                         
         DROP  R2                                                               
                                                                                
NXTCUR56 TM    CURIND,CURIALL      all mode now?                                
         JNZ   NXTCUR74                                                         
                                                                                
         TM    CURIND,CURIMOD      client level data mode?                      
         JZ    NXTCUR64                                                         
                                                                                
         USING CLRTABD,R3                                                       
         XC    CL_VALS(CL_LNQ),CL_VALS                                          
         L     R3,ACURLIST                                                      
         MVC   CL_CUR,CLRTCUR                                                   
         MVC   CL_CLI,CLRTCLI                                                   
         MVC   CL_NAM,CLRTNAM                                                   
         GOTO1 VHEXOUT,DMCB,CLRTRUL,CL_RUL,L'CLRTRUL,0                          
                                                                                
         ZAP   PKWK16A,PZERO       Divide by agency currency                    
         MVO   PKWK16A,CLRTMIN      to get min max for agency currency          
         SRP   PKWK16A,10,0                                                     
         ZAP   PKWK16B,PZERO                                                    
         MVO   PKWK16B,CLRTMAX                                                  
         SRP   PKWK16B,10,0                                                     
*                                                                               
         ZAP   PKWK6,PZERO                                                      
         MVO   PKWK6,CURMAX                                                     
         DP    PKWK16A,PKWK6                                                    
         ZAP   PKWK6,PZERO                                                      
         MVO   PKWK6,CURMIN                                                     
         DP    PKWK16B,PKWK6                                                    
         MVC   CL_MIN,PKWK16A+(L'PKWK16A-L'PKWK6)-8                             
         MVC   CL_MAX,PKWK16B+(L'PKWK16B-L'PKWK6)-8                             
         MVC   CL_DEC,CLRTDPL                                                   
         AHI   R3,CLRTABL                                                       
                                                                                
NXTCUR58 CLI   CLRTCUR,FF                                                       
         JNE   NXTCUR60                                                         
         AHI   R3,CLRTABL                                                       
         J     NXTCUR58                                                         
                                                                                
NXTCUR60 ST    R3,ACURLIST                                                      
         MVI   LP_RMODE,LP_RNEXT                                                
         CLI   CLRTCUR,0           TEST EOT                                     
         JNE   EXITY                                                            
         DROP  R3                                                               
                                                                                
         USING LISTTABD,R3                                                      
         NI    CURIND,FF-CURIMOD   change mode                                  
         TM    CURIND,CURICRL      GETCRL to be processed?                      
         JZ    NXTCUR72                                                         
         L     R3,AGENAREA                                                      
         ST    R3,ACURLIST                                                      
         J     EXITY                                                            
                                                                                
NXTCUR62 ST    R3,ACURLIST                                                      
         MVI   LP_RMODE,LP_RNEXT                                                
         CLI   BYTE1,1                                                          
         JE    EXITY                                                            
                                                                                
NXTCUR64 XC    CL_VALS(CL_LNQ),CL_VALS                                          
         L     R3,ACURLIST                                                      
NXTCUR66 OC    LISTCUR,LISTCUR     TEST EOT                                     
         JZ    NXTCUR72                                                         
         CLC   LISTCUR,D_CURR      Skip office currency                         
         JNE   NXTCUR68                                                         
         AHI   R3,LISTLNQ                                                       
         J     NXTCUR66                                                         
                                                                                
NXTCUR68 MVC   CL_CUR,LISTCUR      - PASS ISO CURRENCY CODE                     
         MVC   CL_NAM,LISTNAM      - PASS CURRENCY'S NAME                       
         GOTO1 VHEXOUT,DMCB,LISTRUL,CL_RUL,L'LISTRUL,0                          
                                                                                
         ZAP   PKWK16A,PZERO       Divide by agency currency                    
         MVO   PKWK16A,LISTMIN      to get min max for agency currency          
         SRP   PKWK16A,10,0                                                     
         ZAP   PKWK16B,PZERO                                                    
         MVO   PKWK16B,LISTMAX                                                  
         SRP   PKWK16B,10,0                                                     
*                                                                               
         ZAP   PKWK6,PZERO                                                      
         MVO   PKWK6,CURMAX                                                     
         DP    PKWK16A,PKWK6                                                    
         ZAP   PKWK6,PZERO                                                      
         MVO   PKWK6,CURMIN                                                     
         DP    PKWK16B,PKWK6                                                    
         MVC   CL_MIN,PKWK16A+(L'PKWK16A-L'PKWK6)-8                             
         MVC   CL_MAX,PKWK16B+(L'PKWK16B-L'PKWK6)-8                             
         MVC   CL_DEC,LISTDPL                                                   
         LLC   R1,BYTE2                                                         
         AHI   R1,1                                                             
         STC   R1,BYTE2                                                         
         MVC   CL_POS,BYTE2                                                     
                                                                                
NXTCUR70 AHI   R3,LISTLNQ                                                       
         ST    R3,ACURLIST                                                      
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
                                                                                
NXTCUR72 OI    CURIND,CURIALL      set new mode                                 
                                                                                
         USING GCURD,R2                                                         
         LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
         MVC   GCKCURU,SPACES                                                   
         MVC   CSVKEY1,GCKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGND+IO1'                               
         JNE   *+2                                                              
         J     NXTCUR78                                                         
                                                                                
NXTCUR74 MVC   IOKEY,CSVKEY2                                                    
                                                                                
NXTCUR76 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGND+IO1'                               
         JNE   *+2                                                              
                                                                                
NXTCUR78 CLC   GCKEY(GCKCURR-GCURD),CSVKEY1                                     
         JNE   NXTCUR90                                                         
         TM    GCDSTAT,GCDSPROD    None production currencies only              
         JNZ   NXTCUR76                                                         
         CLC   GCKCURU,D_CURR      skip office currency                         
         JE    NXTCUR76                                                         
         XC    CL_VALS(CL_LNQ),CL_VALS                                          
         MVC   CL_CUR,GCKCURU                                                   
         MVI   CL_NPC,YESQ                                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGNF+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         AHI   R2,GCFIRST                                                       
         USING GCREL,R2                                                         
                                                                                
NXTCUR80 CLI   GCREL,0                                                          
         JE    NXTCUR88                                                         
         CLI   GCREL,GCRELQ                                                     
         JE    NXTCUR84                                                         
         CLI   GCREL,GDDELQ                                                     
         JE    NXTCUR86                                                         
                                                                                
NXTCUR82 LLC   R0,GCRLEN                                                        
         AR    R2,R0                                                            
         J     NXTCUR80                                                         
                                                                                
NXTCUR84 MVC   CL_DEC,GCRDECP                                                   
         MVC   CL_NAM,SPACES                                                    
         LLC   RE,GCRLEN                                                        
         SHI   RE,GCRLENQ+1                                                     
         LTR   RE,RE                                                            
         JM    NXTCUR76                                                         
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   CL_NAM(0),GCRNAME                                                
         J     NXTCUR82                                                         
                                                                                
NXTCUR86 OC    GDDICT,GDDICT       DDBLDCUR.BLD5 for language - get             
         JZ    NXTCUR82            language name via DICTATE                    
         XC    TEMP,TEMP                                                        
         MVI   TEMP,DD#ESCL                                                     
         MVC   TEMP+1(L'GDDICT),GDDICT                                          
         MVI   TEMP+1+L'GDDICT,L'CURTLONG                                       
         LARL  RE,DDSTRNG                                                       
         ICM   RF,B'1111',0(RE)                                                 
         ICM   RF,B'0001',X#LANG                                                
         GOTOX VDICTATE,DMCB,(RF),TEMP                                          
         MVC   CL_NAM,TEMP                                                      
         J     NXTCUR82                                                         
                                                                                
NXTCUR88 MVC   CSVKEY2,IOKEY                                                    
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
NXTCUR90 MVI   LP_RMODE,LP_RLAST   END OF LIST                                  
         J     EXITY                                                            
                                                                                
                                                                                
NXTCURN1 MVC   LP_ERROR,=AL2(AE$INCUR)   currency error                         
         J     NXTCURN                                                          
NXTCURN2 MVC   LP_ERROR,=AL2(AE$TMIDT)   tabel full error                       
NXTCURN  J     QERROR                                                           
         DROP  R3                                                               
                                                                                
         DS    0H                                                               
DDSTRNG  DC    CL2'SL',XL2'0F00'                                                
         DS    0H                                                               
                                                                                
CLRTABD  DSECT                                                                  
CLRTCUR  DS    CL3                 (0=EoT, FF=Skip)                             
CLRTCLI  DS    CL5                                                              
CLRTNAM  DS    CL35                CURRENCY NAME                                
CLRTDPL  DS    XL1                 DECIMAL PLACES                               
CLRTRUL  DS    0PL7                SEE DDEUREKAD)                               
CLRTEST  DS    CL1                 EXCHANGE STATUS                              
CLRTEXR  DS    PL5                 EXCHANGE RATE                                
CLRTESH  DS    CL1                 EXCHANGE SHIFT VALUE                         
CLRTMIN  DS    CL5                 MIN VALID EXCH RATE (CURR RECORD)            
CLRTMAX  DS    CL5                 MAX VALID EXCH RATE (CURR RECORD)            
CLRTABL  EQU   *-CLRTABD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
*&&                                                                             
***********************************************************************         
* Get estimate record                                                 *         
***********************************************************************         
                                                                                
NXTEST   MVC   IOKEY,SAVEKEY2                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,EGNKEYT,('B#EST',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         MVC   SAVEKEY2,IOKEY                                                   
         LA    R2,IOKEY                                                         
         USING EGNPASD,R2                                                       
         MVC   IODA,EGNPDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ADATA,AIO3                                                    
         J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* Build trailer response                                              *         
***********************************************************************         
                                                                                
BLDTRL   MVI   CH_FNDIS,NOQ                                                     
         CLI   CH_USRFN,YESQ       Is the connected user fin approver           
         JNE   EXITY               No                                           
         TM    CH_IND,CH_IMSAP     Yes - do we have any other missing           
         JNZ   EXITY               approvals                                    
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    BLDTRL02            No                                           
         TM    CH_IND,CH_ILVL1+CH_ILVL2                                         
         JO    EXITY                                                            
                                                                                
BLDTRL02 MVI   CH_FNDIS,YESQ       No - set finance edit display                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Read expense claim records                                          *         
***********************************************************************         
                                                                                
NXTEXP   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTEXP02            No                                           
                                                                                
         XC    LDGAREA(LDGALNQ),LDGAREA  Get VAT ledger                         
         MVC   LDGAUL,=CL2'SG'                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   CH_SGOPO,LDGAOP                                                  
         XC    SAVEKEY1,SAVEKEY1                                                
         XC    SAVEKEY2,SAVEKEY2                                                
         MVI   QM_FUEL,YESQ                                                     
         MVI   QM_DIST,YESQ                                                     
         MVI   QM_ENGSZ,YESQ                                                    
         ZAP   QM_DISAC,PZERO      Zero distance accumulated                    
         MVC   CM_RDATE+1(L'CPXDRDTE),CPXDRDTE                                  
         MVC   CM_RDATE(1),CH_TODP                                              
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,EXNKEYT,('B#EXPREC',0),          +        
               ('$NXTRXGR',SAVED),0,0                                           
         JE    NXTEXP03                                                         
         MVC   LP_ERROR,=AL2(AE$RECNF)                                          
         J     QERROR                                                           
                                                                                
NXTEXP02 MVC   IOKEY,SAVEKEY1                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,EXNKEYT,('B#EXPREC',0),          +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
NXTEXP03 MVC   SAVEKEY1,IOKEY                                                   
         LA    R2,SAVEKEY1                                                      
         USING EXNPASD,R2          R2=A(expense number passive key)             
         TM    EXNPSTAT,EXCSDELT                                                
         JZ    NXTEXP04                                                         
         MVC   LP_ERROR,=AL2(AE$RECID)                                          
         J     QERROR                                                           
NXTEXP04 MVC   IODA,EXNPDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO6'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO6                                                          
         USING EXCRECD,R2                                                       
         CLC   EXCKEY(EXCKSEQ-EXCRECD),SAVEKEY2                                 
         JE    NXTEXP30                                                         
         MVC   SAVEKEY2,EXCKEY                                                  
         XR    R1,R1                                                            
         ICM   R1,3,EXCKDATE                                                    
         LNR   R1,R1                                                            
         STCM  R1,3,CH_DATE                                                     
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   CH_OFF(0),EXCK1RAC  extract office                               
         EX    RE,0(RF)                                                         
         CLI   LP_ACCS,0           skip if no limit access                      
         JE    NXTEXP06                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,CH_OFF    move in office and validate                   
                                                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    NXTEXP06                                                         
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     QERROR                                                           
         DROP  R1                                                               
                                                                                
NXTEXP06 LA    R3,EXCRFST                                                       
         USING CLDELD,R3                                                        
NXTEXP08 CLI   CLDEL,0                                                          
         JE    NXTEXP18                                                         
         CLI   CLDEL,CLDELQ                                                     
         JE    NXTEXP12                                                         
NXTEXP10 LLC   RF,CLDLN                                                         
         AR    R3,RF                                                            
         J     NXTEXP08                                                         
                                                                                
NXTEXP12 CLI   CLDTYPE,CLDTHDRQ                                                 
         JNE   NXTEXP10                                                         
         TM    CLDSTAT,CLDSADVQ    Do we have advances                          
         JZ    NXTEXP18                                                         
         MVC   LP_ERROR,=AL2(AE$ADVBO)  Not supported for Aura                  
         J     QERROR                                                           
                                                                                
NXTEXP18 CLC   EXCKPIDB,CCTPID     Is the connected user the claimer            
         JE    NXTEXP26            Yes - allow them to see claim                
         CLI   QD_CDSRC,YESQ       Do we have ability to search                 
         JNE   NXTEXP28            No - otherwise check limit list              
         CLI   QD_CDOVR,YESQ       Overriding limlst/approver settings?         
         JE    NXTEXP26            Yes have rights to see all claims            
         XC    MYBYTE,MYBYTE                                                    
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
         JNE   NXTEXP28                                                         
*                                                                               
* Check 1R account against GAPLST table                                         
*                                                                               
NXTEXP20 MVC   WORK(L'EXCK1RAC),EXCK1RAC                                        
         LLC   R3,ONERL3L          R3 is NEXT level to check                    
         LA    R4,GAPAREA                                                       
         USING GAPTABD,R4                                                       
NXTEXP22 XC    GAPAREA,GAPAREA                                                  
         MVI   GAPTDAT1,GAPTT1Q                                                 
         MVC   GAPTACT,WORK                                                     
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   NXTEXP24                                                         
         CLC   GAPTACT,WORK                                                     
         JNE   NXTEXP24                                                         
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   NXTEXP24                                                         
         TM    GAPTSTA,GAPTSEQ                                                  
         JO    NXTEXP28            specifically excluded                        
         J     NXTEXP26            match, process                               
* Clear last level checked and set next level (r3)                              
                                                                                
NXTEXP24 LR    RF,R3                                                            
         LA    RE,WORK       Allow for l'(office+dep+subdep+perkcode)           
         AR    RE,R3                                                            
         MVC   0(L'PERKCODE,RE),SPACES    clear lowest level                    
*                                                                               
         LLC   R3,ONERL2L    set r3 one level below last                        
         CR    RF,R3                                                            
         JH    NXTEXP22                                                         
         LLC   R3,ONERL1L                                                       
         CR    RF,R3                                                            
         JH    NXTEXP22                                                         
         SR    R3,R3                                                            
         CR    RF,R3                                                            
         JH    NXTEXP22                                                         
         J     NXTEXP28            No match at any level                        
*                                                                               
NXTEXP26 OI    CH_IND,CH_I1RVW     Set user has access to view claim            
NXTEXP28 MVC   CH_PPID#,EXCKPIDB                                                
         GOTOR (#CSTPRF,ACSTPRF),DMCB,EXCK1RAC  get approvers profiles          
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   CH_COBCA,COBCA                                                   
         MVC   CH_COTNB,COTNB                                                   
         MVC   CH_COLMA,COLMA                                                   
         MVC   CH_COCAE,COCAE                                                   
         MVC   CH_COMIL,COMIL                                                   
                                                                                
         GOTOR POSTOFF,CH_OFF                                                   
                                                                                
NXTEXP30 MVC   LP_ADATA,AIO6                                                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Get Extra Data Records                                              *         
***********************************************************************         
*                                                                               
NXTXDF   GOTOR (#NXTREC,ANXTREC),DMCB,XDFKEYT,('B#XDFREC',0),          +        
               (0,SAVED),AFLTXDF,0                                              
         J     EXITY                                                            
***********************************************************************         
* SAVE OFF XDFCODE                                                    *         
***********************************************************************         
SETECODE L     R2,LP_AINP                                                       
         USING XDFELD,R2                                                        
         MVC   DCODE,XDFCODE      SAVE DATA CODE                                
         MVC   CI_XDPTR+L'XDFRPTR(L'XDFSEQ),XDFSEQ                              
         J     EXITY                                                            
***********************************************************************         
* SAVE OFF XDFRECD INFO                                               *         
***********************************************************************         
SETRCODE L     R2,LP_AINP                                                       
         USING XDFRECD,R2                                                       
         MVC   DOFF,XDFKOFF       SAVE OFFICE                                   
         MVC   DORTY,XDFKORTY     SAVE ORDER TYPE                               
         MVC   DCLI,XDFKCLI       SAVE CLIENT CODE                              
         MVC   DETY,XDFKETY       SAVE EXPENDITURE TYPE                         
         MVC   DWC,XDFKWC         SAVE WORK CODE                                
         MVC   DMED,XDFKMED       SAVE MEDIA CODE                               
         MVC   DSCH,XDFKSCH       SAVE SCHEME CODE                              
         MVC   CI_XDPTR,XDFRPTR                                                 
         J     EXITY                                                            
***********************************************************************         
* read XDL record                                                     *         
***********************************************************************         
NXTXDL   GOTOR (#NXTREC,ANXTREC),DMCB,XDLKEYT,('B#XDLREC',SAVEKEY3),   +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
*** Claim Mileage Record Download *************************************         
***********************************************************************         
                                                                                
REQCLMD  LKREQ H,A#XMIL,OUTCLMD,NEXTREQ=REQCLRD                                 
                                                                                
PerCod   LKREQ F,01,(D,B#SAVED,QM_PERS),CHAR,OLEN=L'QM_PERS,           +        
               MAXLEN=L'QM_PERS,TEXT=AC#PRSN,COL=*                              
CDate    LKREQ F,02,(D,B#SAVED,QM_CDATE),PDAT,OLEN=L'QM_CDATE,         +        
               TEXT=AC#DATE,COL=*                                               
EffDte   LKREQ F,03,(D,B#SAVED,QM_EFDTE),PDAT,OLEN=L'QM_EFDTE,         +        
               TEXT=AC#EFFDT,COL=*                                              
Vehicle  LKREQ F,04,(D,B#SAVED,QM_VEHCL),CHAR,OLEN=L'QM_VEHCL,         +        
               MAXLEN=L'QM_VEHCL,TEXT=AC#FLVEH,COL=*                            
Fuel     LKREQ F,05,(D,B#SAVED,QM_FUEL),CHAR,OLEN=L'QM_FUEL,           +        
               MAXLEN=L'QM_FUEL,TEXT=AC#FUEL,COL=*                              
Engine   LKREQ F,06,(D,B#SAVED,QM_ENGSZ),CHAR,OLEN=L'QM_ENGSZ,         +        
               MAXLEN=L'QM_ENGSZ,TEXT=AC#FLENG,COL=*                            
Distance LKREQ F,07,(D,B#SAVED,QM_DIST),CHAR,OLEN=L'QM_DIST,           +        
               MAXLEN=L'QM_DIST,TEXT=AC#FLDIS,COL=*                             
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLMD  LKOUT H                                                                
                                                                                
CLMVEH   LKOUT R,R#XVEHT                                                        
Array    LKOUT C,1,(A,ARYVEH),FILTROUT=TSTQVEH                                  
                                                                                
         LKOUT E                                                                
                                                                                
*&&UK                                                                           
CLMFUEL  LKOUT R,R#XFUEL                                                        
Array    LKOUT C,1,(A,ARYFUEL),FILTROUT=TSTQFUEL                                
                                                                                
         LKOUT E                                                                
*&&                                                                             
                                                                                
*&&UK                                                                           
CLMENG   LKOUT R,R#XENG                                                         
Array    LKOUT C,1,(A,ARYENG),FILTROUT=TSTQENG                                  
                                                                                
         LKOUT E                                                                
*&&                                                                             
                                                                                
CLMDIST  LKOUT R,R#XDIST                                                        
Array    LKOUT C,1,(A,ARYDTN),FILTROUT=TSTQDTN                                  
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYVEH   LKOUT A,(R,NXTVEH),NROWS=1,ROWNAME=VEHRECD                             
Array    LKOUT C,R#XVEHT,(A,ARYVEL)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYVEL   LKOUT A,(D,B#VEHREC,VEHRFST),EOT=EOR,ROWID=(FFTELD,FFTELQ),   +        
               ROWWIDTH=(V,FFTLN),NEWEL=B                                       
                                                                                
Prout    LKOUT P,FFTTYPE,SETTYPE                                                
VehCod   LKOUT C,01,FFTVEHC,CHAR,FILTROUT=TSTFVEH,SKIPCOLS=VEHSKIPS             
VEHSKIP  EQU   *                                                                
VehNam   LKOUT C,02,FFTVEHN,CHAR,LEN=V                                          
PRout    LKOUT P,FFTELD,DRTSTAT                                                 
AlPass   LKOUT C,03,(D,B#SAVED,CV_ALPAS),CHAR                                   
FuelRq   LKOUT C,04,(D,B#SAVED,CV_FUERQ),CHAR                                   
EngRqd   LKOUT C,05,(D,B#SAVED,CV_ENGRQ),CHAR                                   
CutOff   LKOUT C,08,FFTVCUT,PDAT                                                
AccuNm   LKOUT C,09,FFTVEHAC,LBIN                                               
PRout    LKOUT P,FFTELD,SETVACM                                                 
AccuDi   LKOUT C,10,(D,B#SAVED,CI_DIST),SPAK                                    
VEHSKIPS EQU   (*-VEHSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYFUEL  LKOUT A,(R,NXTFUEL),NROWS=1,ROWNAME=FUERECD                            
Array    LKOUT C,R#XFUEL,(A,ARYFUL)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYFUL   LKOUT A,(D,B#FUEREC,FUERFST),EOT=EOR,ROWID=(FFTELD,FFTELQ),   +        
               ROWWIDTH=(V,FFTLN),NEWEL=B                                       
                                                                                
Prout    LKOUT P,FFTTYPE,SETTYPE                                                
FuelCd   LKOUT C,01,FFTFUEC,CHAR,FILTROUT=TSTFFUE,SKIPCOLS=FUESKIPS             
FUESKIP  EQU   *                                                                
FuelNm   LKOUT C,02,FFTFUEN,CHAR,LEN=V                                          
CutOff   LKOUT C,03,FFTFCUT,PDAT                                                
FUESKIPS EQU   (*-FUESKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYENG   LKOUT A,(R,NXTENG),NROWS=1,ROWNAME=ENGRECD                             
Array    LKOUT C,R#XENG,(A,ARYENL)                                              
                                                                                
         LKOUT E                                                                
                                                                                
ARYENL   LKOUT A,(D,B#ENGREC,ENGRFST),EOT=EOR,ROWID=(FFTELD,FFTELQ),   +        
               ROWWIDTH=(V,FFTLN),NEWEL=B                                       
                                                                                
Prout    LKOUT P,FFTTYPE,SETTYPE                                                
EngCod   LKOUT C,01,FFTENGC,CHAR,FILTROUT=TSTFENG,SKIPCOLS=ENGSKIPS             
ENGSKIP  EQU   *                                                                
EngNam   LKOUT C,02,FFTENGN,CHAR,LEN=V                                          
CutOff   LKOUT C,03,FFTECUT,PDAT                                                
ENGSKIPS EQU   (*-ENGSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYDTN   LKOUT A,(R,NXTDTN),NROWS=1,ROWNAME=DTNRECD                             
Array    LKOUT C,R#XDIST,(A,ARYDTL)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYDTL   LKOUT A,(D,B#DTNREC,DTNRFST),EOT=EOR,ROWID=(FFTELD,FFTELQ),   +        
               ROWWIDTH=(V,FFTLN),NEWEL=B                                       
                                                                                
Prout    LKOUT P,FFTTYPE,SETTYPE                                                
DisCod   LKOUT C,01,FFTDISC,CHAR,FILTROUT=TSTFDTN,SKIPCOLS=DTNSKIPS             
DTNSKIP  EQU   *                                                                
DisNam   LKOUT C,02,FFTDISN,CHAR,LEN=V                                          
DisAmnt  LKOUT C,03,FFTDISA,SPAK                                                
CutOff   LKOUT C,04,FFTDCUT,PDAT                                                
DTNSKIPS EQU   (*-DTNSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
NXTVEH   GOTOR PERDTL,DMCB,QM_PERS,QM_CDATE                                     
         JE    NXTVEH02                                                         
         MVC   LP_ERROR,=AL2(AE$PERDT)                                          
         J     XERROR                                                           
                                                                                
VEH      USING VEHRECD,IOKEY                                                    
NXTVEH02 XC    VEH.VEHKEY,VEH.VEHKEY  Read for vehicle record                   
         MVI   VEH.VEHKTYP,VEHKTYPQ     with office first                       
         MVI   VEH.VEHKSUB,VEHKSUBQ                                             
         MVC   VEH.VEHKCPY,CUXCPY                                               
         MVC   VEH.VEHKOFF,CH_OFF                                               
         MVC   VEH.VEHKDATE,QM_CDATE                                            
         XC    VEH.VEHKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   VEH.VEHKEY(VEHKDATE-VEHKEY),IOKEYSAV                             
         JE    NXTVEH04                                                         
                                                                                
         XC    VEH.VEHKEY,VEH.VEHKEY   Read for vehicle without office          
         MVI   VEH.VEHKTYP,VEHKTYPQ      if not found with office               
         MVI   VEH.VEHKSUB,VEHKSUBQ                                             
         MVC   VEH.VEHKCPY,CUXCPY                                               
         MVC   VEH.VEHKOFF,SPACES                                               
         MVC   VEH.VEHKDATE,QM_CDATE                                            
         XC    VEH.VEHKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   VEH.VEHKEY(VEHKDATE-VEHKEY),IOKEYSAV                             
         JE    NXTVEH04                                                         
         MVC   LP_ERROR,=AL2(AE$MVEH)                                           
         J     XERROR                                                           
                                                                                
NXTVEH04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO5                                                          
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  VEH                                                              
                                                                                
FUE      USING FUERECD,IOKEY                                                    
NXTFUEL  XC    FUE.FUEKEY,FUE.FUEKEY                                            
         MVI   FUE.FUEKTYP,FUEKTYPQ                                             
         MVI   FUE.FUEKSUB,FUEKSUBQ                                             
         MVC   FUE.FUEKCPY,CUXCPY                                               
         MVC   FUE.FUEKOFF,CH_OFF                                               
         MVC   FUE.FUEKDATE,QM_CDATE                                            
         XC    FUE.FUEKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   FUE.FUEKEY(FUEKDATE-FUEKEY),IOKEYSAV                             
         JE    NXTFUE02                                                         
                                                                                
         XC    FUE.FUEKEY,FUE.FUEKEY                                            
         MVI   FUE.FUEKTYP,FUEKTYPQ                                             
         MVI   FUE.FUEKSUB,FUEKSUBQ                                             
         MVC   FUE.FUEKCPY,CUXCPY                                               
         MVC   FUE.FUEKOFF,SPACES                                               
         MVC   FUE.FUEKDATE,QM_CDATE                                            
         XC    FUE.FUEKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   FUE.FUEKEY(FUEKDATE-FUEKEY),IOKEYSAV                             
         JE    NXTFUE02                                                         
         MVC   LP_ERROR,=AL2(AE$MFUEL)                                          
         J     XERROR                                                           
                                                                                
NXTFUE02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO5                                                          
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  FUE                                                              
                                                                                
ENG      USING ENGRECD,IOKEY                                                    
NXTENG   XC    ENG.ENGKEY,ENG.ENGKEY                                            
         MVI   ENG.ENGKTYP,ENGKTYPQ                                             
         MVI   ENG.ENGKSUB,ENGKSUBQ                                             
         MVC   ENG.ENGKCPY,CUXCPY                                               
         MVC   ENG.ENGKOFF,CH_OFF                                               
         MVC   ENG.ENGKDATE,QM_CDATE                                            
         XC    ENG.ENGKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   ENG.ENGKEY(ENGKDATE-ENGKEY),IOKEYSAV                             
         JE    NXTENG02                                                         
                                                                                
         XC    ENG.ENGKEY,ENG.ENGKEY                                            
         MVI   ENG.ENGKTYP,ENGKTYPQ                                             
         MVI   ENG.ENGKSUB,ENGKSUBQ                                             
         MVC   ENG.ENGKCPY,CUXCPY                                               
         MVC   ENG.ENGKOFF,SPACES                                               
         MVC   ENG.ENGKDATE,QM_CDATE                                            
         XC    ENG.ENGKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ENG.ENGKEY(ENGKDATE-ENGKEY),IOKEYSAV                             
         JE    NXTENG02                                                         
         MVC   LP_ERROR,=AL2(AE$MENG)                                           
         J     XERROR                                                           
                                                                                
NXTENG02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO5                                                          
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  ENG                                                              
                                                                                
DTN      USING DTNRECD,IOKEY                                                    
NXTDTN   XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,CH_OFF                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    NXTDTN02                                                         
                                                                                
         XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,SPACES                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    NXTDTN02                                                         
         MVC   LP_ERROR,=AL2(AE$MDTN)                                           
         J     XERROR                                                           
                                                                                
NXTDTN02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO5                                                          
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
         DROP  DTN                                                              
                                                                                
***********************************************************************         
*** Claim Mileage Rate Record Download ********************************         
***********************************************************************         
                                                                                
REQCLRD  LKREQ H,A#CMCD,OUTCLRD,NEXTREQ=REQUESTX                                
                                                                                
PerCod   LKREQ F,01,(D,B#SAVED,QM_PERS),CHAR,OLEN=L'QM_PERS,           +        
               MAXLEN=L'QM_PERS,TEXT=AC#PRSN,COL=*                              
CDate    LKREQ F,02,(D,B#SAVED,QM_CDATE),PDAT,OLEN=L'QM_CDATE,         +        
               TEXT=AC#DATE,COL=*                                               
Vehicle  LKREQ F,03,(D,B#SAVED,QM_VEHCD),CHAR,OLEN=L'QM_VEHCD,         +        
               MAXLEN=L'QM_VEHCD,TEXT=AC#FLVEH,COL=*                            
Fuel     LKREQ F,04,(D,B#SAVED,QM_FUECD),CHAR,OLEN=L'QM_FUECD,         +        
               MAXLEN=L'QM_FUECD,TEXT=AC#FUEL,COL=*                             
Engine   LKREQ F,05,(D,B#SAVED,QM_ENGCD),CHAR,OLEN=L'QM_ENGCD,         +        
               MAXLEN=L'QM_ENGCD,TEXT=AC#FLENG,COL=*                            
Distance LKREQ F,06,(D,B#SAVED,QM_DISAM),SPAK,OLEN=L'QM_DISAM,         +        
               MAXLEN=L'QM_DISAM,TEXT=(*,DISTLIT),COL=*                         
NumPass  LKREQ F,07,(D,B#SAVED,QM_PASS),LBIN,OLEN=L'QM_PASS,           +        
               MAXLEN=L'QM_PASS,TEXT=AC#PASGR,COL=*                             
PrvAccm  LKREQ F,08,(D,B#SAVED,QM_DSPAC),SPAK,OLEN=L'QM_DSPAC,         +        
               MAXLEN=(2*L'QM_DSPAC-1),TEXT=(*,PACULIT),COL=*                   
Distance LKREQ F,09,(D,B#SAVED,QM_DISAC),SPAK,OLEN=L'QM_DISAC,         +        
               MAXLEN=(2*L'QM_DISAC-1),TEXT=AC#FLDIS,COL=*                      
AccumNum LKREQ F,11,(D,B#SAVED,QM_ACUNM),LBIN,OLEN=L'QM_ACUNM,         +        
               MAXLEN=(2*L'QM_ACUNM-1),TEXT=(*,ACCULIT),COL=*                   
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLRD  LKOUT H                                                                
                                                                                
CLMVAT   LKOUT R,R#CMCD                                                         
Array    LKOUT C,1,(A,ARYDRR)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
CLMCMC   LKOUT R,01                                                             
Array    LKOUT C,1,(A,ARYDRT)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYDRR   LKOUT A,(R,BLDDRT),NROWS=1                                             
VATamt   LKOUT C,01,(D,B#SAVED,CM_NVATA),SPAK,FILTROUT=TSTDISCL                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYDRT   LKOUT A,(R,NXTDRT),MULTIROW=Y,ROWNAME=DIS_D                            
Gross    LKOUT C,01,DIS_GRSR,SPAK                                               
VAT      LKOUT C,02,DIS_VATR,SPAK                                               
Passngr  LKOUT C,03,DIS_PASR,SPAK                                               
DstBnd   LKOUT C,04,DIS_DBDC,CHAR                                               
DstBndNm LKOUT C,05,DIS_DBDN,CHAR                                               
DstBndAm LKOUT C,06,DIS_DBDA,SPAK                                               
Dstnce   LKOUT C,07,DIS_DISB,SPAK                                               
TotAmt   LKOUT C,08,DIS_TOTA,SPAK                                               
TotVAT   LKOUT C,09,DIS_VATA,SPAK                                               
                                                                                
         LKOUT E                                                                
                                                                                
NXTDRT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTDRT04            No                                           
                                                                                
NXTDRT02 MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         J     NXTDRT06                                                         
                                                                                
NXTDRT04 LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
                                                                                
NXTDRT06 TM    TSARERRS,TSEEOF                                                  
         JNZ   NXTDRT08                                                         
         LA    R0,EX_BUFF                                                       
         ST    R0,LP_ADATA                                                      
         J     EXITY                                                            
                                                                                
NXTDRT08 MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
                                                                                
BLDDRT   ZAP   CM_NVATA,PZERO                                                   
         ZAP   CM_TOTAM,PZERO                                                   
         ZAP   CM_TOTVT,PZERO                                                   
         GOTOR PRODRT                                                           
         JNE   XERROR                                                           
         ST    R8,LP_ADATA                                                      
         J     EXITY                                                            
                                                                                
SETNAME  GOTOR (#GETPIN,AGETPIN)                                                
         J     EXIT                                                             
                                                                                
SETPFND  L     R1,LP_AINP          Set payment element found                    
         MVC   BYTE1(L'CLM_PFND),0(R1)                                          
         J     EXIT                                                             
                                                                                
FLTAURA  XR    RF,RF               Aura?                                        
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         J     EXIT                                                             
                                                                                
FLTBRAND XR    RF,RF               BrandOcean (not Aura)?                       
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         J     SETCCC                                                           
                                                                                
         EJECT                                                                  
                                                                                
                                                                                
ITMSTAT  L     R1,LP_AINP                                                       
         MVI   CI_ITMDL,NOQ                                                     
         MVI   CI_SELTD,NOQ                                                     
         MVI   CI_NETIN,NOQ                                                     
         MVI   CI_AGYIN,NOQ                                                     
         MVI   CI_DISCL,NOQ                                                     
         TM    0(R1),CIDMSID       Is item deleted                              
         JZ    *+8                                                              
         MVI   CI_ITMDL,YESQ                                                    
         TM    0(R1),CIDMSSS       Is item selected                             
         JZ    *+8                                                              
         MVI   CI_SELTD,YESQ                                                    
         TM    0(R1),CIDMSNT       Is item input net                            
         JZ    *+8                                                              
         MVI   CI_NETIN,YESQ                                                    
         TM    0(R1),CIDMSAI       Is item agency input for foreign             
         JZ    *+8                                            currency          
         MVI   CI_AGYIN,YESQ                                                    
         TM    0(R1),CIDMDIS       Disclaimer completed for this claim          
         JZ    *+8                                             item             
         MVI   CI_DISCL,YESQ                                                    
         J     EXIT                                                             
                                                                                
DISTOTS  L     R1,LP_AINP                                                       
         USING CIDMDNTY,R1                                                      
         ZAP   PACK16,CIDMDGRT                                                  
         MP    PACK16,CIDMDIST                                                  
*&&US*&& SRP   PACK16,64-3,5       US as mileage held to 2dp and rates          
         ZAP   DUB,PACK16                   to 3dp                              
         XR    RE,RE                                                            
         IC    RE,QM_PASS                                                       
         CVD   RE,DUB1                                                          
         ZAP   PACK16,DUB1                                                      
         MP    PACK16,CIDMDPRT                                                  
         MP    PACK16,CIDMDIST                                                  
         AP    DUB,PACK16                                                       
         ZAP   CM_BNGAM,DUB                                                     
         AP    CM_TOTAM,DUB                                                     
         ZAP   PACK16,CIDMDVRT                                                  
         MP    PACK16,CIDMDIST                                                  
         ZAP   CM_BNVTA,PACK16                                                  
         AP    CM_TOTVT,PACK16                                                  
         ZAP   CM_BNNVA,CM_BNGAM                                                
         SP    CM_BNNVA,CM_BNVTA                                                
         AP    CM_NVATA,CM_TOTAM     Work out total non vatable amount          
         SP    CM_NVATA,CM_TOTVT                                                
         J     EXIT                                                             
                                                                                
DRTSTAT  L     R1,LP_AINP                                                       
         USING FFTELD,R1                                                        
         MVI   CV_ALPAS,NOQ                                                     
         MVI   CV_FUERQ,NOQ                                                     
         MVI   CV_ENGRQ,NOQ                                                     
*&&UK                                                                           
         TM    FFTVSTAT,FFTVSAPA   Passengers allowed for vehicle               
         JZ    *+8                                                              
         MVI   CV_ALPAS,YESQ                                                    
         TM    FFTVSTAT,FFTVSRFT   Requires fuel type for vehicle               
         JZ    *+12                                                             
         MVI   CV_FUERQ,YESQ                                                    
         MVI   CV_NOFUE,NOQ                                                     
         TM    FFTVSTAT,FFTVSRES   Requires engine size for vehicle             
         JZ    *+12                                                             
         MVI   CV_ENGRQ,YESQ                                                    
         MVI   CV_NOENG,NOQ                                                     
*&&                                                                             
         OC    FFTVEHAC,FFTVEHAC                                                
         JZ    *+8                                                              
         MVI   CV_NODIS,NOQ                                                     
         CLC   FFTVEHC,QM_VEHCD                                                 
         JNE   EXIT                                                             
         MVC   QM_ACUNM,FFTVEHAC                                                
         J     EXIT                                                             
                                                                                
BLD1RAC  L     R1,LP_AINP                                                       
         MVC   CH_ULA(L'ACTKUNT+L'ACTKLDG),=C'1R'                               
         MVC   CH_ULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                      
         LLC   RE,ONERL3L                                                       
         AR    R1,RE                                                            
         LLC   RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES      Do we have a person code                     
         EX    RF,0(RE)                                                         
         JNH   EXITY               No                                           
         BASR  RE,0                                                             
         MVC   CH_PER(0),0(R1)                                                  
         EX    RF,0(RE)                                                         
         OC    CH_PER,SPACES                                                    
         J     EXIT                                                             
                                                                                
BLDSJAC  L     R1,LP_AINP                                                       
         MVC   CI_SJULA(L'ACTKUNT+L'ACTKLDG),PRODUL                             
         MVC   CI_SJULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   CI_CLI(0),0(R1)                                                  
         EX    RE,0(RF)                                                         
         J     EXIT                                                             
                                                                                
BLD2PAC  L     R1,LP_AINP                                                       
         CLC   0(L'ACTKACT,R1),SPACES                                           
         JNH   EXIT                                                             
         MVC   CI_2PULA(L'ACTKUNT+L'ACTKLDG),=C'2P'                             
         MVC   CI_2PULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         J     EXIT                                                             
                                                                                
BLD2DAC  L     R1,LP_AINP                                                       
         CLC   0(L'ACTKACT,R1),SPACES                                           
         JNH   EXIT                                                             
         MVC   CI_2DULA(L'ACTKUNT+L'ACTKLDG),=C'2D'                             
         MVC   CI_2DULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         J     EXIT                                                             
                                                                                
SETTYPE  L     R1,LP_AINP          Set type of cldel element                    
         MVC   WORK(L'STCXTYP),0(R1)                                            
         J     EXIT                                                             
                                                                                
         USING FFTELD,R1                                                        
SETVACM  L     R1,LP_AINP                                                       
         ZAP   CI_DIST,PZERO                                                    
         LA    R6,CV_DIST                                                       
         XR    RF,RF                                                            
         ICM   RF,1,FFTVEHAC                                                    
         BZR   RE                                                               
         SHI   RF,1                                                             
         LTR   RF,RF                                                            
         JZ    SETVAC02                                                         
         MHI   RF,L'CV_DISAC                                                    
         AR    R6,RF                                                            
SETVAC02 ZAP   CI_DIST,0(L'CI_DIST,R6)                                          
         BR    RE                                                               
                                                                                
         USING CIDELD,R1                                                        
SETQEST  L     R1,LP_AINP                                                       
         CLI   CIDLN,CIDCLNQ                                                    
         BER   RE                                                               
         MVC   CI_ESTN,CIDCEGN                                                  
         BR    RE                                                               
                                                                                
SETDRT   L     R1,LP_AINP                                                       
         ZAP   CM_NVATA,PZERO                                                   
         ZAP   CM_TOTAM,PZERO                                                   
         ZAP   CM_TOTVT,PZERO                                                   
         CLI   CIDLN,CIDMLNQ                                                    
         BLR   RE                                                               
         MVC   QM_PERS,CH_PER      Extract person code                          
         MVC   QM_CDATE,CI_DATE    Item date                                    
         ZAP   QM_DISAM,CIDMMIL    Distance for claim item                      
         MVC   QM_VEHCD,CIDMVEH    Vehicle type used                            
         MVC   QM_PASS,CIDMPASS    Number of passengers                         
         MVC   QM_FUECD,CIDMFUEL   Fuel used                                    
         MVC   QM_ENGCD,CIDMENG    Engine size                                  
         ZAP   QM_DSPAC,CIDMACDS   Previous accumulated distance                
         MVI   QM_DISCL,YESQ                                                    
         ZAP   QM_DISAC,PZERO                                                   
         ST    RE,SAVERE                                                        
         MVC   WORK(L'CM_RDATE),CM_RDATE                                        
         MVC   WORK(1),QM_CDATE                                                 
         CLC   QM_CDATE,WORK                                                    
         JNL   SETDRT2                                                          
         GOTO1 VDATCON,DMCB,(1,WORK),(0,WORK+6)                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),WORK+12,F'-1'                          
         GOTO1 VDATCON,DMCB,(0,WORK+12),(1,WORK)                                
         L     R1,LP_AINP          restore a(input)                             
SETDRT2  LA    R6,CM_DIST                                                       
         LHI   RF,CM_MAXAC                                                      
AC       USING CM_DIST,R6                                                       
SETDRT4  OC    AC.CM_DISAC,AC.CM_DISAC                                          
         JZ    SETDRT8                                                          
         CLC   AC.CM_DISNM,QM_ACUNM                                             
         JNE   SETDRT6                                                          
         CLC   AC.CM_DISDT,WORK                                                 
         JNE   SETDRT6                                                          
         ZAP   QM_DISAC,AC.CM_DISAC                                             
         J     SETDRT8                                                          
SETDRT6  LA    R6,CM_DISTL(R6)                                                  
         JCT   RF,SETDRT4                                                       
SETDRT8  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  AC                                                               
SETACUM  L     R1,LP_AINP                                                       
         CLI   CIDLN,CIDMLNQ                                                    
         BLR   RE                                                               
         ST    RE,SAVERE                                                        
         MVC   WORK(L'CM_RDATE),CM_RDATE                                        
         MVC   WORK(1),QM_CDATE                                                 
         CLC   QM_CDATE,WORK                                                    
         JNL   SETAU02                                                          
         GOTO1 VDATCON,DMCB,(1,WORK),(0,WORK+6)                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),WORK+12,F'-1'                          
         GOTO1 VDATCON,DMCB,(0,WORK+12),(1,WORK)                                
         L     R1,LP_AINP          restore a(input)                             
SETAU02  LA    R6,CM_DIST                                                       
         LHI   RF,CM_MAXAC                                                      
AC       USING CM_DIST,R6                                                       
SETAU10  OC    AC.CM_DISAC,AC.CM_DISAC                                          
         JZ    SETAU30                                                          
         CLC   AC.CM_DISNM,QM_ACUNM                                             
         JNE   SETAU20                                                          
         CLC   AC.CM_DISDT,WORK                                                 
         JNE   SETAU20                                                          
         OC    CIDMMIL,CIDMMIL     Any mileage amount?                          
         JZ    SETAU40                                                          
         AP    AC.CM_DISAC,CIDMMIL                                              
         J     SETAU40                                                          
SETAU20  LA    R6,CM_DISTL(R6)                                                  
         JCT   RF,SETAU10                                                       
         DC    H'0'                Need to have more entries                    
                                                                                
SETAU30  ZAP   AC.CM_DISAC,PZERO                                                
         OC    CIDMMIL,CIDMMIL     Any mileage amount?                          
         JZ    *+10                                                             
         ZAP   AC.CM_DISAC,CIDMMIL                                              
         MVC   AC.CM_DISNM,QM_ACUNM                                             
         MVC   AC.CM_DISDT,WORK                                                 
SETAU40  L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
         DROP  AC                                                               
         DROP  R1                                                               
                                                                                
SETNVT   L     R1,LP_AINP                                                       
         ZAP   0(L'CIDMNVTA,R1),CM_NVATA  Set non vatable amount                
         J     EXIT                                                             
                                                                                
SETAMT   L     R2,AIO6                                                          
         USING EXCRECD,R2                                                       
         TM    EXCRSTAT,EXCSCOMP   If posted don't amend amount                 
         JNZ   EXIT                                                             
         L     R1,LP_AINP                                                       
         CP    0(L'CIDMAMT,R1),CM_TOTAM  Has total amount changed               
         JE    EXIT                                                             
         MVI   CM_RTCHG,YESQ                                                    
         ZAP   0(L'CIDMAMT,R1),CM_TOTAM  Set total amount                       
         J     EXIT                                                             
                                                                                
SETITYPE L     R1,LP_AINP          Set type of cidel element                    
         MVC   CI_TYPE,0(R1)                                                    
         J     EXIT                                                             
                                                                                
SETITDTE L     R2,LP_AINP          Set item date                                
         GOTO1 VDATCON,DMCB,(2,0(R2)),(1,CI_DATE)                               
         J     EXIT                                                             
                                                                                
SETTOTL  L     R1,LP_AINP          Set total of claim                           
         ZAP   CH_TOTAL,0(L'CLDTAMT,R1)                                         
         J     EXIT                                                             
                                                                                
SETHDR   MVI   CH_HDR,YESQ         Set header has been read                     
         J     EXIT                                                             
*&&US                                                                           
SETPRVNM LM    R2,R4,LP_AINP                                                    
         LA    RE,PRVTAB                                                        
         USING PRVTABD,RE                                                       
                                                                                
         MVC   CI_PRVCD,0(R2)                                                   
SETPRV02 CLI   PRVCODE,X'FF'       EOT                                          
         JE    EXIT                                                             
         CLC   0(L'CIDMPROV,R2),PRVCODE                                         
         JE    *+12                                                             
         LA    RE,PRVTABL(,RE)                                                  
         J     SETPRV02                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PRVNAM          PROVINCE NAME DDICT                         
         LA    RF,DSDICTL-SAVED(RF,R8)                                          
         LLC   R1,PRVNAML                                                       
         DROP  RE                                                               
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),0(RF)                                                    
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         STCM  R1,15,LP_OLEN                                                    
         J     EXIT                                                             
                                                                                
SETPRVC  L     R1,LP_AINP                                                       
         MVC   CI_PRVCD,0(R1)                                                   
         J     EXIT                                                             
                                                                                
*&&                                                                             
TESTNVAT DS    0H                                                               
*&&UK*&& TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         BR    RE                                                               
*&&UK                                                                           
TSTMLTC  TM    CPXSTAT1,CPXMCAGY                                                
         J     SETCCC                                                           
*&&                                                                             
TSTCOMPN L     R2,AIO6             Test to fail if expense claim is             
         USING EXCRECD,R2                           posted                      
         TM    EXCRSTAT,EXCSCOMP                                                
         BR    RE                                                               
                                                                                
TSTCOMPY L     R2,AIO6             Test to fail if expense claim is             
         USING EXCRECD,R2                 anything but posted                   
         TM    EXCRSTAT,EXCSCOMP                                                
         J     SETCCC                                                           
                                                                                
TSTEST   OC    CI_ESTN,CI_ESTN                                                  
         J     SETCCC                                                           
                                                                                
TSTFVEH  CLI   WORK,FFTTVEH        Test free form vehicle type                  
         BR    RE                                                               
                                                                                
TSTFFUE  CLI   WORK,FFTTFUE        Test free form fuel type                     
         BR    RE                                                               
                                                                                
TSTFENG  CLI   WORK,FFTTENG        Test free form engine size                   
         BR    RE                                                               
                                                                                
TSTFDTN  CLI   WORK,FFTTDIS        Test free form distance                      
         BR    RE                                                               
                                                                                
TSTADV   CLI   CI_TYPE,C'A'        Test if an advance                           
         BR    RE                                                               
                                                                                
TSTQFUEL CLI   QM_FUEL,YESQ        Fuel records required                        
         BNER  RE                                                               
         CLI   CV_NOFUE,NOQ        Any vehicles with fuel required              
         BR    RE                                                               
                                                                                
TSTPFND  CLI   BYTE1,1             Have we found a payment element              
         BR    RE                                                               
                                                                                
TSTQVEH  CLI   QM_VEHCL,YESQ       Vehicle records required                     
         BR    RE                                                               
                                                                                
TSTQENG  CLI   QM_ENGSZ,YESQ       Engine size record required                  
         BNER  RE                                                               
         CLI   CV_NOENG,NOQ        Any vehicles with engine required            
         BR    RE                                                               
                                                                                
TSTQDTN  CLI   QM_DIST,YESQ        Distance records required                    
         BNER  RE                                                               
         CLI   CV_NODIS,NOQ        Any vehicles with distance required          
         BR    RE                                                               
                                                                                
TSTTHDR  CLI   WORK,CLDTHDRQ       Test if header info element                  
         BR    RE                                                               
                                                                                
TSTTCMT  CLI   WORK,CLDCCMTQ       Test if claim comment element                
         BR    RE                                                               
                                                                                
TSTTLMA  CLI   WORK,CLDLMAPQ       Test if line manager approver ele            
         BR    RE                                                               
                                                                                
TSTTFNA  CLI   WORK,CLDFNAPQ       Test if finance approver element             
         BR    RE                                                               
                                                                                
TSTTMAIN CLI   WORK,CIDTYMQ        Test if main claim item data element         
         BR    RE                                                               
                                                                                
TSTTAPPR CLI   WORK,CIDTYAQ        Test if approver item element                
         BNER  RE                                                               
         CLI   CI_ITMDL,YESQ       Test if item is deleted                      
         J     SETCCC                                                           
                                                                                
TSTTACNT CLI   WORK,CIDTYCQ        Test if account code item element            
         BR    RE                                                               
                                                                                
TSTTORNA CLI   WORK,CIDTYOQ        Test if original narrative ele               
         BR    RE                                                               
                                                                                
TSTTNARR CLI   WORK,CIDTYNQ        Test if narrative element                    
         BR    RE                                                               
*                                                                               
TSTTPYMT CLI   WORK,CIDTYPQ        Test if Expense payment details              
         BR    RE                                                               
                                                                                
TSTXDFC  CLI   WORK,XDFEDCQ        Test if character string for xdata           
         BER   RE                                                               
         CLI   WORK,XDFEDNQ        Test if numerical string for xdata           
         BER   RE                                                               
         CLI   WORK,XDFEDXQ        Test if dropdown for xdata                   
         BER   RE                                                               
         CLI   WORK,XDFEDYQ        Test if yes/no field for xdata               
         BR    RE                                                               
                                                                                
TSTXDFA  CLI   WORK,XDFEDAQ        Test if amount field for xdata               
         BR    RE                                                               
                                                                                
TSTXDFD  CLI   WORK,XDFEDDQ        Test if date field for xdata                 
         BR    RE                                                               
                                                                                
TSTHDR   CLI   CH_HDR,YESQ         Test if header details read                  
         J     SETCCC                                                           
                                                                                
TSTSJOFF CLC   CI_SJOFF,SPACES     Test we have office code                     
         J     SETCCC                                                           
                                                                                
TSTDISCL CLI   QM_DISCL,YESQ       Test we came from display call               
         J     SETCCC                                                           
                                                                                
TSTXAUD  CLI   QD_CDPDF,YESQ       Test we want display to return audit         
         BER   RE                    - needed for PDF and when expense          
         CLI   CH_STAT,REJECTDQ         claim is rejected                       
         BR    RE                                                               
                                                                                
         USING CH_LMAPD,R1                                                      
TSTLMA   L     R1,LP_AINP                                                       
         OC    CH_LMPID,CH_LMPID                                                
         J     SETCCC                                                           
                                                                                
         USING CIDELD,R1                                                        
TSTVEH   L     R1,LP_AINP                                                       
         MVI   CM_RTCHG,NOQ                                                     
         CLI   CIDLN,CIDMLNQ                                                    
         JE    SETCCC                                                           
         OC    CIDMVEH,CIDMVEH     Test we have vehicle code                    
         J     SETCCC                                                           
                                                                                
TST2PAC  L     R1,LP_AINP                                                       
         OC    CIDC2PA,CIDC2PA     Test we have 2P acc code                     
         J     SETCCC                                                           
                                                                                
TST2DAC  L     R1,LP_AINP                                                       
         OC    CIDC2DA,CIDC2DA     Test we have 2D acc code                     
         J     SETCCC                                                           
                                                                                
TSTEXPAC L     R1,LP_AINP          Test if expense account exists               
         OC    CIDCEXP,CIDCEXP                                                  
         J     SETCCC                                                           
                                                                                
TSTSUPAC L     R1,LP_AINP          Test if supplier account exists              
         OC    CIDCSUP,CIDCSUP                                                  
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
TSTPROD  LA    R1,CI_SJULA                                                      
         USING ACTKULA,R1                                                       
         LLC   RF,PCLILEN                                                       
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '                                                       
         J     SETCCC                                                           
                                                                                
TSTJOB   LA    R1,CI_SJULA                                                      
         LLC   RF,PPROLEN                                                       
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '                                                       
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
         USING CLI_D,RF                                                         
TSTSJ    L     RF,LP_AINP          Test it is an SJ account                     
         CLC   CLI_IUL(L'ACTKUNT+L'ACTKLDG),SPACES                              
         J     SETCCC                                                           
*                                                                               
TSTEXP   L     RF,LP_AINP          Test it is an SJ account                     
         CLC   CLI_IEXP(L'ACTKUNT+L'ACTKLDG),SPACES                             
         J     SETCCC                                                           
         DROP  RF                                                               
                                                                                
TSTNFNR  CLI   QS_CSVIE,QS_CSVRQ   Test if not finance report                   
         J     SETCCC                                                           
                                                                                
TSTFINR  CLI   QS_CSVIE,QS_CSVRQ   Test if finance report                       
         BR    RE                                                               
                                                                                
REQUESTX LKREQ X                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
***********************************************************************         
* See what office we should post to                                   *         
* - Reads company or office list or office records                    *         
* - Parm1 = Office                                                    *         
***********************************************************************         
                                                                                
POSTOFF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*POSTOFF'                                                    
*                                                                               
         LR    R3,R1                                                            
         MVI   CH_1RBIL,NOQ                                                     
         MVI   CH_1RNBL,NOQ                                                     
         TM    CPXSTAT5,CPXUS1PY   Do we use 1R office for billable             
         JZ    POSTOF02            No                                           
         MVI   CH_1RBIL,YESQ       Yes                                          
*                                                                               
POSTOF02 TM    CPXSTAT5,CPXUS1HY   Do we use 1R office for non-billable         
         JZ    POSTOF04                                                         
         MVI   CH_1RNBL,YESQ       Yes                                          
         TM    CPXSTAT5,CPXUS1PY   Do we use 1R office for billable             
         JNZ   EXITY                                                            
*                                                                               
K        USING OFLPASD,IOKEY                                                    
POSTOF04 XC    K.OFLPAS,K.OFLPAS   Build office list passive key                
         MVI   K.OFLPTYP,OFLPTYPQ                                               
         MVI   K.OFLPSUB,OFLPSUBQ                                               
         MVC   K.OFLPCPY,CUXCPY                                                 
         MVC   K.OFLPOFF,0(R3)                                                  
         OC    K.OFLPOFF,SPACES                                                 
         MVC   CSVKEY1,K.OFLPAS                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     POSTOF08                                                         
*                                                                               
POSTOF06 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
*                                                                               
POSTOF08 CLC   CSVKEY1(OFLPOFL-OFLPAS),K.OFLPAS NO OFFICE LIST SETTING          
         JNE   POSTOF14                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETELA,CPXELQ                                                    
         JNE   POSTOF06                                                         
OF       USING CPXELD,R1                                                        
         TM    OF.CPXSTAT5,CPXUS1PY   Check office to see whether               
         JZ    POSTOF12               to use 1R office                          
         MVI   CH_1RBIL,YESQ                                                    
                                                                                
POSTOF12 TM    OF.CPXSTAT5,CPXUS1HY                                             
         JZ    POSTOF06                                                         
         MVI   CH_1RNBL,YESQ                                                    
         J     POSTOF06                                                         
         DROP  OF                                                               
*                                                                               
K        USING OFFRECD,IOKEY                                                    
POSTOF14 MVC   K.OFFKEY,SPACES                                                  
         MVI   K.OFFKTYP,OFFKTYPQ                                               
         MVC   K.OFFKCPY,CUXCPY                                                 
         MVC   K.OFFKOFF,0(R3)                                                  
         OC    K.OFFKOFF,SPACES                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   EXITY                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETELA,CPXELQ                                                    
         JNE   EXITY                                                            
OF       USING CPXELD,R1                                                        
         TM    OF.CPXSTAT5,CPXUS1PY                                             
         JZ    POSTOF16                                                         
         MVI   CH_1RBIL,YESQ                                                    
                                                                                
POSTOF16 TM    OF.CPXSTAT5,CPXUS1HY                                             
         JZ    EXITY                                                            
         MVI   CH_1RNBL,YESQ                                                    
         J     EXITY                                                            
         DROP  OF                                                               
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
                                                                                
***********************************************************************         
* Get Person office for claim item date                               *         
***********************************************************************         
                                                                                
PERDTL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PERDTL*'                                                      
         LM    R3,R4,0(R1)                                                      
         LA    R6,CV_DIST                                                       
         LHI   RF,CV_MAXAC                                                      
AC       USING CV_DIST,R6                                                       
PRDT002  ZAP   AC.CV_DISAC,PZERO   Zero the accumulators                        
         LA    R6,CV_DISTL(R6)                                                  
         JCT   RF,PRDT002                                                       
         DROP  AC                                                               
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            Read for person record                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,0(R3)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO1                                                          
         LA    R3,PERRFST          Locate elements                              
         USING LOCELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
PRDT010  CLI   LOCEL,0                                                          
         JE    PRDT050                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    PRDT020                                                          
         CLI   LOCEL,LIDELQ                                                     
         JE    PRDT030                                                          
                                                                                
PRDT015  IC    R0,LOCLN                                                         
         AR    R3,R0                                                            
         J     PRDT010                                                          
                                                                                
PRDT020  OC    LOCEND,LOCEND                                                    
         JZ    PRDT025                                                          
         CLC   0(L'QM_CDATE,R4),LOCEND                                          
         JH    PRDT015                                                          
                                                                                
PRDT025  MVC   CH_OFF,LOCOFF                                                    
         J     PRDT015                                                          
                                                                                
         USING LIDELD,R3                                                        
PRDT030  CLI   LIDTYPE,LIDTDTAC    Is it accumulator list                       
         JNE   PRDT015             No                                           
         LLC   R2,LIDLN                                                         
         AR    R2,R3               R2=A(end of element)                         
         LA    R1,LIDDATA                                                       
M        USING LIDDATA,R1                                                       
PRDT035  CR    R1,R2               Have we reached end of list element          
         JNL   PRDT015             Yes                                          
         CLC   0(L'QM_CDATE,R4),M.LIDDSTDT                                      
         JL    PRDT040                                                          
         CLC   0(L'QM_CDATE,R4),M.LIDDENDT                                      
         JH    PRDT040                                                          
         XR    RF,RF                                                            
         ICM   RF,1,M.LIDDNUM                                                   
         JZ    PRDT040                                                          
         SHI   RF,1                                                             
         LA    R6,CV_DIST                                                       
         LTR   RF,RF                                                            
         JZ    PRDT038                                                          
         MHI   RF,L'CV_DISAC                                                    
         AR    R6,RF                                                            
PRDT038  ZAP   0(L'CV_DISAC,R6),M.LIDDDISS                                      
         CLI   QD_CDVIW,QD_CDVFQ   Finance viewer use posted values             
         JNE   PRDT040                                                          
         ZAP   0(L'CV_DISAC,R6),PZERO   *** temp to skip short els              
         CLI   LIDLN,LIDLNDQ+LIDDLNQ    ***                                     
         JL    PRDT040                  ***                                     
         ZAP   0(L'CV_DISAC,R6),M.LIDDDISP                                      
PRDT040  AHI   R1,LIDDLNQ                                                       
         J     PRDT035                                                          
                                                                                
PRDT050  J     EXITY                                                            
         DROP  M,R3                                                             
***********************************************************************         
* Get option maintain values                                          *         
***********************************************************************         
                                                                                
GETOPTM  NTR1  LABEL=NO,WORK=(RC,VIWORKL)                                       
         J     *+12                                                             
         DC    C'*GETOPTM'                                                      
                                                                                
         USING VIWORKD,RC                                                       
         GOTOR CLRWRK,VIWORKL      Clear work area                              
         USING OB_D,VIRBAREA                                                    
         LA    R2,CI_SJULA         For evey blank line in previous              
         MVI   OB_MTYP,OB_MTYPQ    Build key of buffer record                   
         MVI   OB_MSUB,OB_MSUBQ                                                 
         MVC   OB_MJOB,0(R2)                                                    
         MVC   OB_MWCD,CI_WC                                                    
         GOTOR GETBUF,OB_D                                                      
         JH    EXITN                                                            
         JE    GETOPT04            If found have all values set                 
                                                                                
         L     R0,AGOBLOCB                                                      
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AGOXBLCK                                                      
         LHI   R1,GOXBLKX-GOXBLOCK                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AGOBBLCK                                                      
         LHI   R1,GOBBLKXX-GOBBLOCK                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R3,AGOBLOCB                                                      
         USING GOBLOCKD,R3                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   GOSELCLI(0),L'ACTKLDG+L'ACTKUNT(R2)                              
         EX    RE,0(R1)                                                         
                                                                                
*&&US*&& CLC   GOSELCLI,SPACES                                                  
*&&US*&& JNH   GETOPT04                                                         
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PCLILEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RF,0(RE)                                                         
         JNH   GETOPT02            NO                                           
         MVC   GOSELPRO,SPACES                                                  
         BASR  RE,0                                                             
         MVC   GOSELPRO(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         LR    R1,R2                                                            
         AR    R1,RE                                                            
         AHI   R1,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES      DO WE HAVE A JOB CODE                        
         EX    RF,0(RE)                                                         
         JNH   GETOPT02            NO                                           
         MVC   GOSELJOB,SPACES                                                  
         BASR  RE,0                                                             
         MVC   GOSELJOB(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         MVC   GOSELWC,CI_WC                                                    
                                                                                
GETOPT02 GOTOR VGETOPT,DMCB,GOBLOCKD                                            
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   OB_BILO,GOBILO      Billable only                                
                                                                                
         MVC   OB_NJBNB,GONJLE     Pass no job allowed for non billable         
                                                                                
         GOTOR ADDBUF,OB_D         Add option buffer record                     
         DROP  R3                                                               
GETOPT04 MVC   CI_NJBNB,OB_NJBNB                                                
         MVC   CI_BILO,OB_BILO                                                  
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
VIWORKD  DSECT                     ** VALINC local w/s **                       
VIRBAREA DS    XL(OB_LNQ)          GETOPT buffer record                         
         ORG   VIRBAREA                                                         
OB_MTYP  DS    X                   ** Buffer key **                             
OB_MTYPQ EQU   X'FF'               Buffer type                                  
OB_MSUB  DS    C                                                                
OB_MSUBQ EQU   C'M'                Buffer sub-type                              
OB_MJOB  DS    CL(L'ACTKULA)       Job code                                     
OB_MWCD  DS    CL(L'CI_WC)         Work code                                    
         ORG   VIRBAREA+(OB_DATA-OB_D)                                          
OB_NJBNB DS    CL(L'GONJLE)        No job iput allowed on non billable          
OB_BILO  DS    CL(L'GOBILO)        Billable only                                
         ORG                                                                    
VIWORKL  EQU   *-VIWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit expense record status                                          *         
***********************************************************************         
         SPACE 1                                                                
EDTSTAT  LM    R2,R4,LP_AINP                                                    
         LA    RE,STATAB                                                        
         USING STATABD,RE                                                       
         MVI   0(R4),C'*'                                                       
EDTSTA02 CLI   STATREQ,0                                                        
         JE    EDTSTA06                                                         
         CLC   0(1,R2),STATRST                                                  
         JE    EDTSTA04                                                         
         LA    RE,STATABL(RE)                                                   
         J     EDTSTA02                                                         
                                                                                
EDTSTA04 MVC   0(1,R4),STATREQ                                                  
EDTSTA06 MVC   CH_STAT,0(R4)                                                    
         LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* Edit are approvers chosen by claimer                                *         
***********************************************************************         
         SPACE 1                                                                
EDTAPCH  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EXCKCLAQ                                                   
         JZ    EDTAPCH2                                                         
         MVI   0(R4),YESQ                                                       
EDTAPCH2 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit is claim new approver workflow where approvers are chosen      *         
***********************************************************************         
         SPACE 1                                                                
EDTCAE   LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EXCKCLAQ                                                   
         JZ    EDTCAE02                                                         
         MVI   0(R4),YESQ                                                       
EDTCAE02 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit is claim rejected by finance                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTFNRJ  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EXCKSFRQ                                                   
         JZ    EDTFNRJ2                                                         
         MVI   0(R4),YESQ                                                       
EDTFNRJ2 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit is claim on behalf of someone                                  *         
***********************************************************************         
         SPACE 1                                                                
EDTBHALF LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),EXCKSOBQ                                                   
         JZ    EDTBHA02                                                         
         MVI   0(R4),YESQ                                                       
EDTBHA02 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit connected user raised claim                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTCON   LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         CLC   0(L'CCTPID,R2),CCTPID                                            
         JNE   EDTCON02                                                         
         MVI   0(R4),YESQ                                                       
EDTCON02 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit claim has advances - from CLDSTAT                              *         
***********************************************************************         
         SPACE 1                                                                
EDTCLDAD LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),CLDSADVQ                                                   
         JZ    EDTCLA02                                                         
         MVI   0(R4),YESQ                                                       
EDTCLA02 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit claim to suppress zero amount lines in PDF - from CLDSTAT      *         
***********************************************************************         
         SPACE 1                                                                
EDTCLDZA LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),NOQ                                                        
         TM    0(R2),CLDSZRAQ                                                   
         JZ    EDTCLZ02                                                         
         MVI   0(R4),YESQ                                                       
EDTCLZ02 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit approval status                                                *         
***********************************************************************         
         SPACE 1                                                                
EDTAPST  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'M'          Missing approval                             
         TM    0(R2),CIDAREJ       Rejected                                     
         JZ    EDTAPS02                                                         
         MVI   0(R4),C'R'          Yes                                          
EDTAPS02 TM    0(R2),CIDAAPP       Approved                                     
         JZ    EDTAPS04                                                         
         MVI   0(R4),C'A'          Yes                                          
EDTAPS04 TM    0(R2),CIDAANR       Not required                                 
         JZ    EDTAPS06                                                         
         MVI   0(R4),C'N'          Yes                                          
EDTAPS06 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit whether extra data field is active                             *         
***********************************************************************         
                                                                                
EDTACTV  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         MVI   0(R4),C'A'                                                       
         OC    XDFCUT,XDFCUT                                                    
         JZ    EDTACTVY                                                         
         CLC   XDFCUT,CH_TODP                                                   
         JH    EDTACTVY                                                         
         MVI   0(R4),C'N'                                                       
                                                                                
EDTACTVY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit whether extra data field is required                           *         
***********************************************************************         
                                                                                
EDTREQD  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         TM    XDFSTAT1,XDFSRECP                                                
         JZ    EDTREQ2                                                          
         MVI   0(R4),C'X'                                                       
         J     EDTREQDY                                                         
*                                                                               
EDTREQ2  MVI   0(R4),C'N'                                                       
         TM    XDFSTAT1,XDFSREQD                                                
         JZ    EDTREQDY                                                         
         MVI   0(R4),C'R'                                                       
                                                                                
EDTREQDY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EDIT WHETHER DEFAULT IS SET                                         *         
***********************************************************************         
EDTXLST  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         OC    XDFLCDAT,XDFLCDAT                                                
         JZ    EDTXLST2                                                         
         CLC   XDFLCDAT,CH_TODC                                                 
         JL    EDTXLSTY                 ENTRY IS CUT OFF, CANNOT BE             
*                                                           DEFAULT             
EDTXLST2 TM    XDFLSTAT,XDFLDFT                                                 
         JZ    EDTXLSTY                                                         
         MVI   0(R4),C'Y'                                                       
*                                                                               
EDTXLSTY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit whether extra data field is active                             *         
***********************************************************************         
                                                                                
EDTACTL  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         LA    R3,XDFLCDAT                                                      
         MVI   0(R4),C'A'                                                       
         OC    XDFLCDAT,XDFLCDAT                                                
         JZ    EDTACTLY                                                         
         CLC   XDFLCDAT,CH_TODC                                                 
         JNL   EDTACTLY                                                         
         MVI   0(R4),C'N'                                                       
                                                                                
EDTACTLY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit expenditure category                                           *         
***********************************************************************         
         SPACE 1                                                                
EDTCAT   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ETYKCODE,R2),SPACES                                          
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ETYKCODE),0(R2)                                          
         MVC   CI_ETYP(L'ETYKCODE),0(R2)                                        
         GOTOR VALETY,TEMP2                                                     
         MVC   0(L'OB_ECAT,R4),WORK2                                            
         MVC   CI_EXTNM,TEMP2                                                   
         MVC   CI_ETST2,WORK2+L'OB_ECAT                                         
         LHI   RE,L'OB_ECAT                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit expenditure type name                                          *         
***********************************************************************         
         SPACE 1                                                                
EDTETN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ETYKCODE,R2),SPACES                                          
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ETYKCODE),0(R2)                                          
         MVC   CI_ETYP(L'ETYKCODE),0(R2)                                        
         GOTOR VALETY,TEMP2                                                     
         MVC   0(L'CI_EXTNM,R4),TEMP2                                           
         LHI   RE,L'CI_EXTNM                                                    
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate expenditure type                                           *         
*                                                                     *         
* Ntry:- R1=A(Etype)                                                  *         
***********************************************************************         
                                                                                
VALETY   NTR1  LABEL=NO,WORK=(RC,VEWORKL)                                       
         J     *+12                                                             
         DC    C'*VALETY*'                                                      
                                                                                
         USING VEWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VEWORKL      Clear work area                              
         USING OB_D,VERBAREA                                                    
         MVI   OB_ETYP,OB_ETYPQ    Build key of buffer record                   
         MVI   OB_ESUB,OB_ESUBQ                                                 
         MVC   OB_EETY(L'ETYKCODE),0(R2)                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALETY02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         MVC   WORK2(L'OB_ECAT),OB_ECAT                                         
         MVC   WORK2+L'OB_ECAT(L'OB_EST2),OB_EST2                               
         J     VALETYY                                                          
                                                                                
VALETY02 GOTOR (#GETETN,AGETETN)                                                
         MVC   OB_NAME(L'NAMEREC),TEMP2                                         
         MVC   OB_ECAT,WORK2                                                    
         MVC   OB_EST2,IOKEY+(ETYKSTA2-ETYRECD)                                 
         MVC   WORK2+L'OB_ECAT(L'OB_EST2),OB_EST2                               
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALETYY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VEWORKD  DSECT                     ** VALWCD local w/s **                       
VERBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VERBAREA                                                         
OB_ETYP  DS    X                   ** Buffer key **                             
OB_ETYPQ EQU   X'FF'               Buffer type                                  
OB_ESUB  DS    C                                                                
OB_ESUBQ EQU   C'E'                Buffer sub-type                              
OB_EETY  DS    CL(L'ETYKCODE)      Etype code                                   
         ORG   VERBAREA+(OB_OTHER-OB_D)                                         
OB_ECAT  DS    CL35                Category                                     
OB_EST2  DS    XL1                 Etype status byte 2                          
         ORG                                                                    
VEWORKL  EQU   *-VEWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Read for office code                                                *         
***********************************************************************         
         SPACE 1                                                                
GETOFC   CLC   CH_OFF,SPACES                                                    
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'TRNOFFC),CH_OFF                                          
         GOTOR VALOFF,TEMP2                                                     
         J     EXITY                                                            
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
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate office                                                     *         
*                                                                     *         
* Ntry:- R1=A(Office)                                                 *         
***********************************************************************         
                                                                                
VALOFF   NTR1  LABEL=NO,WORK=(RC,VOWORKL)                                       
         J     *+12                                                             
         DC    C'*VALOFF*'                                                      
                                                                                
         USING VOWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VOWORKL      Clear work area                              
         USING OB_D,VORBAREA                                                    
         MVI   OB_OTYP,OB_OTYPQ    Build key of buffer record                   
         MVI   OB_OSUB,OB_OSUBQ                                                 
         MVC   OB_OOFC(L'TRNOFFC),0(R2)                                         
         MVC   VOFFCODE,0(R2)                                                   
         GOTOR GETBUF,OB_D                                                      
         JL    VALOFF02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
*&&UK                                                                           
         MVC   D_CTRY,OB_CTRY      Set country and currency                     
         MVC   D_CURR,OB_CURR                                                   
*&&                                                                             
         J     VALOFFY                                                          
                                                                                
         USING OFFRECD,R3                                                       
VALOFF02 GOTOR (#GETOFN,AGETOFN)                                                
         MVC   OB_NAME(L'NAMEREC),TEMP2                                         
*&&UK                                                                           
         LA    R3,IOKEY                                                         
         MVC   D_CURR,AGYCURR      Set default from agency                      
         MVC   OFFKEY,SPACES       Read office record                           
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,VOFFCODE                                                 
         OC    OFFKOFF,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   VALOFF08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
VALOFF03 L     R3,AIO3                                                          
         LA    RF,OFFRFST                                                       
*                                                                               
         USING OFIELD,RF                                                        
VALOFF04 CLI   OFIEL,0                                                          
         JE    VALOFF08                                                         
         CLI   OFIEL,OFIELQ                                                     
         JE    VALOFF06                                                         
         LLC   R0,OFILN                                                         
         AR    RF,R0                                                            
         J     VALOFF04                                                         
*                                                                               
VALOFF06 CLI   OFILN,OFILNQ        Check el is long enough                      
         JNH   VALOFF08                                                         
         MVC   OB_CTRY,OFICTRY                                                  
         MVC   D_CTRY,OFICTRY      Set country and currency                     
         CLC   OFICUR,SPACES                                                    
         JNH   *+10                                                             
         MVC   D_CURR,OFICUR                                                    
         MVC   OB_CURR,D_CURR                                                   
         DROP  RF                                                               
*&&                                                                             
VALOFF08 GOTOR ADDBUF,OB_D                                                      
                                                                                
VALOFFY  J     EXITY                                                            
                                                                                
         DROP  RC,R3                                                            
                                                                                
VOWORKD  DSECT                     ** VALOFF local w/s **                       
VOFFCODE DS    CL(L'TRNOFFC)                                                    
VORBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VORBAREA                                                         
OB_OTYP  DS    X                   ** Buffer key **                             
OB_OTYPQ EQU   X'FF'               Buffer type                                  
OB_OSUB  DS    C                                                                
OB_OSUBQ EQU   C'F'                Buffer sub-type                              
OB_OOFC  DS    CL(L'TRNOFFC)       Office code                                  
         ORG                                                                    
VOWORKL  EQU   *-VOWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit vehicle name                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTVEH   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'FFTVEHC,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'FFTVEHC),0(R2)                                           
         GOTOR VALVEH,TEMP2                                                     
         MVC   0(L'FFTVEHN,R4),TEMP2                                            
         LHI   RE,L'FFTVEHN                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate vehicle                                                    *         
*                                                                     *         
* Ntry:- R1=A(Vehicle code)                                           *         
***********************************************************************         
                                                                                
VALVEH   NTR1  LABEL=NO,WORK=(RC,VVWORKL)                                       
         J     *+12                                                             
         DC    C'*VALVEH*'                                                      
                                                                                
         USING VVWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(vehicle code)                           
         GOTOR CLRWRK,VVWORKL      Clear work area                              
         USING OB_D,VVEBAREA                                                    
         MVI   OB_VTYP,OB_VTYPQ    Build key of buffer record                   
         MVI   OB_VSUB,OB_VSUBQ                                                 
         MVC   OB_VEHC(L'FFTVEHC),0(R2)                                         
         GOTOR GETBUF,OB_D                                                      
         JL    VALVEH02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_VEHNM),OB_VEHNM                                       
         MVC   CV_ACUNM,OB_VACNM                                                
         MVI   CV_ALPAS,NOQ                                                     
         MVI   CV_FUERQ,NOQ                                                     
         MVI   CV_ENGRQ,NOQ                                                     
*&&UK                                                                           
         TM    OB_VSTAT,FFTVSAPA                                                
         JZ    *+8                                                              
         MVI   CV_ALPAS,YESQ                                                    
         TM    OB_VSTAT,FFTVSRFT                                                
         JZ    *+8                                                              
         MVI   CV_FUERQ,YESQ                                                    
         TM    OB_VSTAT,FFTVSRES                                                
         JZ    *+8                                                              
         MVI   CV_ENGRQ,YESQ                                                    
*&&                                                                             
         J     VALVEHY                                                          
*                                                                               
         USING EXNPASD,R2                                                       
VALVEH02 LA    R2,IOKEY            Read for expense number passive              
         XC    EXNPAS,EXNPAS         to get PID of claimer                      
         MVI   EXNPTYP,EXNPTYPQ                                                 
         MVI   EXNPSUB,EXNPSUBQ                                                 
         MVC   EXNPCPY,CUXCPY                                                   
         MVC   EXNPTYPE,QD_CDTYP                                                
         MVC   EXNPNUM,QD_CDNUM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
                                                                                
         MVC   CH_PPID#,EXNPXPID                                                
                                                                                
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            Read for PID record to get Person            
         XC    PIDKEY,PIDKEY                                  code              
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,CH_PPID#                                                 
         MVI   PIDKSTYP,PIDKPERQ                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
                                                                                
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JNE   EXITN                                                            
         MVC   QM_PERS,PIDKPER                                                  
                                                                                
         GOTOR PERDTL,DMCB,QM_PERS,QM_CDATE                                     
         JE    VALVEH04                                                         
         DC    H'0'                                                             
                                                                                
                                                                                
VEH      USING VEHRECD,IOKEY                                                    
VALVEH04 XC    VEH.VEHKEY,VEH.VEHKEY                                            
         MVI   VEH.VEHKTYP,VEHKTYPQ                                             
         MVI   VEH.VEHKSUB,VEHKSUBQ                                             
         MVC   VEH.VEHKCPY,CUXCPY                                               
         MVC   VEH.VEHKOFF,CH_OFF                                               
         MVC   VEH.VEHKDATE,QM_CDATE                                            
         XC    VEH.VEHKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   VEH.VEHKEY(VEHKDATE-VEHKEY),IOKEYSAV                             
         JE    VALVEH06                                                         
                                                                                
         XC    VEH.VEHKEY,VEH.VEHKEY                                            
         MVI   VEH.VEHKTYP,VEHKTYPQ                                             
         MVI   VEH.VEHKSUB,VEHKSUBQ                                             
         MVC   VEH.VEHKCPY,CUXCPY                                               
         MVC   VEH.VEHKOFF,SPACES                                               
         MVC   VEH.VEHKDATE,QM_CDATE                                            
         XC    VEH.VEHKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   VEH.VEHKEY(VEHKDATE-VEHKEY),IOKEYSAV                             
         JE    VALVEH06                                                         
         MVC   LP_ERROR,=AL2(AE$MVEH)                                           
         J     XERROR                                                           
                                                                                
VALVEH06 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO3                                                          
         AHI   R2,VEHRFST-VEHRECD                                               
         USING FFTELD,R2                                                        
         MVC   TEMP2,SPACES                                                     
VALVEH08 CLI   FFTEL,0                                                          
         JE    VALVEHY                                                          
         CLI   FFTEL,FFTELQ                                                     
         JE    VALVEH12                                                         
VALVEH10 LLC   R1,FFTLN                                                         
         AR    R2,R1                                                            
         J     VALVEH08                                                         
                                                                                
VALVEH12 CLI   FFTTYPE,FFTTVEH                                                  
         JNE   VALVEH10                                                         
         CLC   FFTVEHC,OB_VEHC                                                  
         JNE   VALVEH10                                                         
         MVC   OB_VSTAT,FFTVSTAT                                                
         MVC   OB_VACNM,FFTVEHAC                                                
         MVC   CV_ACUNM,FFTVEHAC                                                
         MVI   CV_ALPAS,NOQ                                                     
         MVI   CV_FUERQ,NOQ                                                     
         MVI   CV_ENGRQ,NOQ                                                     
         TM    OB_VSTAT,FFTVSAPA                                                
         JZ    *+8                                                              
         MVI   CV_ALPAS,YESQ                                                    
         TM    OB_VSTAT,FFTVSRFT                                                
         JZ    *+8                                                              
         MVI   CV_FUERQ,YESQ                                                    
         TM    OB_VSTAT,FFTVSRES                                                
         JZ    *+8                                                              
         MVI   CV_ENGRQ,YESQ                                                    
         LLC   R1,FFTLN                                                         
         SHI   R1,FFTVLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   TEMP2(0),FFTVEHN                                                 
         EX    R1,0(RE)                                                         
         MVC   OB_VEHNM(L'FFTVEHN),TEMP2                                        
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALVEHY  J     EXITY                                                            
                                                                                
         DROP  VEH,RC                                                           
                                                                                
VVWORKD  DSECT                     ** VALVEH local w/s **                       
VVEBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VVEBAREA                                                         
OB_VTYP  DS    X                   ** Buffer key **                             
OB_VTYPQ EQU   X'FF'               Buffer type                                  
OB_VSUB  DS    C                                                                
OB_VSUBQ EQU   C'V'                Buffer sub-type                              
OB_VEHC  DS    CL(L'TRNOFFC)       Vehicle code                                 
         ORG   VVEBAREA+(OB_OTHER-OB_D)                                         
OB_VEHNM DS    CL50                Vehicle name                                 
OB_VSTAT DS    XL1                 Vehicle status                               
OB_VACNM DS    XL1                 Vehicle accumulator number                   
         ORG                                                                    
VVWORKL  EQU   *-VVWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit fuel name                                                      *         
***********************************************************************         
         SPACE 1                                                                
EDTFUEL  LM    R2,R4,LP_AINP                                                    
         CLC   0(L'FFTFUEC,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'FFTFUEC),0(R2)                                           
         GOTOR VALFUE,TEMP2                                                     
         MVC   0(L'FFTFUEN,R4),TEMP2                                            
         LHI   RE,L'FFTFUEN                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate fuel                                                       *         
*                                                                     *         
* Ntry:- R1=A(Vehicle code)                                           *         
***********************************************************************         
                                                                                
VALFUE   NTR1  LABEL=NO,WORK=(RC,VFWORKL)                                       
         J     *+12                                                             
         DC    C'*VALFUE*'                                                      
                                                                                
         USING VFWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(fuel code)                              
         GOTOR CLRWRK,VFWORKL      Clear work area                              
         USING OB_D,VFUBAREA                                                    
         MVI   OB_FTYP,OB_FTYPQ    Build key of buffer record                   
         MVI   OB_FSUB,OB_FSUBQ                                                 
         MVC   OB_FUEC(L'FFTFUEC),0(R2)                                         
         GOTOR GETBUF,OB_D                                                      
         JL    VALFUE02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_FUENM),OB_FUENM                                       
         J     VALFUEY                                                          
*                                                                               
FUE      USING FUERECD,IOKEY                                                    
VALFUE02 XC    FUE.FUEKEY,FUE.FUEKEY                                            
         MVI   FUE.FUEKTYP,FUEKTYPQ                                             
         MVI   FUE.FUEKSUB,FUEKSUBQ                                             
         MVC   FUE.FUEKCPY,CUXCPY                                               
         MVC   FUE.FUEKOFF,CH_OFF                                               
         MVC   FUE.FUEKDATE,QM_CDATE                                            
         XC    FUE.FUEKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   FUE.FUEKEY(FUEKDATE-FUEKEY),IOKEYSAV                             
         JE    VALFUE04                                                         
                                                                                
         XC    FUE.FUEKEY,FUE.FUEKEY                                            
         MVI   FUE.FUEKTYP,FUEKTYPQ                                             
         MVI   FUE.FUEKSUB,FUEKSUBQ                                             
         MVC   FUE.FUEKCPY,CUXCPY                                               
         MVC   FUE.FUEKOFF,SPACES                                               
         MVC   FUE.FUEKDATE,QM_CDATE                                            
         XC    FUE.FUEKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   FUE.FUEKEY(FUEKDATE-FUEKEY),IOKEYSAV                             
         JE    VALFUE04                                                         
         MVC   LP_ERROR,=AL2(AE$MFUEL)                                          
         J     XERROR                                                           
                                                                                
VALFUE04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO3                                                          
         AHI   R2,FUERFST-FUERECD                                               
         USING FFTELD,R2                                                        
         MVC   TEMP2,SPACES                                                     
VALFUE08 CLI   FFTEL,0                                                          
         JE    VALFUEY                                                          
         CLI   FFTEL,FFTELQ                                                     
         JE    VALFUE12                                                         
VALFUE10 LLC   R1,FFTLN                                                         
         AR    R2,R1                                                            
         J     VALFUE08                                                         
                                                                                
VALFUE12 CLI   FFTTYPE,FFTTFUE                                                  
         JNE   VALFUE10                                                         
         CLC   FFTFUEC,OB_FUEC                                                  
         JNE   VALFUE10                                                         
         MVC   OB_FUEDT,FFTFCUT                                                 
         LLC   R1,FFTLN                                                         
         SHI   R1,FFTFLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   TEMP2(0),FFTFUEN                                                 
         EX    R1,0(RE)                                                         
         MVC   OB_FUENM(L'FFTFUEN),TEMP2                                        
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALFUEY  J     EXITY                                                            
                                                                                
         DROP  FUE,RC,R2                                                        
                                                                                
VFWORKD  DSECT                     ** VALFUE local w/s **                       
VFUBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VFUBAREA                                                         
OB_FTYP  DS    X                   ** Buffer key **                             
OB_FTYPQ EQU   X'FF'               Buffer type                                  
OB_FSUB  DS    C                                                                
OB_FSUBQ EQU   C'F'                Buffer sub-type                              
OB_FUEC  DS    CL(L'TRNOFFC)       Fuel code                                    
         ORG   VFUBAREA+(OB_OTHER-OB_D)                                         
OB_FUENM DS    CL50                Fuel name                                    
OB_FUEDT DS    PL3                 Fuel cut off date                            
         ORG                                                                    
VFWORKL  EQU   *-VFWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit distance band name                                             *         
***********************************************************************         
         SPACE 1                                                                
EDTBNDN  LM    R2,R4,LP_AINP                                                    
         CLC   0(L'FFTDISC,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
DTN      USING DTNRECD,IOKEY                                                    
EDTDTN02 XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,CH_OFF                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    EDTDTN04                                                         
                                                                                
         XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,SPACES                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    EDTDTN04                                                         
         DC    H'0'                                                             
                                                                                
EDTDTN04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R3,AIO3                                                          
         AHI   R3,DTNRFST-DTNRECD                                               
         USING FFTELD,R3                                                        
         MVC   TEMP2,SPACES                                                     
EDTDTN08 CLI   FFTEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ                                                     
         JE    EDTDTN12                                                         
EDTDTN10 LLC   R1,FFTLN                                                         
         AR    R3,R1                                                            
         J     EDTDTN08                                                         
                                                                                
EDTDTN12 CLI   FFTTYPE,FFTTDIS                                                  
         JNE   EDTDTN10                                                         
         CLC   FFTDISC,0(R2)                                                    
         JNE   EDTDTN10                                                         
         LLC   R1,FFTLN                                                         
         SHI   R1,FFTELNQ+1                                                     
         BASR  RE,0                                                             
         MVC   0(0,R4),FFTDISN                                                  
         EX    R1,0(RE)                                                         
         ZAP   CM_BNDAM,FFTDISA                                                 
         AHI   R1,1                                                             
         STCM  R1,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Edit engine name                                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTENG   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'FFTENGC,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'FFTENGC),0(R2)                                           
         GOTOR VALENG,TEMP2                                                     
         MVC   0(L'FFTENGN,R4),TEMP2                                            
         LHI   RE,L'FFTENGN                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate engine                                                     *         
*                                                                     *         
* Ntry:- R1=A(engine code)                                            *         
***********************************************************************         
                                                                                
VALENG   NTR1  LABEL=NO,WORK=(RC,VNWORKL)                                       
         J     *+12                                                             
         DC    C'*VALENG*'                                                      
                                                                                
         USING VNWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(vehicle code)                           
         GOTOR CLRWRK,VNWORKL      Clear work area                              
         USING OB_D,VNGBAREA                                                    
         MVI   OB_NTYP,OB_NTYPQ    Build key of buffer record                   
         MVI   OB_NSUB,OB_NSUBQ                                                 
         MVC   OB_ENGC(L'FFTENGC),0(R2)                                         
         GOTOR GETBUF,OB_D                                                      
         JL    VALENG02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_ENGNM),OB_ENGNM                                       
         J     VALENGY                                                          
*                                                                               
ENG      USING ENGRECD,IOKEY                                                    
VALENG02 XC    ENG.ENGKEY,ENG.ENGKEY                                            
         MVI   ENG.ENGKTYP,ENGKTYPQ                                             
         MVI   ENG.ENGKSUB,ENGKSUBQ                                             
         MVC   ENG.ENGKCPY,CUXCPY                                               
         MVC   ENG.ENGKOFF,CH_OFF                                               
         MVC   ENG.ENGKDATE,QM_CDATE                                            
         XC    ENG.ENGKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   ENG.ENGKEY(ENGKDATE-ENGKEY),IOKEYSAV                             
         JE    VALENG04                                                         
                                                                                
         XC    ENG.ENGKEY,ENG.ENGKEY                                            
         MVI   ENG.ENGKTYP,ENGKTYPQ                                             
         MVI   ENG.ENGKSUB,ENGKSUBQ                                             
         MVC   ENG.ENGKCPY,CUXCPY                                               
         MVC   ENG.ENGKOFF,SPACES                                               
         MVC   ENG.ENGKDATE,QM_CDATE                                            
         XC    ENG.ENGKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ENG.ENGKEY(ENGKDATE-ENGKEY),IOKEYSAV                             
         JE    VALENG04                                                         
         MVC   LP_ERROR,=AL2(AE$MENG)                                           
         J     XERROR                                                           
                                                                                
                                                                                
VALENG04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO3                                                          
         AHI   R2,ENGRFST-ENGRECD                                               
         USING FFTELD,R2                                                        
         MVC   TEMP2,SPACES                                                     
VALENG08 CLI   FFTEL,0                                                          
         JE    VALENGY                                                          
         CLI   FFTEL,FFTELQ                                                     
         JE    VALENG12                                                         
VALENG10 LLC   R1,FFTLN                                                         
         AR    R2,R1                                                            
         J     VALENG08                                                         
                                                                                
VALENG12 CLI   FFTTYPE,FFTTENG                                                  
         JNE   VALENG10                                                         
         CLC   FFTENGC,OB_ENGC                                                  
         JNE   VALENG10                                                         
         LLC   R1,FFTLN                                                         
         SHI   R1,FFTELNQ+1                                                     
         BASR  RE,0                                                             
         MVC   TEMP2(0),FFTENGN                                                 
         EX    R1,0(RE)                                                         
         MVC   OB_ENGNM(L'FFTENGN),TEMP2                                        
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALENGY  J     EXITY                                                            
                                                                                
         DROP  ENG,RC                                                           
                                                                                
VNWORKD  DSECT                     ** VALENG local w/s **                       
VNGBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VNGBAREA                                                         
OB_NTYP  DS    X                   ** Buffer key **                             
OB_NTYPQ EQU   X'FF'               Buffer type                                  
OB_NSUB  DS    C                                                                
OB_NSUBQ EQU   C'V'                Buffer sub-type                              
OB_ENGC  DS    CL(L'FFTENGC)       Engine code                                  
         ORG   VNGBAREA+(OB_OTHER-OB_D)                                         
OB_ENGNM DS    CL50                Engine name                                  
         ORG                                                                    
VNWORKL  EQU   *-VNWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit suppress item from viewer                                      *         
***********************************************************************         
         SPACE 1                                                                
EDTSUP   LM    R2,R4,LP_AINP                                                    
         USING CIDELD,R2                                                        
         MVI   0(R4),NOQ                                                        
         XC    CI_ITAPP(CI_ITAPL*CH_MAXAP),CI_ITAPP                             
         TM    CH_IND,CH_I1RAP     Have we got 1R based appprovers              
         JNZ   EDTSUP02            Yes - no need to get again                   
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    EDTSUP02            No                                           
         GOTOR GETMAP                                                           
         OI    CH_IND,CH_I1RAP     Set we got 1R based appprovers               
EDTSUP02 CLI   CIDTYPE,CIDTYCQ     Do we have account code element              
         JNE   EDTSUP04            No                                           
         GOTOR GETCAP,CIDELD       Find client approvers                        
EDTSUP04 SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Aura                                         
         JE    EDTSUP06                                                         
         CLI   QD_CDVIW,QD_CDVOQ   owner view?                                  
         JE    EDTSUP10                                                         
                                                                                
EDTSUP06 GOTOR CHKAPP              Check is connected user is approver          
         JE    EDTSUP10            Yes                                          
         TM    CH_IND,CH_I1RVW     Does connected user have rights to           
         JNZ   EDTSUP10                                   view                  
         CLI   CH_USRFN,YESQ       Is the connected user fin approver           
         JE    EDTSUP10                                                         
         MVI   0(R4),YESQ          No - suppress from viewing item              
EDTSUP10 LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit department code from 2D account                                *         
***********************************************************************         
         SPACE 1                                                                
EDT2DC   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         LHI   RE,L'ACTKACT                                                     
         USING ACTKACT,R2                                                       
         LA    R1,ACTKACT                                                       
         TM    SCPYEL+CPYSTAT1-CPYELD,CPYSOROE                                  
         JZ    EDT2DC02                                                         
         LHI   RE,L'ACTKACT-1                                                   
         LA    R1,ACTKACT+1                                                     
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    EDT2DC02                                                         
         LHI   RE,L'ACTKACT-2                                                   
         LA    R1,ACTKACT+2                                                     
                                                                                
EDT2DC02 SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),0(R1)                                                    
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
                                                                                
EDT2DC04 STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit VAT code                                                       *         
***********************************************************************         
         SPACE 1                                                                
EDTVATC  LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         LHI   RE,1                                                             
         MVC   0(1,R4),0(R2)                                                    
*&&UK                                                                           
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         JNZ   EDTVATC2                                                         
         MVC   0(L'ACTKACT,R4),2(R2)                                            
         LHI   RE,L'ACTKACT                                                     
*&&                                                                             
                                                                                
EDTVATC2 STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit VAT name and get rate                                          *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
EDTPSTN  LA    RF,CI_PRVCD                                                      
         J     *+6                                                              
*&&                                                                             
*&&UK                                                                           
EDTVTNM  MVI   MYBYTE,FF           VAT CODE/ACCOUNT IN STCXVAT                  
         J     *+10                                                             
*&&                                                                             
EDTVATN  XR    RF,RF                                                            
         MVI   MYBYTE,0                                                         
         LM    R2,R4,LP_AINP                                                    
*                                                                               
*&&UK                                                                           
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         JZ    EDTVTN04                                                         
         CLI   MYBYTE,FF           VAT CODE/ACCOUNT IN STCXVAT                  
         JNE   *+14                No                                           
         CLC   1(L'ACTKACT-1,R2),SPACES                                         
         JH    EDTVTN04            New VAT code is only one char                
                                                                                
V        USING VTCD,ELEMENT                                                     
         XC    V.VTCD(VTCLNQ),V.VTCD                                            
         MVI   V.VTCACTN,VTCAILUP                                               
         MVC   V.VTCCPY,CUXCPY                                                  
         MVC   V.VTCOFFC,SPACES                                                 
         MVC   V.VTCCOMF,ACOMFACS                                               
         MVC   V.VTCINVD,CI_DATE                                                
         MVC   V.VTCTYPE,0(R2)                                                  
         MVC   V.VTCCPYS1,SCPYEL+CPYSTAT1-CPYELD                                
         MVC   V.VTCSGOPO,CH_SGOPO                                              
         CLI   CH_SGOPO,LDGONONE   don't pass office for offpos=T               
         JE    EDTVTN02            as VATICAN won't work                        
         CLI   CH_SGOPO,LDGOTRAN                                                
         JE    EDTVTN02                                                         
         MVC   V.VTCOFFC,CI_OFF                                                 
EDTVTN02 GOTO1 VATICAN,V.VTCD                                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CI_VRATE,V.VTCRATE                                               
         MVC   0(L'VTCTTYPE,R4),V.VTCTTYPE                                      
         LHI   RE,L'VTCTTYPE                                                    
         J     EDTVTN14                                                         
                                                                                
         USING ACTRECD,R1                                                       
EDTVTN04 LA    R1,IOKEY                                                         
         MVC   ACTKEY,SPACES       BUILD KEY TO READ VAT ACCOUNT                
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R1,AIO1                                                          
         LA    R1,ACTRFST                                                       
         USING RATELD,R1                                                        
         XR    RE,RE                                                            
                                                                                
EDTVTN06 CLI   RATEL,RATEVATQ                                                   
         JE    EDTVTN10                                                         
         CLI   RATEL,NAMELQ                                                     
         JE    EDTVTN12                                                         
         CLI   RATEL,0                                                          
         JE    EDTVTN14                                                         
                                                                                
EDTVTN08 XR    R0,R0                                                            
         IC    R0,RATLN                                                         
         AR    R1,R0                                                            
         J     EDTVTN06                                                         
                                                                                
EDTVTN10 MVC   CI_VRATE,RATRATE                                                 
         J     EDTVTN08                                                         
                                                                                
         USING NAMELD,R1                                                        
EDTVTN12 LLC   RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         LTR   RE,RE                                                            
         JM    EDTVTN08                                                         
         BASR  RF,0                                                             
         MVC   0(0,R4),NAMEREC                                                  
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         J     EDTVTN08                                                         
         DROP  R1                                                               
                                                                                
EDTVTN14 STCM  RE,15,LP_OLEN                                                    
*&&                                                                             
*&&US                                                                           
         CLI   0(R2),C' '          Any VAT code?                                
         JNH   EXITY                                                            
*                                                                               
         USING TAXRECD,R3                                                       
V        LA    R3,IOKEY            READ FOR CURRENT OFFICE VAT RECORD           
         XC    TAXKEY,TAXKEY                                                    
         MVI   TAXKTYP,TAXKTYPQ                                                 
         MVC   TAXKCPY,CUXCPY                                                   
         LTR   RF,RF               Calling with province code?                  
         JZ    *+10                                                             
         MVC   TAXKPRV,CI_PRVCD                                                 
         CLI   CH_SGOPO,LDGONONE   don't pass office for offpos=T               
         JE    EDTVTN02                                                         
         CLI   CH_SGOPO,LDGOTRAN                                                
         JE    EDTVTN02                                                         
         MVC   TAXKOFF,CI_OFF                                                   
                                                                                
EDTVTN02 XR    RE,RE                                                            
         ICM   RE,7,CI_DATE                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,TAXKDATE                                                    
                                                                                
         MVC   CSVKEY2,TAXKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY2(TAXKDATE-TAXRECD),TAXKEY                                 
         JE    EDTVTN04                                                         
                                                                                
         MVC   TAXKEY,CSVKEY2      read for current agency VAT record           
         MVC   TAXKOFF,EFFS                                                     
         MVC   CSVKEY2,TAXKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY2(TAXKDATE-TAXRECD),TAXKEY                                 
         JNE   EXITY                                                            
                                                                                
EDTVTN04 DS    0H                  process VAT record                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO1                                                          
         LA    R3,TAXRFST          look for VAT elements                        
                                                                                
         USING TAXELD,R3                                                        
EDTVTN06 CLI   TAXEL,0                                                          
         JE    EXITY                                                            
         CLI   TAXEL,TAXIELQ                                                    
         JE    EDTVTN10                                                         
EDTVTN08 LLC   R0,TAXLN                                                         
         AR    R3,R0                                                            
         J     EDTVTN06                                                         
*                                                                               
EDTVTN10 CLC   TAXCODE,0(R2)       Match on code                                
         JNE   EDTVTN08                                                         
         MVC   0(L'TAXTYPE,R4),TAXTYPE                                          
         LHI   RE,L'TAXTYPE                                                     
         STCM  RE,15,LP_OLEN                                                    
         MVC   CI_VRATE,TAXRATE                                                 
*&&                                                                             
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
         XC    CI_OFF,CI_OFF                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         AHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,0(RF)                                                         
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         MVC   CI_OFF,CI_SJOFF                                                  
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit product name - parm 1 unit/ledger/account                      *         
***********************************************************************         
         SPACE 1                                                                
EDTPRN   LM    R2,R4,LP_AINP                                                    
         XC    CI_SJOFF,CI_SJOFF                                                
         LHI   RE,0                                                             
         STCM  RE,15,LP_OLEN                                                    
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
         CLC   0(0,RE),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RE,0(R1)                                                         
         JNH   EXITY               NO                                           
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         AHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,0(R1)                                                         
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         OC    CI_SJOFF,CI_SJOFF                                                
         JZ    *+10                                                             
         MVC   CI_OFF,CI_SJOFF                                                  
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit job name - parm 1 is unit/ledger/account                       *         
***********************************************************************         
         SPACE 1                                                                
EDTJBN   LM    R2,R4,LP_AINP                                                    
         LHI   RE,0                                                             
         STCM  RE,15,LP_OLEN                                                    
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
         CLC   0(0,RE),SPACES      DO WE HAVE A JOB CODE                        
         EX    RE,0(R1)                                                         
         JNH   EXITY               NO EXIT                                      
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR VALACT,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
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
         MVC   CI_2PAN(CI_EXLNQ),OB_2PAN                                        
         MVC   CI_SJOFF,OB_SJOFF                                                
         MVC   CI_DSCRT,OB_RATE                                                 
         J     VALACTY                                                          
                                                                                
VALACT02 GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
         MVI   OB_2PAN,C'N'        Default set everything as No                 
         MVC   OB_2PAN+1(CI_EXLNQ-1),OB_2PAN                                    
         XC    OB_SJOFF,OB_SJOFF                                                
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R3,ACTRFST                                                       
         USING RSTELD,R3                                                        
         SR    R0,R0                                                            
VALACT04 CLI   RSTEL,0                                                          
         JE    VALACT26                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    VALACT08                                                         
         CLI   RSTEL,PPRELQ                                                     
         JE    VALACT14                                                         
         CLI   RSTEL,RATEDSCQ                                                   
         JE    VALACT16                                                         
*&&US*&& CLI   RSTEL,JOBELQ                                                     
*&&US*&& JE    VALACT18                                                         
VALACT06 IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     VALACT04                                                         
                                                                                
VALACT08 DS    0H                                                               
*&&US                                                                           
         CLC   =C'SI',OB_KEY                                                    
         JE    VALACT10                                                         
         CLC   =C'SE',OB_KEY                                                    
         JNE   VALACT12                                                         
         USING CATD,R1                                                          
         LA    R1,CATBLK           Get cost setting from CATCALL                
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR                                                 
         MVC   CATSEAC(1),CUXCPY   Company code                                 
         MVC   CATSEAC+1(L'ACTKULA),OB_KEY SE account from above                
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'Y'         Do they require cost postings                
         JNE   VALACT12                                                         
         MVI   OB_CLAN,C'Y'                                                     
         J     VALACT12                                                         
*&&                                                                             
VALACT10 CLI   RSTCOSTG,C' '                                                    
         JNH   *+8                                                              
         MVI   OB_CLAN,C'Y'                                                     
                                                                                
VALACT12 TM    RSTSTAT1,RSTSGPEI   Personnel analysis on expense ac             
         JZ    *+8                                                              
         MVI   OB_2PAN,C'Y'                                                     
         TM    RSTSTAT1,RSTSEADD   Department analysis on expense ac            
         JZ    *+8                                                              
         MVI   OB_2DAN,C'Y'                                                     
         TM    RSTSTAT1,RSTSACIL   Is account locked                            
         JZ    *+8                                                              
         MVI   OB_LOCK,C'Y'                                                     
         TM    RSTSTAT1,RSTSACIC   Is account closed                            
         JZ    *+8                                                              
         MVI   OB_CLOS,C'Y'                                                     
                                                                                
         TM    RSTSTAT2,RSTSMILE   Mile analysis on expense account             
         JZ    *+8                                                              
         MVI   OB_MILE,C'Y'                                                     
                                                                                
         TM    RSTSTAT4,RSTSJREA   Job analysis on expense account              
         JZ    *+8                                                              
         MVI   OB_JBAN,C'Y'                                                     
                                                                                
         TM    RSTSTAT5,RSTSPREA   Product analysis on expense account          
         JZ    *+8                                                              
         MVI   OB_PRAN,C'Y'                                                     
         J     VALACT06                                                         
                                                                                
         USING PPRELD,R3                                                        
VALACT14 CLI   PPRGAOFF,X'40'      Have we got a valid office                   
         JNH   VALACT06                                                         
         MVC   OB_SJOFF,PPRGAOFF   Yes - extract it                             
         J     VALACT06                                                         
                                                                                
         USING RATELD,R3                                                        
VALACT16 MVC   OB_RATE,RATRATE                                                  
         J     VALACT06                                                         
                                                                                
         USING JOBELD,R3                                                        
VALACT18 TM    JOBSTA1,JOBSXJOB    Is it expense job                            
         JZ    VALACT06                                                         
         MVI   OB_XJOB,YESQ                                                     
         J     VALACT06                                                         
                                                                                
VALACT26 MVC   CI_2PAN(CI_EXLNQ),OB_2PAN                                        
         MVC   CI_SJOFF,OB_SJOFF                                                
         MVC   CI_DSCRT,OB_RATE                                                 
                                                                                
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALACTY  J     EXITY                                                            
                                                                                
         DROP  RC,R3                                                            
                                                                                
VAWORKD  DSECT                     ** VALACT local w/s **                       
VARBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VARBAREA+(OB_OTHER-OB_D)                                         
OB_2PAN  DS    CL1                 Personnel analysis                           
OB_2DAN  DS    CL1                 Department analysis                          
OB_CLAN  DS    CL1                 Client analysis                              
OB_PRAN  DS    CL1                 Product analysis                             
OB_JBAN  DS    CL1                 Job analysis                                 
OB_MILE  DS    CL1                 Mile analysis                                
OB_LOCK  DS    CL1                 Locked                                       
OB_CLOS  DS    CL1                 Closed                                       
OB_XJOB  DS    CL1                 Expense job                                  
OB_SJOFF DS    CL2                 Office for production accounts               
OB_RATE  DS    XL2                 Discount rate                                
         ORG                                                                    
VAWORKL  EQU   *-VAWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit workcode description - parm 1 is workcode                      *         
***********************************************************************         
         SPACE 1                                                                
EDTWCD   LM    R2,R4,LP_AINP                                                    
         MVC   CI_WC,SPACES                                                     
         CLC   0(L'WCOKWRK,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   CI_WC,0(R2)                                                      
         MVC   TEMP2(L'WCOKWRK),0(R2)                                           
         GOTOR VALWCD,TEMP2                                                     
         MVC   0(L'WCODESC,R4),TEMP2                                            
         LHI   RE,L'WCODESC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit out an exchange rate                                           *         
***********************************************************************         
         SPACE 1                                                                
EDTXRAT  LM    R2,R4,LP_AINP                                                    
         LLC   RF,AFCXSHFT-AFCX(R2)                                             
         AHI   RF,2                                                             
         STC   RF,CI_FCDP                                                       
                                                                                
         GOTOR VHEXOUT,DMCB,(R2),0(R4),L'CIDMRAT                                
         LHI   RE,14                                                            
         STCM  RE,15,LP_OLEN                                                    
         J     EXIT                                                             
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
* Establish approver and back-up approver PID for claim item          *         
***********************************************************************         
                                                                                
GETCAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETCAP*'                                                      
                                                                                
         LR    R7,R1                                                            
C        USING CIDELD,R7                                                        
         LA    R7,C.CIDCCPJ                                                     
         ST    R7,SAVERF                                                        
         LLC   RF,PPROLEN                                                       
         AR    RF,R7                                                            
         MVC   CI_MED,0(RF)        Extract media code                           
         LR    R7,R1                                                            
         L     R2,AEXPREC                                                       
         AHI   R2,EXCRFST-EXCRECD                                               
         USING CIDELD,R2                                                        
GETCAP02 CLI   CIDEL,0                                                          
         JE    EXITY                                                            
         CLI   CIDEL,CIDELQ                                                     
         JNE   GETCAP04                                                         
         CLI   CIDTYPE,CIDTYAQ                                                  
         JNE   GETCAP04                                                         
         CLC   CIDSEQ,C.CIDSEQ                                                  
         JE    GETCAP06                                                         
                                                                                
GETCAP04 LLC   RF,CIDLN                                                         
         AR    R2,RF                                                            
         J     GETCAP02                                                         
                                                                                
GETCAP06 LA    R7,C.CIDCCPJ                                                     
         DROP  C                                                                
P        USING ACTKACT,R7                                                       
         CLI   CH_COBCA,YESQ       Are we by passing client approvals           
         JE    GETCAP10            Yes                                          
         CLI   CI_TYPE,C'N'        For non billable check whether               
         JNE   GETCAP08                    etype requires approval              
         TM    CI_ETST2,ETYSAPND                                                
         JZ    GETCAP10            No approval required                         
         TM    CPXSTAT6,CPX2LAEI   check TNB unless WPP workflow.               
         JNZ   GETCAP08                                                         
         CP    CH_TOTAL,CH_COTNB   Is item above threshold for approver         
         JL    GETCAP10            No - no approval required                    
GETCAP08 CLC   P.ACTKACT,SPACES    Do we have a production account              
         JH    GETCAP12            Yes                                          
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JNZ   GETCAP12            Yes                                          
GETCAP10 MVI   CIDEL,X'FF'         No - mark approval elements as not           
         J     EXITY                                 needed                     
                                                                                
GETCAP12 LLC   R0,CIDANUM          Number of approvers                          
         LTR   R0,R0                                                            
         JNZ   GETCAP14                                                         
         CLI   CIDLN,CIDALN1Q      Any room on element for approvers            
         JE    GETCAP14            No                                           
         CLI   CIDLN,CIDALN1Q+CIDALNQ                                           
         JNE   GETCAP14                                                         
         LHI   R0,1                                                             
GETCAP14 LA    R2,CIDANTRY                                                      
AP       USING CIDANTRY,R2                                                      
         LA    R6,CI_ITAPP                                                      
         LA    R5,CH_LMAP                                                       
         CLI   CI_TYPE,C'N'        Is item billable or non-billable             
         JE    *+8                                                              
         LA    R5,2*CH_LMAPL(R5)                                                
LM       USING CH_LMAPD,R5                                                      
         LA    R3,JOBTYPE                                                       
         USING JOBTYPD,R3                                                       
GETCAP18 LA    R4,APRTAB                                                        
         USING APRTABD,R4                                                       
SJ       USING JOBPASD,IOKEY       Find job, product or client approver         
GETCAP20 XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVC   SJ.JOBPAPPL,JOBBILL                                              
         CLI   CI_TYPE,C'N'        Is item billable or non-billable             
         JNE   *+10                Billable                                     
         MVC   SJ.JOBPAPPL,JOBNONB Non-Billable                                 
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JNZ   *+8                 Yes                                          
         MVI   SJ.JOBPAPPL,JOBPAEXP                                             
         MVC   SJ.JOBPCPJ,SPACES                                                
         MVC   SJ.JOBPCOFF,SPACES                                               
         MVC   SJ.JOBPCMED,SPACES                                               
         TM    APRSTAT,APRJOB      Are we looking at job level                  
         JZ    GETCAP22                                                         
         LLC   RF,PPROLEN                                                       
         LA    RF,P.ACTKACT(RF)                                                 
         CLI   0(RF),X'40'         Have we got a job                            
         JNH   GETCAP32            No                                           
         LLC   RF,PJOBLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
         J     GETCAP26                                                         
GETCAP22 TM    APRSTAT,APRPRO                                                   
         JZ    GETCAP24                                                         
         LLC   RF,PCLILEN                                                       
         LA    RF,P.ACTKACT(RF)                                                 
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   GETCAP32            No                                           
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
         J     GETCAP26                                                         
GETCAP24 TM    APRSTAT,APRCLI                                                   
         JZ    GETCAP26                                                         
         CLI   P.ACTKACT,X'40'                                                  
         JNH   GETCAP32                                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),P.ACTKACT                                          
         EX    RF,0(RE)                                                         
GETCAP26 TM    APRSTAT,APRMED                                                   
         JZ    GETCAP28                                                         
         CLC   CI_MED,SPACES                                                    
         JNH   GETCAP32                                                         
         MVC   SJ.JOBPCMED,CI_MED                                               
                                                                                
GETCAP28 TM    APRSTAT,APROFF                                                   
         JZ    GETCAP30                                                         
         OC    CI_OFF,CI_OFF                                                    
         JZ    GETCAP32                                                         
         MVC   SJ.JOBPCOFF,CI_OFF                                               
                                                                                
GETCAP30 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),IOKEYSAV                             
         JE    GETCAP38                                                         
GETCAP32 LA    R4,APRTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         JNE   GETCAP20                                                         
                                                                                
GETCAP34 TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    GETCAP56                                                         
         MVC   0(L'CI_ITPID*(CH_MXBUQ+1),R6),LM.CH_LMPID                        
         LTR   R0,R0                                                            
         JZ    *+10                                                             
         MVC   AP.CIDAPID,LM.CH_LMPID                                           
         J     GETCAP56                                                         
                                                                                
GETCAP38 TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    GETCAP40                                                         
         CLC   CH_PPID#,SJ.JOBPPIDB                                             
         JE    GETCAP34                                                         
                                                                                
GETCAP40 MVC   0(L'CI_ITPID,R6),SJ.JOBPPIDB                                     
         LTR   R0,R0                                                            
         JZ    *+10                                                             
         MVC   AP.CIDAPID,SJ.JOBPPIDB                                           
         DROP  AP,P,SJ,LM,R2,R3,R4                                              
         LA    R7,L'CI_ITPID(R6)                                                
                                                                                
PP       USING APPRECD,IOKEY                                                    
         MVI   CH_SVBU,CH_MXBUQ    Maximum number of back up approvers          
         XC    PP.APPKEY,PP.APPKEY                                              
         MVI   PP.APPKTYP,APPKTYPQ                                              
         MVI   PP.APPKSUB,APPKSUBQ                                              
         MVC   PP.APPKCPY,CUXCPY                                                
         MVC   PP.APPKPIDB,0(R6)                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     GETCAP44                                                         
                                                                                
GETCAP42 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
GETCAP44 CLC   PP.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                              
         JNE   GETCAP56                                                         
         DROP  PP                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    RF,RF                                                            
         USING LIDELD,R1                                                        
GETCAP46 CLI   LIDEL,0             Test end of record                           
         JE    GETCAP42                                                         
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETCAP50                                                         
GETCAP48 IC    RF,LIDLN                                                         
         AR    R1,RF                                                            
         J     GETCAP46                                                         
                                                                                
GETCAP50 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETCAP48                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   RE,LIDLN                                                         
         AR    RE,R1               RE=End of element                            
GETCAP52 TM    LIDLAPPL,LIDLEXPN   Is this entry for expenses                   
         JZ    GETCAP54            No                                           
         MVC   0(L'CI_ITBUP,R7),LIDLPID                                         
         LLC   RF,CH_SVBU          Saved number of back up approvers            
         SHI   RF,1                decrement the number                         
         STC   RF,CH_SVBU          save the number                              
         CHI   RF,0                Have we saved the maximum number of          
         JE    GETCAP56                          back up approvers              
                                                                                
GETCAP54 LLC   RF,LIDITLN          Increment R4 to look at next entry           
         AR    R4,RF                                                            
         LA    R7,L'CI_ITBUP(R7)                                                
         CR    RE,R4               Check we haven't reached end of el           
         JH    GETCAP52            No - check next entry                        
         J     GETCAP48            Yes - get next element                       
                                                                                
GETCAP56 TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    EXITY               No                                           
         LA    R2,CIDALNQ(R2)                                                   
         LA    R6,CI_ITAPL(R6)                                                  
         LA    R5,CH_LMAPL(R5)                                                  
         LTR   R0,R0                                                            
         JZ    *+8                                                              
         SHI   R0,1                                                             
         LA    R3,JOBTYPLQ(R3)     WPP workflow has 2 levels                    
         L     R7,SAVERF                                                        
         CLI   0(R3),X'FF'                                                      
         JNE   GETCAP18                                                         
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
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
* Get approver and back-up approver PID for expense claim             *         
***********************************************************************         
         USING LP_D,R5                                                          
GETMAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETMAP*'                                                      
         LA    R2,CH_ACT                                                        
P        USING ACTKACT,R2                                                       
                                                                                
         LA    R6,CH_LMAP                                                       
AP       USING CH_LMAPD,R6                                                      
         L     R1,LP_AINP                                                       
AMT      USING CLDLNTRY,R1                                                      
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JNZ   GETMAP60            Yes                                          
GETMAP02 OC    AP.CH_LMVAL,AP.CH_LMVAL                                          
         JZ    GETMAP04                                                         
         LA    R6,CH_LMAPL(R6)                                                  
         J     GETMAP02                                                         
                                                                                
GETMAP04 L     R3,AIO6             Test to fail if expense claim is             
         USING EXCRECD,R3                           posted                      
         TM    EXCRSTA2,EXCKCLAQ   Approvers chosen by user                     
         JZ    GETMAP30            No - automatic                               
         DROP  R3                                                               
GETMAP06 ZAP   AP.CH_LMVAL,AMT.CLDLAPVL                                         
         MVC   AP.CH_LMPID,AMT.CLDLAPP                                          
         MVC   AP.CH_LMSTA,AMT.CLDLSTAT                                         
         MVC   AP.CH_LMLVL,AMT.CLDLAPLV                                         
         J     EXITY                                                            
                                                                                
MN       USING DPAPASD,IOKEY                                                    
GETMAP30 XC    MN.DPAPAS,MN.DPAPAS                                              
         MVI   MN.DPAPTYP,DPAPTYPQ                                              
         MVI   MN.DPAPSUB,DPAPSUBQ                                              
         MVI   MN.DPAPAPPL,DPAPAEXP                                             
         MVC   MN.DPAPCPY,CUXCPY                                                
         ZAP   MN.DPAPXVAL,AMT.CLDLAPVL                                         
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETMAP32 MVC   MN.DPAP1RAC,SPACES                                               
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   MN.DPAP1RAC(0),P.ACTKACT                                         
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GETMAP34                                                         
         DC    H'0'                                                             
                                                                                
GETMAP34 CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JE    GETMAP37                                                         
                                                                                
GETMAP36 MVC   MN.DPAPAS,IOKEYSAV  Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETMAP32         Do for number of 1R levels                   
         MVC   MN.DPAP1RAC,SPACES                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JNE   GETMAPN                                                          
GETMAP37 ZAP   AP.CH_LMVAL,MN.DPAPXVAL                                          
         MVC   AP.CH_LMPID,MN.DPAPPIDB                                          
         L     R1,LP_AINP                                                       
         MVC   AMT.CLDLAPP,MN.DPAPPIDB                                          
         MVC   AP.CH_LMSTA,AMT.CLDLSTAT                                         
         DROP  MN                                                               
                                                                                
PP       USING APPRECD,IOKEY                                                    
GETMAP38 XC    PP.APPKEY,PP.APPKEY                                              
         MVI   PP.APPKTYP,APPKTYPQ                                              
         MVI   PP.APPKSUB,APPKSUBQ                                              
         MVC   PP.APPKCPY,CUXCPY                                                
         MVC   PP.APPKPIDB,AP.CH_LMPID                                          
         MVI   CH_SVBU,CH_MXBUQ                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     GETMAP42                                                         
                                                                                
GETMAP40 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
GETMAP42 CLC   PP.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                              
         JNE   GETMAP58                                                         
         DROP  PP                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,IOADDR                                                        
         MVC   IOKEY,0(R1)                                                      
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETMAP48 CLI   LIDEL,0             Test end of record                           
         JE    GETMAP40                                                         
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETMAP52                                                         
GETMAP50 IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GETMAP48                                                         
                                                                                
GETMAP52 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETMAP50                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
         LA    RF,CH_MXBUQ                                                      
GETMAP54 TM    LIDLAPPL,LIDLEXPN   Is this entry for expenses                   
         JZ    GETMAP56            No                                           
         MVC   AP.CH_LMBUP,LIDLPID Back up approver                             
         LLC   RF,CH_SVBU          Saved number of back up approvers            
         SHI   RF,1                decrement the number                         
         STC   RF,CH_SVBU          save the number                              
         CHI   RF,0                Have we saved the maximum number of          
         JE    GETMAP58                          back up approvers              
GETMAP56 LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         LA    R6,L'CH_LMBUP(R6)                                                
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETMAP54            No - check next entry                        
         J     GETMAP50            Yes - get next element                       
                                                                                
GETMAP58 J     EXITY                                                            
         DROP  AMT,R4                                                           
                                                                                
GETMAP60 LA    R6,CH_LMAP                                                       
         LA    R5,EXPTYPE                                                       
MN       USING DPAPASD,IOKEY                                                    
GETMAP62 XC    MN.DPAPAS,MN.DPAPAS                                              
         MVI   MN.DPAPTYP,DPAPTYPQ                                              
         MVI   MN.DPAPSUB,DPAPSUBQ                                              
         MVC   MN.DPAPAPPL,0(R5)                                                
         MVC   MN.DPAPCPY,CUXCPY                                                
         ZAP   MN.DPAPXVAL,PZERO                                                
         ZAP   AP.CH_LMVAL,PZERO                                                
         MVI   CH_SVBU,CH_MXBUQ    Save maximum number of back up aprvs         
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETMAP64 MVC   MN.DPAP1RAC,SPACES                                               
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   MN.DPAP1RAC(0),P.ACTKACT                                         
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GETMAP66                                                         
         DC    H'0'                                                             
                                                                                
GETMAP66 CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JNE   GETMAP68                                                         
         MVC   AP.CH_LMPID,MN.DPAPPIDB                                          
         J     GETMAP70                                                         
                                                                                
GETMAP68 MVC   MN.DPAPAS,IOKEYSAV  Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETMAP64         Do for number of 1R levels                   
         MVC   MN.DPAP1RAC,SPACES                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JNE   GETMAPN                                                          
         MVC   AP.CH_LMPID,MN.DPAPPIDB                                          
         DROP  MN,P                                                             
                                                                                
GETMAP70 LA    R7,AP.CH_LMBUP      R7=A(Back up approver)                       
PP       USING APPRECD,IOKEY                                                    
         XC    PP.APPKEY,PP.APPKEY  Read approver for possible backup           
         MVI   PP.APPKTYP,APPKTYPQ                                              
         MVI   PP.APPKSUB,APPKSUBQ                                              
         MVC   PP.APPKCPY,CUXCPY                                                
         MVC   PP.APPKPIDB,AP.CH_LMPID                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     GETMAP74                                                         
                                                                                
GETMAP72 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
GETMAP74 CLC   PP.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                              
         JNE   GETMAP90                                                         
         DROP  PP                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,IOADDR                                                        
         MVC   IOKEY,0(R1)                                                      
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETMAP80 CLI   LIDEL,0             Test end of record                           
         JE    GETMAP72                                                         
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETMAP84                                                         
GETMAP82 IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GETMAP80                                                         
                                                                                
GETMAP84 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETMAP82                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
                                                                                
GETMAP86 TM    LIDLAPPL,LIDLEXPN   Is this entry for expenses                   
         JZ    GETMAP88            No                                           
         MVC   0(L'CH_LMBUP,R7),LIDLPID  Back up approver                       
         LLC   RF,CH_SVBU          Saved number of back up approvers            
         SHI   RF,1                decrement the number                         
         STC   RF,CH_SVBU          save the number                              
         CHI   RF,0                Have we saved the maximum number of          
         JE    GETMAP90                          back up approvers              
                                                                                
GETMAP88 LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         LA    R7,L'CH_LMBUP(R7)                                                
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETMAP86            No - check next entry                        
         J     GETMAP82            Yes - get next element                       
                                                                                
GETMAP90 LA    R6,CH_LMAPL(R6)                                                  
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         JNE   GETMAP62                                                         
         J     EXITY                                                            
         DROP  R1,R4,AP                                                         
                                                                                
GETMAPN  MVC   ROUERRV,=AL2(AE$INAPP)                                           
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Get fiance approver and back-up approver PID for expense claim      *         
***********************************************************************         
                                                                                
GETFAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETFAP*'                                                      
                                                                                
         LA    R2,CH_ACT                                                        
P        USING ACTKACT,R2                                                       
         MVI   FNIND,0                                                          
         MVI   CH_FNAPN,1          Set always one finance approver              
         XC    CH_FNAP(CH_FNAPL*CH_FNMAX),CH_FNAP                               
MN       USING DPAPASD,IOKEY                                                    
GETFAP02 XC    MN.DPAPAS,MN.DPAPAS                                              
         MVI   MN.DPAPTYP,DPAPTYPQ                                              
         MVI   MN.DPAPSUB,DPAPSUBQ                                              
         MVI   MN.DPAPAPPL,DPAPAEXF                                             
         MVC   MN.DPAPCPY,CUXCPY                                                
         ZAP   MN.DPAPXVAL,PZERO                                                
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETFAP04 MVC   MN.DPAP1RAC,SPACES                                               
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   MN.DPAP1RAC(0),P.ACTKACT                                         
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GETFAP08                                                         
         DC    H'0'                                                             
GETFAP06 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    GETFAP08                                                         
         DC    H'0'                                                             
                                                                                
GETFAP08 CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JNE   GETFAP16                                                         
                                                                                
GETFAP10 OI    FNIND,FNIFND        Set finance approver found                   
         MVC   CSVKEY1,MN.DPAPAS                                                
         TM    MN.DPAPSTAT,DPAPDFLT                                             
         JZ    GETFAP12                                                         
         MVC   CH_FNPID,MN.DPAPPIDB                                             
                                                                                
GETFAP12 CLC   MN.DPAPPIDB,CCTPID  Is connected user finance approver           
         JNE   GETFAP06            No                                           
         MVI   CH_USRFN,YESQ       Yes                                          
         J     GETFAP06                                                         
                                                                                
GETFAP16 TM    FNIND,FNIFND+FNIHIGH Have we found approver                      
         JNZ   GETFAP40            Yes - don't go down 1R levels                
         MVC   MN.DPAPAS,IOKEYSAV  Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETFAP04         Do for number of 1R levels                   
         OI    FNIND,FNIHIGH       set read highest level                       
GETFAP18 MVI   MN.DPAP1RAC,X'FF'                                                
         MVC   MN.DPAP1RAC+1(L'DPAP1RAC-1),MN.DPAP1RAC                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                             
         JE    GETFAP10                                                         
         DROP  P                                                                
                                                                                
GETFAP40 TM    FNIND,FNIFND        Have we found approver                       
         JZ    GETFAPN             No                                           
         OC    CH_FNPID,CH_FNPID   Did we find a default approver               
         JNZ   EXITY               Yes                                          
         MVC   ROUERRV,=AL2(AE$NDFFN) No - set error message                    
         J     EXITN                                                            
                                                                                
GETFAPN  MVC   ROUERRV,=AL2(AE$INAPP)                                           
         J     EXITN                                                            
         DROP  MN                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Check connected user is an approver                                 *         
***********************************************************************         
                                                                                
CHKAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKAPP*'                                                      
                                                                                
         XC    BYTE1,BYTE1                                                      
         XC    BYTE2,BYTE2                                                      
         XC    CI_CLAP,CI_CLAP                                                  
         XC    CI_CLBU,CI_CLBU                                                  
         TM    CPXSTAT6,CPX2LAEI   WPP workflow                                 
         JZ    CHKAPP16            No                                           
         J     CHKAPP32            Yes                                          
LM       USING CH_LMAPD,R3                                                      
IT       USING CI_ITAPP,R2                                                      
         LA    R2,CI_ITAPP                                                      
         NI    CH_IND,X'FF'-CH_IISAP                                            
                                                                                
         LA    R3,CH_LMAP                                                       
         CLI   CI_TYPE,C'N'        non billable                                 
         JE    CHKAPP02                                                         
         LA    R3,2*CH_LMAPL(R3)                                                
                                                                                
CHKAPP02 LA    R0,2                Number of levels                             
         LA    RF,CH_MXBUQ         Number of back up approvers                  
CHKAPP04 OC    IT.CI_ITPID,IT.CI_ITPID Do we have a client approver             
         JZ    CHKAPP14            No - try 1R level                            
         LA    R1,IT.CI_ITPID                                                   
         CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP06            No                                           
         LA    R1,IT.CI_ITBUP                                                   
                                                                                
CHKAPP06 CLC   CCTPID,0(R1)        Does pid match connected user                
         JNE   CHKAPP08                                                         
         OI    CH_IND,CH_IISAP                                                  
         CHI   R0,2                                                             
         JE    CHKAPP10                                                         
         OI    CH_IND,CH_ILVL1     Set level 1 approver                         
         J     CHKAPP12                                                         
                                                                                
CHKAPP08 CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP12            No                                           
         LA    R1,L'CI_ITBUP(R1)   Yes - bump to next entry                     
         JCT   RF,CHKAPP06                                                      
         J     CHKAPP12                                                         
                                                                                
CHKAPP10 OI    CH_IND,CH_ILVL2     Set level 2 approver                         
CHKAPP12 LA    R2,CI_ITAPL(R2)     No - bump to next level                      
         LA    R3,CH_LMAPL(R3)                                                  
         LA    RF,CH_MXBUQ         Number of back up approvers                  
         JCT   R0,CHKAPP04                                                      
         TM    CH_IND,CH_IISAP     Is connected user approver                   
         JNZ   EXITY               Yes                                          
         J     EXITN                                                            
                                                                                
CHKAPP14 OC    LM.CH_LMPID,LM.CH_LMPID Do we have an approver                   
         JZ    CHKAPP12            No - go to next level                        
         LA    R1,LM.CH_LMPID      R1=A(1R approver)                            
         CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP06            No                                           
         LA    R1,LM.CH_LMBUP      R1=A(1R back up approver)                    
         J     CHKAPP06                                                         
                                                                                
CHKAPP16 LA    R3,CH_LMAP                                                       
CHKAPP18 LA    RF,CH_MXBUQ         Number of back up approvers                  
         SR    R2,R2                                                            
         ICM   R2,3,CUXPNUM                                                     
         CHI   R2,XPRODIKQ         Aura                                         
         JNE   CHKAPP20            No - Brandocean                              
         LA    RF,1+CH_MXBUQ       approvers and back up                        
CHKAPP20 OC    LM.CH_LMPID,LM.CH_LMPID Do we have an approver                   
         JZ    CHKAPP32            No - go to next level                        
         LA    R1,LM.CH_LMPID      R1=A(1R approver)                            
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,CUXPNUM                                                     
         CHI   R2,XPRODIKQ         Aura                                         
         JE    CHKAPP22            Yes - back up setting N/A                    
                                                                                
         CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP22            No                                           
         LA    R1,LM.CH_LMBUP      R1=A(1R back up approver)                    
CHKAPP22 CLC   CCTPID,0(R1)        Does pid match connected user                
         JE    CHKAPP30            Yes - only one match needed                  
         SR    R2,R2                                                            
         ICM   R2,3,CUXPNUM                                                     
         CHI   R2,XPRODIKQ         Aura                                         
         JNE   CHKAPP24            No - Brandocean                              
         CLI   QD_CDSRC,YESQ       Do we have ability to search                 
         JE    CHKAPP26            Yes check both approver and back up          
         J     CHKAPP28            Otherwise only check approver                
CHKAPP24 CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP28            No                                           
CHKAPP26 LA    R1,L'CI_ITBUP(R1)   Yes - bump to next entry                     
         JCT   RF,CHKAPP22                                                      
CHKAPP28 LA    R3,CH_LMAPL(R3)                                                  
         J     CHKAPP18                                                         
                                                                                
CHKAPP30 MVI   BYTE1,YESQ          Set connected user is an approver            
                                                                                
CHKAPP32 LA    R2,CI_ITAPP                                                      
CHKAPP34 LA    RF,CH_MXBUQ         Number of back up approvers                  
         SR    R3,R3                                                            
         ICM   R3,3,CUXPNUM                                                     
         CHI   R3,XPRODIKQ         Aura                                         
         JNE   CHKAPP36            No - Brandocean                              
         LA    RF,CH_MXBUQ+1       approvers and back up                        
CHKAPP36 OC    IT.CI_ITPID,IT.CI_ITPID Do we have a client approver             
         JZ    CHKAPP48            No - can't be an approver                    
         LA    R1,IT.CI_ITPID      R1=A(Client approver)                        
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,CUXPNUM                                                     
         CHI   R3,XPRODIKQ         Aura                                         
         JE    CHKAPP38            Yes - back up setting N/A                    
                                                                                
         CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP38            No                                           
         LA    R1,IT.CI_ITBUP      R1=A(Client back up approver)                
CHKAPP38 CLC   CCTPID,0(R1)        Does pid match connected user                
         JE    CHKAPP46            Yes - only one match needed                  
         SR    R3,R3                                                            
         ICM   R3,3,CUXPNUM                                                     
         CHI   R3,XPRODIKQ         Aura                                         
         JNE   CHKAPP40            No - Brandocean                              
         CLI   QD_CDSRC,YESQ       Do we have ability to search                 
         JE    CHKAPP42            Yes check both approver and back up          
         J     CHKAPP44            Otherwise only check approver                
CHKAPP40 CLI   QD_CDVIW,QD_CDVBQ   Back up approver view?                       
         JNE   CHKAPP44            No                                           
CHKAPP42 LA    R1,L'CI_ITBUP(R1)   Yes - bump to next entry                     
         MVI   BYTE2,1                                                          
         JCT   RF,CHKAPP38                                                      
CHKAPP44 LA    R2,CI_ITAPL(R2)     No - bump to next level                      
         XC    BYTE2,BYTE2                                                      
         J     CHKAPP34                                                         
                                                                                
CHKAPP46 MVI   CI_CLAP,YESQ        Set connected user is client app             
         OC    BYTE2,BYTE2                                                      
         JZ    EXITY                                                            
         MVI   CI_CLBU,YESQ        Set user is client back up approver          
         J     EXITY                                                            
CHKAPP48 CLI   BYTE1,YESQ          Were they the approver                       
         JE    EXITY                                                            
         J     EXITN                                                            
         DROP  IT,LM                                                            
         EJECT                                                                  
***********************************************************************         
* Check connected user is an can finance approve                      *         
***********************************************************************         
                                                                                
CHKFIN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKFIN*'                                                      
         L     R1,LP_AINP                                                       
         USING CIDASTAT,R1                                                      
CHKFIN02 TM    CIDASTAT,CIDAANR    Does this need approval                      
         JNZ   CHKFINX             No                                           
         TM    CIDASTAT,CIDAAPP    Has it already been approved                 
         JNZ   CHKFINX             Yes                                          
         CLC   CCTPID,CIDAPID      Is the connected user the approver           
         JE    CHKFINX             Yes                                          
         OI    CH_IND,CH_IMSAP     Set missing approval                         
CHKFINX  J     EXITY                                                            
         DROP  R1                                                               
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
         LA    R1,EX_BUFF                                                       
         ST    R1,TB.TSAREC        Address of record buffer area                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TB.TSARD(TSPNEWL),TB.TSARD                                       
         MVC   TB.TSACTN,0(R2)                                                  
         MVC   TB.TSACOM,ACOMFACS                                               
         LHI   R0,1024                                                          
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL      Set require 1MB off-line                    
         MVI   TB.TSRECI,TSRXTN+TSRMINB1+TSRVAR                                 
         MVI   TB.TSKEYL,CLM_KEYL   Set key length                              
         MVI   TB.TSINDS,TSINODSK   Set no disk writes (save/restore)           
         MVI   TB.TSIND2,TSI2MANY                                               
         LHI   R0,500                                                           
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
* Set status of expense records                                       *         
***********************************************************************         
                                                                                
SETFSTA  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*SETFSTA'                                                      
                                                                                
         XC    CH_FSTAT,CH_FSTAT                                                
         MVI   CH_FSTAT,X'FF'                                                   
         CLI   0(R1),C' '                                                       
         JNH   EXITY                                                            
                                                                                
         LA    RE,STATAB                                                        
         USING STATABD,RE                                                       
                                                                                
SETFS2   CLI   STATREQ,0                                                        
         JE    EXITN                                                            
                                                                                
         CLC   0(1,R1),STATREQ                                                  
         JE    SETFS4                                                           
                                                                                
         LA    RE,STATABL(RE)                                                   
         J     SETFS2                                                           
                                                                                
SETFS4   MVC   CH_FSTAT(L'EXCKSTAT),STATRST                                     
         J     EXITY                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set status of approver                                              *         
***********************************************************************         
                                                                                
CB       USING CLM_D,EX_BUFF                                                    
APRSTA   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*APRSTA*'                                                      
                                                                                
         LA    RF,DSPTAB                                                        
         USING DSPTABD,RF                                                       
KEY      USING EXCRECD,SAVEKEY4                                                 
APRST02  CLI   DSPKTYP,0                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DSPKTYP,KEY.EXCKTYP                                              
         JNE   APRST04                                                          
         CLC   DSPKSUB,KEY.EXCKSUB                                              
         JE    APRST06                                                          
APRST04  LA    RF,DSPTABL(RF)                                                   
         J     APRST02                                                          
                                                                                
APRST06  LLC   R1,DSPSTAT                                                       
         LA    R1,SAVEKEY4(R1)                                                  
         DROP  KEY                                                              
                                                                                
         LA    RF,STATAB                                                        
         USING STATABD,RF                                                       
                                                                                
APRST08  CLI   STATREQ,0                                                        
         JE    EXITN                                                            
                                                                                
         CLC   0(1,R1),STATRST                                                  
         JE    APRST10                                                          
                                                                                
         LA    RF,STATABL(RF)                                                   
         J     APRST08                                                          
                                                                                
APRST10  MVC   CB.CLM_APST,STATAOP                                              
         CLI   QL_CLOWN,QL_CLOFQ   finance approval view?                       
         JNE   EXITY                                                            
         MVC   CB.CLM_APST,STATFOP                                              
         J     EXITY                                                            
         DROP  CB,RF                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
* Get person profile                                                  *         
***********************************************************************         
                                                                                
SETPERD  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*SETPERD'                                                      
                                                                                
         ZAP   CH_EBV,=P'3'        Set default for back view                    
                                                                                
         MVC   TEMP2(2),CCTPID                                                  
         GOTOR (#ACCPID,AACCPID)                                                
         JNE   EXITN                                                            
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            Read for person record                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TEMP2+10                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO1                                                          
         LA    R3,PERRFST          Locate elements                              
         USING LOCELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SETPD10  CLI   LOCEL,0                                                          
         JE    EXITN                                                            
         CLI   LOCEL,LOCELQ                                                     
         JE    SETPD20                                                          
                                                                                
SETPD15  IC    R0,LOCLN                                                         
         AR    R3,R0                                                            
         J     SETPD10                                                          
                                                                                
SETPD20  OC    LOCEND,LOCEND                                                    
         JZ    SETPD25                                                          
         CLC   CH_TODP,LOCEND                                                   
         JH    SETPD15                                                          
                                                                                
SETPD25  MVC   CH_OFF,LOCOFF                                                    
         MVC   CH_DEPT,LOCDEPT                                                  
         MVC   CH_SUBD,LOCSUB                                                   
         MVC   CH_PER,PERKCODE                                                  
                                                                                
         MVC   ORACCNT,SPACES                                                   
         LA    RE,1                                                             
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   *+8                                                              
         LA    RE,0                                                             
         BASR  RF,0                                                             
         MVC   ORACCNT(0),CH_OFF                                                
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         LA    R3,ORACCNT                                                       
         AR    R3,RE                                                            
         LLC   R1,ONERL2L                                                       
         SR    R1,RE               R1=length of dept                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),CH_DEPT                                                  
         EX    R1,0(RF)                                                         
         AHI   R1,1                                                             
         AR    R3,R1               R3=A(sub dept on account code)               
         LLC   R1,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    R1,RE               R1=length of sub department                  
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),CH_SUBD                                                  
         EX    R1,0(RF)                                                         
         AHI   R1,1                                                             
         AR    R3,R1               R3=A(person on account code)                 
         LLC   R1,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    R1,RE               R1=length of person code                     
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),CH_PER                                                   
         EX    R1,0(RF)                                                         
         GOTOR (#CSTPRF,ACSTPRF),DMCB,ORACCNT   get approvers profiles          
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         ZAP   CH_EBV,COEBV                                                     
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Get my own expense claims                                           *         
***********************************************************************         
                                                                                
CLLONE   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLLONE*'         === OWNER VIEW / STATUS & DATE ===           
         USING EXCRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         XC    CH_ACT,CH_ACT                                                    
         LA    R2,IOKEY            build start key                              
CLLONE05 MVI   EXCKTYP,EXCKTYPQ                                                 
         MVI   EXCKSUB,EXCKSUBQ                                                 
         MVC   EXCKCPY,CUXCPY                                                   
         MVC   EXCKPIDB,CCTPID                                                  
         OC    CH_PPID#,CH_PPID#                                                
         JZ    *+10                                                             
         MVC   EXCKPIDB,CH_PPID#                                                
         MVC   EXCKDATE,CH_ENDDC                                                
         MVC   EXCKTYPE,QL_CLTYP                                                
         MVC   SAVEKEY1,EXCKEY                                                  
                                                                                
CLLONE10 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLLONE22                                                         
         J     CLLONEX                                                          
                                                                                
CLLONE20 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   CLLONEX                                                          
                                                                                
CLLONE22 CLC   EXCKEY(EXCKDATE-EXCRECD),SAVEKEY1                                
         JNE   CLLONEX                                                          
         MVC   SAVEKEY4,EXCKEY                                                  
         CLC   EXCKDATE,CH_STADC   ensure within date range                     
         JH    CLLONEX                                                          
                                                                                
         TM    EXCKSTA,EXCSDELT                                                 
         JNZ   CLLONE20                                                         
                                                                                
         CLI   QS_CSCNP,C' '       CLAIM NUMBER FILTER?                         
         JNH   CLLONE24                                                         
         CLC   EXCKREF,QS_CSCNP                                                 
         JNE   CLLONE20                                                         
                                                                                
CLLONE24 CLI   QL_CLTYP,C' '       type filter?                                 
         JNH   CLLONE35                                                         
         CLC   EXCKTYPE,QL_CLTYP                                                
         JE    CLLONE35                                                         
         CLI   EXCKTYPE,EXCPTLQ    Live or draft?                               
         JE    CLLONE30                                                         
         MVI   EXCKTYPE,EXCPTLQ    Read high for live                           
         J     CLLONE10                                                         
                                                                                
CLLONE30 MVI   EXCKTYPE,FF         Read high for next date                      
         J     CLLONE10                                                         
                                                                                
CLLONE35 CLI   EXCKSEQ,0           Only main record                             
         JE    CLLONE40                                                         
CLLONE38 MVI   EXCKSEQ,FF          Set to read next claim                       
         J     CLLONE10                                                         
                                                                                
CLLONE40 CLI   CH_FSTAT,FF         Apply status filter                          
         JE    CLLONE50            anything goes                                
                                                                                
         CLI   QL_CLTYP,EXCPTDQ                                                 
         JE    CLLONE50                                                         
                                                                                
         LA    R1,CH_FSTAT                                                      
         ICM   RF,1,0(R1)                                                       
         JNZ   CLLONE42                                                         
         OC    EXCKSTAT,EXCKSTAT                                                
         JZ    CLLONE50                                                         
         J     CLLONE44                                                         
CLLONE42 ICM   RF,1,0(R1)                                                       
         JZ    CLLONE20                                                         
         BASR  RE,0                                                             
         TM    EXCKSTAT,0                                                       
         EX    RF,0(RE)                                                         
         JNZ   CLLONE50                                                         
CLLONE44 LA    R1,1(R1)                                                         
         J     CLLONE42                                                         
                                                                                
CLLONE50 CLI   LP_ACCS,0           Skip if no limit access                      
         JE    CLLONE60                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   OFFAOFFC(0),EXCK1RAC  Move in office and validate                
         EX    RE,0(RF)                                                         
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   CLLONE38                                                         
                                                                                
CLLONE60 OC    CH_PPID#,CH_PPID#   Staff search                                 
         JZ    CLLONE70            no                                           
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights           
         JE    CLLONE70                                                         
         CLC   CH_ACT,EXCK1RAC     match on previous                            
         JE    CLLONE70                                                         
         XC    GAPAREA2,GAPAREA2                                                
         LA    R1,EXCK1RAC                                                      
         GOTOR TT1RVGT             test against table                           
         JNE   CLLONE38                                                         
         MVC   CH_ACT,EXCK1RAC                                                  
*                                                                               
CLLONE70 CLI   QS_CSVIE,QS_CSVRQ   Finance report?                              
         JE    CLLONE72                                                         
         GOTOR PROCLL                                                           
         J     CLLONE74                                                         
*                                                                               
CLLONE72 GOTOR PROCLI              Process claim items                          
                                                                                
CLLONE74 LA    R2,IOKEY                                                         
         MVC   EXCKEY,SAVEKEY4     reestablish key                              
         MVI   EXCKSEQ,X'FF'                                                    
         J     CLLONE10                                                         
                                                                                
CLLONEX  J     EXITY                                                            
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  Process claim record and add to buffer                             *         
***********************************************************************         
         USING EXCRECD,R4                                                       
PROCLL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*PROCLL*'                                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         MVC   SAVEKEY2,EXCKEY                                                  
         CLI   QL_CLOWN,QL_CLOMQ   List - view own expenses?                    
         JE    PROCLL04            Yes                                          
         CLI   QS_CSVIE,QS_CSVBQ   Search - Back up approver                    
         JE    PROCLL02            Yes                                          
         CLI   QL_CLOWN,QL_CLOFQ   List - finance approval view?                
         JE    PROCLL02            No                                           
         CLI   QS_CSVIE,QS_CSVSQ   Search - STAFF                               
         JNE   PROCLL04            No                                           
         CLI   GAPAREA,GAPTT9Q     Finance approver                             
         JNE   PROCLL04            No                                           
PROCLL02 CLC   CCTPID,EXCKPIDB     Don't let them self approve                  
         JE    EXITY                                                            
                                                                                
CB       USING CLM_D,EX_BUFF                                                    
PROCLL04 LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    CH_IND,CH_IND                                                    
                                                                                
         MVI   CB.CLM_APST,NOTAPPLQ                                             
         CLI   QL_CLOWN,QL_CLOAQ   Only want approver views to go               
         JE    PROCLL06             through APRSTA routine                      
         CLI   QL_CLOWN,QL_CLOFQ                                                
         JE    PROCLL06                                                         
         CLI   QS_CSVIE,QS_CSVBQ   Search - backup approver                     
         JNE   PROCLL10            no - status check not needed?                
PROCLL06 GOTOR APRSTA                                                           
                                                                                
PROCLL10 MVC   CB.CLM_TYPE,EXCKTYPE                                             
         MVC   CB.CLM_NUM,EXCKREF                                               
         MVC   CB.CLM_PIDB,EXCKPIDB                                             
         MVC   CB.CLM_1RAC,EXCK1RAC                                             
         MVC   CB.CLM_STAT,EXCRSTAT                                             
         MVC   CB.CLM_STA2,EXCRSTA2                                             
         XR    R1,R1                                                            
         ICM   R1,3,EXCKDATE                                                    
         LNR   R1,R1                                                            
         STCM  R1,3,CB.CLM_DATE                                                 
         ZAP   CB.CLM_APRS,EXCRNAPP                                             
         ZAP   CB.CLM_ITMS,EXCRNITM                                             
         MVC   CB.CLM_BNAM,SPACES                                               
         ZAP   CB.CLM_ADVS,PZERO                                                
         ZAP   CB.CLM_BITM,PZERO                                                
         ZAP   CB.CLM_NBTM,PZERO                                                
         ZAP   CB.CLM_PTOT,PZERO                                                
         XC    CB.CLM_RPDT,CB.CLM_RPDT                                          
         MVC   HALF2,SPACES                                                     
         LLC   RF,ONERL1L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   HALF2(0),EXCK1RAC                                                
         GOTOR VALOFF,HALF2                                                     
*&&UK                                                                           
         MVC   CB.CLM_CTRY,D_CTRY  Extract currency and country                 
         MVC   CB.CLM_CURR,D_CURR                                               
*&&                                                                             
                                                                                
         LA    R2,EXCRFST                                                       
         USING CLDELD,R2                                                        
PROCLL12 CLI   CLDEL,0             loop through record                          
         JE    PROCLL44                                                         
         CLI   CLDEL,CLDELQ        as main record only must be CLDELD           
         JE    PROCLL16                                                         
         CLI   CLDEL,CBIELQ                                                     
         JE    PROCLL42                                                         
PROCLL14 LLC   R0,CLDLN                                                         
         AR    R2,R0                                                            
         J     PROCLL12                                                         
                                                                                
PROCLL16 CLI   CLDTYPE,CLDTHDRQ    header?                                      
         JNE   PROCLL20                                                         
         MVC   CB.CLM_DESC,SPACES                                               
         XR    R1,R1                                                            
         IC    R1,CLDLN                                                         
         CLI   CLDLN,CLDLNQ                                                     
         JNH   PROCLL18                                                         
         SHI   R1,CLDLNQ+1                                                      
         CHI   R1,L'CLM_DESC-1                                                  
         JNH   *+8                                                              
         LHI   R1,L'CLM_DESC-1     DESCRIPTION TOO LONG                         
         BASR  RF,0                                                             
         MVC   CB.CLM_DESC(0),CLDDESC                                           
         EX    R1,0(RF)                                                         
PROCLL18 MVC   CB.CLM_CLST,CLDSTAT                                              
         MVC   CB.CLM_UID,CLDUID                                                
         MVC   CB.CLM_BUID,CLDBUID                                              
         MVC   CB.CLM_ABY,CLDPIDAD                                              
         MVC   CB.CLM_ID,CLDIDN                                                 
         ZAP   CB.CLM_TOT,CLDTAMT                                               
         ZAP   CB.CLM_ALAM,CLDTAMT                                              
         MVC   CB.CLM_BREF,CLDBREF                                              
         MVC   CB.CLM_BPID,CLDBABY                                              
         MVC   CB.CLM_BADD,CLDBADD                                              
         MVC   CB.CLM_APDT,CLDAPDTE                                             
         MVC   CB.CLM_APPD,CLDAPPID                                             
         J     PROCLL14                                                         
                                                                                
PROCLL20 CLI   CLDTYPE,CLDSUBQ     Submitter?                                   
         JNE   PROCLL14                                                         
         MVC   CB.CLM_SUBP,CLDSPID                                              
         MVC   CB.CLM_SUBD,CLDSDAT                                              
         J     PROCLL14                                                         
                                                                                
         USING CBIELD,R2                                                        
PROCLL42 MVC   CB.CLM_BNAM,CBIBNA                                               
         J     PROCLL14                                                         
         DROP  R2                                                               
                                                                                
PROCLL44 LA    R3,CB.CLM_RFST                                                   
*        TM    EXCRSTA2,EXCKSFCQ+EXCKSADQ+EXCKWPPQ                              
*        JZ    PROCLL74            No currency or advances - skip               
                                                                                
         USING CLM_FCD,R3                                                       
PROCLL46 LA    R2,EXCRFST                                                       
         USING CIDELD,R2                                                        
PROCLL48 CLI   CIDEL,0             Loop through record                          
         JE    PROCLL68                                                         
         CLI   CIDEL,CIDELQ                                                     
         JE    PROCLL52                                                         
PROCLL50 LLC   R0,CIDLN                                                         
         AR    R2,R0                                                            
         J     PROCLL48                                                         
                                                                                
PROCLL52 LA    R3,CB.CLM_RFST                                                   
         CLI   CIDTYPE,CIDTYMQ     look for main CID elements                   
         JNE   PROCLL64                                                         
         TM    CIDMSTA,CIDMSID     skip deletes                                 
         JNZ   PROCLL50                                                         
         LA    RF,CB.CLM_BITM                                                   
         CLI   CIDMTYP,C'B'        billable?                                    
         JE    *+8                                                              
         LA    RF,CB.CLM_NBTM                                                   
         AP    0(L'CLM_BITM,RF),CIDMAMT add to bill/non-bill total              
         CLI   CIDMTYP,C'A'        advance?                                     
         JNE   PROCLL54                                                         
         AP    CB.CLM_ADVS,CIDMAMT add to advance total                         
         AP    CB.CLM_ALAM,CIDMAMT add to alternative amount                    
                                                                                
PROCLL54 CLC   CIDMCUR,AGYCURR     FC?                                          
         JE    PROCLL50                                                         
         CP    CIDMAMT,PZERO                                                    
         JNE   PROCLL50                                                         
                                                                                
PROCLL56 OC    CLM_FCCD,CLM_FCCD   free entry?                                  
         JZ    PROCLL58                                                         
         CLC   CLM_FCCD,CIDMCUR                                                 
         JE    PROCLL60                                                         
         LA    R3,CLM_FCLQ(R3)                                                  
         J     PROCLL56                                                         
                                                                                
PROCLL58 MVI   CLM_FCI,CLM_FCIQ                                                 
         MVI   CLM_FCLN,CLM_FCLQ                                                
         MVC   CLM_FCCD,CIDMCUR                                                 
         ZAP   CLM_FCAM,PZERO                                                   
                                                                                
PROCLL60 CLI   CIDMTYP,C'A'        advance?                                     
         JE    PROCLL62                                                         
         AP    CLM_FCAM,CIDMFCA                                                 
         J     PROCLL50                                                         
                                                                                
PROCLL62 SP    CLM_FCAM,CIDMFCA                                                 
         J     PROCLL50                                                         
                                                                                
PROCLL64 CLI   CIDTYPE,CIDTYAQ     look for main CID elements                   
         JNE   PROCLL67                                                         
         CLC   CCTPID,CIDAPID      Is connected user level 1 approver           
         JNE   PROCLL66                                                         
         OI    CH_IND,CH_ILVL1                                                  
PROCLL66 CLI   CIDANUM,1                                                        
         JNH   PROCLL50                                                         
         CLC   CCTPID,CIDAPID+CIDALNQ Is connected user level 2 approvr         
         JNE   PROCLL50                                                         
         OI    CH_IND,CH_ILVL2                                                  
         J     PROCLL50                                                         
*                                                                               
PROCLL67 CLI   CIDTYPE,CIDTYPQ     look for Expence Payment details ?           
         JNE   PROCLL50            No, process next element                     
*                                  Found ,                                      
         MVI   CB.CLM_PFND,1        Set payment found on claim                  
         AP    CB.CLM_PTOT,CIDPYAMT Add total payment againest claim            
         CLC   CB.CLM_RPDT,CIDPYMDT Most recent payment ?                       
         JNL   PROCLL50             No, continue                                
*                                   Yes,                                        
         MVC   CB.CLM_RPDT,CIDPYMDT Save Most recent payment date               
         MVC   CB.CLM_RPRF,CIDPYMTR Save most recent payment reference          
         J     PROCLL50                                                         
*                                                                               
PROCLL68 MVC   IOKEY,SAVEKEY2      Restablish primary key                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   IOKEY(EXCKSEQ-EXCRECD),SAVEKEY2                                  
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   PROCLL70                                                         
         CLC   IOKEY(EXCKSEQ-EXCRECD),SAVEKEY2                                  
         JNE   PROCLL70                                                         
         MVC   SAVEKEY2,IOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    PROCLL44                                                         
         DC    H'0'                                                             
                                                                                
PROCLL70 LA    R3,CB.CLM_RFST                                                   
PROCLL72 CLI   CLM_FCI,0                                                        
         JE    PROCLL74                                                         
         LA    R3,CLM_FCLQ(R3)                                                  
         J     PROCLL72                                                         
                                                                                
PROCLL74 CLI   QL_CLOWN,QL_CLOFQ   List - finance approval view?                
         JE    PROCLL76            Yes                                          
         CLI   QS_CSVIE,QS_CSVSQ   Search - staff                               
         JNE   PROCLL80            No                                           
         CLI   GAPAREA,GAPTT9Q     Finance approver                             
         JNE   PROCLL80            No                                           
PROCLL76 TM    CH_IND,CH_ILVL1+CH_ILVL2                                         
         JO    PROCLL78                                                         
         J     PROCLL80                                                         
PROCLL78 LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
PROCLL80 LA    RE,EX_BUFF                                                       
         SR    R3,RE                                                            
         STH   R3,CB.CLM_LEN                                                    
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    EXITL                                                            
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   PROCLL90                                                         
         DC    H'0'                Duplicate key                                
PROCLL90 MVC   LP_ERROR,=AL2(AE$MAX#)                                           
         J     EXITH                                                            
         DROP  CB,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
*  Process claim items and add to buffer                              *         
***********************************************************************         
         USING EXCRECD,R3                                                       
PROCLI   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*PROCLI*'                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2                                                          
         MVC   SAVEKEY2,EXCKEY                                                  
         CLI   GAPAREA,GAPTT9Q     Finance approver                             
         JNE   *+14                No                                           
         CLC   CCTPID,EXCKPIDB     Don't let them self approve                  
         JE    EXITY                                                            
*                                                                               
CB       USING CLI_D,EX_BUFF                                                    
         LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   CB.CLI_IPTO,PZERO                                                
         ZAP   CB.CLI_IAMT,PZERO                                                
         ZAP   CB.CLI_INET,PZERO                                                
         ZAP   CB.CLI_IVMT,PZERO                                                
         ZAP   CB.CLI_IFMT,PZERO                                                
*&&US*&& ZAP   CB.CLI_IPSA,PZERO                                                
*                                                                               
         MVC   CB.CLI_TYPE,EXCKTYPE                                             
         MVC   CB.CLI_NUM,EXCKREF                                               
         MVC   CB.CLI_PIDB,EXCKPIDB                                             
         MVC   CB.CLI_ULDG,=C'1R'                                               
         MVC   CB.CLI_ACC,EXCK1RAC                                              
         LLC   RF,ONERL1L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   HALF2(0),EXCK1RAC                                                
         GOTOR VALOFF,HALF2                                                     
         MVC   CB.CLI_STAT,EXCRSTAT                                             
         MVC   CB.CLI_STA2,EXCRSTA2                                             
         XR    R1,R1                                                            
         ICM   R1,3,EXCKDATE                                                    
         LNR   R1,R1                                                            
         STCM  R1,3,CB.CLI_DATE                                                 
         ZAP   CB.CLI_APST,EXCRNAPP                                             
*&&UK*&& MVC   CB.CLI_CTRY,D_CTRY  Extract currency and country                 
         MVI   MYBYTE,0            BYTE1 USED TO HOLD SEQUENCE                  
*                                                                               
         LA    R4,EXCRFST                                                       
*                                                                               
         USING CIDELD,R4                                                        
PROCLI02 CLI   CIDEL,0                                                          
         JE    PROCLI40                                                         
         CLI   CIDEL,CLDELQ                                                     
         JE    PROCLI06                                                         
         CLI   CIDEL,CIDELQ                                                     
         JE    PROCLI08                                                         
PROCLI04 LLC   R0,CIDLN                                                         
         AR    R4,R0                                                            
         J     PROCLI02                                                         
*                                                                               
         USING CLDELD,R4                                                        
PROCLI06 CLI   CLDTYPE,CLDTHDRQ    Header info                                  
         JNE   PROCLI07                                                         
         MVC   CB.CLI_UID,CLDUID                                                
         MVC   CB.CLI_ABY,CLDPIDAD                                              
         J     PROCLI04                                                         
*                                                                               
PROCLI07 CLI   CLDTYPE,CLDLMAPQ    Line manager approvals?                      
         JNE   PROCLI04                                                         
         MVC   CB.CLI_LMPD,CLDLAPP                                              
         J     PROCLI04                                                         
*                                                                               
         USING CIDELD,R4                                                        
PROCLI08 OC    MYBYTE,MYBYTE       FIRST TIME?                                  
         JZ    PROCLI10                                                         
         CLC   MYBYTE,CIDSEQ       SAME SEQUENCE?                               
         JNE   PROCLI10                                                         
         TM    RUNINDS,RUNISKIP    SKIPPING DATA FOR THIS ENTRY                 
         JNZ   PROCLI04                                                         
*                                                                               
PROCLI10 CLI   CIDTYPE,CIDTYMQ     MAIN ELEMENT?                                
         JNE   PROCLI22                                                         
         OC    MYBYTE,MYBYTE       FIRST TIME?                                  
         JZ    PROCLI14                                                         
         TM    RUNINDS,RUNISKIP    SKIPPING DATA FOR THIS ENTRY                 
         JNZ   PROCLI12                                                         
         LHI   RE,CLI_RLNQ                                                      
         STH   RE,CB.CLI_RLEN                                                   
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    PROCLI12                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   PROCLIMX                                                         
         DC    H'0'                Duplicate key                                
*                                                                               
PROCLI12 LA    R0,CB.CLI_IFST      Clear out item values                        
         LHI   R1,CLI_ILNQ                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   CB.CLI_IPTO,PZERO                                                
         ZAP   CB.CLI_IAMT,PZERO                                                
         ZAP   CB.CLI_INET,PZERO                                                
         ZAP   CB.CLI_IVMT,PZERO                                                
         ZAP   CB.CLI_IFMT,PZERO                                                
*&&US*&& ZAP   CB.CLI_IPSA,PZERO                                                
*                                                                               
PROCLI14 NI    RUNINDS,X'FF'-RUNISKIP Clear skip flag                           
         MVC   MYBYTE,CIDSEQ       Save sequence                                
         CLC   QS_ETYC,SPACES      Any expenditure type filter                  
         JNH   *+14                                                             
         CLC   CIDMEXP,QS_ETYC     Any match?                                   
         JNE   PROCLISK                                                         
         CLC   QS_RCPT,SPACES      Any receipt filter                           
         JNH   PROCLI16                                                         
         CLI   QS_RCPT,BOTHQ       Include lines with/without receipt           
         JE    PROCLI16                                                         
         CLI   QS_RCPT,NOQ         Receipt = NO                                 
         JNE   *+12                                                             
         CLI   CIDMREC,C' '        Binary zeros/space = NO                      
         JNH   PROCLI16                                                         
         CLC   QS_RCPT,CIDMREC     Check receipt flag matches                   
         JNE   PROCLISK                                                         
*                                                                               
PROCLI16 CLC   QS_BILL,SPACES      Any receipt filter                           
         JNH   PROCLI18                                                         
         CLI   QS_BILL,BOTHQ       Include bill/non-bill                        
         JE    PROCLI18                                                         
         CLI   QS_BILL,YESQ        Billable only?                               
         JNE   *+12                                                             
         CLI   CIDMTYP,CIDMBILQ                                                 
         JNE   PROCLISK                                                         
         CLI   QS_BILL,NOQ         Non-billable only?                           
         JNE   *+12                                                             
         CLI   CIDMTYP,CIDMNBLQ                                                 
         JNE   PROCLISK                                                         
*                                                                               
PROCLI18 CLC   QS_CURR,SPACES      Any currency filter?                         
         JNH   *+14                                                             
         CLC   QS_CURR,CIDMCUR     Check currency matches                       
         JNE   PROCLISK                                                         
*                                                                               
PROCLI20 MVC   CB.CLI_ISEQ,CIDSEQ                                               
         CLC   CIDMDAT,CH_ENDDT   date filtering                                
         JH    PROCLISK                                                         
         CLC   CIDMDAT,CH_STADT                                                 
         JL    PROCLISK                                                         
         MVC   CB.CLI_IDTE,CIDMDAT                                              
         MVC   CB.CLI_IETY,CIDMEXP                                              
         MVC   CB.CLI_IBIL,CIDMTYP                                              
         MVC   CB.CLI_IRCP,CIDMREC                                              
         MVC   CB.CLI_IWRK,CIDMWCD                                              
*&&UK*&& MVC   CB.CLI_CURR,CIDMCUR                                              
*&&US                                                                           
         MVC   CB.CLI_IPRC,CIDMPROV                                             
         TP    CIDMGST                                                          
         JNZ   *+10                                                             
         ZAP   CB.CLI_IVMT,CIDMGST GST/PST and province for Canada              
         TP    CIDMPST                                                          
         JNZ   *+10                                                             
         ZAP   CB.CLI_IPSA,CIDMPST                                              
*&&                                                                             
*&&UK                                                                           
         ZAP   DUB,CIDMAMT         Calculate vat amount for the UK              
         CP    CIDMFCA,PZERO                                                    
         JNH   *+10                                                             
         ZAP   DUB,CIDMFCA         If FC use FC amount                          
         SP    DUB,CIDMNET                                                      
         ZAP   CB.CLI_IVMT,DUB                                                  
*&&                                                                             
         ZAP   CB.CLI_IAMT,CIDMAMT                                              
         ZAP   CB.CLI_INET,CIDMNET                                              
         MVC   CB.CLI_IXRT,CIDMRAT                                              
         ZAP   CB.CLI_IFMT,CIDMFCA                                              
         J     PROCLI04                                                         
*                                                                               
PROCLI22 CLI   CIDTYPE,CIDTYCQ     Account element?                             
         JNE   PROCLI34                                                         
         CLC   CIDCCPJ,SPACES      Any cpj?                                     
         JNH   PROCLI26                                                         
*                                                                               
         LA    R1,CIDCCPJ                                                       
         CLC   QS_CLI,SPACES       Any client filter?                           
         JNH   PROCLI24                                                         
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_CLI      Check client matches                         
         EX    RF,0(RE)                                                         
         JNE   PROCLISK                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
*                                                                               
         CLC   QS_PRO,SPACES       Any product filter?                          
         JNH   PROCLI24                                                         
                                                                                
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_PRO      Check product matches                        
         EX    RF,0(RE)                                                         
         JNE   PROCLISK                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
*                                                                               
         CLC   QS_JOB,SPACES       Any product filter?                          
         JNH   PROCLI24                                                         
                                                                                
         LLC   RF,PJOBLEN                                                       
         LLC   RE,PPROLEN                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_JOB      Check job matches                            
         EX    RF,0(RE)                                                         
         JNE   PROCLISK                                                         
*                                                                               
PROCLI24 MVC   CB.CLI_IUL,=C'SJ'                                                
         MVC   CB.CLI_IACC,CIDCCPJ                                              
         CLC   QS_CLIO,SPACES      Client office filter?                        
         JNH   PROCLI25                                                         
         XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                                                              
         LHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   CIDCOFF(0),QS_CLIO  Check office matches                         
         EX    RF,0(RE)                                                         
         JNE   PROCLISK                                                         
PROCLI25 MVC   CB.CLI_IOFF,CIDCOFF Check client office matches?                 
         J     PROCLI28                                                         
*                                                                               
PROCLI26 CLC   QS_CLI,SPACES       Any client filter?                           
         JH    PROCLISK            Then skip non bill rows as well              
         CLC   QS_CLIO,SPACES      Client office filter?                        
         JH    PROCLISK            Then skip entry if 1r office                 
         MVC   CB.CLI_IEXP,CIDCEXP                                              
         XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                                                              
         LHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   CB.CLI_IOFF(0),EXCK1RAC                                          
         EX    RF,0(RE)                                                         
*                                                                               
PROCLI28 CLC   QS_VATA,SPACES      Any VAT filter                               
         JNH   PROCLI32                                                         
*&&UK                                                                           
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         JNZ   PROCLI30            Check new vat                                
         CLC   QS_VATA,CIDCVAC                                                  
         JNE   PROCLISK                                                         
         J     PROCLI32                                                         
*&&                                                                             
PROCLI30 CLC   QS_VATA(L'CIDCVAT),CIDCVAT                                       
         JNE   PROCLISK            Check VAT matches                            
*                                                                               
PROCLI32 DS    0H                                                               
*&&UK                                                                           
         MVC   CB.CLI_IVTC(L'CIDCVAT),CIDCVAT                                   
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         JNZ   *+10                Check new vat                                
         MVC   CB.CLI_IVTC,CIDCVAC                                              
*&&                                                                             
*&&US*&& MVC   CB.CLI_IVTC(L'CIDCVAT),CIDCVAT Set VAT/GST code                  
*&&US*&& MVC   CB.CLI_IPST,CIDCPST Set Canadian PST Code                        
         J     PROCLI04                                                         
                                                                                
PROCLI34 CLI   CIDTYPE,CIDTYNQ     Finance narrative                            
         JNE   PROCLI36                                                         
         LLC   RF,CIDLN                                                         
         SHI   RF,1+(CIDDATA-CIDELD)                                            
         BASR  RE,0                                                             
         MVC   CB.CLI_FNAR(0),CIDNARR                                           
         EX    RF,0(RE)                                                         
         OC    CB.CLI_FNAR,SPACES                                               
         J     PROCLI04                                                         
*                                                                               
PROCLI36 CLI   CIDTYPE,CIDTYOQ     Original (claimaint) narrative?              
         JNE   PROCLI38                                                         
         LLC   RF,CIDLN                                                         
         SHI   RF,1+(CIDDATA-CIDELD)                                            
         BASR  RE,0                                                             
         MVC   CB.CLI_INAR(0),CIDNARR                                           
         EX    RF,0(RE)                                                         
         OC    CB.CLI_INAR,SPACES                                               
         J     PROCLI04                                                         
*                                                                               
PROCLI38 CLI   CIDTYPE,CIDTYPQ     Payment info?                                
         JNE   PROCLI04                                                         
         MVC   CB.CLI_IPYR,CIDPYMTR                                             
         MVC   CB.CLI_IPYD,CIDPYMDT                                             
         ZAP   CB.CLI_IPTO,CIDPYAMT                                             
         J     PROCLI04                                                         
*                                                                               
PROCLI40 TM    RUNINDS,RUNISKIP    Skipping data for this entry                 
         JNZ   PROCLIMX                                                         
         CLC   CB.CLI_IDTE,SPACES  Any entry to put to the buffer?              
         JNH   EXITY                                                            
         LHI   RE,CLI_ILNQ                                                      
         STH   RE,CB.CLI_RLEN                                                   
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    EXITY                                                            
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   PROCLIMX                                                         
         DC    H'0'                Duplicate key                                
*                                                                               
PROCLISK OI    RUNINDS,RUNISKIP    Set flag to skip CIDELDs for this            
         J     PROCLI04            Sequence                                     
*                                                                               
PROCLIMX MVC   LP_ERROR,=AL2(AE$MAX#)                                           
         J     EXITH                                                            
         EJECT                                                                  
         DROP  R3,R4,CB                                                         
***********************************************************************         
*  Get claims according to approvers rights and status                *         
***********************************************************************         
                                                                                
CLLASS   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLLASS*'                                                      
         USING GAPTABD,R4                                                       
                                                                                
         LA    R4,GAPAREA          Point to approves table                      
         XC    IOKEY,IOKEY                                                      
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CLLASS02 TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLASSX             Buffer is empty                              
         TM    GAPTSTA,GAPTSMQ     look for main entries only                   
         JZ    CLLASS06                                                         
         CLI   GAPTDAT1,GAPTT1Q    branch to 1R code - line manager             
         JE    CLLASS20                                                         
         CLI   GAPTDAT1,GAPTT9Q    branch to 1R code - finance approver         
         JE    CLLASS20                                                         
         CLI   GAPTDAT1,GAPTT6Q    branch to Media code                         
         JE    CLLASS20                                                         
         CLI   GAPTDAT1,GAPTT2Q    branch to SJ code                            
         JE    CLLASS20                                                         
         J     CLLASS06                                                         
                                                                                
CLLASS04 GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CLLASS06 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CLLASS02                                                         
*                                                                               
         USING EXSPASD,R2                                                       
CLLASS20 LA    R2,IOKEY            use status passive for all status'           
         XC    EXSPAS,EXSPAS                                                    
         MVI   EXSPTYP,EXSPTYPQ                                                 
         MVI   EXSPSUB,EXSPSUBQ                                                 
         MVC   EXSPCPY,CUXCPY                                                   
         MVC   EXSPKYST,CH_FSTAT   set status                                   
         MVC   EXSPCAT,GAPTAPPL                                                 
         MVC   EXSPCODE,GAPTCODE                                                
         XC    CH_ACT,CH_ACT                                                    
CLLASS30 MVC   SAVEKEY1,EXSPASD    save start key                               
                                                                                
CLLASS32 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLLASS36                                                         
         J     CLLASSX                                                          
                                                                                
CLLASS34 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   CLLASSX                                                          
                                                                                
CLLASS36 CLC   SAVEKEY1(EXSPCODE-EXSPASD),EXSPAS                                
         JNE   CLLASS04                                                         
         TM    EXSPSTAT,EXCSDELT                                                
         JNZ   CLLASS34                                                         
         CLI   GAPTLVL,GAPTSL0                                                  
         JE    CLLASS42                                                         
                                                                                
CLLASS38 SR    RE,RE                                                            
         IC    RE,GAPTLEN          test same 1R/SJ account                      
         SHI   RE,1                                                             
         BASR  RF,0                                                             
CLLASS40 CLC   EXSPCODE(0),SAVEKEY1+EXSP1RAC-EXSPASD                            
         EX    RE,0(RF)                                                         
         JNE   CLLASS04                                                         
                                                                                
CLLASS42 CLI   QS_CSCNP,C' '       CLAIM NUMBER FILTER?                         
         JNH   CLLASS43                                                         
         CLC   EXSPREF,QS_CSCNP                                                 
         JNE   CLLASS34            NO MATCH, NEXT CLAIM                         
                                                                                
CLLASS43 CLI   GAPTDAT1,GAPTT6Q    SJ account?                                  
         JE    *+12                                                             
         CLI   GAPTDAT1,GAPTT2Q                                                 
         JNE   CLLASS50                                                         
                                                                                
         USING OFFALD,R1                                                        
CLLASS45 CLI   LP_ACCS,0           skip if no limit access                      
         JE    CLLASS4A                                                         
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EXSPCOFF                                                
         CLI   GAPTDAT1,GAPTT2Q    If SJ use client office                      
         JE    *+10                                                             
         MVC   OFFAOFFC,EXSPMOFF                                                
         CLI   GAPTDAT1,GAPTT9Q    finance use 1r office                        
         JNE   *+10                                                             
         MVC   OFFAOFFC,EXSP1RAC                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   CLLASS48                                                         
         DROP  R1                                                               
                                                                                
CLLASS4A MVC   GAPAREA2,GAPAREA                                                 
         LA    R1,EXSPCODE         test whether Cli/pro/job wanted              
         GOTOR TTSJVGT                                                          
         JE    CLLASS67                                                         
CLLASS48 XR    RE,RE               read high for next a/c                       
         IC    RE,EXSPSJAC+L'EXSPSJAC-1                                         
         AHI   RE,1                                                             
         STC   RE,EXSPSJAC+L'EXSPSJAC-1                                         
         J     CLLASS32                                                         
                                                                                
CLLASS50 CLI   GAPTDAT1,GAPTT9Q    1r account filtering                         
         JE    *+12                                                             
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   CLLASS67                                                         
                                                                                
         USING OFFALD,R1                                                        
         CLI   LP_ACCS,0           skip if no limit access                      
         JE    CLLASS52                                                         
         L     R1,AOFFAREA                                                      
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   OFFAOFFC(0),EXSP1RAC                                             
         EX    RF,0(RE)                                                         
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   CLLASS60                                                         
*                                                                               
                                                                                
CLLASS52 CLC   CH_ACT,EXSP1RAC     match on previous                            
         JE    CLLASS67                                                         
         MVC   GAPAREA2,GAPAREA                                                 
         LA    R1,EXSP1RAC                                                      
         GOTOR TT1RVGT             test against table                           
         JE    CLLASS65                                                         
CLLASS60 MVI   EXSPMED,FF          reset key to next record                     
         J     CLLASS32                                                         
                                                                                
CLLASS65 MVC   CH_ACT,EXSP1RAC                                                  
                                                                                
CLLASS67 CLC   EXSPDATE,CH_ENDDC   date filtering                               
         JNL   CLLASS70                                                         
         MVC   EXSPDATE,CH_ENDDC                                                
         MVI   EXSPREF,0                                                        
         J     CLLASS32            read high                                    
                                                                                
CLLASS70 CLC   EXSPDATE,CH_STADC                                                
         JNH   CLLASS75                                                         
         MVC   EXSPDATE,=XL2'FFFF'                                              
         J     CLLASS32            read high                                    
                                                                                
CLLASS75 MVC   SAVEKEY4,EXSPAS     save for sequences                           
                                                                                
         XC    EX_BUFF(L'CLM_LEN+CLM_KEYL),EX_BUFF                              
CB       USING CLM_D,EX_BUFF                                                    
CI       USING CLI_D,EX_BUFF                                                    
         MVC   CB.CLM_NUM,EXSPREF                                               
         MVI   CB.CLM_TYPE,EXNPTLQ                                              
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    CLLASS34                                                         
         CLI   QS_CSVIE,QS_CSVRQ   For finance report                           
         JNE   CLLASS76                                                         
         CLC   CB.CLM_NUM,EXSPREF  Check record record matches                  
         JNE   CLFASS39                                                         
         CLI   CB.CLM_TYPE,EXNPTLQ                                              
         JNE   CLFASS39                                                         
         OC    CI.CLI_ISEQ,CI.CLI_ISEQ                                          
         JNZ   CLFASS24                                                         
*                                                                               
CLLASS76 TM    TSARERRS,TSERNF     Test record not found                        
         JNZ   CLLASS80                                                         
         DC    H'0'                Duplicate key                                
         DROP  CB                                                               
                                                                                
CLLASS80 CLI   EXSPSEQ,0                                                        
         JE    CLLASS90                                                         
         LA    R2,SAVEKEY4                                                      
INI      USING EXNPASD,IOKEY                                                    
         XC    INI.EXNPAS,INI.EXNPAS                                            
         MVI   INI.EXNPTYP,EXNPTYPQ                                             
         MVI   INI.EXNPSUB,EXNPSUBQ                                             
         MVC   INI.EXNPCPY,CUXCPY                                               
         MVC   INI.EXNPNUM,EXSPREF                                              
         MVI   INI.EXNPTYPE,EXNPTLQ                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLASS90                                                         
         DC    H'0'                                                             
         DROP  INI,R2                                                           
                                                                                
CLLASS90 CLI   QS_CSVIE,QS_CSVRQ                                                
         JE    CLLASS92                                                         
         GOTOR PROCLL                                                           
         J     CLLASS94                                                         
*                                                                               
CLLASS92 GOTOR PROCLI              Process claim items                          
*                                                                               
CLLASS94 MVC   IOKEY,SAVEKEY4      reestablish key                              
         LLC   RF,IOKEY+EXSPSEQ-EXSPASD                                         
         AHI   RF,1                                                             
         STC   RF,IOKEY+EXSPSEQ-EXSPASD                                         
         J     CLLASS32                                                         
                                                                                
CLLASSX  J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  Get claims for finance view according to status   s                *         
***********************************************************************         
                                                                                
CLFASS   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLFASS*'                                                      
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JE    CLFASS07                                                         
         USING GAPTABD,R4                                                       
*                                                                               
         LA    R4,GAPAREA          Point to approves table                      
         XC    IOKEY,IOKEY                                                      
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CLFASS02 TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLFASSX             Buffer is empty                              
         TM    GAPTSTA,GAPTSMQ     look for main entries only                   
         JZ    CLFASS06                                                         
         CLI   GAPTDAT1,GAPTT9Q    branch to 1R code - finance approver         
         JE    CLFASS07                                                         
         J     CLFASS06                                                         
                                                                                
CLFASS04 GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CLFASS06 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CLFASS02                                                         
                                                                                
CLFASS07 LA    R6,CH_FSTAT                                                      
                                                                                
         USING EXSPASD,R2                                                       
CLFASS08 LA    R2,IOKEY            use status passive for all status'           
         XC    EXSPAS,EXSPAS                                                    
         MVI   EXSPTYP,EXSPTYPQ                                                 
         MVI   EXSPSUB,EXSPSUBQ                                                 
         MVC   EXSPCPY,CUXCPY                                                   
         MVC   EXSPKYST,0(R6)      set status                                   
         CLI   EXSPKYST,EXCSPAPP   Part approved?                               
         JNE   *+8                                                              
         MVI   EXSPKYST,EXCSSUBM   Set to submitted                             
         MVI   EXSPCAT,EXSPLFN1                                                 
         XC    EXSPCODE,EXSPCODE                                                
*                                                                               
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JE    *+10                                                             
         MVC   EXSPCODE,GAPTCODE   If not populate key with GAPTCODE            
*                                                                               
         CLC   QS_OFFC,SPACES      Any 1r office code filter?                   
         JNH   CLFASS10                                                         
         XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2 2 char office?                   
         JZ    *+8                                                              
         LHI   RF,1                                                             
*                                                                               
         CLC   GAPTCODE,SPACES     Default entry (i.e. all access)              
         JNH   *+12                                                             
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JNE   CLFASS09                                                         
         BASR  RE,0                Establish key with office                    
         MVC   EXSPCODE(0),QS_OFFC                                              
         EX    RF,0(RE)                                                         
         J     CLFASS10                                                         
*                                                                               
CLFASS09 BASR  RE,0                                                             
         CLC   EXSPCODE(0),QS_OFFC Check GAPTCODE matches office                
         EX    RF,0(RE)                                                         
         JNE   CLFASS06                                                         
*                                                                               
CLFASS10 CLC   QS_DEPC,SPACES      Any 1r dep code filter?                      
         JNH   CLFASS14                                                         
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
*                                                                               
         LLC   R1,ONERL1L                                                       
         LA    R1,EXSPCODE(R1)                                                  
         CLC   GAPTCODE,SPACES     Default entry (i.e. all access)              
         JNH   *+12                                                             
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JNE   CLFASS12                                                         
         BASR  RE,0                Establish key with office                    
         MVC   0(0,R1),QS_DEPC                                                  
         EX    RF,0(RE)                                                         
         J     CLFASS14                                                         
*                                                                               
CLFASS12 BASR  RE,0                                                             
         CLC   0(0,R1),SPACES      Any GAPTCODE filter?                         
         EX    RF,0(RE)                                                         
         JNH   CLFASS20                                                         
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_DEPC     Check GAPTCODE matches office                
         EX    RF,0(RE)                                                         
         JNE   CLFASS06                                                         
*                                                                               
CLFASS14 CLC   QS_SDPC,SPACES      Any 1r sub code filter?                      
         JNH   CLFASS20                                                         
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
*                                                                               
         LLC   R1,ONERL2L                                                       
         LA    R1,EXSPCODE(R1)                                                  
         BCTR  RF,0                                                             
         CLC   GAPTCODE,SPACES     Default entry (i.e. all access)              
         JNH   *+12                                                             
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JNE   CLFASS16                                                         
         BASR  RE,0                Establish key with office                    
         MVC   0(0,R1),QS_SDPC                                                  
         EX    RF,0(RE)                                                         
         J     CLFASS20                                                         
*                                                                               
CLFASS16 BASR  RE,0                                                             
         CLC   0(0,R1),SPACES      Any GAPTCODE filter?                         
         EX    RF,0(RE)                                                         
         JNH   CLFASS20                                                         
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_SDPC     Check GAPTCODE matches subdepartment         
         EX    RF,0(RE)                                                         
         JNE   CLFASS06                                                         
*                                                                               
CLFASS20 MVC   SAVEKEY1,EXSPASD    save start key                               
                                                                                
CLFASS22 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLFASS28                                                         
         J     CLFASS26                                                         
                                                                                
CLFASS24 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    CLFASS28                                                         
*                                                                               
CLFASS26 AHI   R6,L'EXCKSTAT                                                    
         LA    RF,CH_FSTAT+L'CH_FSTAT                                           
         CLI   0(R6),0             Empty means no more statuses to              
         JE    *+10                                        read                 
         CR    R6,RF                                                            
         JL    CLFASS08            Any more statuses to check?                  
         CLI   QS_OVAPP,YESQ       Overriding limlist/approver rights?          
         JE    CLFASSX                                                          
         J     CLFASS06                                                         
                                                                                
CLFASS28 CLC   SAVEKEY1(EXSPCODE-EXSPASD),EXSPAS                                
         JNE   CLFASS26                                                         
         TM    EXSPSTAT,EXCSDELT                                                
         JNZ   CLFASS24                                                         
*                                                                               
         CLI   QS_PRTAP,YESQ       Looking for part approved?                   
         JE    *+12                Yes                                          
         TM    EXSPSTAT,EXCSPAPP   No - Check part approved status              
         JNZ   CLFASS24              and skip if it is                          
         CLI   QS_SUBMT,YESQ       Looking for submitted?                       
         JE    *+12                Yes                                          
         TM    EXSPSTAT,EXCSSUBM   No - Check not submitted status              
         JNZ   CLFASS24              and skip if it is                          
         CLI   QS_CSCNP,C' '       Claim number filter                          
         JNH   CLFASS30                                                         
         CLC   EXSPREF,QS_CSCNP                                                 
         JNE   CLFASS24            No match, next claim                         
*                                                                               
CLFASS30 CLI   QS_OVAPP,YESQ                                                    
         JE    CLFASS32                                                         
         CLI   GAPTLVL,GAPTSL0     Check result matches GAPLST entry            
         JE    CLFASS32                                                         
         SR    RE,RE                                                            
         IC    RE,GAPTLEN          test same 1R/SJ account                      
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   EXSPCODE(0),SAVEKEY1+EXSP1RAC-EXSPASD                            
         EX    RE,0(RF)                                                         
         JNE   CLFASS26                                                         
*                                                                               
CLFASS32 CLC   QS_OFFC,SPACES      Any 1R office code filter?                   
         JNH   CLFASS34                                                         
         XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                                                              
         LHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   EXSPCODE(0),QS_OFFC Check office code                            
         EX    RF,0(RE)                                                         
         JNE   CLFASS24            done if not matching                         
                                                                                
         USING OFFALD,R1                                                        
CLFASS34 CLI   LP_ACCS,0           Skip if no limit access                      
         JE    CLFASS35                                                         
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EXSP1RAC   GAPTT9Q use 1r office                        
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   CLFASS24                                                         
         DROP  R1                                                               
*                                                                               
CLFASS35 CLC   QS_DEPC,SPACES      Any 1R dep code filter?                      
         JNH   CLFASS36                                                         
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
*                                                                               
         LLC   R1,ONERL1L                                                       
         LA    R1,EXSPCODE(R1)                                                  
*                                                                               
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_DEPC     Check department code                        
         EX    RF,0(RE)                                                         
         JNE   CLFASS24            done if not matching                         
*                                                                               
CLFASS36 CLC   QS_SDPC,SPACES      Any 1R sub code filter?                      
         JNH   CLFASS38                                                         
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
*                                                                               
         LLC   R1,ONERL2L                                                       
         LA    R1,EXSPCODE(R1)                                                  
*                                                                               
         BASR  RE,0                                                             
         CLC   0(0,R1),QS_SDPC     Check subdepartment code                     
         EX    RF,0(RE)                                                         
         JNE   CLFASS24            done if not matching                         
*                                                                               
CLFASS38 CLI   QS_CSVIE,QS_CSVRQ   Finance report                               
         JE    CLFASS3A            date filtering is on the row                 
         CLC   EXSPDATE,CH_ENDDC   date filtering                               
         JL    CLFASS24                                                         
         CLC   EXSPDATE,CH_STADC                                                
         JH    CLFASS24                                                         
*                                                                               
CLFASS3A MVC   SAVEKEY4,EXSPAS     save for sequences                           
                                                                                
         XC    EX_BUFF(L'CLM_LEN+CLM_KEYL),EX_BUFF                              
CB       USING CLM_D,EX_BUFF                                                    
CI       USING CLI_D,EX_BUFF                                                    
         MVC   CB.CLM_NUM,EXSPREF                                               
         MVI   CB.CLM_TYPE,EXNPTLQ                                              
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    CLFASS24                                                         
         CLI   QS_CSVIE,QS_CSVRQ   For finance report                           
         JNE   CLFASS39                                                         
         CLC   CB.CLM_NUM,EXSPREF  Check record record matches                  
         JNE   CLFASS39                                                         
         CLI   CB.CLM_TYPE,EXNPTLQ                                              
         JNE   CLFASS39                                                         
         OC    CI.CLI_ISEQ,CI.CLI_ISEQ                                          
         JNZ   CLFASS24                                                         
*                                                                               
CLFASS39 TM    TSARERRS,TSERNF     Test record not found                        
         JNZ   CLFASS40                                                         
         DC    H'0'                Duplicate key                                
         DROP  CB,CI                                                            
                                                                                
CLFASS40 CLI   EXSPSEQ,0                                                        
         JE    CLFASS42                                                         
         LA    R2,SAVEKEY4                                                      
INI      USING EXNPASD,IOKEY                                                    
         XC    INI.EXNPAS,INI.EXNPAS                                            
         MVI   INI.EXNPTYP,EXNPTYPQ                                             
         MVI   INI.EXNPSUB,EXNPSUBQ                                             
         MVC   INI.EXNPCPY,CUXCPY                                               
         MVC   INI.EXNPNUM,EXSPREF                                              
         MVI   INI.EXNPTYPE,EXNPTLQ                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLFASS42                                                         
         DC    H'0'                                                             
         DROP  INI,R2                                                           
                                                                                
CLFASS42 CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JE    CLFASS44                                                         
         GOTOR PROCLL                                                           
         J     CLFASS46                                                         
*                                                                               
CLFASS44 GOTOR PROCLI              Process record                               
*                                                                               
CLFASS46 MVC   IOKEY,SAVEKEY4      reestablish key                              
         LLC   RF,IOKEY+EXSPSEQ-EXSPASD                                         
         AHI   RF,1                                                             
         STC   RF,IOKEY+EXSPSEQ-EXSPASD                                         
         J     CLFASS22                                                         
                                                                                
CLFASSX  J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  Get claims according to PID passive for approver                   *         
*  Supports workflow where approvers are chosen by submitter          *         
***********************************************************************         
                                                                                
CLLAPS   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLLAPS*'                                                      
                                                                                
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            use PID passive for all status'              
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,CCTPID                                                   
         MVI   PIDKSTYP,PIDKXAPQ                                                
         CLI   QL_CLVBY,QL_CLVSQ   Are we looking for claims via status         
         JNE   CLLAPS02            No                                           
         MVC   PIDKXSTA,CH_FSTAT   set status                                   
         MVC   PIDKXDTE,CH_ENDDC                                                
CLLAPS02 MVC   SAVEKEY1,PIDRECD    save start key                               
                                                                                
CLLAPS04 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLLAPS08                                                         
         J     CLLAPSX                                                          
                                                                                
CLLAPS06 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   CLLAPSX                                                          
CLLAPS08 LA    RE,PIDKXDTE-(PIDRECD+1)                                          
         CLI   QL_CLVBY,QL_CLVSQ   Are we looking for claims via status         
         JE    *+8                 Yes                                          
         LA    RE,PIDKXSTA-(PIDRECD+1)  No - must be via date                   
         BASR  RF,0                                                             
         CLC   SAVEKEY1(0),PIDKEY                                               
         EX    RE,0(RF)                                                         
         JNE   CLLAPSX                                                          
         TM    PIDKSTAT,EXCSDELT                                                
         JNZ   CLLAPS06                                                         
                                                                                
         CLI   QS_CSCNP,C' '       CLAIM NUMBER FILTER?                         
         JNH   CLLAPS10                                                         
         CLC   PIDKXCLM,QS_CSCNP                                                
         JNE   CLLAPS06            NO MATCH, NEXT CLAIM                         
                                                                                
         USING OFFALD,R1                                                        
         CLI   LP_ACCS,0           skip if no limit access                      
         JE    CLLAPS10                                                         
         L     R1,AOFFAREA                                                      
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   OFFAOFFC(0),PIDKXOFF                                             
         EX    RF,0(RE)                                                         
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   CLLAPS06                                                         
         DROP  R1                                                               
                                                                                
CLLAPS10 CLC   PIDKXDTE,CH_ENDDC   date filtering                               
         JNL   CLLAPS12                                                         
         MVC   PIDKXDTE,CH_ENDDC                                                
         MVI   PIDKXCLM,0                                                       
         J     CLLAPS04            read high                                    
                                                                                
CLLAPS12 CLC   PIDKXDTE,CH_STADC                                                
         JNH   CLLAPS14                                                         
         MVC   PIDKXDTE,=XL2'FFFF'                                              
         J     CLLAPS04            read high                                    
                                                                                
CLLAPS14 MVC   SAVEKEY4,PIDKEY     save for sequences                           
                                                                                
         XC    EX_BUFF(L'CLM_LEN+CLM_KEYL),EX_BUFF                              
CB       USING CLM_D,EX_BUFF                                                    
         MVC   CB.CLM_NUM,PIDKXCLM                                              
         MVI   CB.CLM_TYPE,EXNPTLQ                                              
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    CLLAPS06                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JNZ   CLLAPS16                                                         
         DC    H'0'                Duplicate key                                
         DROP  CB                                                               
                                                                                
CLLAPS16 CLI   PIDKSEQ,0                                                        
         JE    CLLAPS18                                                         
         LA    R2,SAVEKEY4                                                      
INI      USING EXNPASD,IOKEY                                                    
         XC    INI.EXNPAS,INI.EXNPAS                                            
         MVI   INI.EXNPTYP,EXNPTYPQ                                             
         MVI   INI.EXNPSUB,EXNPSUBQ                                             
         MVC   INI.EXNPCPY,CUXCPY                                               
         MVC   INI.EXNPNUM,PIDKXCLM                                             
         MVI   INI.EXNPTYPE,EXNPTLQ                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLAPS18                                                         
         DC    H'0'                                                             
         DROP  INI,R2                                                           
                                                                                
CLLAPS18 GOTOR PROCLL              process record                               
         MVC   IOKEY,SAVEKEY4      reestablish key                              
         LLC   RF,IOKEY+PIDKSEQ-PIDRECD                                         
         AHI   RF,1                                                             
         STC   RF,IOKEY+PIDKSEQ-PIDRECD                                         
         J     CLLAPS04                                                         
                                                                                
CLLAPSX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
*  Get claims from claim number                                       *         
***********************************************************************         
                                                                                
CLLAIM   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLLAIM*'         === approver view/ by claim no.===           
                                                                                
         CLI   QS_CSCNP,C' '       CLAIM NUMBER FILTER?                         
         JNH   CLLAIMX                                                          
                                                                                
         USING EXNPASD,R2                                                       
         LA    R2,IOKEY            build exact key                              
         XC    EXNPAS,EXNPAS                                                    
         MVI   EXNPTYP,EXNPTYPQ                                                 
         MVI   EXNPSUB,EXNPSUBQ                                                 
         MVC   EXNPCPY,CUXCPY                                                   
         MVI   EXNPTYPE,EXNPTLQ                                                 
         MVC   EXNPNUM,QS_CSCNP                                                 
         MVC   SAVEKEY1,EXNPAS                                                  
                                                                                
CLLAIM10 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLAIM20                                                         
         MVC   EXNPAS,SAVEKEY1                                                  
         MVI   EXNPTYPE,EXNPTDQ                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLAIM20                                                         
         J     CLLAIMX                                                          
                                                                                
CLLAIM20 MVC   SAVEKEY4,EXNPAS                                                  
         TM    EXNPSTA,EXCSDELT                                                 
         JNZ   CLLAIMX                                                          
*                                                                               
CLLAIM30 CLI   CH_FSTAT,FF         Apply status filter                          
         JE    CLLAIM40               Anything goes                             
                                                                                
         LA    R1,CH_FSTAT                                                      
         ICM   RF,1,0(R1)                                                       
         JNZ   CLLAIM35                                                         
         OC    EXNPSTAT,EXNPSTAT                                                
         JZ    CLLAIM40                                                         
         J     CLLAIM37                                                         
CLLAIM35 ICM   RF,1,0(R1)                                                       
         JZ    CLLAIMX             Status doesn't match                         
         BASR  RE,0                prog note - this style req when              
         TM    EXNPSTAT,0                           jumping                     
         EX    RF,0(RE)                                                         
         JNZ   CLLAIM40                                                         
CLLAIM37 LA    R1,1(R1)                                                         
         J     CLLAIM35                                                         
                                                                                
CLLAIM40 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         L     R2,AIO2             read rec for office and 1R acc               
         USING EXCRECD,R2                                                       
         CLC   EXCKDATE,CH_STADC   ensure within date range                     
         JH    CLLAIMX                                                          
         CLC   EXCKDATE,CH_ENDDC                                                
         JL    CLLAIMX                                                          
*                                                                               
         CLI   LP_ACCS,0           Skip if no limit access                      
         JE    CLLAIM50                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   OFFAOFFC(0),EXCK1RAC  Move in office and validate                
         EX    RE,0(RF)              &&&                                        
                                                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   CLLAIMX                                                          
         DROP  R1                                                               
*                                                                               
CLLAIM50 CLI   QS_OVAPP,YESQ       OVERRIDING LIMLST/APPROVER SETTINGS?         
         JE    CLLAIM90                                                         
         MVI   QL_CLVBY,QL_CLVSQ   Set status view for approvers                
         XC    MYBYTE,MYBYTE                                                    
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
         JE    CLLAIM55                                                         
*                                                                               
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLAPPR',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
         JE    CLLAIM55                                                         
*                                  set to read back up approvers                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLBKAP',SPACES),('GAPLTACC',GAPLPARM),('QEXPCLM',0)          
         JE    CLLAIM55                                                         
         MVC   LP_ERROR,FULL2      I don't have any access                      
         J     CLLAIMX             &&& SET CONDC?                               
*                                                                               
* Check 1R account against GAPLST table                                         
* Access at account level?                                                      
CLLAIM55 MVC   WORK(L'EXCK1RAC),EXCK1RAC                                        
         LLC   R3,ONERL3L          R3 is NEXT level to check                    
         LA    R4,GAPAREA                                                       
         USING GAPTABD,R4                                                       
CLLAIM57 XC    GAPAREA,GAPAREA                                                  
         MVI   GAPTDAT1,GAPTT1Q                                                 
         MVC   GAPTACT,WORK                                                     
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLAIM60                                                         
         CLC   GAPTACT,WORK                                                     
         JNE   CLLAIM60                                                         
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   CLLAIM60                                                         
         TM    GAPTSTA,GAPTSEQ                                                  
         JO    CLLAIM65            specifically excluded                        
         J     CLLAIM90            match, process                               
* Clear last level checked and set next level (r3)                              
                                                                                
CLLAIM60 LR    RF,R3                                                            
         LA    RE,WORK       Allow for l'(office+dep+subdep+perkcode)           
         AR    RE,R3                                                            
         MVC   0(L'PERKCODE,RE),SPACES    clear lowest level                    
*                                                                               
         LLC   R3,ONERL2L    set r3 one level below last                        
         CR    RF,R3                                                            
         JH    CLLAIM57                                                         
         LLC   R3,ONERL1L                                                       
         CR    RF,R3                                                            
         JH    CLLAIM57                                                         
         SR    R3,R3                                                            
         CR    RF,R3                                                            
         JH    CLLAIM57                                                         
         J     CLLAIM65            No match at any level                        
*                                                                               
* Check SJ account (NO MEDIA) against GAPLST table                              
* Must have 1 item for a job I have access to                                   
                                                                                
CLLAIM65 LA    R2,EXCRFST                                                       
         USING CIDELD,R2                                                        
CLLAIM66 CLI   CIDEL,0                                                          
         JE    CLLAIM85            no match on this claim record                
         CLI   CIDEL,CIDELQ                                                     
         JNE   CLLAIM68                                                         
         CLI   CIDTYPE,CIDTYCQ                                                  
         JNE   CLLAIM68                                                         
         CLC   CIDCCPJ,SPACES                                                   
         JH    CLLAIM70                                                         
CLLAIM68 LLC   RF,CIDLN                                                         
         AR    R2,RF                                                            
         J     CLLAIM66                                                         
* Access at account level?                                                      
CLLAIM70 MVC   WORK(L'CIDCCPJ),CIDCCPJ                                          
         LLC   R3,PPROLEN          R3 is NEXT level to check                    
         LA    R4,GAPAREA                                                       
         USING GAPTABD,R4                                                       
CLLAIM71 XC    GAPAREA,GAPAREA                                                  
         MVI   GAPTDAT1,GAPTT2Q                                                 
         MVC   GAPTCOFF,CIDCOFF                                                 
         MVC   GAPTACC,WORK                                                     
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLAIM75                                                         
         CLC   GAPTCOFF,CIDCOFF                                                 
         JNE   CLLAIM75                                                         
         CLC   GAPTACC,WORK                                                     
         JNE   CLLAIM75                                                         
         CLI   GAPTDAT1,GAPTT2Q                                                 
         JNE   CLLAIM75                                                         
         TM    GAPTSTA,GAPTSEQ                                                  
         JO    CLLAIM68            specifically excluded                        
         LLC   RF,PPROLEN                                                       
         CR    R3,RF                                                            
         JE    CLLAIM90            job level match, process                     
* For client/product level approval, check for media exceptions                 
CLLAIM72 MVC   WORK(GAPTLNQ),GAPAREA                                            
CLLAIM73 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLAIM90            No exceptions found, MATCH                   
         CLI   GAPTDAT1,GAPTT2Q                                                 
         JNE   CLLAIM90                                                         
         CLC   GAPTCOFF,CIDCOFF                                                 
         JNE   CLLAIM90                                                         
         CLC   GAPTACC,WORK+GAPTACC-GAPTABD                                     
         JNE   CLLAIM90                                                         
         CLC   GAPTCMED,CIDCMED                                                 
         JNE   CLLAIM73                                                         
         TM    GAPTSTA,GAPTSEQ                                                  
         JO    CLLAIM68            specifically excluded                        
         J     CLLAIM73                                                         
* Clear last level checked and set next level (r3)                              
                                                                                
CLLAIM75 LR    RF,R3                                                            
         LA    RE,WORK       Allow for l'(CLI+PRO+SJ ACC-2)                     
         AR    RE,R3                                                            
         MVC   0(L'CIDCCPJ-2,RE),SPACES     clear lowest level                  
*                                                                               
         LLC   R3,PCLILEN    set r3 one level below last                        
         CR    RF,R3                                                            
         JH    CLLAIM71                                                         
         SR    R3,R3                                                            
         CR    RF,R3                                                            
         JH    CLLAIM71                                                         
         J     CLLAIM78            No match at any level                        
*                                                                               
* Check for SJ-with-media approver match                                        
CLLAIM78 MVC   WORK(L'CIDCCPJ),CIDCCPJ                                          
         LLC   R3,PPROLEN          R3 is NEXT level to check                    
         LA    R4,GAPAREA                                                       
         USING GAPTABD,R4                                                       
CLLAIM79 XC    GAPAREA,GAPAREA                                                  
         MVI   GAPTDAT1,GAPTT6Q                                                 
         MVC   GAPTOFF,CIDCOFF                                                  
         MVC   GAPTCPJ,WORK                                                     
         MVC   GAPTMED,CIDCMED                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   X        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLAIM82                                                         
         CLC   GAPTOFF,CIDCOFF                                                  
         JNE   CLLAIM82                                                         
         CLC   GAPTMED,CIDCMED                                                  
         JNE   CLLAIM82                                                         
         CLC   GAPTCPJ,WORK                                                     
         JNE   CLLAIM82                                                         
         CLI   GAPTDAT1,GAPTT6Q                                                 
         JNE   CLLAIM82                                                         
         TM    GAPTSTA,GAPTSEQ                                                  
         JO    CLLAIM68            specifically excluded                        
         J     CLLAIM90            match, process                               
* Clear last level checked and set next level (r3)                              
                                                                                
CLLAIM82 LR    RF,R3                                                            
         LA    RE,WORK       Allow for l'(CLI+PRO+SJ ACC-2)                     
         AR    RE,R3                                                            
         MVC   0(L'CIDCCPJ-2,RE),SPACES     clear lowest level                  
*                                                                               
         LLC   R3,PCLILEN    set r3 one level below last                        
         CR    RF,R3                                                            
         JH    CLLAIM79                                                         
         SR    R3,R3                                                            
         CR    RF,R3                                                            
         JH    CLLAIM79                                                         
         J     CLLAIM68            No match at any level                        
*                                                                               
* No match, more items on sequential claim recs?                                
                                                                                
CLLAIM85 L     R2,AIO2                                                          
         USING EXCRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'EXCKEY),EXCKEY                                           
         LLC   RF,EXCKSEQ                                                       
         LA    RF,1(RF)                                                         
         STC   RF,EXCKSEQ-EXCKEY+IOKEY                                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   CLLAIMX             NO FURTHER RECORDS                           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   CLLAIMX                                                          
         J     CLLAIM65                                                         
*                                                                               
CLLAIM90 L     R2,AIO2             May need to reread main claim rec            
         CLI   EXCKSEQ,0                 for PROCLL                             
         JE    CLLAIM95                                                         
         MVC   IOKEY(L'EXCKEY),EXCKEY                                           
         LA    R2,IOKEY                                                         
         MVI   EXCKSEQ,0                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         MVC   SAVEKEY4,IOKEY                                                   
*                                                                               
CLLAIM95 CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JE    CLLAIM98                                                         
         GOTOR PROCLL                                                           
         J     CLLAIMX                                                          
                                                                                
CLLAIM98 GOTOR PROCLI              Process record                               
                                                                                
CLLAIMX  J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  Get claims according to approvers rights and date                  *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
CLLADV   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CLLADV*'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         LA    R4,GAPAREA          Point to approves table                      
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
                                                                                
CLLADV02 TM    ATSRERRS,TSEEOF                                                  
         JNZ   CLLADVX             Buffer is empty                              
         TM    GAPTSTA,GAPTSMQ     look for main entries only                   
         JZ    CLLADV06                                                         
         CLI   GAPTDAT1,GAPTT1Q    branch to 1R code - line manager             
         JE    CLLADV08                                                         
         CLI   GAPTDAT1,GAPTT9Q    branch to 1R code - finance manager          
         JE    CLLADV08                                                         
         CLI   GAPTDAT1,GAPTT2Q    branch to Media code                         
         JE    CLLADV08                                                         
         CLI   GAPTDAT1,GAPTT6Q    branch to SJ code                            
         JE    CLLADV08                                                         
         J     CLLADV06                                                         
CLLADV04 GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CLLADV06 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CLLADV02                                                         
                                                                                
*                                                                               
CLLADV08 CLI   GAPTDAT1,GAPTT9Q    1R entry for finance approver                
         JE    CLLADV12                                                         
         CLI   GAPTDAT1,GAPTT1Q    1R entry for line approver                   
         JNE   CLLADV50                                                         
                                                                                
         USING EXDPASD,R2                                                       
CLLADV12 XC    EXDPAS,EXDPAS       build start passive for entry                
         MVI   EXDPTYP,EXDPTYPQ                                                 
         MVI   EXDPSUB,EXDPSUBQ                                                 
         MVC   EXDPCPY,CUXCPY                                                   
         MVC   EXDPCAT,GAPTAPPL                                                 
         MVI   EXDPCLS,EXDPCODQ                                                 
         MVC   EXDP1RAC,GAPTACT                                                 
         OC    EXDP1RAC,SPACES                                                  
         MVC   EXDPDATE,CH_ENDDC                                                
                                                                                
         MVC   SAVEKEY1,EXDPAS     save start key and clear hours               
         XC    CH_1RAC,CH_1RAC                                                  
                                                                                
CLLADV14 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLLADV18                                                         
         DC    H'0'                no errors should occur                       
                                                                                
CLLADV16 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    CLLADV18                                                         
         DC    H'0'                no errors should occur                       
                                                                                
CLLADV18 CLC   SAVEKEY1(EXDP1RAC-EXDPAS),EXDPAS                                 
         JNE   CLLADV04            end of this office                           
*                                                                               
         CLI   EXDPSRC,YESQ        Is it for search only                        
         JE    CLLADV16            Yes - get next key                           
         CLI   GAPTLVL,GAPTSL0                                                  
         JE    CLLADV22                                                         
         SR    RE,RE                                                            
         IC    RE,GAPTLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
CLLADV20 CLC   EXDP1RAC(0),GAPTACT                                              
         EX    RE,0(RF)                                                         
         JNE   CLLADV04                                                         
*                                                                               
CLLADV22 TM    EXDPSTAT,EXCSDELT                                                
         JNZ   CLLADV16                                                         
         USING OFFALD,R1                                                        
         CLI   LP_ACCS,0           skip if no limit access                      
         JE    CLLADV23                                                         
         L     R1,AOFFAREA                                                      
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   OFFAOFFC(0),EXDP1RAC                                             
         EX    RF,0(RE)                                                         
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
*        JNE   CLLADV04            THIS LINE BAD FOR GAPTSL0                    
         JE    CLLADV23                                                         
         CLI   GAPTLVL,GAPTSL0                                                  
         JNE   CLLADV04                                                         
         LLC   RF,ONERL1L                                                       
         LA    RE,EXDP1RAC                                                      
         AR    RE,RF                                                            
         MVI   0(RE),X'FF'                                                      
         J     CLLADV14            CHECK NEXT OFFICE                            
         DROP  R1                                                               
*                                                                               
CLLADV23 CLC   CH_1RAC,EXDP1RAC    match on previous                            
         JE    CLLADV24                                                         
         LA    R1,EXDP1RAC                                                      
         MVC   GAPAREA2,GAPAREA                                                 
         GOTOR TT1RVGT             test against table                           
         JE    CLLADV24                                                         
                                                                                
         MVI   EXDP1RAC+L'EXDP1RAC,FF                                           
         J     CLLADV14                                                         
*                                                                               
CLLADV24 MVC   CH_1RAC,EXDP1RAC                                                 
*                                                                               
         CLC   EXDPDATE,CH_ENDDC   date filtering                               
         JNL   CLLADV26                                                         
         MVC   EXDPDATE,CH_ENDDC                                                
         MVI   EXDPNUM,0                                                        
         J     CLLADV14            read high                                    
                                                                                
CLLADV26 CLC   EXDPDATE,CH_STADC                                                
         JNH   CLLADV28                                                         
         MVC   EXDPDATE,=XL2'FFFF'                                              
         J     CLLADV14            read high                                    
                                                                                
CLLADV28 MVC   SAVEKEY4,EXDPAS     save for sequences                           
                                                                                
         XC    EX_BUFF(L'CLM_LEN+CLM_KEYL),EX_BUFF                              
CB       USING CLM_D,EX_BUFF                                                    
         MVC   CB.CLM_NUM,EXDPNUM                                               
         MVI   CB.CLM_TYPE,EXNPTLQ                                              
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    CLLADV16                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JNZ   CLLADV30                                                         
         DC    H'0'                Duplicate key                                
         DROP  CB                                                               
                                                                                
CLLADV30 CLI   EXDPSEQ,0                                                        
         JE    CLLADV32                                                         
         LA    R2,SAVEKEY4                                                      
INI      USING EXNPASD,IOKEY                                                    
         XC    INI.EXNPAS,INI.EXNPAS                                            
         MVI   INI.EXNPTYP,EXNPTYPQ                                             
         MVI   INI.EXNPSUB,EXNPSUBQ                                             
         MVC   INI.EXNPCPY,CUXCPY                                               
         MVC   INI.EXNPNUM,EXDPNUM                                              
         MVI   INI.EXNPTYPE,EXNPTLQ                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLADV32                                                         
         DC    H'0'                                                             
                                                                                
CLLADV32 GOTOR PROCLL              process record                               
         MVC   IOKEY,SAVEKEY4      reestablish key                              
         LLC   RF,IOKEY+EXDPSEQ-EXDPAS                                          
         AHI   RF,1                                                             
         STC   RF,IOKEY+EXDPSEQ-EXDPAS                                          
         LA    R2,IOKEY                                                         
         J     CLLADV14                                                         
         DROP  R2                                                               
                                                                                
         USING EXJPASD,R2                                                       
CLLADV50 XC    EXJPAS,EXJPAS     clear key                                      
         XC    CSVKEY3,CSVKEY3                                                  
         MVI   EXJPTYP,EXJPTYPQ  build start key                                
         MVI   EXJPSUB,EXJPSUBQ                                                 
         MVC   EXJPCPY,CUXCPY                                                   
         MVC   EXJPVIEW,GAPTAPPL                                                
         MVC   EXJPCODE,GAPTCODE pass client from table                         
         MVC   EXJPDATE,CH_ENDDC                                                
         MVC   CSVKEY1,EXJPAS    save start key                                 
*                                                                               
CLLADV54 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CLLADV58                                                         
         DC    H'0'                no errors should occur                       
*                                                                               
CLLADV56 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    CLLADV58                                                         
         DC    H'0'                no errors should occur                       
*                                                                               
CLLADV58 CLC   CSVKEY1(EXJPCODE-EXJPAS),EXJPAS                                  
         JNE   CLLADV04            end of this office                           
*                                                                               
         CLI   EXJPSRC,YESQ        Is passive for search only                   
         JE    CLLADV56            Yes - get next record                        
         SR    RE,RE                                                            
         IC    RE,GAPTLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   EXJPCODE(0),GAPTACT                                              
         EX    RE,0(RF)                                                         
         JNE   CLLADV04                                                         
*                                                                               
CLLADV60 TM    EXJPSTAT,EXCSDELT                                                
         JNZ   CLLADV56                                                         
         CLI   LP_ACCS,0           skip if no limit access                      
         JE    CLLADV62                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EXJPCOFF                                                
         CLI   GAPTDAT1,GAPTT6Q                                                 
         JNE   *+10                                                             
         MVC   OFFAOFFC,EXJPMOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   CLLADV04            Not valid                                    
         DROP  R1                                                               
CLLADV62 MVC   GAPAREA2,GAPAREA                                                 
         LA    R1,EXJPCODE         test whether Cli/pro/job wanted              
         GOTOR TTSJVGT                                                          
         JE    CLLADV84                                                         
         MVI   EXJPCODE+L'EXJPCODE,FF    read high for next SJ a/c              
         J     CLLADV54                                                         
*                                                                               
CLLADV84 CLC   EXJPDATE,CH_ENDDC   date filtering                               
         JNL   CLLADV86                                                         
         MVC   EXJPDATE,CH_ENDDC                                                
         MVI   EXJPNUM,0                                                        
         J     CLLADV54            read high                                    
                                                                                
CLLADV86 CLC   EXJPDATE,CH_STADC                                                
         JNH   CLLADV88                                                         
         MVC   EXJPDATE,=XL2'FFFF'                                              
         J     CLLADV54            read high                                    
                                                                                
CLLADV88 MVC   SAVEKEY4,EXJPAS     save for sequences                           
                                                                                
         XC    EX_BUFF(L'CLM_LEN+CLM_KEYL),EX_BUFF                              
CB       USING CLM_D,EX_BUFF                                                    
         MVC   CB.CLM_NUM,EXJPNUM                                               
         MVI   CB.CLM_TYPE,EXNPTLQ                                              
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
         JE    CLLADV56                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JNZ   CLLADV90                                                         
         DC    H'0'                Duplicate key                                
         DROP  CB                                                               
                                                                                
CLLADV90 CLI   EXJPSEQ,0                                                        
         JE    CLLADV92                                                         
         LA    R2,SAVEKEY4                                                      
INI      USING EXNPASD,IOKEY                                                    
         XC    INI.EXNPAS,INI.EXNPAS                                            
         MVI   INI.EXNPTYP,EXNPTYPQ                                             
         MVI   INI.EXNPSUB,EXNPSUBQ                                             
         MVC   INI.EXNPCPY,CUXCPY                                               
         MVC   INI.EXNPNUM,EXJPNUM                                              
         MVI   INI.EXNPTYPE,EXNPTLQ                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    CLLADV92                                                         
         DC    H'0'                                                             
         DROP  INI,R2,R4                                                        
                                                                                
CLLADV92 GOTOR PROCLL              process record                               
         MVC   IOKEY,SAVEKEY4      reestablish key                              
         LLC   RF,IOKEY+EXDPSEQ-EXDPAS                                          
         AHI   RF,1                                                             
         STC   RF,IOKEY+EXDPSEQ-EXDPAS                                          
         LA    R2,IOKEY                                                         
         J     CLLADV54                                                         
                                                                                
CLLADVX  XIT1                                                                   
                                                                                
         EJECT                                                                  
***********************************************************************         
* Test current 1R account versus GAPTAB entries                       *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TT1RVGT  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*TT1RVGT'                                                      
         LR    R3,R1               point to 1R account code                     
P        USING ACTKACT,R3                                                       
                                                                                
         LA    R4,GAPAREA2         point to main/office entry                   
         OC    GAPAREA2,GAPAREA2                                                
         JZ    TT1RV50                                                          
                                                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         LA    R4,GAPAREA2         point to main/office entry                   
TT1RV08  LLC   RF,GAPTLEN                                                       
         AR    RF,R3                                                            
         CLI   0(RF),C' '          Is the account any longer than entry         
         JE    TT1RVGLY            No - so keep account                         
                                                                                
TT1RV10  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     End of buffer                                
         JNZ   TT1RVGLY            Yes                                          
         TM    GAPTSTA,GAPTSMQ     next group/office                            
         JO    TT1RVGLY                                                         
                                                                                
TT1RV12  LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTACT(0),P.ACTKACT                                             
         EX    RF,0(RE)                                                         
         JNE   TT1RV10                                                          
         J     TT1RVGLN                                                         
*                                  MAIN ENTRY NOT SUPPLIED                      
TT1RV50  MVI   GAPTDAT1,GAPTT1Q                                                 
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JE    *+12                                                             
         CLI   QS_CSVIE,QS_CSVFQ   Finance view                                 
         JNE   TT1RV52             Yes                                          
         MVI   GAPTDAT1,GAPTT9Q    Set finance 1R                               
*                                                                               
TT1RV52  LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   GAPTCODE(0),P.ACTKACT                                            
         EX    RF,0(RE)                                                         
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         J     TT1RV60                                                          
*                                                                               
TT1RV59  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
*                                                                               
TT1RV60  TM    ATSRERRS,TSEEOF     LOOK FOR A MAIN ENTRY MATCH                  
         JNZ   TT1RVGLN                                                         
         CLI   QS_CSVIE,QS_CSVFQ   Finance view                                 
         JE    *+12                Yes                                          
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view                          
         JNE   TT1RV62             Yes                                          
         CLI   GAPTDAT1,GAPTT9Q    Check finance 1R                             
         JNE   TT1RVGLN                                                         
         J     TT1RV64                                                          
*                                                                               
TT1RV62  CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   TT1RVGLN                                                         
*                                                                               
TT1RV64  LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTCODE(0),P.ACTKACT                                            
         EX    RF,0(RE)                                                         
         JNE   TT1RV59                                                          
         TM    GAPTSTA,GAPTSMQ                                                  
         JZ    TT1RV59                                                          
*                                                                               
TT1RV70  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF            AND THEN CHECK FOR EXCEPTIONS                
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   TT1RVGLY                                                         
         CLI   QS_CSVIE,QS_CSVFQ   Finance view                                 
         JE    *+12                                                             
         CLI   QS_CSVIE,QS_CSVRQ   Finance report view?                         
         JNE   TT1RV72             Yes                                          
         CLI   GAPTDAT1,GAPTT9Q    Check finance 1R                             
         JNE   TT1RVGLY                                                         
         J     TT1RV74                                                          
*                                                                               
TT1RV72  CLI   GAPTDAT1,GAPTT1Q    1r acc type                                  
         JNE   TT1RVGLY                                                         
*                                                                               
TT1RV74  TM    GAPTSTA,GAPTSMQ     NEXT MAIN ENTRY, WE'RE OK                    
         JNZ   TT1RVGLY                                                         
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTCODE(0),P.ACTKACT                                            
         EX    RF,0(RE)                                                         
         JNE   TT1RV70             SOME OTHER EXCEPTION, CARRY ON               
         J     TT1RVGLN                                                         
*                                                                               
TT1RVGLY J     EXITY                                                            
                                                                                
TT1RVGLN J     EXITN                                                            
         DROP  P,R4                                                             
***********************************************************************         
* Test current SJ account and production office versus GAPTAB entries *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TTSJVGT  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*TTTSJVG'                                                      
         LR    R3,R1               point to SJ account code                     
                                                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
P        USING EXSPCODE,R3                                                      
         LA    R4,GAPAREA2         point to client entry                        
         CLI   GAPTLVL,GAPTSL4     Is table entry lowest level                  
         JE    TTSJVGLY            Yes - accept key                             
                                                                                
         CLI   GAPTLVL,GAPTSL9     Is table entry lowest level                  
         JE    TTSJVGLY            Yes - accept key                             
                                                                                
         XR    RE,RE                                                            
         IC    RE,GAPTLEN                                                       
         AR    RE,R3                                                            
         CLI   0(RE),C' '          Is the code any longer                       
         JE    TTSJVGLY            No - accept key                              
                                                                                
TTSJVG06 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     End of buffer                                
         JNZ   TTSJVGLY                                                         
         TM    GAPTSTA,GAPTSMQ                                                  
         JNZ   TTSJVGLY                                                         
         CLI   GAPTDAT1,GAPTT6Q    Is code by media office client               
         JE    TTSJVG08            Yes                                          
*                                  Look for exceptions                          
         CLI   GAPTLVL,GAPTSL5     Do we have media code                        
         JL    TTSJVG08            No                                           
                                                                                
         CLC   P.EXSPMED,GAPTCMED                                               
         JNE   TTSJVG06                                                         
                                                                                
TTSJVG08 LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTCODE(0),P.EXSPCODE                                           
         EX    RF,0(RE)                                                         
         JNE   TTSJVG06                                                         
         J     TTSJVGLN                                                         
                                                                                
TTSJVGLY CR    RB,RB                                                            
         J     TTSJVGLX                                                         
                                                                                
TTSJVGLN LTR   RB,RB                                                            
                                                                                
TTSJVGLX XIT1                                                                   
         DROP  P,R4                                                             
                                                                                
***********************************************************************         
* Process comments from status change element                         *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R4                                                        
PROCCMT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*PROCCMT'                                                      
         XC    BYTE1,BYTE1                                                      
         XC    BYTE2,BYTE2                                                      
         MVI   CA_NEW,NOQ                                                       
         UNPK  DUB2,STCTIME                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   CA_TIME,DUB2+2                                                   
         CLI   STCIND,STCIEXP2       Is it new or old style                     
         JNE   PROCOM22                                                         
         MVI   BYTE1,2               Set new expense stcel                      
         MVI   CA_NEW,YESQ                                                      
         CLI   STCXTYP,STCXITAM      Test status change is item amend           
         JNE   EXITY                                                            
                                                                                
         LA    R1,COMTTAB                                                       
         MVI   CA_COMMT,C' '         If item amend put out comments             
         MVC   CA_COMMT+1(L'CA_COMMT-1),CA_COMMT                                
         LA    R3,CA_COMMT           to say what fields have been               
         USING COMTTABD,R1           amended - all fields are displayed         
PROCOM02 CLI   0(R1),X'FF'                                                      
         JE    EXITY                                                            
         OC    COMTSTA1,COMTSTA1                                                
         JZ    PROCOM04                                                         
         MVC   BYTE3,COMTSTA1                                                   
         NC    BYTE3,STCXSTAT                                                   
         JNZ   PROCOM10                                                         
                                                                                
PROCOM04 OC    COMTSTA2,COMTSTA2                                                
         JZ    PROCOM06                                                         
         MVC   BYTE3,COMTSTA2                                                   
         NC    BYTE3,STCXSTA2                                                   
         JNZ   PROCOM10                                                         
                                                                                
PROCOM06 OC    COMTSTA3,COMTSTA3                                                
         JZ    PROCOM08                                                         
         MVC   BYTE3,COMTSTA3                                                   
         NC    BYTE3,STCXSTA3                                                   
         JNZ   PROCOM10                                                         
                                                                                
PROCOM08 OC    COMTSTA4,COMTSTA4                                                
         JZ    PROCOM09                                                         
         MVC   BYTE3,COMTSTA4                                                   
         NC    BYTE3,STCXSTA4                                                   
         JNZ   PROCOM10                                                         
                                                                                
PROCOM09 OC    COMTSTA5,COMTSTA5                                                
         JZ    PROCOM16                                                         
         MVC   BYTE3,COMTSTA5                                                   
         NC    BYTE3,STCXSTA5                                                   
         JZ    PROCOM16                                                         
                                                                                
PROCOM10 MVC   LAREADDR,COMTDIC                                                 
         EX    0,LARE                                                           
                                                                                
         LA    R0,CA_COMMT         test 1st time through                        
         CR    R3,R0                                                            
         JE    *+8                                                              
         MVI   0(R3),C','                                                       
         LA    RF,L'AC@EXPA-1(RE)  find net l'literal                           
         LA    R0,L'AC@EXPA-1                                                   
         CLI   0(RF),C' '                                                       
         JH    *+12                                                             
         AHI   RF,-1                                                            
         JCT   R0,*-12                                                          
         LR    R2,R0                                                            
         LA    R2,1(R2,R3)         R2=space needed                              
         LA    RF,CA_COMMT+L'CA_COMMT-1                                         
         CR    R2,RF               test it will fit                             
         JNL   EXITY               No                                           
         LR    RF,R0                                                            
         MVC   1(0,R3),0(RE)                                                    
         EX    RF,*-6                                                           
         LA    R3,2(RF,R3)         bump to next                                 
PROCOM16 LA    R1,COMTTABL(R1)                                                  
         J     PROCOM02                                                         
                                                                                
PROCOM22 CLI   STCCOMM,C'T'          old style - edit out type change           
         JNE   PROCOM24                                                         
         MVC   CA_TYP,STCCOMM                                                   
         MVC   CA_ITM,STCCOMM+1                                                 
         MVC   CA_VAL(1),STCCOMM+4                                              
         MVI   BYTE1,1                                                          
         J     EXITY                                                            
                                                                                
PROCOM24 CLI   STCCOMM,C'J'          edit out SJ account change                 
         JNE   PROCOM26                                                         
         MVC   CA_TYP,STCCOMM                                                   
         MVC   CA_ITM,STCCOMM+1                                                 
         MVC   CA_VAL(14),STCCOMM+4                                             
         MVI   BYTE1,1                                                          
         J     EXITY                                                            
                                                                                
PROCOM26 CLI   STCCOMM,C'E'          edit out Exp. type change                  
         JNE   PROCOM28                                                         
         MVC   CA_TYP,STCCOMM                                                   
         MVC   CA_ITM,STCCOMM+1                                                 
         MVC   CA_VAL(3),STCCOMM+4                                              
         MVI   BYTE1,1                                                          
         J     EXITY                                                            
                                                                                
PROCOM28 CLI   STCCOMM,C'S'           edit out Supplier change                  
         JNE   PROCOM30                                                         
         MVC   CA_TYP,STCCOMM                                                   
         MVC   CA_ITM,STCCOMM+1                                                 
         MVC   CA_VAL(14),STCCOMM+4                                             
         MVI   BYTE1,1                                                          
         J     EXITY                                                            
                                                                                
PROCOM30 CLI   STCCOMM,C'O'           edit out Office change                    
         JNE   EXITY                                                            
         MVC   CA_TYP,STCCOMM                                                   
         MVC   CA_ITM,STCCOMM+1                                                 
         MVC   CA_VAL(2),STCCOMM+4                                              
         MVI   BYTE1,1                                                          
         J     EXITY                                                            
         DROP  R1,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
* Process distance bands and distance rate records                    *         
***********************************************************************         
         SPACE 1                                                                
PRODRT   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*PRODRT*'                                                      
                                                                                
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
                                                                                
         GOTOR PERDTL,DMCB,QM_PERS,QM_CDATE                                     
         JE    PRODRT02                                                         
         MVC   LP_ERROR,=AL2(AE$PERDT)                                          
         J     EXITN                                                            
                                                                                
DS       USING DIS_D,EX_BUFF                                                    
PRODRT02 LA    R0,EX_BUFF                                                       
         LHI   R1,L'EX_BUFF                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                  Set amount so far here in case no            
         ZAP   DS.DIS_DISB,QM_DISAM distance bands in use                       
         OC    QM_ACUNM,QM_ACUNM   Can't have distance bands if no              
         JZ    PRODRT21            accumulator                                  
DTN      USING DTNRECD,IOKEY                                                    
         XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,CH_OFF                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    PRODRT04                                                         
                                                                                
         XC    DTN.DTNKEY,DTN.DTNKEY                                            
         MVI   DTN.DTNKTYP,DTNKTYPQ                                             
         MVI   DTN.DTNKSUB,DTNKSUBQ                                             
         MVC   DTN.DTNKCPY,CUXCPY                                               
         MVC   DTN.DTNKOFF,SPACES                                               
         MVC   DTN.DTNKDATE,QM_CDATE                                            
         XC    DTN.DTNKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   DTN.DTNKEY(DTNKDATE-DTNKEY),IOKEYSAV                             
         JE    PRODRT04                                                         
         MVC   LP_ERROR,=AL2(AE$MDRTE)                                          
         J     EXITN                                                            
                                                                                
PRODRT04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         ZAP   CM_ODIST,PZERO                                                   
         LA    R6,QM_DSPAC                                                      
         OC    QM_DSPAC,QM_DSPAC                                                
         JNZ   PRODRT08                                                         
         XR    RF,RF                                                            
         ICM   RF,1,QM_ACUNM                                                    
         JZ    PRODRT12                                                         
         SHI   RF,1                                                             
         LA    R6,CV_DIST                                                       
         LTR   RF,RF                                                            
         JZ    PRODRT08                                                         
         MHI   RF,L'CV_DISAC                                                    
         AR    R6,RF                                                            
PRODRT08 ZAP   CM_ODIST,0(L'CV_DISAC,R6)                                        
                                                                                
PRODRT12 AP    CM_ODIST,QM_DISAC   Total distance to date                       
                                                                                
         L     R2,AIO4                                                          
         AHI   R2,DTNRFST-DTNRECD                                               
         USING FFTELD,R2                                                        
PRODRT14 CLI   FFTEL,0             EOR                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
PRODRT15 CLI   FFTEL,FFTELQ                                                     
         JE    PRODRT18                                                         
PRODRT16 LLC   RF,FFTLN                                                         
         AR    R2,RF                                                            
         J     PRODRT14                                                         
                                                                                
PRODRT18 CLI   FFTTYPE,FFTTDIS                                                  
         JNE   PRODRT16                                                         
         ZAP   CM_BNDAM,FFTDISA                                                 
         CP    CM_BNDAM,CM_ODIST   Does band exceed total distance              
         JNH   PRODRT16            No - get next distance band                  
         MVC   DS.DIS_DBDC,FFTDISC Extract distance band code                   
         ZAP   DS.DIS_DBDA,FFTDISA Extract distance band amount                 
         LLC   R1,FFTLN                                                         
         SHI   R1,FFTVLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   DS.DIS_DBDN(0),FFTDISN                                           
         EX    R1,0(RE)                                                         
                                                                                
         SP    CM_BNDAM,CM_ODIST   What is the remaining distance               
         ZAP   DUB,QM_DISAM                                                     
         SP    QM_DISAM,CM_BNDAM   Actual distance                              
         CP    QM_DISAM,PZERO      Does band exceed distance entered            
         JL    PRODRT20            Yes                                          
         ZAP   DS.DIS_DISB,CM_BNDAM No                                          
         AP    CM_ODIST,CM_BNDAM   Add the remaining of band to total           
         J     PRODRT21                                                         
                                                                                
PRODRT20 ZAP   DS.DIS_DISB,DUB                                                  
*                                                                               
DRT      USING DRTRECD,IOKEY                                                    
PRODRT21 XC    DRT.DRTKEY,DRT.DRTKEY                                            
         MVI   DRT.DRTKTYP,DRTKTYPQ                                             
         MVI   DRT.DRTKSUB,DRTKSUBQ                                             
         MVC   DRT.DRTKCPY,CUXCPY                                               
         MVC   DRT.DRTKOFF,CH_OFF                                               
         MVC   DRT.DRTKDATE,QM_CDATE                                            
         XC    DRT.DRTKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   DRT.DRTKEY(DRTKDATE-DRTKEY),IOKEYSAV                             
         JE    PRODRT24                                                         
                                                                                
         XC    DRT.DRTKEY,DRT.DRTKEY                                            
         MVI   DRT.DRTKTYP,DRTKTYPQ                                             
         MVI   DRT.DRTKSUB,DRTKSUBQ                                             
         MVC   DRT.DRTKCPY,CUXCPY                                               
         MVC   DRT.DRTKOFF,SPACES                                               
         MVC   DRT.DRTKDATE,QM_CDATE                                            
         XC    DRT.DRTKDATE,EFFS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    PRODRT23                                                         
         DC    H'0'                                                             
                                                                                
PRODRT22 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PRODRT23 CLC   DRT.DRTKEY(DRTKDATE-DRTKEY),IOKEYSAV                             
         JE    PRODRT24                                                         
         MVC   LP_ERROR,=AL2(AE$MDRTE)                                          
         J     EXITN                                                            
                                                                                
PRODRT24 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R3,AIO3                                                          
         AHI   R3,DRTRFST-DRTRECD                                               
         USING LIDELD,R3                                                        
PRODRT26 CLI   LIDEL,0             EOR                                          
         JE    PRODRT22                                                         
         CLI   LIDEL,LIDELQ        List element                                 
         JE    PRODRT30                                                         
PRODRT28 LLC   RF,LIDLN            Increment to next element                    
         AR    R3,RF                                                            
         J     PRODRT26                                                         
                                                                                
PRODRT30 CLI   LIDTYPE,LIDTDRTL    Distance rate list element                   
         JNE   PRODRT28            No - get next element                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    R4,LIDDATA          R4=A(entries within list element)            
         IC    RF,LIDLN            Total length of element                      
         SHI   RF,LIDLNDQ          RF=Length of entries in list                 
         DROP  R3                                                               
         USING LIDDATA,R4                                                       
         LHI   R1,LIDRLNQ          R1=length of each entry in list              
         DR    RE,R1               RF=number of entries in list                 
                                                                                
PRODRT32 CLC   QM_VEHCD,LIDRVEHT   Does the vehicle type match                  
         JNE   PRODRT34            No - get next entry from list                
         LA    R1,BHQ                                                           
         CLC   LIDRFUET,SPACES                                                  
         JNH   *+8                                                              
         LA    R1,BNHQ                                                          
         CLC   QM_FUECD,SPACES     Any fuel code?                               
         NOP   PRODRT34                                                         
         EX    R1,*-4                                                           
                                                                                
         OC    QM_FUECD,QM_FUECD   Any fuel                                     
         JZ    *+14                No - then skip test                          
         CLC   QM_FUECD,LIDRFUET   Does the fuel type match                     
         JNE   PRODRT34            No                                           
                                                                                
         LA    R1,BHQ                                                           
         CLC   LIDRENGS,SPACES                                                  
         JNH   *+8                                                              
         LA    R1,BNHQ                                                          
         CLC   QM_ENGCD,SPACES     Any engine code?                             
         NOP   PRODRT34                                                         
         EX    R1,*-4                                                           
                                                                                
         OC    QM_ENGCD,QM_ENGCD   Any engine                                   
         JZ    *+14                No - then skip test                          
         CLC   QM_ENGCD,LIDRENGS   Does the engine code match                   
         JNE   PRODRT34            No                                           
                                                                                
         LA    R1,BNZQ                                                          
         CLC   LIDRDISB,SPACES     Any distance band                            
         JNH   *+8                                                              
         LA    R1,BZQ              Yes                                          
         OC    QM_ACUNM,QM_ACUNM   Any accumulator                              
         NOP   PRODRT34                                                         
         EX    R1,*-4                                                           
                                                                                
         OC    QM_ACUNM,QM_ACUNM   Can't have distance bands if no              
         JZ    PRODRT36            accumulator                                  
         CLC   LIDRDISB,DS.DIS_DBDC  Does distance band match                   
         JE    PRODRT36            Yes                                          
PRODRT34 AHI   R4,LIDRLNQ          No - look to next entry in list              
         JCT   RF,PRODRT32         Any more entries in list element             
         J     PRODRT28            No - get next element                        
                                                                                
PRODRT36 ZAP   DS.DIS_GRSR,LIDRGRSR                                             
*&&UK*&& ZAP   DS.DIS_VATR,LIDRVATR                                             
*&&UK*&& ZAP   DS.DIS_PASR,LIDRPASR                                             
         ZAP   PACK16,DS.DIS_GRSR                                               
         MP    PACK16,DS.DIS_DISB                                               
*&&US*&& SRP   PACK16,64-3,5       US as mileage held to 2dp and rates          
         ZAP   DUB,PACK16                   to 3dp                              
*&&UK                                                                           
         XR    RE,RE                                                            
         IC    RE,QM_PASS                                                       
         CVD   RE,DUB1                                                          
         ZAP   PACK16,DUB1                                                      
         MP    PACK16,DS.DIS_PASR                                               
         MP    PACK16,DS.DIS_DISB                                               
         ZAP   DUB1,PACK16                                                      
         AP    DUB,DUB1                                                         
*&&                                                                             
         ZAP   DS.DIS_TOTA,DUB                                                  
*&&UK                                                                           
         ZAP   PACK16,DS.DIS_VATR                                               
         MP    PACK16,DS.DIS_DISB                                               
         ZAP   DUB,PACK16                                                       
         ZAP   DS.DIS_VATA,DUB                                                  
*&&                                                                             
         AP    CM_TOTAM,DS.DIS_TOTA  Add up total amount of claim               
*&&UK                                                                           
         AP    CM_NVATA,DS.DIS_TOTA  Work out total non vatable amount          
         SP    CM_NVATA,DS.DIS_VATA                                             
*&&                                                                             
         LA    R4,DIS_LNQ                                                       
         STH   R4,DS.DIS_LEN                                                    
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JNE   PRODRT38                                                         
         OC    QM_ACUNM,QM_ACUNM   Can't have distance bands if no              
         JZ    EXITY               accumulator                                  
         CP    QM_DISAM,PZERO      Have we got distance left to rate            
         JH    PRODRT16                                                         
         J     EXITY                                                            
PRODRT38 TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   PRODRT40                                                         
         DC    H'0'                Duplicate key                                
PRODRT40 MVC   LP_ERROR,=AL2(AE$MAX#)                                           
         J     EXITH                                                            
         DROP  DS,R2,RB                                                         
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* Key Filter For XDFRECD                                              *         
***********************************************************************         
                                                                                
FLTXDF   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*FLTXDF*'                                                      
         OC    CI_CLI,SPACES                                                    
         LA    R2,IOKEY                                                         
         USING XDFRECD,R2                                                       
         OC    XDFKOFF,XDFKOFF                                                  
         JNZ   FLTXDF02                                                         
         OC    XDFKCLI(XDFKSEQ-XDFKCLI),XDFKCLI                                 
         JNZ   FLTXDF02                                                         
         CLI   XDFKORTY,XDFKDFT    Always show it if it is top level            
         JE    EXITY                                                            
         CLI   XDFKORTY,XDFKTYEX   or top application level                     
         JE    EXITY                                                            
         J     EXITN               Otherwise - get next record                  
*                                                                               
FLTXDF02 CLI   XDFKORTY,XDFKDFT    Default order type ?                         
         JE    *+12                                                             
         CLI   XDFKORTY,XDFKTYEX                                                
         JNE   EXITN               No - get the next one                        
                                                                                
         OC    XDFKOFF,XDFKOFF                                                  
         JZ    *+14                                                             
         CLC   XDFKOFF,CI_OFF                                                   
         JNE   EXITN                                                            
         OC    XDFKCLI,XDFKCLI                                                  
         JZ    *+14                                                             
         CLC   XDFKCLI,CI_CLI                                                   
         JNE   EXITN                                                            
         OC    XDFKETY,XDFKETY                                                  
         JZ    *+14                                                             
         CLC   XDFKETY,CI_ETYP                                                  
         JNE   EXITN                                                            
         OC    XDFKWC,XDFKWC                                                    
         JNZ   EXITN                                                            
FLTXDF10 OC    XDFKMED,XDFKMED                                                  
         JZ    *+14                                                             
         CLC   XDFKMED,CI_MED                                                   
         JNE   EXITN               Get next XData record                        
         OC    XDFKSCH,XDFKSCH                                                  
         JNZ   EXITN                                                            
         J     EXITY                                                            
***********************************************************************         
* Filter office code based on the passed office code                            
***********************************************************************         
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
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
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
APPRVDQ  EQU   C'A'                APPROVED                                     
REJECTDQ EQU   C'R'                REJECTED                                     
AWAITAPQ EQU   C'M'                AWAITING APPROVAL                            
NOTREQDQ EQU   C'N'                NOT REQUIRED                                 
NOTAPPLQ EQU   C'*'                NOT APPLICABLE                               
BOTHQ    EQU   C'B'                BOTH                                         
                                                                                
GAPSMAX  EQU   5000                Max number of entries                        
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
EXPDDL#  DC    AL2(A#XDIS)         EXPENSE DISPLAY DOWNLOAD                     
         DC    AL1(MAPXDIS)                                                     
         DC    AL1(0)                                                           
                                                                                
EXPLDL#  DC    AL2(A#CLLD)         EXPENSE LIST DOWNLOAD                        
         DC    AL1(MAPXLIS)                                                     
         DC    AL1(0)                                                           
                                                                                
EXPSDL#  DC    AL2(A#CLSD)         EXPENSE SEARCH DOWNLOAD                      
         DC    AL1(MAPXSRC)                                                     
         DC    AL1(0)                                                           
                                                                                
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
STATAB   DS    0X                        Claim status table                     
         DC    C'V',AL1(0,NOTAPPLQ,NOTAPPLQ) In progress - draft claims         
         DC    C'S',AL1(EXCSSUBM,AWAITAPQ,NOTAPPLQ) Awaiting approval           
         DC    C'F',AL1(EXCSFNTA,APPRVDQ,AWAITAPQ)  Await fin or apprd          
         DC    C'R',AL1(EXCSREJE,REJECTDQ,REJECTDQ) Rejected                    
         DC    C'C',AL1(EXCSCOMP,APPRVDQ,APPRVDQ)   Posted                      
         DC    C'P',AL1(EXCSPAPP,AWAITAPQ,NOTAPPLQ) Part approved               
         DC    C'D',AL1(EXCSLOGD,NOTAPPLQ,NOTAPPLQ) Deleted                     
         DC    X'00'                                                            
                                                                                
DSPTAB   DS    0X                                                               
         DC    AL1(PIDKTYPQ,PIDKSUBQ,PIDKXSTA-PIDRECD)                          
         DC    AL1(EXSPTYPQ,EXSPSUBQ,EXSPKYST-EXSPASD)                          
         DC    AL1(EXDPTYPQ,EXDPSUBQ,EXDPKSTA-EXDPASD)                          
         DC    AL1(EXJPTYPQ,EXJPSUBQ,EXJPKSTA-EXJPASD)                          
         DC    X'00'                                                            
                                                                                
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
         DC    B'00000000'         Agency - use 1R approvers                    
         DC    X'FF'                                                            
                                                                                
JOBTYPE  DS    0H                                                               
         DC    AL1(JOBPAX1B,JOBPAX1N)  Level 1 approvers                        
         DC    AL1(JOBPAX2B,JOBPAX2N)  Level 2 approvers                        
         DC    X'FF'                                                            
                                                                                
EXPTYPE  DS    0H                                                               
         DC    AL1(DPAPAEX1)           Level 1 approver - non bill              
         DC    AL1(DPAPAEX2)           Level 2 approver - non bill              
         DC    AL1(DPAPAEB1)           Level 1 approver - billable              
         DC    AL1(DPAPAEB2)           Level 2 approver - billable              
         DC    X'FF'                                                            
                                                                                
         DS    0D                                                               
COMTTAB  DS    0C                                                               
         DC    S(AC@EXPA),AL1(STCXXAC,0,0,0,0,0)                                
         DC    S(AC@TYPE),AL1(STCXSTYP,0,0,0,0,0)                               
         DC    S(AC@AMT),AL1(STCXSAMT,0,0,0,0,0)                                
         DC    S(AC@WC),AL1(STCXSWC,0,0,0,0,0)                                  
         DC    S(AC@NRTV),AL1(STCXSNAR,0,0,0,0,0)                               
         DC    S(AC@ORDER),AL1(STCXSORD,0,0,0,0,0)                              
         DC    S(AC@EST),AL1(STCXSEST,0,0,0,0,0)                                
         DC    S(AC@ETYPE),AL1(STCXSETY,0,0,0,0,0)                              
         DC    S(AC@SUP),AL1(0,STCXSSAC,0,0,0,0)                                
         DC    S(AC@VATC1),AL1(0,STCXSVAT,0,0,0,0)                              
         DC    S(AC@PRSNL),AL1(0,STCXS2PA,0,0,0,0)                              
         DC    S(AC@2DDPC),AL1(0,STCXS2DA,0,0,0,0)                              
         DC    S(AC@MILES),AL1(0,STCXSMIL,0,0,0,0)                              
         DC    S(AC@REF),AL1(0,STCXSREF,0,0,0,0)                                
         DC    S(AC@SELED),AL1(0,STCXSSEL,0,0,0,0)                              
         DC    S(AC@RCPTS),AL1(0,STCXSRCP,0,0,0,0)                              
         DC    S(AC@CLIC),AL1(0,0,STCXSCLI,0,0,0)                               
         DC    S(AC@PROC),AL1(0,0,STCXSPRO,0,0,0)                               
         DC    S(AC@JOBC),AL1(0,0,STCXSJOB,0,0,0)                               
         DC    S(AC@DATE),AL1(0,0,STCXSDTE,0,0,0)                               
         DC    S(AC@CURRC),AL1(0,0,STCXSFCC,0,0,0)                              
         DC    S(AC@FCAMT),AL1(0,0,STCXSFAM,0,0,0)                              
         DC    S(AC@INTRF),AL1(0,0,STCXSINT,0,0,0)                              
         DC    S(AC@OFFC),AL1(0,0,STCXSOFF,0,0,0)                               
         DC    S(AC@DISAM),AL1(0,0,0,STCXSDSC,0,0)                              
         DC    S(AC@FNNRC),AL1(0,0,0,STCXSFNC,0,0)                              
         DC    S(AC@FNNRA),AL1(0,0,0,STCXSFNA,0,0)                              
         DC    S(AC@FNNRD),AL1(0,0,0,STCXSFND,0,0)                              
         DC    S(AC@VEHCH),AL1(0,0,0,STCXSVEH,0,0)                              
         DC    S(AC@FUECH),AL1(0,0,0,STCXSFUE,0,0)                              
         DC    S(AC@ENGCH),AL1(0,0,0,STCXSENG,0,0)                              
         DC    S(AC@DISCH),AL1(0,0,0,STCXSDIS,0,0)                              
         DC    S(AC@PASCH),AL1(0,0,0,0,STCXSPAS,0)                              
         DC    X'FF'                                                            
*&&US                                                                           
PRVTAB   DS    0C                                                               
         DC    C'BC',AL2(AC@PRVBC-DSDICTL)   BRITISH COLUMBIA                   
         DC    AL1(L'AC@PRVBC)                                                  
         DC    C'AL',AL2(AC@PRVAL-DSDICTL)   ALBERTA                            
         DC    AL1(L'AC@PRVAL)                                                  
         DC    C'SA',AL2(AC@PRVSA-DSDICTL)   SASKATCHEWAN                       
         DC    AL1(L'AC@PRVSA)                                                  
         DC    C'MA',AL2(AC@PRVMA-DSDICTL)   MANITOBA                           
         DC    AL1(L'AC@PRVMA)                                                  
         DC    C'ON',AL2(AC@PRVON-DSDICTL)   ONTARIO                            
         DC    AL1(L'AC@PRVON)                                                  
         DC    C'PQ',AL2(AC@PRVPQ-DSDICTL)   QUEBEC                             
         DC    AL1(L'AC@PRVPQ)                                                  
         DC    C'NB',AL2(AC@PRVNB-DSDICTL)   NEW BRUNSWICK                      
         DC    AL1(L'AC@PRVNB)                                                  
         DC    C'NS',AL2(AC@PRVNS-DSDICTL)   NOVA SCOTIA                        
         DC    AL1(L'AC@PRVNS)                                                  
         DC    C'PE',AL2(AC@PRVPE-DSDICTL)   PRINCE EDWARD ISLAND               
         DC    AL1(L'AC@PRVPE)                                                  
         DC    C'NF',AL2(AC@PRVNF-DSDICTL)   NEWFOUNDLAND                       
         DC    AL1(L'AC@PRVNF)                                                  
PRVTABN  EQU   (*-PRVTAB)/PRVTABL                                               
         DC    X'FF'               EOT                                          
*&&                                                                             
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
EFFS     DC    X'FFFFFFFFFF'                                                    
PZERO    DC    P'0'                                                             
         DS    0D                                                               
LARE     DC    X'41E0'                                                          
LAREADDR DC    S(0)                                                             
ONENUL   DC    C'1N'                                                            
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
         EJECT                                                                  
                                                                                
ASTERS   DC    16C'*'                                                           
                                                                                
         EJECT                                                                  
*                                                                               
EXNKEYT  LKKEY H,EXNPAS,SAVED      ** EXPENSE NUMBER KEY DRIVER **              
         LKKEY LIT,EXNPTYP,EXNPTYPQ                                             
         LKKEY LIT,EXNPSUB,EXNPSUBQ                                             
         LKKEY SIN,EXNPCPY,AGENCY                                               
         LKKEY LIT,EXNPREM,0                                                    
         LKKEY SIN,EXNPTYPE,QD_CDTYP                                            
         LKKEY SIN,EXNPNUM,QD_CDNUM                                             
         LKKEY ALL,EXNPSEQ                                                      
         LKKEY E                                                                
                                                                                
AUDKEYT  LKKEY H,AUDKEY            ** AUDIT KEY DRIVER **                       
         LKKEY LIT,AUDKTYP,AUDKTYPQ                                             
         LKKEY LIT,AUDKSUB,AUDKSUBQ                                             
         LKKEY SIN,AUDKCPY,AGENCY                                               
         LKKEY LIT,AUDKAUDT,AUDKEXPC                                            
         LKKEY LIT,AUDKREMC,0                                                   
         LKKEY SIN,AUDKCLM,QD_CDNUM                                             
         LKKEY SIN,AUDKCTY,QD_CDTYP                                             
         LKKEY ALL,AUDKSEQ                                                      
         LKKEY E                                                                
                                                                                
EGNKEYT  LKKEY H,EGNPAS            ** ESTAIMATE NUMBER KEY DRIVER **            
         LKKEY LIT,EGNPTYP,EGNPTYPQ                                             
         LKKEY LIT,EGNPSUB,EGNPSUBQ                                             
         LKKEY SIN,EGNPCPY,AGENCY                                               
         LKKEY LIT,EGNPREM,0                                                    
         LKKEY SIN,EGNPNUM,CI_ESTN                                              
         LKKEY ALL,EGNPCLI                                                      
         LKKEY ALL,EGNPPRO                                                      
         LKKEY ALL,EGNPJOB                                                      
         LKKEY ALL,EGNPLNO                                                      
         LKKEY E                                                                
XDFKEYT  LKKEY H,XDFKEY            ** XDATA RECORD KEY DRIVER **                
         LKKEY LIT,XDFKTYP,XDFKTYPQ                                             
         LKKEY LIT,XDFKSUB,XDFKSUBQ                                             
         LKKEY SIN,XDFKCPY,AGENCY                                               
         LKKEY LIT,XDFKREM,0                                                    
         LKKEY ALL,XDFKOFF                                                      
         LKKEY ALL,XDFKORTY                                                     
         LKKEY ALL,XDFKCLI                                                      
         LKKEY ALL,XDFKETY                                                      
         LKKEY ALL,XDFKWC                                                       
         LKKEY ALL,XDFKMED                                                      
         LKKEY ALL,XDFKSCH                                                      
         LKKEY ALL,XDFKSEQ                                                      
         LKKEY E                                                                
                                                                                
XDLKEYT  LKKEY H,XDLKEY            ** XDLIST RECORD KEY DRIVER **               
         LKKEY LIT,XDLKTYP,XDLKTYPQ                                             
         LKKEY LIT,XDLKSUB,XDLKSUBQ                                             
         LKKEY SIN,XDLKCPY,AGENCY                                               
         LKKEY LIT,XDLKREM,0                                                    
         LKKEY SIN,XDLKOFF,DOFF                                                 
         LKKEY SIN,XDLKORTY,DORTY                                               
         LKKEY SIN,XDLKCLI,DCLI                                                 
         LKKEY SIN,XDLKETY,DETY                                                 
         LKKEY SIN,XDLKWC,DWC                                                   
         LKKEY SIN,XDLKMED,DMED                                                 
         LKKEY SIN,XDLKSCH,DSCH                                                 
         LKKEY SIN,XDLKCODE,DCODE                                               
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
                                                                                
*&&US                                                                           
LCL#LIT  DC    C'Live Claim Number'                                             
*&&                                                                             
LOV#LIT  DC    C'Override limit list'                                           
ACCULIT  DC    C'Accumulator number'                                            
DISTLIT  DC    C'Distance input for item'                                       
PACULIT  DC    C'Previous accumulated distance'                                 
OVERLIT  DC    C'Override limit list/approver rules'                            
SRCHLIT  DC    C'Has search permission'                                         
XPDFLIT  DC    C'Expense PDF'                                                   
                                                                                
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(FLTXDF)                                                        
         DC    V(ACGETCRL)                                                      
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#EXPA,L'AC@EXPA                                                
         DCDDL AC#SUP,L'AC@SUP                                                  
         DCDDL AC#WC,L'AC@WC                                                    
         DCDDL AC#TYPE,L'AC@TYPE                                                
         DCDDL AC#ETYPE,L'AC@ETYPE                                              
         DCDDL AC#CURRC,L'AC@CURRC                                              
         DCDDL AC#FCAMT,L'AC@FCAMT                                              
         DCDDL AC#AMT,L'AC@AMT                                                  
         DCDDL AC#DATE,L'AC@DATE                                                
         DCDDL AC#VATC1,L'AC@VATC1                                              
         DCDDL AC#NRTV,L'AC@NRTV                                                
         DCDDL AC#MILES,L'AC@MILES                                              
         DCDDL AC#REF,L'AC@REF                                                  
         DCDDL AC#EST,L'AC@EST                                                  
         DCDDL AC#ORDER,L'AC@ORDER                                              
         DCDDL AC#CLIC,L'AC@CLIC                                                
         DCDDL AC#PROC,L'AC@PROC                                                
         DCDDL AC#JOBC,L'AC@JOBC                                                
         DCDDL AC#PRSNL,L'AC@PRSNL                                              
         DCDDL AC#2DDPC,L'AC@2DDPC                                              
         DCDDL AC#SELED,L'AC@SELED                                              
         DCDDL AC#RCPTS,L'AC@RCPTS                                              
         DCDDL AC#DISAM,L'AC@DISAM                                              
         DCDDL AC#OFFC,L'AC@OFFC                                                
         DCDDL AC#INTRF,L'AC@INTRF                                              
         DCDDL AC#FNNRC,L'AC@FNNRC                                              
         DCDDL AC#FNNRA,L'AC@FNNRA                                              
         DCDDL AC#FNNRD,L'AC@FNNRD                                              
         DCDDL AC#VEHCH,L'AC@VEHCH                                              
         DCDDL AC#FUECH,L'AC@FUECH                                              
         DCDDL AC#ENGCH,L'AC@ENGCH                                              
         DCDDL AC#DISCH,L'AC@DISCH                                              
         DCDDL AC#PASCH,L'AC@PASCH                                              
*&&US                                                                           
         DCDDL AC#PRVC,L'AC@PRVBC                                               
         DCDDL AC#PRVAL,L'AC@PRVAL                                              
         DCDDL AC#PRVSA,L'AC@PRVSA                                              
         DCDDL AC#PRVMA,L'AC@PRVMA                                              
         DCDDL AC#PRVON,L'AC@PRVON                                              
         DCDDL AC#PRVPQ,L'AC@PRVPQ                                              
         DCDDL AC#PRVNB,L'AC@PRVNB                                              
         DCDDL AC#PRVNS,L'AC@PRVNS                                              
         DCDDL AC#PRVPE,L'AC@PRVPE                                              
         DCDDL AC#PRVNF,L'AC@PRVNF                                              
*&&                                                                             
DCDICTLX DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
B#EXPREC EQU   3                   IO2 - EXPENSE RECORD                         
B#XDFREC EQU   4                   IO3 - EXTRA DATA RECORD                      
AEXPREC  EQU   LP_BLKS+((B#EXPREC-1)*L'LP_BLKS)                                 
B#AUD    EQU   5                   IO4 - AUDIT RECORD                           
B#XDLREC EQU   5                   IO4 - EXTRA DATA LIST RECORD                 
B#EST    EQU   6                   IO5 - ESTIMATE RECORD                        
B#VEHREC EQU   6                   IO5 - VEHICLE RECORD                         
B#FUEREC EQU   6                   IO5 - FUEL RECORD                            
B#ENGREC EQU   6                   IO5 - ENGINE RECORD                          
B#DTNREC EQU   6                   IO5 - DISTANCE RECORD                        
B#BUFREC EQU   7                   EX_BUFF - BUFFERED EXPENSE RECORD            
B#LMAPR  EQU   8                   CH_LMAP - BUFFERED LINE MANAGER              
B#COBLCK EQU   9                       - COST PROFILE AREA                      
ACOSTPR  EQU   LP_BLKS+((B#COBLCK-1)*L'LP_BLKS)                                 
B#SVRDEF EQU   10                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   11                      - LP_D                                   
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
WVALUES  DS    0X                  ** LITERAL VALUES **                         
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTXDF  DS    A                   A(FLTXDF)                                    
VGETCRL  DS    A                   A(GETCRL)                                    
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
ACURLIST DS    A                   A(LIST OF CURRENCIES)                        
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
ALOCEL   DS    A                   A(LOCEL)                                     
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPXDIS  EQU   C'1'                Expense display                              
MAPXLIS  EQU   C'2'                Expense list                                 
MAPXSRC  EQU   C'3'                Expense search                               
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN VALUES                     
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPAREA2 DS    XL(GAPTLNQ)         Area to use for buffer rec                   
ATSRERRS DS    XL1                 Error area for buffer                        
MYBYTE   DS    XL1                 Byte for use in routines                     
RUNINDS  DS    X                   Run indicator                                
RUNISKIP EQU   X'80'               Skip processing                              
                                                                                
CH_TODC  DS    XL2                 Today compressed format                      
CH_TODP  DS    XL3                 Today packed format                          
PACK16   DS    PL16                                                             
                                                                                
DSDICTL  DS    0C                                                               
AC@EXPA  DS    CL25                                                             
AC@SUP   DS    CL25                                                             
AC@WC    DS    CL25                                                             
AC@TYPE  DS    CL25                                                             
AC@ETYPE DS    CL25                                                             
AC@CURRC DS    CL25                                                             
AC@FCAMT DS    CL25                                                             
AC@AMT   DS    CL25                                                             
AC@DATE  DS    CL25                                                             
AC@VATC1 DS    CL25                                                             
AC@NRTV  DS    CL25                                                             
AC@MILES DS    CL25                                                             
AC@REF   DS    CL25                                                             
AC@EST   DS    CL25                                                             
AC@ORDER DS    CL25                                                             
AC@CLIC  DS    CL25                                                             
AC@PROC  DS    CL25                                                             
AC@JOBC  DS    CL25                                                             
AC@PRSNL DS    CL25                                                             
AC@2DDPC DS    CL25                                                             
AC@SELED DS    CL25                                                             
AC@RCPTS DS    CL25                                                             
AC@DISAM DS    CL25                                                             
AC@OFFC  DS    CL25                                                             
AC@INTRF DS    CL25                                                             
AC@FNNRC DS    CL25                                                             
AC@FNNRA DS    CL25                                                             
AC@FNNRD DS    CL25                                                             
AC@VEHCH DS    CL25                                                             
AC@FUECH DS    CL25                                                             
AC@ENGCH DS    CL25                                                             
AC@DISCH DS    CL25                                                             
AC@PASCH DS    CL25                                                             
*&&US                                                                           
AC@PRVBC DS    CL25                                                             
AC@PRVAL DS    CL25                                                             
AC@PRVSA DS    CL25                                                             
AC@PRVMA DS    CL25                                                             
AC@PRVON DS    CL25                                                             
AC@PRVPQ DS    CL25                                                             
AC@PRVNB DS    CL25                                                             
AC@PRVNS DS    CL25                                                             
AC@PRVPE DS    CL25                                                             
AC@PRVNF DS    CL25                                                             
*&&                                                                             
SAVEKEY1 DS    CL64                                                             
SAVEKEY2 DS    CL64                                                             
SAVEKEY3 DS    CL64                                                             
SAVEKEY4 DS    CL64                                                             
SAVERF   DS    F                   SAVED REGISTER RF                            
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
* Claim Display call values                                                     
QD_CDTYP DS    CL1                 D(raft) or L(ive) claim number               
QD_CDNUM DS    CL6                 Claim number                                 
QD_CDVIW DS    CL1                 View                                         
QD_CDVOQ EQU   C'U'                - Owner                                      
QD_CDVAQ EQU   C'A'                - Approver                                   
QD_CDVBQ EQU   C'B'                - Back Up approver (not for Aura)            
QD_CDVFQ EQU   C'F'                - Finance                                    
QD_CDOVR DS    CL1                 Override limit lst and approvr rules         
QD_CDSRC DS    CL1                 Search permission                            
QD_CDPDF DS    CL1                 PDF call - return audit too                  
                                                                                
* Search call values                                                            
QS_CSVIE DS    CL1                 view                                         
QS_CSVMQ EQU   C'1'                - my claims                                  
QS_CSVSQ EQU   C'2'                - staff view (incl. approver)                
QS_CSVBQ EQU   C'3'                - back up approver view                      
QS_CSVFQ EQU   C'4'                - finance staff view                         
QS_CSVRQ EQU   C'5'                - Finance report view                        
QS_CSFDA DS    XL2                 from date                                    
QS_CSTDA DS    XL2                 to date                                      
QS_INPRG DS    CL1                 In progress                                  
QS_SUBMT DS    CL1                 Submitted                                    
QS_APPRD DS    CL1                 Approved                                     
QS_REJED DS    CL1                 Rejected                                     
QS_POSTD DS    CL1                 Posted                                       
QS_PRTAP DS    CL1                 Part approved                                
QS_CSCNP DS    CL6                 (live) claim number prefix                   
QS_CSPER DS    CL8                 person (claimer)                             
QS_OVAPP DS    CL1                 (Y/N) override limitaccess                   
QS_OFFC  DS    CL2                 Office code                                  
QS_DEPC  DS    CL3                 Department code                              
QS_SDPC  DS    CL2                 Sub-department code                          
QS_ETYC  DS    CL3                 Expenditure type code                        
QS_RCPT  DS    CL1                 Receipt (B/N/Y)                              
QS_BILL  DS    CL1                 Billable(B/N/Y)                              
QS_CURR  DS    CL3                 Currency code                                
QS_VATA  DS    CL12                Vat code/account                             
QS_CLIO  DS    CL2                 Client office code                           
QS_CLI   DS    CL5                 Client code                                  
QS_PRO   DS    CL3                 Product code                                 
QS_JOB   DS    CL7                 Job code                                     
                                                                                
* Claim List call values                                                        
QL_CLOWN DS    CL1                 owner: mine or approvals or finance          
QL_CLOAQ EQU   C'A'                                                             
QL_CLOMQ EQU   C'M'                                                             
QL_CLOFQ EQU   C'F'                                                             
QL_CLVBY DS    CL1                 view by: status or date                      
QL_CLVSQ EQU   C'S'                                                             
QL_CLVDQ EQU   C'D'                                                             
QL_CLTYP DS    CL1                 type: draft or live, empty: both             
QL_CLDFR DS    XL2                 from date                                    
QL_CLDTO DS    XL2                 to date                                      
QL_CLSTA DS    CL1                 Status (see STATAB)                          
QL_CLGAO DS    CL1                 GAOV call Y/N                                
*                                                                               
QM_PERS  DS    CL(L'PERKCODE)      Person code                                  
QM_CDATE DS    XL3                 Item date packed                             
QM_EFDTE DS    XL3                 Effective date                               
QM_VEHCD DS    CL(L'FFTVEHC)       Vehicle code                                 
QM_FUECD DS    CL(L'FFTFUEC)       Fuel code                                    
QM_ENGCD DS    CL(L'FFTENGC)       Engine code                                  
QM_DISAM DS    PL6                 Distance amount                              
QM_PASS  DS    XL1                 Number of passengers                         
QM_DISAC DS    PL6                 Distance accumulated within claim            
QM_DSPAC DS    PL6                 Distance previously accumulated              
QM_VEHCL DS    CL1                 Include vehicle records                      
QM_FUEL  DS    CL1                 Include fuel records                         
QM_ENGSZ DS    CL1                 Include engine size records                  
QM_DIST  DS    CL1                 Include distance band records                
QM_ACUNM DS    XL1                 Distance accumulator number                  
QM_DISCL DS    CL1                 Display call to look up rate                 
         ORG   QVALUES+L'QVALUES                                                
QVALUESL EQU   *-QVALUES                                                        
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
GAPLPARM DS    XL1                 GAPLST Parameter                             
DCODE    DS    CL(L'XDFCODE)       XDFCODE                                      
DOFF     DS    XL(L'XDFKOFF)       XDFKOFF                                      
DORTY    DS    XL(L'XDFKORTY)      XDFKORTY                                     
DCLI     DS    XL(L'XDFKCLI)       XDFKCLI                                      
DETY     DS    XL(L'XDFKETY)       XDFKETY                                      
DWC      DS    XL(L'XDFKWC)        XDFKWC                                       
DMED     DS    XL(L'XDFKMED)       XDFKMED                                      
DSCH     DS    XL(L'XDFKSCH)       XDFKSCH                                      
*&&UK                                                                           
D_CTRY   DS    XL1                 Country                                      
D_CURR   DS    CL3                 Currency                                     
*&&                                                                             
                                                                                
DAC1RIND DS    X                                                                
DAAC1R   DS    AL3                 BUILT 1R ACCOUNT IN WMP                      
DAC1RAXQ EQU   20                                                               
CURIND   DS    X                   Process currencies                           
CURICLI  EQU   X'80'               Client level currency found                  
CURICRL  EQU   X'40'               Have currency list                           
CURIMOD  EQU   X'20'               Client level mode                            
CURIALL  EQU   X'10'               All mode                                     
CURIOFC  EQU   X'08'               Office currency mode                         
CURRULE  DS    CL(L'EURKRULE)      Currency rate                                
CURMXMN  DS    XL6                                                              
CURMAX   DS    CL5                 Agency currency max rate                     
CURMIN   DS    CL5                 Agency currency min rate                     
PKWK16A  DS    PL16                                                             
PKWK16B  DS    PL16                                                             
PKWK6    DS    PL6                                                              
                                                                                
X#LANG   DS    XL1                                                              
                                                                                
DVALUESL EQU   *-DVALUES                                                        
                                                                                
FNIND    DS    XL1                 Finance approver indicator                   
FNIHIGH  EQU   X'80'               Highest level has been read                  
FNIFND   EQU   X'40'               Found approver                               
AGENCY   DS    XL(L'CUXCPY)        Company code                                 
USRID    DS    XL(L'CUUSER)        Connected user id                            
*                                                                               
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AALL     DS    AL3                 All value WMP entry                          
ANZR     DS    AL3                 Non-zero value WMP entry                     
*                                                                               
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
*                                                                               
CL_VALS  DS    0X                                                               
CL_CUR   DS    CL3                 - ISO code                                   
CL_NAM   DS    CL35                - Name                                       
CL_CLI   DS    CL5                 - Client code                                
CL_DEC   DS    XL1                 - Ddecimal places                            
CL_POS   DS    XL1                 - Pos. in list (*=unknown,þ=inactiv)         
CL_RUL   DS    XL14                (see DDEUREKAD)                              
CL_MIN   DS    XL5                 - Minimum rate                               
CL_MAX   DS    XL5                 - Maximum rate                               
CL_NPC   DS    CL1                 - None production currency if Y              
CL_LNQ   EQU   *-CL_VALS                                                        
*                                                                               
* Expense Header Values                                                         
CH_HDR   DS    CL1                 Header indicator                             
CH_ULA   DS    0CL14               Claimers 1R account code                     
CH_UNT   DS    CL1                 Unit                                         
CH_LDG   DS    CL1                 Ledger                                       
CH_ACT   DS    CL12                Account                                      
CH_1RAC  DS    CL12                Account                                      
CH_TOTAL DS    PL6                 Total amount of claim                        
CH_STADT DS    XL2                 Start date                                   
CH_STADC DS    XL2                 Start date 2s compliment                     
CH_ENDDT DS    XL2                 End date                                     
CH_ENDDC DS    XL2                 End date 2s compliment                       
CH_OFF   DS    CL(L'LOCOFF)        Office                                       
CH_DEPT  DS    CL(L'LOCDEPT)       Department                                   
CH_SUBD  DS    CL(L'LOCSUB)        Sub-department                               
CH_PER   DS    CL(L'PERKCODE)      Person                                       
CH_EBV   DS    PL4                 Expenses back view months                    
CH_FSTAT DS    XL(L'EXCKSTAT*7)    Expenses status                              
CH_DATE  DS    XL2                 Date of claim                                
CH_ITDTE DS    PL3                 Lowest item date                             
CH_STAT  DS    CL1                 Claim status                                 
                                                                                
CH_COBCA DS    CL(L'COBCA)         Bypass client approvals                      
CH_COTNB DS    PL(L'COTNB)         Threshold for non billable claims            
CH_COLMA DS    CL(L'COLMA)         Line manager not reqd when cli apprd         
CH_COCAE DS    CL(L'COCAE)         Approvers chosen on front end                
CH_COMIL DS    CL(L'COMIL)         New mileage rate system in use               
CH_1RBIL DS    CL1                 Y/N override 1R office bill                  
CH_1RNBL DS    CL1                 Y/N override 1R office non bill              
                                                                                
CH_SGOPO DS    XL(L'LDGOPOS)       SG ledger office position                    
CH_PPID# DS    XL2                 PID code                                     
CH_IND   DS    XL1                 Header indicator                             
CH_I1RAP EQU   X'80'               1R approvers found                           
CH_IMSAP EQU   X'40'               Missing approval                             
CH_ILVL1 EQU   X'20'               level 1 approver                             
CH_ILVL2 EQU   X'10'               level 2 approver                             
CH_IISAP EQU   X'08'               Is an approver at item level                 
CH_I1RVW EQU   X'04'               Connected user has rights to view            
                                                                                
CH_FNAPN DS    XL1                 Number of finance approvers                  
                                                                                
CH_FNMAX EQU   10                                                               
CH_FNAP  DS    0X                  Finance approver block                       
CH_FNSTA DS    XL1                 Finance approver status                      
CH_FNPID DS    XL2                 Finance approver                             
CH_FNAPL EQU   *-CH_FNAP           Length of approver block                     
         DS    (CH_FNMAX-1)XL(CH_FNAPL)                                         
                                                                                
CH_USRFN DS    XL1                 User is finance approver                     
CH_FNDIS DS    XL1                 Display finance edit display                 
                                                                                
CH_OWNVL DS    PL6                 Highest value user approves                  
                                                                                
CH_LMAPN DS    XL1                 Number of line manager approvers             
CH_SVBU  DS    XL1                 Temp storage of max back up aprvs            
                                                                                
CH_MAXAP EQU   4                   Maximum level of approvers                   
CH_MXBUQ EQU   20                  Maximum number of back up approvers          
CH_LMAP  DS    (CH_MAXAP)XL(CH_LMAPL)                                           
                                                                                
* Expense item Values                                                           
CI_SJULA DS    0CL14               Production account                           
CI_SJUNT DS    CL1                 Unit                                         
CI_SJLDG DS    CL1                 Ledger                                       
CI_SJACT DS    CL12                Account                                      
CI_SJOFF DS    CL2                 Office for particular SJ level               
CI_CLI   DS    CL5                 Client code                                  
CI_OFF   DS    CL2                 Office of production account                 
CI_MED   DS    CL1                 Media code                                   
CI_TYPE  DS    CL1                 Type of item                                 
CI_DATE  DS    XL3                 Item date - packed format                    
CI_ESTN  DS    CL6                 Estimate number (global)                     
CI_WC    DS    CL2                 Work code                                    
CI_DIST  DS    PL6                 Distance accumulator for item                
                                                                                
CI_ITAPP DS    0X                  Item manager approver block                  
CI_ITPID DS    XL2                 Item manager approver                        
CI_ITBUP DS    (CH_MXBUQ)XL2       Item manager back up approver                
CI_ITAPL EQU   *-CI_ITAPP          Length of approver block                     
         DS    (CH_MAXAP-1)XL(CI_ITAPL)                                         
                                                                                
CI_2DULA DS    0CL14               Department account                           
CI_2DUNT DS    CL1                 Unit                                         
CI_2DLDG DS    CL1                 Ledger                                       
CI_2DACT DS    CL12                Account                                      
                                                                                
CI_2PULA DS    0CL14               Personnel account                            
CI_2PUNT DS    CL1                 Unit                                         
CI_2PLDG DS    CL1                 Ledger                                       
CI_2PACT DS    CL12                Account                                      
                                                                                
CI_ITMDL DS    CL1                 Item is deleted                              
CI_NETIN DS    CL1                 Net input                                    
CI_AGYIN DS    CL1                 Agency input for foreign currency            
CI_SELTD DS    CL1                 Selected item                                
CI_DISCL DS    CL1                 Disclaimer                                   
CI_RCPT  DS    CL1                 Receipt                                      
CI_ETYP  DS    CL3                 Expenditure type                             
CI_EXTNM DS    CL36                Expenditure type name                        
CI_ETST2 DS    XL1                 Expenditure type status byte 2               
CI_CLAP  DS    CL1                 Is client approver?  Y/N                     
CI_CLBU  DS    CL1                 Is client back up approver?  Y/N             
                                                                                
CI_ITXRT DS    CL14                Exchange rate                                
CI_FCDP  DS    XL1                 Foreign currency decimal places              
CI_VRATE DS    XL2                 Vat Rate                                     
CI_DSCRT DS    XL2                 Vat Rate                                     
                                                                                
CI_2PAN  DS    CL1                 Personnel analysis                           
CI_2DAN  DS    CL1                 Department analysis                          
CI_CLAN  DS    CL1                 Client analysis                              
CI_PRAN  DS    CL1                 Product analysis                             
CI_JBAN  DS    CL1                 Job analysis                                 
CI_MILE  DS    CL1                 Mile analysis                                
CI_LOCK  DS    CL1                 Locked                                       
CI_CLOS  DS    CL1                 Closed                                       
CI_XJOB  DS    CL1                 Expense Job                                  
CI_EXLNQ EQU   *-CI_2PAN           Length of analysis details                   
                                                                                
CI_NJBNB DS    CL1                 No job input allowed on non billable         
CI_BILO  DS    CL1                 Billable only                                
*&&US                                                                           
CI_PRVCD DS    CL2                 PROVINCE CODE                                
CI_PRVNM DS    CL20                PROVINCE NAME                                
*&&                                                                             
CI_LNQ   EQU   *-CI_SJULA                                                       
                                                                                
CI_XDPTR DS    XL(L'XDFRPTR+L'XDFSEQ) Extra data pointer                        
                                                                                
* Claim mileage vehicle information                                             
CV_ALPAS DS    CL1                 Allow passengers with this vehicle           
CV_FUERQ DS    CL1                 Fuel code required with this vehicle         
CV_ENGRQ DS    CL1                 Engine code required with vehicle            
CV_NOFUE DS    CL1                 No fuel on any vehicle                       
CV_NOENG DS    CL1                 No engine on any vehicle                     
CV_NODIS DS    CL1                 No distance on any vehicle                   
CV_ACUNM DS    XL1                 Accumulator number                           
CV_MAXAC EQU   10                  Max number of accumulators                   
CV_DIST  DS    0X                  Distance accum block for person              
CV_DISAC DS    PL6                 Distance accumulator                         
CV_DISTL EQU   *-CV_DIST           Length of distance accumulator block         
         DS    (CV_MAXAC-1)XL(CV_DISTL)                                         
* Claim mileage calculation information                                         
CM_RTCHG DS    CL1                 Rate has changed                             
CM_TOTAM DS    PL6                 Claim total amount                           
CM_BNGAM DS    PL6                 Band gross amount                            
CM_NVATA DS    PL6                 Claim non vatable amount                     
CM_BNNVA DS    PL6                 Band non vatable amount                      
CM_TOTVT DS    PL6                 Claim total vatable amount                   
CM_BNVTA DS    PL6                 Band vatable amount                          
CM_ODIST DS    PL6                 Previous accumulated distance                
CM_BNDAM DS    PL6                 Distance band amount                         
                                                                                
CM_RDATE DS    XL3                 Refresh date for accumulators                
CM_MAXAC EQU   10                  Max number of accumulators                   
CM_DIST  DS    0X                  Distance accumulator block for claim         
CM_DISDT DS    XL3                 Distance accumulator start date              
CM_DISNM DS    XL1                 Distance accumulator number                  
CM_DISAC DS    PL6                 Distance accumulator                         
CM_DISTL EQU   *-CM_DIST           Length of distance accumulator block         
         DS    (CM_MAXAC-1)XL(CM_DISTL)                                         
                                                                                
* Claim audit information                                                       
CA_TYP   DS    CL1                 Type if data change                          
CA_ITM   DS    CL3                 Item number if data change                   
CA_VAL   DS    CL14                Value if data change                         
CA_NEW   DS    CL1                 Whether new style stceld                     
CA_TIME  DS    CL6                 Time                                         
CA_LNQ   EQU   *-CA_TYP                                                         
CA_COMMT DS    CL(L'STCXCOMT)      Value if data change                         
OVALUESL EQU   *-OVALUES                                                        
                                                                                
EX_BUFF  DS    XL500               Buffer area for expense claims               
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
*&&UK                                                                           
OB_CURR  DS    CL3                 Currency                                     
OB_CTRY  DS    XL1                 Country                                      
*&&                                                                             
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
                                                                                
***********************************************************************         
* Line manager approver DSECT                                         *         
***********************************************************************         
                                                                                
CH_LMAPD DSECT                                                                  
CH_LMVAL DS    PL6                 Line manager approval value                  
CH_LMLVL DS    XL1                 Line manager approval level                  
CH_LMSTA DS    XL1                 Line manager approval status                 
CH_LMPID DS    XL2                 Line manager approver                        
CH_LMBUP DS    (CH_MXBUQ)XL2       Line manager back up approver                
CH_LMAPL EQU   *-CH_LMAPD          Length of approver block                     
                                                                                
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
* Displacement to status byte table DSECT                             *         
***********************************************************************         
DSPTABD  DSECT                                                                  
DSPKTYP  DS    XL1                 Key type equate                              
DSPKSUB  DS    XL1                 Key sub type equate                          
DSPSTAT  DS    XL1                 Displacement in key to status                
DSPTABL  EQU   *-DSPTABD                                                        
                                                                                
***********************************************************************         
* Status table DSECT                                                  *         
***********************************************************************         
STATABD  DSECT                                                                  
STATREQ  DS    CL1                 Request value                                
STATRST  DS    XL1                 Record status                                
STATAOP  DS    XL1                 Approver output value                        
STATFOP  DS    XL1                 Finance approver output value                
STATABL  EQU   *-STATABD                                                        
                                                                                
***********************************************************************         
* Job passive type search table DSECT                                 *         
***********************************************************************         
JOBTYPD  DSECT                                                                  
JOBBILL  DS    XL1                 Billable type                                
JOBNONB  DS    XL1                 Non billable type                            
JOBTYPLQ EQU   *-JOBTYPD                                                        
                                                                                
*&&US                                                                           
***********************************************************************         
* Province table DSECT                                                *         
***********************************************************************         
PRVTABD  DSECT                                                                  
PRVCODE  DS    CL2                 Province code                                
PRVNAM   DS    XL2                 Province name                                
PRVNAML  DS    XL1                 Province name length                         
PRVTABL  EQU   *-PRVTABD                                                        
*&&                                                                             
                                                                                
***********************************************************************         
* Claim record buffer DSECT                                           *         
***********************************************************************         
                                                                                
CLM_D    DSECT                                                                  
CLM_LEN  DS    XL2                                                              
CLM_KEY  DS    0X                                                               
CLM_NUM  DS    CL6                 Claim number                                 
CLM_TYPE DS    CL1                 Claim type live or draft                     
         DS    XL1                                                              
CLM_KEYL EQU   *-CLM_KEY           Key length                                   
CLM_DATE DS    XL2                 Claim date compressed                        
CLM_1RAC DS    CL12                Claim 1R account                             
CLM_LN1Q EQU   *-CLM_D             Length for compare                           
CLM_DESC DS    CL36                Description                                  
CLM_PIDB DS    XL2                 PID of claimer                               
CLM_UID  DS    XL2                 User id of claim                             
CLM_TOT  DS    PL6                 Total of claim                               
CLM_ALAM DS    PL6                 Alternate amount                             
CLM_ITMS DS    PL2                 Number of items                              
CLM_APRS DS    PL2                 Number of approvers                          
CLM_STAT DS    XL1                 Record status byte 1                         
CLM_STA2 DS    XL1                 Record status byte 2                         
CLM_CLST DS    XL1                 CLDELD status                                
CLM_APST DS    XL1                 Approval status                              
CLM_ABY  DS    XL2                 PID who raised claim                         
CLM_ADVS DS    PL6                 Advances                                     
CLM_ID   DS    XL2                 ID index number                              
CLM_BREF DS    CL6                 Batch reference                              
CLM_BNAM DS    CL14                Batch name                                   
CLM_BADD DS    XL2                 Batch added date                             
CLM_BPID DS    XL2                 Batch PID                                    
CLM_APDT DS    XL3                 Approved date                                
CLM_APPD DS    XL2                 Approved PID                                 
CLM_BUID DS    XL2                 Batch User id                                
CLM_BITM DS    PL6                 Claim billable item total                    
CLM_NBTM DS    PL6                 Claim non-billable item total                
CLM_RPRF DS    CL6                 Most recent payment reference number         
CLM_RPDT DS    XL2                 Most recent payment date details             
CLM_PTOT DS    PL6                 Total payment againest Expence claim         
CLM_PFND DS    XL1                 Payment found                                
*&&UK                                                                           
CLM_CTRY DS    XL1                 Country from 1R office                       
CLM_CURR DS    CL3                 Currency from 1R office                      
*&&                                                                             
CLM_SUBP DS    XL2                 Submitter pid                                
CLM_SUBD DS    PL3                 Submitted date                               
CLM_RFST EQU   *                                                                
CLM_RLEN EQU   *-CLM_D             Basic record length                          
                                                                                
CLM_FCD  DSECT                     Foreign currency DSECT                       
CLM_FCI  DS    XL1                 Foreign currency entry                       
CLM_FCIQ EQU   X'90'               Identifier                                   
CLM_FCLN DS    XL1                 Foreign currency length                      
CLM_FCCD DS    CL3                 Foreign currency code                        
CLM_FCAM DS    PL6                 Foreign currency amount                      
CLM_FCLQ EQU   *-CLM_FCI           Foreign currency item length                 
                                                                                
***********************************************************************         
* Claim item record buffer DSECT                                      *         
***********************************************************************         
                                                                                
CLI_D    DSECT                                                                  
CLI_RLEN DS    XL2                                                              
CLI_RKEY DS    0X                                                               
CLI_NUM  DS    CL6                 Claim number                                 
CLI_TYPE DS    CL1                 Claim type live or draft                     
CLI_ISEQ DS    XL1                 Claim item sequence                          
CLI_KEYL EQU   *-CLI_RKEY          Key length                                   
*              CLI_D+(CLM_KEYL-(*-CLI_D))  Trap for length overflow             
CLI_DATE DS    XL2                 Claim date compressed                        
CLI_ULA  DS    0XL14               1R account                                   
CLI_ULDG DS    CL(L'ACTKUNT+L'ACTKLDG) unit/ledger                              
CLI_ACC  DS    CL12                Claim 1R account                             
CLI_PIDB DS    XL2                 PID of claimer                               
CLI_ABY  DS    XL2                 PID who raised the claim                     
CLI_UID  DS    XL2                 User id of claim                             
CLI_STAT DS    XL1                 Record status byte 1                         
CLI_STA2 DS    XL1                 Record status byte 2                         
CLI_APST DS    XL1                 Approval status                              
CLI_LMPD DS    XL2                 Line manager approver PID                    
*&&UK                                                                           
CLI_CTRY DS    XL1                 Country from 1R office                       
CLI_CURR DS    CL3                 Currency from 1R office                      
*&&                                                                             
CLI_LN1Q EQU   *-CLM_D             Length for compare                           
*                                                                               
CLI_IFST DS    0X                                                               
CLI_IDTE DS    XL2                 Claim item date compressed                   
CLI_IETY DS    CL3                 Expenditure type                             
CLI_IBIL DS    CL1                 Billable                                     
CLI_IRCP DS    CL1                 Receipt Y/N                                  
CLI_IEXP DS    CL14                Expense account code                         
CLI_ISJU DS    0CL14               SJ account code                              
CLI_IUL  DS    CL(L'ACTKUNT+L'ACTKLDG) Unit/ledger                              
CLI_IACC DS    CL(L'ACTKACT)       account                                      
CLI_IOFF DS    CL2                 Office code                                  
CLI_IVTC DS    CL12                Vat account code                             
CLI_IAMT DS    PL6                 Agency amount                                
CLI_INET DS    PL6                 Net amount                                   
CLI_IVMT DS    PL6                 Vat amount (GST amount)                      
CLI_IFMT DS    PL6                 Foreign amount                               
CLI_IXRT DS    XL7                 Exchange rate                                
*&&US                                                                           
CLI_IPRC DS    CL2                 Province code                                
CLI_IPST DS    CL(L'CIDCPST)       PST code                                     
CLI_IPSA DS    PL6                 PST amount                                   
*&&                                                                             
CLI_IWRK DS    CL2                 Workcode                                     
CLI_IPYD DS    XL2                 Payment date (compressed)                    
CLI_IPYR DS    CL6                 Payment ref                                  
CLI_IPTO DS    PL6                 Payment amount                               
CLI_INAR DS    CL(L'CIDNARR)       Original narrative                           
CLI_FNAR DS    CL(L'CIDNARR)       Finance narrative                            
CLI_RLNQ EQU   *-CLI_D                                                          
CLI_ILNQ EQU   *-CLI_IFST          Basic record length                          
                                                                                
***********************************************************************         
* Distance rate buffer DSECT                                          *         
***********************************************************************         
                                                                                
DIS_D    DSECT                                                                  
DIS_LEN  DS    XL2                                                              
DIS_KEY  DS    0X                                                               
DIS_DBDC DS    CL2                 Distance band code                           
DIS_KEYL EQU   *-CLM_KEY           Key length                                   
DIS_DISB DS    PL6                 Distance under this band                     
DIS_DBDN DS    CL50                Distance band name                           
DIS_DBDA DS    PL6                 Distance band amount limit                   
DIS_GRSR DS    PL6                 Distance gross rate                          
DIS_VATR DS    PL6                 Distance vat rate                            
DIS_PASR DS    PL6                 Distance passenger rate                      
DIS_TOTA DS    PL6                 Distance total amount for this band          
DIS_VATA DS    PL6                 Distance total vat amount for band           
DIS_LNQ  EQU   *-DIS_D                                                          
                                                                                
***********************************************************************         
* Audit comment DSECT                                                 *         
***********************************************************************         
                                                                                
COMTTABD DSECT                                                                  
COMTDIC  DS    S                   Dictionary entry                             
COMTSTA1 DS    XL1                 Status byte 1 bits - see STCXSTA1            
COMTSTA2 DS    XL1                 Status byte 2 bits - see STCXSTA2            
COMTSTA3 DS    XL1                 Status byte 3 bits - see STCXSTA3            
COMTSTA4 DS    XL1                 Status byte 4 bits - see STCXSTA4            
COMTSTA5 DS    XL1                 Status byte 5 bits - see STCXSTA5            
         DS    XL1                 Spare                                        
COMTTABL EQU   *-COMTTABD                                                       
                                                                                
*&&US                                                                           
*&&                                                                             
***********************************************************************         
* included books                                                      *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
*&&UK                                                                           
       ++INCLUDE GEGENCUR                                                       
*&&                                                                             
*&&US                                                                           
       ++INCLUDE ACCATCALLD                                                     
*&&                                                                             
       ++INCLUDE GEGENGEN                                                       
       ++INCLUDE GEGENFEE                                                       
       ++INCLUDE GEGENEXC                                                       
       ++INCLUDE GEGENXLI                                                       
       ++INCLUDE DDDDEQUS                                                       
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACBRA15   01/28/21'                                      
         END                                                                    
