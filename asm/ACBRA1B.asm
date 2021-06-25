*          DATA SET ACBRA1B    AT LEVEL 076 AS OF 02/04/21                      
*PHASE T6241BA                                                                  
                                                                                
ACBRA1B  TITLE '- BrandOcean eTime Upload Server'                               
                                                                                
***********************************************************************         
* General registers used throughout:-                                 *         
*                                                                     *         
* R7=A(TSAR time record)       - covers TT_D                          *         
* R8=A(Saved working storage)  - covers SAVED                         *         
* R9=A(Global working storage) - covers WORKD                         *         
* RA=A(Global literal values)  - covers GLOBALS                       *         
*                                                                     *         
* R5 generally points to LP_D - the transaction routines use this     *         
* register as a work register                                         *         
*                                                                     *         
* Note that the uploads can run off-line.  In this case the on-line   *         
* validation routines pass pre-validated blocks of information in     *         
* the worker file records.  This means that the off-line instance     *         
* does not have to re-validate or look-up values - it should only be  *         
* concerned with either creating a FACWRK recovery file or updating   *         
* the file directly (if the file is opened as a global file). Passed  *         
* blocks have record map numbers 64 and greater.                      *         
*                                                                     *         
* Level change comments                                               *         
* ---------------------                                               *         
* UK Levels                                                           *         
* ---------                                                           *         
* NSHE 25SEP08 002 Add new opt maint setting to not allow job input   *         
* NSHE 23OCT08 004 New layout for job, backup and non client approver *         
* YNGX 22JAN09 004 <LO01-8552> Bug fix building 1C/14 c/a correctly   *         
* JFOS 27FEB09 005 <BR23494L> look for SPATINCO on 1R for SK override *         
* JFOS 03Mar09 006 look for SPATINCO on 1R for SK override            *         
* SMAN 04MAR09 004 <BR23596L> Do not overshoot GENAREAX-255           *         
* YNGX 25MAR09 005 <BR13983D> Bug fix building APEELD correctly       *         
* NSHE 23MAR09 006 Change of audit record                             *         
* NSHE 21MAY09 006 Add sciel to contain either cost or sales amount   *         
* MPEN 20MAR09 007 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT BY OFF *         
* TFRY 07JUL09 007 Add timesheet MOA to stcel                         *         
* NSHE 19SEP09 009 Deal with media and office approvers               *         
* NSHE 22OCT09 010 Add submitted date to time records                 *         
* SMAN 04NOV09 011 Change to an error msg                             *         
* NSHE 27MAY10 014 Change to lock key definition                      *         
* JFOS 28MAY10     Include job code in TIMLOCK error message          *         
* NSHE 19AUG10 018 Add adjustment setting for US                      *         
* NSHE             Fix rejection logic when not posting               *         
* NRAK             OILL change R0-GR0 - looks wrong though...         *         
* YNGX 28OCT09 018 <BR16779D> Bug fix reading department passives     *         
* NRAK 19JAN11 019 <BR39221L> Bug previous level - bad length         *         
* NRAK 25FEB11 020 <BR17556D> Existing audit for 'new' timesheet      *         
* SMAN 01APR11 021 <BR40855L> Check agency is on offices for N-time   *         
* NRAK 04MAY11 022 <BR17910D> SUPPORT MULITPLE APPRECS IN GETMAP      *         
* MPEN 04MAY11 023 <PR000927> version for build 31                    *         
* SMAN 10AUG11 025 <BR18414D> Re-establish sequence to read APPRECDs  *         
* NSHE 19OCT11 026 Fixes made for US                                  *         
* NSHE 15NOV11 027 Use default time for resource management           *         
* NSHE 30NOV11     Use sorter to buffer records offline               *         
* NSHE 29MAR12 028 Add further validation for resource management     *         
* NSHE 04MAY12 029 Move IO areas used to allow for bigger approver tab*         
* NSHE 20JUN12 030 Fix for future time                                *         
* MPEN 20AUG12 031 <BR20147D> Don't dump if ERRTAB full+handle big t/s*         
* NSHE 31AUG12 033 <PR000796> Deal with adjustments by posting        *         
*                  difference if only hours changes                   *         
* JFOS 24MAY13 037 (Untested) fixes for BROOM131,128,122              *         
* NSHE 29MAY13 038 <DSBO-38> Fix to update transaction routine        *         
* NSHE 05JUN13 039 <DSBO-51> Further fix to update transaction routine*         
* NSHE 05JUL13 040 Fix for mobile approvers                           *         
* NSHE 02SEP13 041 <DSST-1> <DSSA-9> Fix to audit and zero hours      *         
* NSHE 21FEB14 042 Strip out zero rows on submit/set app in audit     *         
* MPEN 19FEB14     <DSRD-186> Allow time update to save a template    *         
* JFOS 24JAN14     <DSPCA447> Re-validate+update SI/SK on t/s approve *         
* NSHE 03APR14     <DSBO-776> Make removing zero hours Aura only      *         
* MPEN 12MAY14 043 <OT79420L> Use RUNNER DDSORTER                     *         
*NRAK 06MAY14  044<DSBO-822> Time type validation fix                *          
*NSHE 08MAY14 045 Fix to ensure missing history is dealt with online *          
*NRAK 28MAY14     <DSBO-860> update MOA on delete                               
*NRAK 18JUN14     <DSBO-997> handle blanks in valwcd                            
*MPEN 19JUN14 047 <DSPCA-1016> Fix for lvl 42 to only do it if adjust *         
*NRAK 11DEC14 048 <DSRD-5418> Don't save holidays to template                   
*TKLU 01Sep14 049 <PCA01157> DATCON/15 for post production gap issue  *         
*NSHE 30DEC14 050 <DSRD-4705> Fix narrative validation for Aura       *         
*JFOS 08OCT15 051 <PCA-1989> Support Limit Account Access                       
*MPEN 26APR16 052 <DSRD-11144> Fix for MOA issue                      *         
*NSHE 06Jan14 053 <DSRD-5496> Fix backup approver index number bug    *         
*MPEN 08JAN15     <DSPCA-1241> Fix MOA posting issue                  *         
*NSHE 08JAN15     <DSBO-1386> Fix MOA posting issue for approved time *         
*TKLU 12Oct16 054 <PCA02486>  Pass user ID to BMONVAL                 *         
*TKLU 15Nov16 055 <PCA01157> Adjustments for DATCON/15                *         
*MPEN 19Oct16 056 <DSRD-13791> Pass all approvers for notification    *         
*MPEN 01Dec16 057 <DSRD-14282> Fix for deleting item lines on submit  *         
*MPEN 15Dec16     <DSRD-14365> Fix for backup approvals               *         
*MPEN 05Jan17     <DSRD-14467> Fix for deleting item lines on submit  *         
*MPEN 23Jan17 058 <DSRD-14692> Fix for deleting item lines on submit  *         
*MPEN 23Mar17 059 <DSRD-15169> Check est provided if compulsory       *         
*NSHE 11May17 060 <DSRD-15651> Ensure office gets set for empty ts    *         
*MPEN 30Jun17 061 <DSRD-14836> Fix for self approved status           *         
*MPEN 12Jul17     <DSRD-15589> Extend timeline narrative to 200 chars *         
*NSHE 18Aug17 062 <DSRD-16642> Give error message is no time type     *         
*                 sent for mobile upload calls + GroupM fix US        *         
*NSHE 05Mar18 063 DSRD-18379 Deal with fiscal start month correctly   *         
*NSHE 04Apr18 064 DSRD-18595 Give user error when template too big    *         
*NSHE 15Jun18 065 DSRD-19339 Fix disappearing time line                         
*MPEN 13Jul18 066 DSRD-18406 Fix for adjust updating all lines        *         
*MPEN 23Oct18 067 DSRD-19562 Fix for approval row info on adjust      *         
*MPEN 30Nov18 068 DSRD-20853 Time approver green tick                 *         
*MPEN 24Apr19 069 DSRD-22197 Fix for reset t/s                        *         
*MPEN 26Apr19 070 DSRD-22248 Store timeoff id                         *         
*MPEN 20Jun19     DSRD-22909 Improve locked from t/s error handling   *         
*ABID 01OCT19 071 DSRD-23960 LOCK 1R ACCOUNT IN OFFLINE PASS TO AVOID           
*                 DSRD-23960 DUPLICATE CPRRECD RECORD                           
*ABID 01OCT19 071 SPEC-39198 FIX RUNNER FULIURE WHEN READING ARCHIVED           
*                 SPEC-39198 TRANSACTIONS                                       
*MPEN 29OCT19 072 DSRD-24181 Store new timeoff application bit        *         
*MPEN 24APR20 073 DSRD-26093 Fix for office checking                            
*NRAK 06May20     SPEC-45730 skip srupd60 if SORTER empty                       
*NRAK 02Jul20 074 SPEC-47666 Validate MOA on all rows.                          
*MPEN 06Aug20     DSRD-27177 Time comments by day                               
*MPEN 13Oct20 075 DSRD-27689 Capture day narrative audit                        
*MPEN 12Nov20     DSRD-27923 Fix for day narrative                              
*NSHE 11Dec20 076 DSRD-27923 further fixes for day narrative                    
*                                                                               
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,CODE=ENTRY,RLEN=2000,REQUEST=*,WORKERKEY=BOTU,  +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ,         +        
               FILES=FILES,SERVERTYPE=TSTBOTU,SYSPHASE=SYSPHASE,IDF=Y, +        
               PROGRAM=RCVPBRAQ                                                 
         EJECT                                                                  
ENTRY    NMOD1 0,**BO1B**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         BNZ   ENTRY02                                                          
         L     R9,LP_ABLK1                                                      
         ICM   R8,15,RSVRSAVE      R8=A(Save area)                              
         B     ENTRY04                                                          
                                                                                
ENTRY02  L     R9,RSVRSAVE                                                      
         ST    R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(Global w/s)                             
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(Save w/s)                               
         MVC   MASTC,RMASTC        Set A(MASTC)                                 
         MVC   VSORTER,RSORTER                                                  
                                                                                
ENTRY04  ST    R8,LP_ABLK2                                                      
         MVC   WRKBLKR,RWRKBLKR    Set A(FACWRK WRKIO block)                    
         ST    RE,SRVRRELO         Save program relocation factor               
         MVC   RUNMODE,RUNPMODE    Set calling mode                             
         DROP  R6,R7                                                            
                                                                                
         LA    R7,TT_CUR                                                        
         USING TT_D,R7             R7=A(Current time buffer record)             
OLD      USING TT_D,TT_OLD         Old time record                              
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
***********************************************************************         
* Handle DDLINK/RUNNER modes                                          *         
***********************************************************************         
                                                                                
         CLI   RUNMODE,RRUNSTRQ    Test 'First for run' mode                    
         BE    SRUN                                                             
         CLI   RUNMODE,RINIREQQ    Test 'Initialise' mode                       
         BE    SREQ                                                             
         CLI   RUNMODE,RVALREQQ    Test 'Validate record' mode                  
         BE    UPLD                                                             
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         BE    UPLD                                                             
         CLI   RUNMODE,RRUNENDQ    Test 'Last for request' mode                 
         JE    EREQ                                                             
         J     EXITY               Ignore any other modes                       
         EJECT                                                                  
***********************************************************************         
* Handle 'First for run' (once only off-line) mode                    *         
***********************************************************************         
                                                                                
SRUN     LA    R0,SAVED            Clear sacred storage                         
         LHI   R1,SAVEVAR-SAVED                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,LP_ACOM                                                       
         MVC   DATAMGR,CDATAMGR-COMFACSD(RE)                                    
         MVC   PROTOFF,CPROTOFF-COMFACSD(RE)                                    
         MVC   PROTON,CPROTON-COMFACSD(RE)                                      
         MVC   LOCKET,CLOCKET-COMFACSD(RE)                                      
         MVC   XDATAMGR,VDATAMGR   Point to real DATAMGR entry point            
         MVC   IDATAMGR,VDATAMGR                                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
         L     RF,LP_ACOM          Load subsidiary phases & initialise          
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)        Set A(Routine overlay 1)                     
         MVC   LP_AUIR1,AROUT1                                                  
         GOTOR (RF),(R1),('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)        Set A(Routine overlay 2)                     
         MVC   LP_AUIR2,AROUT2                                                  
         GOTOR (RF),(R1),0,(C'E',SRUPD60)                                       
         MVC   ASRUPD60,0(R1)      Set A(SRUPD60)                               
         LA    R0,LP_D                                                          
         ST    R0,ALP              Set A(LP_D) in global w/s                    
         MVC   ACOMFACS,LP_ACOM    Set A(COMFACS)                               
         MVC   ATWA,LP_ATWA        Set A(TWA)                                   
         GOTOR (#WRKINI,AWRKINI)   Initialise working storage                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Handle 'First for request' (before first upload record) mode        *         
***********************************************************************         
                                                                                
SREQ     LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   TWAMODE,0           Set no errors encountered                    
                                                                                
         L     RF,LP_ACOM          Extract A(LINKIO) from COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
         MVC   AALIOB,LP_ALIOB     Set A(LIOB)                                  
                                                                                
         NI    RUNINDS,FF-RUNIPUTF Set first time for PUTOUT                    
         NI    RUNINDS,FF-RUNITIMF Set first time for TIMTRN                    
         NI    RUNINDS,FF-RUNIATRF Set first time for GOATRN                    
         NI    RUNINDS,FF-RUNIBUFF Set first time for BUFREC                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    SREQ02                                                           
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                                
                                                                                
         L     RF,MASTC            Set I/O trace option                         
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
SREQ02   GOTOR PROTOFF             Turn off storage protection                  
         LA    R0,LP_D                                                          
         ST    R0,LLP              Save A(LP_D) locally                         
         GOTOR PROTON              Turn on storage protection                   
                                                                                
         MVC   ACOMFACS,LP_ACOM    Point to real copy of COMFACS                
                                                                                
         MVI   GIND2,GI2ETIM       Set want 1R values returned                  
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         USING CPYELD,SCPYEL                                                    
         USING CPXELD,SCPXEL                                                    
                                                                                
         GOTOR INIBUF              Initialise record buffer                     
                                                                                
         TM    LP_FLAG,LP_FDRFT    Test validation mode                         
         JZ    SREQ04                                                           
         GOTOR (#SETFAC,ASETFAC),'BRO#TIME'                                     
         J     EXITY                                                            
                                                                                
SREQ04   LA    R0,COMFACS          Take local copy of COMFACS                   
         LHI   R1,COMFACSL                                                      
         L     RE,LP_ACOM                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RF,COMFACS                                                       
         ST    RF,ACOMFACS         Set A(COMFACS) as local copy                 
         LARL  R0,DMGRITRN                                                      
         ST    R0,IDATAMGR         Set internal DATAMGR entry point             
         LARL  R0,DMGRXTRN                                                      
         ST    R0,XDATAMGR         Set external DATAMGR entry point             
         MVC   VDATAMGR,IDATAMGR   Set addresses for callers                    
         MVC   CDATAMGR-COMFACSD(,RF),XDATAMGR                                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Handle 'Run request' (upload a record) mode                         *         
***********************************************************************         
                                                                                
UPLD     LA    RF,RECTAB                                                        
         USING RECTABD,RF          RF=A(Record map table)                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         BE    *+12                                                             
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         MVC   RECTYPE,RECTTYPE    Set internal record type                     
         DROP  RF                                                               
                                                                                
         MVI   ERRTAB,ET_EOTQ      Clear error table                            
         MVC   ERRTXT,SPACES       Clear error text                             
                                                                                
         CLI   RECTYPE,RECTHDRQ    Test header record                           
         JE    UHDR                                                             
         CLI   RECTYPE,RECTTIMQ    Test Time Line                               
         JE    UTIM                                                             
         CLI   RECTYPE,RECTMATQ    Test Materials Line                          
         JE    UMAT                                                             
         CLI   RECTYPE,RECTDNRQ    Test Materials Line                          
         JE    UDNR                                                             
         CLI   RECTYPE,RECTTRLQ    Test trailer record                          
         JE    UTRL                                                             
         DC    H'0'                Invalid record                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Process time upload header record                                   *         
***********************************************************************         
                                                                                
UHDR     ZAP   TOTHOURS,PZERO      Initialise total hours                       
                                                                                
         GOTOR BUFTIM,DMCB,('TSAINI',OLDBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         NI    RUNINDS,FF-RUNIUPD  Set first time for UPDATE                    
                                                                                
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UHDR0010                                                         
         ICM   RE,15,QH_AHEAD      Test header values passed                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,HD_VALS                                                       
         LHI   R1,HD_VALSL                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    HD_IND,FW_INOPR     Test any previous time records found         
         JNZ   UHDR0200            No                                           
         GOTOR OLDTIM              Build old time buffer                        
         JE    UHDR0200                                                         
         DC    H'0'                                                             
                                                                                
UHDR0010 LA    R0,HD_VALS          Initialise header values                     
         LHI   R1,HD_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,TR_VALS          Initialise trailer values                    
         LHI   R1,TR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   HD_PEDT,QH_PEDT     Set period end date                          
         MVC   HD_1RPER,QH_1RPER   Set person code                              
*&&UK                                                                           
         CLI   CUTSYS,X'73'                                                     
         JNE   UHDR0020                                                         
         MVC   HD_TODP,=X'B40101'  Set this date for                            
         MVC   HD_TODP5,HD_TODP                                                 
         CLC   CUAALF,=C'BA'       QAROD*, QFROD*, QMROD*                       
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'BB'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'BC'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'AD'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'AA'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'AB'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'AC'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'CA'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'CB'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'CC'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'A1'                                                    
         JE    UHDR0020                                                         
         CLC   CUAALF,=C'Y9'                                                    
         JE    UHDR0020                                                         
         MVC   HD_TODP,=X'B60101'  Set this date for                            
         CLC   CUAALF,=C'16'                                                    
         JE    UHDR0020                                                         
         MVC   HD_TODP,=X'A60701'                                               
         MVC   HD_TODP5,HD_TODP                                                 
UHDR0015 GOTOR VDATCON,DMCB,(1,HD_TODP),(2,HD_TODC)                             
         GOTOR VDATCON,DMCB,(1,HD_TODP),(0,HD_TODF)                             
         J     UHDR0030                                                         
*&&                                                                             
UHDR0020 GOTOR VDATCON,DMCB,(15,0),(1,HD_TODP)                                  
         GOTOR (RF),(R1),(15,0),(2,HD_TODC)                                     
         GOTOR (RF),(R1),(15,0),(0,HD_TODF)                                     
         GOTOR VDATCON,DMCB,(5,0),(1,HD_TODP5)                                  
UHDR0030 SR    R0,R0                                                            
         ICM   R0,7,HD_PEDT        Period end                                   
         LNR   R0,R0                                                            
         STCM  R0,7,HD_PEDTC       Period end complemented                      
                                                                                
         ICM   RE,7,QH_ADAT        Initialise date/MOA table                    
         JZ    *+2                                                              
         MVC   HD_DATEN,LW_NUMN+1-LW_D(RE)                                      
         LA    R2,LW_DATA2-LW_D(RE)                                             
         MVC   HD_PSTR,0(R2)                                                    
         MVC   HD_PEND,HD_PSTR                                                  
         LLC   R0,HD_DATEN         R0=Number of dates                           
         LA    R4,HD_DMOA                                                       
         USING HD_DMOA,R4                                                       
UHDR0040 MVC   HD_DMDAT,0(R2)      Set date (CHKMOA sets MOA)                   
         AHI   R2,L'HD_DMDAT       Bump date pointer                            
         AHI   R4,HD_DMOAL         Bump array pointer                           
         JCT   R0,UHDR0040         Do for number of dates                       
         DROP  R4                                                               
                                                                                
         GOTOR GETPER              Read person record                           
         JE    UHDR0050                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RPER,HD_1RPER)                        
         J     UFATAL                                                           
                                                                                
UHDR0050 GOTOR DOLOCK              Lock the PID now                             
         JE    UHDR0055                                                         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UFATAL                                                           
UHDR0055 GOTOR SETEDT              Set period end dates                         
                                                                                
         GOTOR BLDPER,DMCB,HD_1RULA,HD_1ROFF,HD_1RDEP,HD_1RSUB,HD_1RPER         
                                                                                
         CLI   LP_ACCS,0           Skip if no limit access                      
         JE    UHDR0060                                                         
                                                                                
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,HD_1ROFF   Move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL                                                           
         JE    UHDR0060                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1ROFF,HD_1ROFF)                        
         J     UFATAL                                                           
         DROP  R1                                                               
                                                                                
UHDR0060 SR    RE,RE                                                            
         ICM   RE,1,CPYSFST                                                     
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   HD_CSTR,HD_PEDT                                                  
         MVC   HD_CSTR+1(1),BYTE1                                               
         CLC   HD_PEDT+1(1),BYTE1                                               
         JNL   UHDR0070                                                         
                                                                                
         GOTOR VDATCON,DMCB,(1,HD_CSTR),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,HD_CSTR)                              
                                                                                
UHDR0070 GOTOR VDATCON,DMCB,(1,HD_CSTR),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'M',WORK),WORK+6,11                                
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,HD_CEND)                              
                                                                                
K        USING CASKEY,IOKEY                                                     
         XC    K.CASKEY,K.CASKEY   Read calendar for office                     
         MVI   K.CASKTYP,CASKTYPQ                                               
         MVI   K.CASKSUB,CASKSUBQ                                               
         MVC   K.CASKCPY,CUXCPY                                                 
         MVC   K.CASKEMOA,HD_CEND                                               
         MVC   K.CASKSMOA,HD_CSTR                                               
         MVC   K.CASKOFC,HD_1ROFF  Set person office code                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    UHDR0080                                                         
                                                                                
         MVC   K.CASKEY,IOKEYSAV   Read all office calendar                     
         MVC   K.CASKOFC,SPACES                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   UHDR0090                                                         
         DROP  K                                                                
                                                                                
UHDR0080 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    UHDR0100                                                         
                                                                                
UHDR0090 MVC   ROUERRV,=AL2(AE$NOCAL)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UFATAL                                                           
                                                                                
UHDR0100 L     R2,IOADDR                                                        
         AHI   R2,CASRFST-CASRECD                                               
         USING TMPELD,R2                                                        
         SR    R0,R0                                                            
UHDR0110 CLI   TMPEL,0             Locate period element in calendar            
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TMPEL,TMPELQ                                                     
         JNE   *+14                                                             
         CLC   TMPEND,HD_PEDT      Match period end date                        
         JE    *+14                                                             
         IC    R0,TMPLN                                                         
         AR    R2,R0                                                            
         J     UHDR0110                                                         
                                                                                
         MVC   HD_PERNO,TMPNUMB    Set period number                            
         MVC   HD_MOA,TMPMTH       Set month                                    
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Resource         
         CHI   RF,XPRESMAQ          management is connected                     
         JNE   UHDR0115                                                         
         L     R3,ALOCEL                                                        
         USING LOCELD,R3                                                        
         CLC   LOCSTART,TMPSTART   Does period contain split location           
         JNH   UHDR0115            No                                           
         MVC   ROUERRV,=AL2(AE$SPINV)  Yes - error                              
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UFATAL                                                           
         DROP  R2,R3                                                            
                                                                                
UHDR0115 MVC   HD_1RNAM,SPACES     Pre-clear 1R/14 names                        
         MVC   HD_1RCST,SPACES                                                  
         MVC   HD_142NM,SPACES                                                  
         MVC   HD_143NM,SPACES                                                  
         MVC   HD_INULA,SPACES                                                  
         MVI   HD_1RFJT,NOQ        Set force job is no                          
         MVI   HD_1RFPT,NOQ        Set force product is no                      
         GOTOR GETTSN              Get current time sheet number                
         JE    UHDR0120                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RULA,HD_1RULA)                        
                                                                                
UHDR0120 GOTOR GETODS              Look at higher levels of 1R                  
         JE    UHDR0130            to establish names/costing group             
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RULA,HD_1RULA)                        
                                                                                
UHDR0130 L     R2,ACOBLOCK         Get cost allocation profiles                 
         USING COBLOCKD,R2                                                      
         XC    COBLOCKD(COPTIONS-COBLOCKD),COBLOCKD                             
         MVC   COADM,XDATAMGR                                                   
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CUXCPY                                                    
         MVC   COKOFC,HD_1ROFF                                                  
         MVC   COKDPT,HD_1RDEP                                                  
         MVC   COKSDT,HD_1RSUB                                                  
         MVC   COKPER,HD_1RPER                                                  
         GOTOR VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,0                                                       
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   HD_COATM,COATM      Extract option values we need                
         MVC   HD_CODTM,CODTM      Default type of time                         
         MVC   HD_COBRT,COBRTE     Post sales rate not cost for B time          
         MVC   HD_CORRT,CORRTE     Post sales rate not cost for R time          
         MVC   HD_COTUP,COTUP      When is time posted                          
         MVC   HD_COACS,COACS      Approval concurrently or sequential          
         MVC   HD_COFNR,COFNR      Force narrative                              
         MVC   HD_COMAT,COMAT      Materials allowed                            
         MVC   HD_COIOS,COTBTL     Income or suspense for B time                
         MVC   HD_CONJB,CONJB      No job input allowed                         
         MVC   HD_CONDA,CONDA      Number of days allowed in future             
         MVC   HD_COFAP,COFAP      Use account setting for future time          
         MVC   HD_COFTA,COFTA      Type of time future time allowed             
         ZAP   DUB1,CONDO                                                       
         CVB   R4,DUB1                                                          
         LNR   R4,R4                                                            
         MVC   WORK(L'HD_TODF),HD_TODF  Work out Overdue date                   
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R4)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,HD_OVRDU)                             
         DROP  R2                                                               
                                                                                
         GOTOR GETMAP,HD_1RACT     Get approver and back-up PIDs                
         JE    UHDR0140                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACT,HD_1RACT)                        
                                                                                
UHDR0140 CLI   QH_STAT,QH_SDELQ    Test deleting time sheet                     
         JNE   *+8                                                              
         MVI   TR_TSSTN,TIMSDELT   Set deleted time sheet                       
                                                                                
         CLI   QH_STAT,QH_SSAVQ    Test saving                                  
         JE    UHDR0150                                                         
         CLC   HD_PPID#,HD_MANAP   Is line manager approver for user?           
         JNE   UHDR0150                                                         
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JE    UHDR0170                                                         
                                                                                
UHDR0150 CLC   HD_PPID#,CCTPID     Is it user?                                  
         JE    UHDR0180                                                         
         CLC   HD_MANAP,CCTPID     Is it line manager?                          
         JE    UHDR0160                                                         
         CLI   QH_BUAPR,C'Y'       Back up approver?                            
         JNE   UHDR0180                                                         
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,HD_MANBA         Search backups approvers for match           
UHDR0155 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    UHDR0180                                                         
         CLC   CCTPID,0(R1)        Is line manager a back up approver?          
         JE    UHDR0160                                                         
         LA    R1,L'PIDNO(R1)                                                   
         JCT   R0,UHDR0155                                                      
         J     UHDR0180                                                         
                                                                                
UHDR0160 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    UHDR0170                                                         
         CLI   QH_STAT,QH_SREJQ    or rejecting?                                
         JNE   UHDR0180                                                         
UHDR0170 OI    TR_IND,TR_ILNMG     Set line manager approving/rejecting         
                                                                                
UHDR0180 GOTOR CHKMOA              Check MOA                                    
         JE    UHDR0190                                                         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
UHDR0190 GOTOR OLDTIM              Build old time buffer                        
         JE    UHDR0200                                                         
         OI    HD_IND,FW_INOPR     Set no previous time records found           
         CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    *+12                                                             
         CLI   QH_STAT,QH_SREJQ    or rejecting?                                
         JNE   UHDR0200                                                         
         GOTOR SAVERR,DMCB,=AL2(AE$TSNEX),0                                     
         J     UFATAL                                                           
                                                                                
UHDR0200 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UHDR0210                                                         
                                                                                
         CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    *+12                                                             
         CLI   QH_STAT,QH_SREJQ    or rejecting?                                
         JNE   *+8                                                              
         OI    HD_IND,FW_APRQ      Set Approval Mode                            
                                                                                
*&&US                                                                           
         TM    HD_TSSTO,TIMSFAPP   Time sheet was fully approved                
         JZ    UHDR0205                                                         
         CLI   TR_TSSTN,TIMSDELT   Are we trying to Delete it?                  
         JNE   UHDR0205                                                         
         MVC   ROUERRV,=AL2(AE$DEAPP)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
*&&                                                                             
UHDR0205 L     R2,AAPPTAB          Initialise approver table                    
         USING APPTABD,R2                                                       
         MVI   APPRLN,APPKLNQ                                                   
         MVI   APPRTYP,APPRTMAN    Build manager entry in table                 
         MVC   APPRPID,HD_MANAP                                                 
         AHI   R2,APPKLNQ                                                       
         MVI   APPRLN,0                                                         
         ST    R2,AAPPTN           Set A(Next approver entry)                   
         DROP  R2                                                               
                                                                                
UHDR0210 GOTOR BUFTIM,DMCB,('TSAINI',NEWBUF)                                    
                                                                                
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
                                                                                
UHDR0220 GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',HD_VALS),HD_VALSL                     
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process time line data - create time buffer record                  *         
***********************************************************************         
                                                                                
UTIM     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (No further processing)                      
                                                                                
         LA    R0,TL_VALS          Clear time line values                       
         LHI   R1,TL_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    QI_VALS(QI_VALSL),QI_VALS                                        
         XC    QD_VALS(QD_VALSL),QD_VALS                                        
         CLI   QH_STAT,QH_SDELQ    Test deleting a row                          
         JE    UEXIT                                                            
         TM    HD_IND,FW_INOPR     Test previous time existed                   
         JNZ   UTIM0010                                                         
         CLI   QT_AMEND,C'Y'       Test changing a line                         
         JE    UTIM0010                                                         
         CLI   QH_TEMPL,QH_TEMPO   force validation if template upload          
         JE    UTIM0010                                                         
         GOTOR CPYONE              No - copy from old buffer                    
         J     UEXIT                                                            
                                                                                
UTIM0010 GOTOR DOTSAR,QX_TSAR      Use TSAR record if passed                    
         JE    UEXIT               (adds TT_D record if true)                   
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
                                                                                
         MVC   TL_12ULA,SPACES     Clear account names                          
         MVC   TL_12NAM,SPACES                                                  
         MVC   TL_CANAM,SPACES                                                  
         MVC   ANYACCNT,SPACES                                                  
         MVC   TL_TTYPE,QT_TTYPE   Set time type                                
         CLI   TL_TTYPE,TL_TEMPQ   Are we dealing with empty row                
         JE    UTIM0040                                                         
                                                                                
         OC    QT_1NACT,QT_1NACT   Test 1N account given                        
         JZ    UTIM0020                                                         
         CLI   TL_TTYPE,TL_TDEFQ   Have we been passed default                  
         JNE   UTIM0015                                                         
         MVI   TL_TTYPE,TL_TNONQ   Yes - set N time                             
UTIM0015 GOTOR VALNCA,QT_1NACT     Validate 1N account                          
         JE    UTIM0020                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QT_1NACT,QT_1NACT)                        
                                                                                
UTIM0020 OC    QT_SJCLI,QT_SJCLI   Test client given                            
         JZ    UTIM0030                                                         
         GOTOR BLDJOB,DMCB,ANYACCNT,QT_SJCLI,QT_SJPRO,QT_SJJOB                  
         GOTOR VALJOB,ANYACCNT                                                  
         JE    UTIM0030                                                         
         OI    TL_ICPJ,TL_IERR     Set found error                              
         GOTOR SAVERR,DMCB,ROUERRV,(L'ANYACCNT,ANYACCNT)                        
                                                                                
UTIM0030 TM    TL_ICPJ,TL_ICLI+TL_INON                                          
         JNZ   UTIM0040                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPRODIKQ          or aura is connected                        
         JE    UTIM0032                                                         
         CHI   RF,XPMOBILQ                                                      
         JNE   UTIM0040                                                         
UTIM0032 MVI   TL_TTYPE,TL_TNONQ   Set some type of time for mobile             
         XR    R3,R3                                                            
         ICM   R3,7,QT_AHRS        R3=A(Uploaded hours)                         
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
UTIM0036 OC    0(L'TL_EHRS1,R3),0(R3)                                           
         JZ    UTIM0038                                                         
         CP    0(L'TL_EHRS1,R3),PZERO                                           
         JE    UTIM0038                                                         
         MVC   ROUERRV,=AL2(AE$MIACC)  Don't allow hours and no account         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UTIM0040                                                         
UTIM0038 AHI   R3,L'TL_EHRS1       Bump hour pointer                            
         JCT   R0,UTIM0036         Do for number of dates                       
                                                                                
UTIM0040 MVC   TL_ULA,ANYACCNT     Set SJ/1N account code                       
         MVC   SJOFFC,TL_SJOFF                                                  
         TM    CPYSTAT1,CPYSOROE                                                
         JNZ   *+10                                                             
         MVC   TL_SJOFF,SPACES                                                  
                                                                                
         GOTOR VALWCD,QT_TASK      Validate work code                           
         JE    UTIM0045                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QT_TASK,QT_TASK)                          
                                                                                
UTIM0045 TM    TL_ICPJ,TL_ICLI     Test production ledger                       
         JZ    UTIM0046                                                         
         TM    TL_ICPJ,TL_IERR     Any error in client validation               
         JNZ   UTIM0046            Yes - don't call get opt                     
         GOTOR VALINC              Yes - validate income account                
         JE    UTIM0046                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'TL_INULA,ERRTXT)                          
                                                                                
UTIM0046 TM    TL_ICPJ,TL_IJOB     Test job level                               
         JZ    UTIM0050                                                         
         L     RF,ACOBLOCK         Get cost allocation profiles                 
         CLI   COEST-COBLOCKD(RF),C'C' Check estimate number compulsory         
         JNE   UTIM0050                                                         
         CLC   QT_EST#,SPACES                                                   
         JH    UTIM0050                                                         
         MVC   ROUERRV,=AL2(AE$ESTNC)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
UTIM0050 CLI   QH_TEMPL,QH_TEMPO   Are we uploading a template?                 
         JE    UTIM0086            Skip hours check on template                 
         LLC   R6,HD_DATEN         R6=Number of dates                           
         ICM   R3,7,QT_AHRS        R3=A(Uploaded hours)                         
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
         LA    R4,TL_HDMA                                                       
DTE      USING TL_HDMA,R4          R4=A(Hours/date array)                       
         LA    R2,HD_DMOA                                                       
MOA      USING HD_DMOA,R2          R2=A(Date/MOA array)                         
UTIM0060 ZAP   DTE.TL_EHRS1,PZERO      Clear hours                              
         MVC   DTE.TL_ETDT1,MOA.HD_DMDAT   Set date                             
         LTR   R0,R0               Test any hours remaining                     
         JZ    UTIM0070                                                         
         BCTR  R0,0                Decrement number of hours                    
         OC    0(L'TL_EHRS1,R3),0(R3)                                           
         JZ    UTIM0070                                                         
         CP    0(L'TL_EHRS1,R3),PZERO                                           
         JE    UTIM0070                                                         
         MVC   DTE.TL_EHRS1,0(R3)      Set hours                                
         MVC   TL_HIDTE,MOA.HD_DMDAT   Set highest date in time line            
*&&US*&& AP    MOA.HD_EDHRS,DTE.TL_EHRS1                                        
         CP    DTE.TL_EHRS1,MAXHOURS   Test vs. maximum allowed                 
         JNH   UTIM0070                                                         
         ZAP   DUB,DTE.TL_EHRS1        Can't use TL_EHRS1 in CURED              
         CURED DUB,(10,ERRTXT),2,ZERO=YES,ALIGN=LEFT                            
         MVC   ROUERRV,=AL2(AE$TMHRS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,((R0),ERRTXT)                                
UTIM0070 AHI   R2,HD_DMOAL         Bump date/MOA pointer                        
         AHI   R3,L'TL_EHRS1       Bump hour pointer                            
         AHI   R4,TL_HDMNL         Bump array pointer                           
         JCT   R6,UTIM0060         Do for number of dates                       
         DROP  MOA,DTE                                                          
                                                                                
         CLI   TL_TTYPE,TL_TEMPQ   Are we dealing with empty row                
         JE    UTIM0130                                                         
         OC    TL_HIDTE,TL_HIDTE                                                
         JZ    UTIM0086                                                         
         GOTOR VDATCON,DMCB,(1,TL_HIDTE),(2,TL_CHIDT)                           
*&&UK*&& GOTO1 VPERVERT,DMCB,HD_TODC,TL_CHIDT,C'CMPR'                           
*&&US                                                                           
         GOTOR VDATCON,DMCB,(2,HD_TODC),(0,WORK)                                
         GOTOR VDATCON,DMCB,(2,TL_CHIDT),(0,WORK+6)                             
         GOTO1 VPERVERT,DMCB,WORK,WORK+6,C'CMPR'                                
*&&                                                                             
         LH    R1,DMCB+8           R1=number of days ahead                      
         LTR   R1,R1               Is highest date before today                 
         JM    UTIM0086            Yes                                          
         CHI   R1,1                Is it the same day                           
         JE    UTIM0086            Yes                                          
*                                                                               
         LA    R2,HD_COFTA                                                      
         LA    R3,L'HD_COFTA                                                    
         USING FUTTABD,R6                                                       
UTIM0076 LA    R6,FUTTAB                                                        
         CLI   0(R2),C' '          Have we reached end of profile               
         JL    UTIM0084                                                         
*                                                                               
UTIM0078 CLC   0(L'FUTPROF,R2),0(R6) Do we have a match between the             
         JNE   UTIM0080            profile and table                            
         CLC   TL_TTYPE,FUTTYPE    Does this match the type of time             
         JNE   UTIM0080                                                         
         LLC   R4,FUTICPJ          Does the non client or client match          
         BASR  RF,0                                                             
         TM    TL_ICPJ,0                                                        
         EX    R4,0(RF)                                                         
         JNZ   UTIM0082            Match allow                                  
UTIM0080 LA    R6,FUTTABL(R6)      No match go to next entry in table           
         CLI   0(R6),X'FF'         Have we reached end of table                 
         JNE   UTIM0078            No                                           
         LA    R2,L'FUTPROF*2(R2)  Yes - go to next profile value               
         JCT   R3,UTIM0076         Any profiles left                            
         J     UTIM0084            No - error                                   
         DROP  R6                                                               
*                                                                               
UTIM0082 ZAP   DUB,HD_CONDA                                                     
         CVB   RF,DUB              RF=Number of days allowed in future          
         CR    R1,RF               Does days exceed allowed                     
         JH    UTIM0084            Yes                                          
         CLI   HD_COFAP,YESQ       Are we checking account setting              
         JNE   UTIM0086            No                                           
         TM    TL_ICPJ,TL_INON     Only applicable to 1N time                   
         JZ    UTIM0086            Not 1N so not interested                     
         TM    TL_RST7,RSTSFUTM    Is future time allowed                       
         JNZ   UTIM0086            Yes                                          
         MVC   ROUERRV,=AL2(AE$NOFUT)                                           
         J     *+10                                                             
UTIM0084 MVC   ROUERRV,=AL2(AE$DTFIF)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
UTIM0086 GOTOR CHKTTY,TL_TTYPE     Check time type is OK for locations          
         JE    UTIM0096                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACT,HD_1RACT)                        
                                                                                
UTIM0096 CLI   QH_TEMPL,QH_TEMPO   Are we uploading a template?                 
         JE    UTIM0110            Skip narrative check on template             
         OC    QX_DESCI,QX_DESCI   Test any narrative entered                   
         JNZ   UTIM0110                                                         
         TM    TL_ICPJ,TL_ICLI+TL_INON  Test production ledger                  
         JZ    UTIM0110                                                         
                                                                                
         LA    R1,TL_TFNAR         Test narrative required for time             
         LHI   R0,L'TL_TFNAR                                                    
UTIM0098 CLC   TL_TTYPE,0(R1)                                                   
         JE    UTIM0100                                                         
         AHI   R1,1                                                             
         JCT   R0,UTIM0098                                                      
                                                                                
         CLI   HD_COFNR,C'Y'       Test forcing narrative                       
         JNE   UTIM0110                                                         
                                                                                
UTIM0100 MVC   ROUERRV,=AL2(AE$NARRQ)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
UTIM0110 DS    0H                  Don't save holiday 1Ns to template           
         CLI   QH_TEMPL,QH_TEMPO                                                
         JNE   UTIM0130                                                         
         TM    TL_ICPJ,TL_1NHOL                                                 
         JNZ   UEXIT                                                            
*                                                                               
UTIM0130 GOTOR NEWTIM              Add record to TSAR buffer                    
         JE    UEXIT                                                            
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process material line data - add dummy buffer records               *         
***********************************************************************         
                                                                                
UMAT     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (no further processing)                      
                                                                                
         CLI   QH_STAT,QH_SDELQ    Are we deleting?                             
         JE    UEXIT                                                            
         TM    HD_IND,FW_INOPR     No previous time existed                     
         JNZ   UMAT0010                                                         
         CLI   QT_AMEND,C'Y'       Test changing an item                        
         JE    UMAT0010                                                         
         GOTOR CPYONE              No - copy from old buffer                    
         J     UEXIT                                                            
                                                                                
UMAT0010 GOTOR DOTSAR,QX_TSAR      Use TSAR record if passed                    
         JE    EXITY               (adds TT_D record if true)                   
                                                                                
         GOTOR NEWTIM              Add record to TSAR buffer                    
         JE    UEXIT                                                            
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process day narrative data - add dummy buffer records               *         
***********************************************************************         
                                                                                
UDNR     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (no further processing)                      
                                                                                
         CLI   QH_STAT,QH_SDELQ    Are we deleting?                             
         JE    UEXIT                                                            
         TM    HD_IND,FW_INOPR     No previous time existed                     
         JNZ   UDNR0010                                                         
         CLI   QT_AMEND,C'Y'       Test changing an item                        
         JE    UDNR0010                                                         
         GOTOR CPYONE              No - copy from old buffer                    
         J     UEXIT                                                            
                                                                                
UDNR0010 GOTOR DOTSAR,QX_TSAR      Use TSAR record if passed                    
         JE    EXITY               (adds TT_D record if true)                   
                                                                                
         GOTOR NEWDNR              Add record to TSAR buffer                    
         JE    UEXIT                                                            
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process trailer record                                              *         
***********************************************************************         
                                                                                
UTRL     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (no further processing)                      
         ZAP   TR_EDHRS,PZERO                                                   
                                                                                
         GOTOR BUFGEN,DMCB,('TSAINI',TIMBUF),0                                  
         JE    *+6                 Initialise timeld buffer                     
         DC    H'0'                                                             
                                                                                
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UTRL0002                                                         
         CLI   QH_TEMPL,QH_TEMPO   Are we uploading a template?                 
         JNE   UTRL0010                                                         
         GOTOR UPDTPL              Yes - validate you can add data              
         JE    UTRL0130                                                         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UTRL0130                                                         
                                                                                
UTRL0002 ICM   RE,15,QT_ATRLR      Test trailer values passed                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,TR_VALS                                                       
         LHI   R1,TR_VALSL                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   QH_TEMPL,QH_TEMPO   Are we uploading a template?                 
         JNE   UTRL0006                                                         
                                                                                
         GOTOR UPDTPL              Update template record                       
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UTRL0006 GOTOR UPDTSN              Unlock locket record offline                 
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   QH_TEMPL,QH_TEMPO   Are we uploading a template?                 
         JE    UTRL0130                                                         
                                                                                
UTRL0010 MVC   TR_INDX,QT_INDEX    Set audit index number                       
         MVC   TR_INDXN,QT_INDEX                                                
         LH    R1,TR_INDX                                                       
         CLI   QH_STAT,QH_SDELQ    Test deleting time sheet                     
         JE    UTRL0030                                                         
         CLI   QH_UTYP,QH_UAPRQ    Approval only?                               
         JNE   UTRL0020            No - must be change                          
         CLI   QH_STAT,QH_SSUBQ    With Aura we can submit from list            
         JE    UTRL0020            so treat as non approval                     
         CLC   CCTPID,HD_MANAP     Yes - is it manager approval?                
         JE    UTRL0020            Yes - index must increment                   
         CLI   QH_BUAPR,C'Y'       Back up approver possible?                   
         JNE   UTRL0030            No                                           
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R2,HD_MANBA         Search backups approvers for match           
UTRL0015 OC    0(L'PIDNO,R2),0(R2) Any Pids left in table?                      
         JZ    UTRL0030                                                         
         CLC   CCTPID,0(R2)        Is line manager a back up approver?          
         JE    UTRL0020                                                         
         LA    R2,L'PIDNO(R2)                                                   
         JCT   R0,UTRL0015                                                      
         J     UTRL0030                                                         
                                                                                
UTRL0020 AHI   R1,1                Increment index number for change            
UTRL0030 STH   R1,TR_INDXN         Set new audit index number                   
                                                                                
         TM    HD_IND,FW_INOPR     No previous time existed                     
         JNZ   UTRL0060                                                         
         CLI   QH_UTYP,QH_UAPRQ    Approval only?                               
         JNE   UTRL0050            No                                           
                                                                                
UTRL0040 GOTOR CPYALL              Copy time buffer 2 to buffer 1               
                                                                                
UTRL0050 GOTOR CHKAUD              Check audit record for index number          
         JE    UTRL0060                                                         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
UTRL0060 TM    TWAMODE,TWAMERP+TWAMEDP                                          
         JNZ   UEXIT                                                            
                                                                                
         CLI   QH_UTYP,QH_UUPDQ    Time update (delete add or change)           
         JNE   UTRL0072            No - just approval/rejection                 
         CLI   QH_STAT,QH_SDELQ    Are we deleting?                             
         JNE   UTRL0070            No                                           
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UTRL0130                                                         
         GOTOR DMGRITRN,DMCB,('FW_TRHDR',$TIMREC)                               
         GOTOR DMGRITRN,DMCB,('FW_TRDEL',$TIMREC)                               
         GOTOR DMGRITRN,DMCB,('FW_TREND',$TIMREC)                               
         J     UTRL0110                                                         
                                                                                
UTRL0070 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UTRL0072                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPMOBILQ         Check mobile app connected                   
         JNE   UTRL0072                                                         
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    UTRL0072            Yes - skip hours check                       
         CP    TOTHOURS,PZERO      Ensure t/s hours > 0                         
         JNZ   UTRL0072                                                         
         MVC   ROUERRV,=AL2(AE$MSHRS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UTRL0080                                                         
                                                                                
UTRL0072 GOTOR REMZHL              Remove zero hour timelines for aura          
         GOTOR DOSTAT              Create time/status elements                  
         TM    RUNINDS,RUNIUPD     Have we made any changes                     
         JNZ   UTRL0074                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ                                                      
         JE    UTRL0074            Allow updates with Aura                      
         MVC   ROUERRV,=AL2(AE$NTGUP)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
                                                                                
UTRL0074 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UTRL0090                                                         
                                                                                
         CLI   QH_UTYP,QH_UAPRQ    Time approval?                               
         JNE   UTRL0076            No - approval skip hours check               
         CLI   QH_STAT,QH_SSUBQ    With Aura we can submit from list            
         JNE   UTRL0080            so only skip for approve/reject              
UTRL0076 TM    CPYSTAT9,CPYSEDHO                                                
         JZ    UTRL0080                                                         
         GOTOR CHKHRS              Check edit hours                             
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    UTRL0080            Yes - skip hours check                       
         CP    TOTHOURS,TR_EDHRS                                                
         JNL   UTRL0080                                                         
         CURED (P3,TR_EDHRS),(10,ERRTXT),2,ZERO=YES,ALIGN=LEFT                  
         MVC   ROUERRV,=AL2(AE$MNHRS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,((R0),ERRTXT)                                
                                                                                
UTRL0080 TM    TWAMODE,TWAMERP+TWAMEDP                                          
         JNZ   UEXIT                                                            
         J     UTRL0130                                                         
                                                                                
UTRL0090 CLI   QH_UTYP,QH_UAPRQ    Time approval?                               
         JNE   UTRL0100            No                                           
         CLI   QH_STAT,QH_SSUBQ    With Aura we can submit from list            
         JE    UTRL0100            so treat as non approval                     
         TM    TR_IND,TR_ICLAT     Yes - any client approval?                   
         JNZ   UTRL0100            Yes                                          
                                                                                
         GOTOR DMGRITRN,DMCB,('FW_TRHDR',$TIMREC)                               
         GOTOR DMGRITRN,DMCB,('FW_TRSTA',$TIMREC)                               
         GOTOR DMGRITRN,DMCB,('FW_TREND',$TIMREC)                               
         J     UTRL0110                                                         
                                                                                
UTRL0100 GOTOR PUTTIM              Send old and new time buffers                
                                                                                
UTRL0110 GOTOR TIMTRN              Send transactions and buckets                
                                                                                
UTRL0120 TM    HD_IND,FW_INOPR     Test no previous time existed                
         JZ    *+12                                                             
         CLI   QH_STAT,QH_SDELQ    Test deleting time sheet                     
         JE    UTRL0130                                                         
                                                                                
         GOTOR UPDAUD              Update time audit record                     
         JE    UTRL0130                                                         
         MVI   ERRTAB,ET_EOTQ      Clear error table                            
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
                                                                                
UTRL0130 CLI   QH_BUAPR,C'Y'       Back up approver?                            
         JNE   UTRL0150                                                         
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,HD_MANBA         Search backups approvers for match           
UTRL0135 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    UTRL0160                                                         
         CLC   CCTPID,0(R1)        Is line manager a back up approver?          
         JE    UTRL0140                                                         
         LA    R1,L'PIDNO(R1)                                                   
         JCT   R0,UTRL0135                                                      
         J     UTRL0160                                                         
                                                                                
UTRL0140 CLI   QH_STAT,QH_SSUBQ    Submitting this time?                        
         JNE   *+14                                                             
         CLC   HD_PPID#,HD_MANAP   Is connected pid line manager and            
         JNE   UTRL0160            timesheet owner                              
         GOTOR SETAPP,HD_MANAP     Set approved by line manager                 
         J     UTRL0160                                                         
                                                                                
UTRL0150 GOTOR SETAPP,0(RF)        Update apptab approval status                
                                                                                
UTRL0160 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',TR_VALS),TR_VALSL                     
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Exit handling for all upload records                                *         
***********************************************************************         
                                                                                
UFATAL   OI    TWAMODE,TWAMEDP     Set fatal error on                           
                                                                                
UEXIT    TM    TWAMODE,TWAMERP+TWAMEDP                                          
         JNZ   UEXIT26                                                          
         TM    TWAMODE,TWAMPER     Any previous error                           
         JZ    UEXIT02             No                                           
         GOTOR UNLOCK              Yes - unlock record locked this time         
UEXIT02  TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   UEXIT40             Yes - nothing more to do                     
         CLI   RECTYPE,RECTTRLQ    Test just processed trailer record           
         JNE   UEXIT40                                                          
         TM    LP_FLAG,LP_FDRFT    Only send reply in 'draft' mode              
         JZ    UEXIT40                                                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',QH_TOKEN),(L'QH_TOKEN,0)         
         GOTOR VHEXOUT,DMCB,TR_INDXN,WORK,L'TR_INDXN                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#IDN),('LD_CHARQ',WORK),(L'TR_INDXN*2,0)             
                                                                                
         CLI   TR_TSSTN,0          Set time sheet status and send               
         JNE   UEXIT04                                                          
         MVI   BYTE1,TS#PROGR      'In progress'                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ                                                      
         JE    UEXIT04                                                          
         CLC   HD_PEDT,HD_OVRDU                                                 
         JNL   UEXIT04                                                          
         MVI   BYTE1,TS#OVERD      'Overdue'                                    
                                                                                
UEXIT04  TM    TR_TSSTN,TIMSREJE                                                
         JZ    *+8                                                              
         MVI   BYTE1,TS#REJEC      'Rejected'                                   
         TM    TR_TSSTN,TIMSSUBM                                                
         JZ    *+8                                                              
         MVI   BYTE1,TS#SUBMT      'Submitted'                                  
         TM    TR_TSSTN,TIMSPAPP                                                
         JZ    *+8                                                              
         MVI   BYTE1,TS#PAAPR      'Partly approved'                            
         TM    TR_TSSTN,TIMSFAPP                                                
         JZ    *+8                                                              
         MVI   BYTE1,TS#APPRO      'Fully approved'                             
         TM    TR_TSSTN,TIMSDELT                                                
         JZ    UEXIT06                                                          
         MVI   BYTE1,TS#NSTRT      'Not started'                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ                                                      
         JE    UEXIT06                                                          
         CLC   HD_PEDT,HD_OVRDU                                                 
         JNL   UEXIT06                                                          
         MVI   BYTE1,TS#OVERD      'Overdue'                                    
                                                                                
UEXIT06  GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#STA),('LD_CHARQ',BYTE1),(L'BYTE1,0)                 
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or Aura             
         CHI   RF,XPRODIKQ                                                      
         JNE   UEXIT22                                                          
         CLI   TR_TSSTN,0          Set time sheet status and send               
         JE    UEXIT10                                                          
         TM    TR_TSSTN,TIMSDELT                                                
         JZ    UEXIT12                                                          
UEXIT10  CLC   HD_PEDT,HD_OVRDU                                                 
         JNL   UEXIT22                                                          
         MVI   BYTE1,TS#OVERD      'Overdue'                                    
         J     UEXIT22                                                          
                                                                                
UEXIT12  CLI   QH_STAT,QH_SAPRQ                                                 
         JNE   *+8                                                              
         MVI   BYTE1,TS#APPRO      'Fully approved'                             
                                                                                
UEXIT14  CLI   QH_STAT,QH_SREJQ                                                 
         JNE   *+8                                                              
         MVI   BYTE1,TS#REJEC      'Rejected'                                   
                                                                                
UEXIT16  CLI   QH_STAT,QH_SSAVQ                                                 
         JNE   *+8                                                              
         MVI   BYTE1,TS#SUBMT      'Submitted'                                  
                                                                                
UEXIT18  CLI   QH_STAT,QH_SSUBQ                                                 
         JNE   *+8                                                              
         MVI   BYTE1,TS#SUBMT      'Submitted'                                  
                                                                                
UEXIT22  GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#STAV),('LD_CHARQ',BYTE1),(L'BYTE1,0)                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Send approver run sequence                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRUN',A#TEAP)                
                                                                                
UEXIT24  MVI   BYTE1,C'2'          Set time as the email application            
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',D#CAE),       +        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
         MVI   BYTE1,C'N'          No emails                                    
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',D#CAEML),     +        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTERU',0),0,0                 
         J     UEXIT40                                                          
                                                                                
UEXIT26  GOTOR UNLOCK              Unlock everything I locked so far            
         CLI   RECTYPE,RECTTRLQ    Test just processed trailer record           
         JNE   UEXIT28                                                          
         NI    TWAMODE,FF-(TWAMERP+TWAMEDP)  remove error this time             
         OI    TWAMODE,TWAMPER     set previous error                           
UEXIT28  TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    *+6                                                              
         DC    H'0'                Die to unwind any updates                    
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         CLI   RECTYPE,RECTTRLQ    Test trailer record                          
         JE    UEXIT30                                                          
         CLI   RECTYPE,RECTHDRQ    Test header record                           
         JE    UEXIT30                                                          
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',QX_TOKEN),(L'QX_TOKEN,0)         
         J     UEXIT32                                                          
UEXIT30  GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',QH_TOKEN),(L'QH_TOKEN,0)         
                                                                                
UEXIT32  LA    R2,ERRTAB           Send errors                                  
         USING ET_D,R2             R2=A(Error table)                            
         LA    R1,DMCB                                                          
         USING GETTXTD,R1          Build error text string in ELEMENT           
UEXIT34  CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    UEXIT38                                                          
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    R0,ELEMENT                                                       
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    UEXIT36                                                          
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,7,GTATXT         Set A(Extra text)                            
UEXIT36  GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
         GOTOR AALINKIO,(R1),('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#ERR),('LD_CHARQ',ELEMENT),((R0),0)                  
         LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
         J     UEXIT34                                                          
         DROP  R1,R2                                                            
                                                                                
UEXIT38  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    EXITN                                                            
                                                                                
UEXIT40  J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Handle 'Last for request' mode (only passed when running live)      *         
***********************************************************************         
                                                                                
EREQ     GOTOR GOATRN,0            Call GOATRN for last time                    
         GOTOR BUFREC,0            Flush pending updative I/Os                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   EREQ02              Yes - using SORTER not FACWRK                
         L     R1,WRKBLKR          Copy WRKR parameters                         
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,CLOSE  Close the FACWRK file                        
                                                                                
EREQ02   OI    LP_FLAG,LP_FFWRK    Set FACWRK file built                        
         MVC   VDATAMGR,DATAMGR    Point to real DATAMGR                        
         MVC   ACOMFACS,LP_ACOM    Point to real COMFACS                        
                                                                                
***********************************************************************         
* If we are running off-line with updative (global) files re-open the *         
* FACWRK recovery file and read the header record,  call  DATAMGR to  *         
* checkpoint recovery (COMMIT), call SRUPD60 to update the files and  *         
* then checkpoint recovery again to clear the global lock table.      *         
***********************************************************************         
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
         TM    RUNINDS,RUNIPUTF    no SORTER Puts, nothing to post              
         JZ    EXITY                                                            
         TM    LP_OFLG1,LP_OFDFT   Test 'draft' upload                          
         JNZ   EREQ04                                                           
         TM    LP_INDS,LP_IGLOB    Test global (updative) file                  
         JZ    EXITY                                                            
                                                                                
EREQ04   GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
                                                                                
         LA    RE,TSARPASS         Pass A(TSARPASS) in LP_ABLK3                 
         LA    RF,TSARRECS         Pass A(TSARRECS) in LP_ABLK4                 
         LA    R0,TSAROLDT         Pass A(TSAROLDT) in LP_ABLK5                 
         LA    R1,TSARNEWT         Pass A(TSARNEWT) in LP_ABLK6                 
         LA    R2,TSAROBUF         Pass A(TSAROBUF) in LP_ABLK7                 
         LA    R3,VSORTER          Pass A(VSORTER) in LP_ABLK8                  
         STM   RE,R3,LP_ABLK3                                                   
         GOTOR ASRUPD60,DMCB,('FF',LP_D),('00',PARM),AIO1                       
                                                                                
         GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Check time type                                                     *         
***********************************************************************         
                                                                                
CHKTTY   LA    RF,HD_COATM         Point to valid time types                    
         LHI   R0,L'HD_COATM       R0=Number of valid time types                
CHKTTY02 CLI   0(RF),C','          Ignore embedded commas                       
         JE    *+12                                                             
         CLC   0(1,RF),0(R1)       Match type to input                          
         BER   RE                  Exit with CC equal on match                  
         AHI   RF,1                Else bump and try again                      
         JCT   R0,CHKTTY02                                                      
         MVC   ROUERRV,=AL2(AE$ITTML)                                           
         LTR   RE,RE               Set CC to not equal and return               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Check MOA for open time                                             *         
***********************************************************************         
                                                                                
CHKMOA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKMOA*'                                                      
                                                                                
         LA    R4,HD_DMDAT                                                      
         USING HD_DMDAT,R4         R4=A(Date/MOA array)                         
         LHI   R3,TT_DAYS                                                       
CHKMOA02 OC    HD_DMDAT,HD_DMDAT   Test date slot is empty                      
         JZ    CHKMOA10            Yes - ignore                                 
         LA    RF,HD_MOA                                                        
                                                                                
CHKMOA04 MVC   SVMOA,EFFS          = No future limit                            
         CLC   0(2,RF),HD_TODP     Is time in the future?                       
         JNH   *+10                                                             
         MVC   SVMOA,0(RF)         Future limit=period month                    
         GOTOR MTHLCK,DMCB,(RF),SVMOA                                           
         JE    CHKMOA06                                                         
         MVC   ROUERRV,=AL2(AE$IMOAR)                                           
         J     EXITN                                                            
                                                                                
CHKMOA06 MVC   HD_DMMOA,HD_CDAT                                                 
         J     EXITY                                                            
CHKMOA10 AHI   R4,HD_DMOAL                                                      
         JCT   R3,CHKMOA02                                                      
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Check previous levels of 1R for costing and set 14 account names    *         
***********************************************************************         
                                                                                
GETODS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETODS*'                                                      
                                                                                
         LHI   R4,3                R4=Number of levels to do                    
         LA    R3,ONERL3L          R3=A(level lengths)                          
                                                                                
K        USING ACTRECD,IOKEY                                                    
GETODS02 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),HD_1RULA                          
         LLC   RF,0(R3)            RF=Length of account at this level           
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),HD_1RACT                                            
         EX    RF,0(RE)                                                         
         DROP  K                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,HD_142NM                                                      
         CHI   R4,2                Test department level                        
         JE    GETODS04                                                         
         LA    R1,HD_143NM                                                      
         CHI   R4,3                Test sub-department level                    
         JNE   GETODS06                                                         
GETODS04 GOTOR GETNAM,(R1)         Set department/sub-department name           
                                                                                
GETODS06 GOTOR GETELA,RSTELQ       Locate status element on record              
*&&UK*&& JNE   GETODS16                                                         
*&&US*&& JNE   GETODS10                                                         
         CLC   HD_1RCST,SPACES     Test costing group resolved                  
         JH    GETODS08                                                         
         MVC   HD_1RCST,RSTCOSTG-RSTELD(R1)                                     
GETODS08 CLI   RSTLN-RSTELD(R1),RSTLN3Q                                         
*&&US*&& JL    GETODS10                                                         
*&&UK*&& JL    GETODS16                                                         
         TM    RSTSTAT5-RSTELD(R1),RSTSPROD                                     
         JZ    *+8                                                              
         MVI   HD_1RFPT,YESQ                                                    
         TM    RSTSTAT5-RSTELD(R1),RSTSPRJB                                     
         JZ    *+8                                                              
         MVI   HD_1RFJT,YESQ                                                    
*&&UK*&& J     GETODS16                                                         
                                                                                
*&&US                                                                           
GETODS10 GOTOR GETELA,SPAELQ       Get special posting element                  
         JNE   GETODS16                                                         
         SR    R0,R0                                                            
         USING SPAELD,R1                                                        
GETODS12 CLI   SPATYPE,SPATINCO    Test income account                          
         JE    GETODS14            Yes                                          
         IC    R0,SPALN            Bump to next element                         
         AR    R1,R0                                                            
         CLI   SPAEL,SPAELQ        Test special posting element                 
         JE    GETODS12            Yes                                          
         J     GETODS16                                                         
                                                                                
GETODS14 CLC   HD_INULA,SPACES     Test have income account already             
         JH    GETODS16                                                         
         MVC   HD_INULA,SPAAULA    Set override income account                  
         DROP  R1                                                               
*&&                                                                             
                                                                                
GETODS16 BCTR  R3,0                Back-up to previous length                   
         JCT   R4,GETODS02         Do for number of levels                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Read audit record and set if manager approved time previously       *         
***********************************************************************         
                                                                                
CHKAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKAUD*'                                                      
                                                                                
K        USING AUDKEY,IOKEY                                                     
         XC    K.AUDKEY,K.AUDKEY                                                
         MVI   K.AUDKTYP,AUDKTYPQ                                               
         MVI   K.AUDKSUB,AUDKSUBQ                                               
         MVI   K.AUDKAUDT,AUDKTIME                                              
         MVC   K.AUDKCPY,CUXCPY                                                 
         MVC   K.AUDKPIDB,HD_PPID#                                              
         MVC   K.AUDKPEDT,HD_LEDTC                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.AUDKINDX,TR_INDX                                               
         JE    CHKAUD04                                                         
         MVC   ROUERRV,=AL2(AE$FATAL)                                           
         J     EXITN                                                            
                                                                                
CHKAUD02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         CLC   K.AUDKEY(AUDKSEQ-AUDKEY),IOKEYSAV                                
         JNE   EXITY                                                            
         DROP  K                                                                
                                                                                
CHKAUD04 TM    TR_IND,TR_ILNMG     Is line manager approve/reject now           
         JNZ   EXITY               Yes - ignore previous actions                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,AUDRFST-AUDRECD                                               
         USING TIMELD,R2           R2=A(Time element)                           
         SR    R0,R0                                                            
CHKAUD06 CLI   TIMEL,0             Test end of record                           
         JE    CHKAUD02            Yes - get next record                        
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   CHKAUD08                                                         
         CLI   TIMETYP,TIMEARIN    Is it approval info TIMEL?                   
         JNE   CHKAUD08                                                         
         OC    TIMAIDNO,TIMAIDNO   Is it line manager TIMEL?                    
         JNZ   CHKAUD08                                                         
         TM    TIMASTAT,TIMASREJ   Test rejected                                
         JZ    *+8                                                              
         OI    TR_IND,TR_IRLCL     Set line manager rejected previously         
         TM    TIMASTAT,TIMASAPR   Test approved                                
         JZ    *+8                                                              
         OI    TR_IND,TR_ILMGP     Set line manager approved previously         
         J     EXITY                                                            
                                                                                
CHKAUD08 IC    R0,TIMLN            Bump to next element on record               
         AR    R2,R0                                                            
         J     CHKAUD06                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Update index on audit time record and update status elements to     *         
* last record - add new records where none exist or there isn't       *         
* enough room to store the audit data                                 *         
***********************************************************************         
                                                                                
UPDAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDAUD*'                                                      
                                                                                
         MVI   BYTE1,0             Sequence number                              
         MVI   BYTE2,0             Indicators                                   
                                                                                
         L     R3,AAUDREC                                                       
         USING AUDRECD,R3          R3=A(Audit record)                           
                                                                                
         LAY   R0,I_GEN            Clear buffer                                 
         LHI   R1,I_GENL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
                                                                                
         GOTOR BUFGEN,DMCB,('TSARDH',TIMBUF),0                                  
         CLI   TSAERR,0            Is it the end of the buffer?                 
         JE    UPDAUD00                                                         
         CLI   QH_STAT,QH_SDELQ    Is change to delete time sheet?              
         JE    UPDAUD00            Then ok for no audit                         
         TM    RUNINDS,RUNIUPD     Have we made any changes                     
         JZ    UPDAUD02            No                                           
         DC    H'0'                                                             
                                                                                
UPDAUD00 CLI   QH_STAT,QH_SDELQ    Is change to delete time sheet?              
         JNE   UPDAUD02                                                         
         LA    R4,ELEMENT          Build delete status element                  
AUD      USING STCELD,R4                                                        
         GOTOR BLDSTC,STCTTSDL                                                  
                                                                                
***********************************************************************         
* Read audit record for current time sheet period - update time sheet *         
* status on all audit records for this period                         *         
***********************************************************************         
                                                                                
K        USING AUDKEY,IOKEY                                                     
UPDAUD02 XC    K.AUDKEY,K.AUDKEY   Build key of audit record                    
         MVI   K.AUDKTYP,AUDKTYPQ                                               
         MVI   K.AUDKSUB,AUDKSUBQ                                               
         MVI   K.AUDKAUDT,AUDKTIME                                              
         MVC   K.AUDKCPY,CUXCPY                                                 
         MVC   K.AUDKPEDT,HD_LEDTC                                              
         MVC   K.AUDKPIDB,HD_PPID#                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   UPDAUD24                                                         
*                                                                               
UPDAUD04 MVC   BYTE1,K.AUDKSEQ     Save current sequence number                 
         TM    HD_IND,FW_INOPR                                                  
         JNZ   UPDAUD05            new t/s, previous audit data                 
         CLC   K.AUDKINDX,TR_INDX  Test correct index number                    
         JE    UPDAUD05                                                         
         MVC   ROUERRV,=AL2(AE$FATAL)                                           
         J     EXITN                                                            
*                                                                               
UPDAUD05 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   AUDRINDX,TR_INDXN   Set new index number in record               
         CLI   QH_STAT,QH_SDELQ    Is change to delete time sheet?              
         JNE   *+12                                                             
         MVI   AUDRSTAT,0          Reset audit record status                    
         J     UPDAUD12                                                         
*                                                                               
         MVC   AUDRSTAT,TR_TSSTN   Set new time sheet status                    
*                                                                               
         LAY   RF,I_GEN                                                         
         MVC   SVTI_KEY,0(RF)                                                   
         GOTOR UPDAPP,AUDRECD      Update approver elements                     
*                                                                               
         LAY   R0,I_GEN            Clear buffer                                 
         LHI   R1,I_GENL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LAY   RF,I_GEN                                                         
         MVC   0(TI_KEYL,RF),SVTI_KEY                                           
*                                                                               
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
*                                                                               
         GOTOR BUFGEN,DMCB,('TSARDH',TIMBUF),0                                  
*                                                                               
UPDAUD06 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   UPDAUD20            Finished                                     
         CLI   AUD.STCEL,STCELQ    Test status element                          
         JE    UPDAUD08                                                         
         CLI   AUD.STCEL,TIMELQ    Test time element                            
         JNE   UPDAUD08                                                         
         CLI   AUD.STCLN,TIMDLNQ   Test short delete element                    
         JE    UPDAUD10                                                         
                                                                                
UPDAUD08 GOTOR TSTFIT              Test data fits on current record             
         JH    UPDAUD20            No                                           
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AUDRECD,AUD.STCELD,ADDEND              
         CLI   12(R1),0                                                         
         JE    UPDAUD10                                                         
         DC    H'0'                                                             
                                                                                
UPDAUD10 CLI   QH_STAT,QH_SDELQ    Is change to delete time sheet?              
         JE    UPDAUD20                                                         
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BUFGEN,DMCB,('TSANXT',TIMBUF),0                                  
         J     UPDAUD06                                                         
                                                                                
***********************************************************************         
* Here if deleting a time sheet - delete all approval elements (every *         
* record ) and add delete element to audit (once only)                *         
***********************************************************************         
                                                                                
UPDAUD12 MVC   TR_TSSTN,AUDRSTAT   Save current audit status                    
                                                                                
         LA    R1,AUDRFST          Delete all approval elements                 
         USING TIMELD,R1                                                        
         SR    RE,RE                                                            
UPDAUD14 CLI   TIMEL,0             Delete all approval elements                 
         JE    UPDAUD18                                                         
         CLI   TIMEL,TIMELQ                                                     
         JNE   UPDAUD16                                                         
         CLI   TIMETYP,TIMEARIN                                                 
         JNE   UPDAUD16                                                         
         MVI   TIMEL,FF                                                         
UPDAUD16 IC    RE,TIMLN            Bump to next record element                  
         AR    R1,RE                                                            
         J     UPDAUD14                                                         
         DROP  R1                                                               
                                                                                
UPDAUD18 GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',AUDRECD),0                       
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    BYTE2,X'80'         Test last STCEL added                        
         JNZ   UPDAUD20                                                         
         GOTOR TSTFIT              Test status fits on current record           
         JH    UPDAUD20            No                                           
         OI    BYTE2,X'80'         Set last STCEL added                         
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AUDRECD,AUD.STCELD,ADDEND              
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BUFGEN,DMCB,('TSANXT',TIMBUF),0                                  
         J     UPDAUD20                                                         
                                                                                
***********************************************************************         
* Here to write back current audit record and get next one            *         
***********************************************************************         
                                                                                
UPDAUD20 GOTOR SETDTE              Set low and high date on audit rec           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         CLC   K.AUDKEY(AUDKSEQ-AUDRECD),IOKEYSAV                               
         JE    UPDAUD04            Process next record                          
                                                                                
         CLI   QH_STAT,QH_SDELQ    Is the change to delete time sheet?          
         JNE   UPDAUD22                                                         
         TM    BYTE2,X'80'         Test last STCEL added                        
         JNZ   EXITY               Yes - all done                               
                                                                                
***********************************************************************         
* Here when all current audit records have been processed - set       *         
* sequence number of next audit record (to be added) and test if we   *         
* we have further elements to add                                     *         
***********************************************************************         
                                                                                
UPDAUD22 LLC   RE,BYTE1            Bump key sequence number                     
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
                                                                                
UPDAUD24 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   EXITY               Finished                                     
         CLI   AUD.STCEL,STCELQ    Test status element                          
         JE    UPDAUD28                                                         
         CLI   AUD.STCEL,TIMELQ    Have we got a TIMEL                          
         JNE   UPDAUD25                                                         
         CLI   AUD.STCLN,TIMDLNQ   Is it short delete one?                      
         JNE   UPDAUD28                                                         
UPDAUD25 GOTOR BUFGEN,DMCB,('TSANXT',TIMBUF),0                                  
         J     UPDAUD24            Get next audit element                       
                                                                                
***********************************************************************         
* Here if audit record for current period doesn't exist or we have    *         
* run out of space for audit elements - create a new record and set   *         
* flag so that we issue an ADDREC                                     *         
***********************************************************************         
                                                                                
UPDAUD28 XC    AUDRECD(256),AUDRECD                                             
         MVC   AUDKEY,IOKEYSAV     Build a new audit record                     
         MVC   AUDKSEQ,BYTE1                                                    
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         MVC   AUDRSTAT,TR_TSSTN                                                
         MVC   AUDRINDX,TR_INDXN                                                
         OI    BYTE2,X'40'         Set adding new records                       
                                                                                
UPDAUD32 GOTOR TSTFIT              Test element fits on current record          
         JH    UPDAUD36            No                                           
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AUDRECD,AUD.STCELD,ADDEND              
         CLI   12(R1),0                                                         
         JE    UPDAUD34                                                         
         DC    H'0'                                                             
                                                                                
UPDAUD34 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BUFGEN,DMCB,('TSANXT',TIMBUF),0                                  
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   UPDAUD36            Finished                                     
         CLI   AUD.STCEL,STCELQ                                                 
         JE    UPDAUD32                                                         
         CLI   AUD.STCEL,TIMELQ    Is it time element?                          
         JNE   UPDAUD34                                                         
         CLI   AUD.STCLN,TIMDLNQ   Is it short delete one?                      
         JE    UPDAUD34            Yes - ignore these                           
         J     UPDAUD32                                                         
                                                                                
UPDAUD36 GOTOR SETDTE              Set low and high date on audit rec           
         TM    BYTE2,X'40'         Test creating a new record                   
         JZ    UPDAUD38                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    UPDAUD22                                                         
         DC    H'0'                                                             
                                                                                
UPDAUD38 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    UPDAUD22                                                         
         DC    H'0'                                                             
                                                                                
TSTFIT   SR    R0,R0               Test element fits on audit record            
         ICM   R0,3,AUDRLEN                                                     
         LLC   RF,AUD.STCLN                                                     
         AR    R0,RF                                                            
         CHI   R0,MAXRECLN         Set condition code                           
         BR    RE                                                               
                                                                                
SETDTE   XC    FULL2,FULL2                                                      
         MVC   FULL1,EFFS                                                       
         SR    R0,R0                                                            
         LA    R2,AUDRFST                                                       
         USING STCELD,R2                                                        
SETDTE02 CLI   STCEL,0                                                          
         JE    SETDTE08                                                         
         CLI   STCEL,STCELQ                                                     
         JE    SETDTE06                                                         
SETDTE04 LLC   RF,STCLN                                                         
         AR    R2,RF                                                            
         J     SETDTE02                                                         
                                                                                
SETDTE06 CLC   FULL2,STCTDTE                                                    
         JNL   *+10                                                             
         MVC   FULL2,STCTDTE                                                    
         CLC   FULL1,STCTDTE                                                    
         JNH   *+10                                                             
         MVC   FULL1,STCTDTE                                                    
         J     SETDTE04                                                         
                                                                                
SETDTE08 LR    R2,RE                                                            
         GOTOR VDATCON,DMCB,(1,FULL1),(2,AUDRSTDT)                              
         GOTOR VDATCON,DMCB,(1,FULL2),(2,AUDRENDT)                              
         BR    R2                                                               
         DROP  R3,K                                                             
         EJECT                                                                  
***********************************************************************         
* Update approver TIMEL in time audit record                          *         
*                                                                     *         
* Ntry:- R1=A(Audit record)                                           *         
***********************************************************************         
                                                                                
UPDAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDAPP*'                                                      
                                                                                
         LR    R2,R1                                                            
         USING AUDRECD,R2                                                       
         LA    R3,AUDRFST                                                       
REC      USING TIMEL,R3                                                         
UPDAPP02 CLI   REC.TIMEL,0         Test end of record                           
         JE    UPDAPP24                                                         
         CLI   REC.TIMEL,TIMELQ    Test time element                            
         JE    UPDAPP06                                                         
UPDAPP04 LLC   R0,REC.TIMLN        Bump to next element on record               
         AR    R3,R0                                                            
         J     UPDAPP02                                                         
                                                                                
UPDAPP06 CLI   REC.TIMETYP,TIMEARIN                                             
         JNE   UPDAPP04                                                         
         OC    REC.TIMAIDNO,REC.TIMAIDNO  Is line manager approval?             
         JZ    UPDAPP12                   Yes                                   
                                                                                
         LAY   R0,I_GEN            Clear buffer                                 
         LHI   R1,I_GENL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
                                                                                
         GOTOR BUFGEN,DMCB,('TSARDH',TIMBUF),0                                  
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   UPDAPP04            Yes - nothing to audit                       
         J     UPDAPP08                                                         
*                                                                               
UPDAPP07 GOTOR BUFGEN,DMCB,('TSANXT',TIMBUF),0                                  
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   UPDAPP04            Finished                                     
*                                                                               
TIM      USING TIMELD,R4                                                        
UPDAPP08 CLI   TIM.TIMEL,STCELQ                                                 
         JE    UPDAPP07                                                         
         CLI   TIM.TIMEL,TIMELQ                                                 
         JNE   UPDAPP07                                                         
         CLI   TIM.TIMLN,TIMDLNQ                                                
         JNE   UPDAPP07                                                         
         CLC   REC.TIMAIDNO,TIM.TIMAIDNO                                        
         JNE   UPDAPP07                                                         
         MVI   REC.TIMEL,FF                                                     
         J     UPDAPP04                                                         
         DROP  TIM                                                              
                                                                                
UPDAPP12 TM    TR_IND,TR_ILNMG+TR_ILMGP Has line manager approval               
         JNZ   UPDAPP14                 been cleared?                           
         XC    REC.TIMASTAT,REC.TIMASTAT                                        
         XC    REC.TIMAPDAC,REC.TIMAPDAC                                        
         XC    REC.TIMADATE,REC.TIMADATE                                        
         XC    REC.TIMAUSER,REC.TIMAUSER                                        
         XC    REC.TIMATIME,REC.TIMATIME                                        
         J     UPDAPP04                                                         
                                                                                
UPDAPP14 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    UPDAPP16                                                         
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JE    UPDAPP16                                                         
         CLI   QH_STAT,QH_SSUBQ    Are we submitting?                           
         JNE   UPDAPP04                                                         
         CLC   HD_MANAP,HD_PPID#   Is line manager approver the user?           
         JNE   UPDAPP04            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   UPDAPP04            No                                           
         J     UPDAPP20                                                         
                                                                                
UPDAPP16 CLC   CCTPID,HD_MANAP                                                  
         JE    UPDAPP18                                                         
         CLI   QH_BUAPR,C'Y'                                                    
         JNE   UPDAPP04                                                         
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,HD_MANBA         Search backups approvers for match           
UPDAPP17 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    UPDAPP04                                                         
         CLC   CCTPID,0(R1)        Is line manager a back up approver?          
         JE    UPDAPP18                                                         
         LA    R1,L'PIDNO(R1)                                                   
         JCT   R0,UPDAPP17                                                      
         J     UPDAPP04                                                         
                                                                                
UPDAPP18 CLI   QH_STAT,QH_SAPRQ    Are we rejecting?                            
         JNE   *+8                                                              
UPDAPP20 MVI   REC.TIMASTAT,TIMASAPR   Set item as approved                     
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   *+8                                                              
         MVI   REC.TIMASTAT,TIMASREJ   Set item as rejected                     
         MVC   REC.TIMAPDAC,CCTPID                                              
         MVC   REC.TIMADATE,HD_TODP                                             
         MVC   REC.TIMAUSER,LP_USRID                                            
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    REC.TIMASTAT,TIMAMOBL Set this has been done via mobile          
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    REC.TIMASTAT,TIMAAURA Set this has been done by aura             
         GOTOR GETTIM,REC.TIMATIME                                              
                                                                                
         CLI   QH_COMSL,0          Test any rejection comments                  
         JE    UPDAPP04                                                         
                                                                                
         GOTOR BLDCOM,0            Build comment element                        
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    UPDAPP04                                                         
         DC    H'0'                                                             
                                                                                
UPDAPP24 GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',AUDRECD),0                       
         CLI   12(R1),0                                                         
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  REC,R2                                                           
         EJECT                                                                  
***********************************************************************         
* Send old and new time buffer records to output file                 *         
***********************************************************************         
                                                                                
PUTTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTTIM*'                                                      
                                                                                
         GOTOR DMGRITRN,DMCB,('FW_TRHDR',$TIMREC)                               
                                                                                
         XC    TT_KEY(TT_KEYL),TT_KEY                                           
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         J     PUTTIM04                                                         
PUTTIM02 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF),0                                  
PUTTIM04 TM    TIMERR,TSEEOF                                                    
         JNZ   PUTTIM06                                                         
         GOTOR DMGRITRN,DMCB,('FW_TROLD',$TIMREC)                               
         J     PUTTIM02                                                         
                                                                                
PUTTIM06 XC    TT_KEY(TT_KEYL),TT_KEY                                           
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),0                                  
         J     PUTTIM10                                                         
PUTTIM08 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),0                                  
PUTTIM10 TM    TIMERR,TSEEOF                                                    
         JNZ   PUTTIM12                                                         
         GOTOR DMGRITRN,DMCB,('FW_TRNEW',$TIMREC)                               
         J     PUTTIM08                                                         
                                                                                
PUTTIM12 GOTOR DMGRITRN,DMCB,('FW_TREND',$TIMREC)                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Copy time line from old buffer to new buffer and flag as copied     *         
***********************************************************************         
                                                                                
CPYONE   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CPYONE*'                                                      
                                                                                
         XC    TT_D(TT_KEYL),TT_D                                               
         MVC   TT_TIME#,QT_TIME#   Set time row                                 
         MVC   TT_ITEM#,QI_ITEM#   Set materials row                            
         MVC   TT_NAR#,QD_SEQ#     Set narrative sequence                       
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                Yes die as it shouldn't be empty             
         OC    TT_ITEM#,TT_ITEM#   Is this a time row?                          
         JNZ   CPYONE08                                                         
         OC    TT_NAR#,TT_NAR#     Skip narrative as well                       
         JNZ   CPYONE08                                                         
*&&US                                                                           
         LA    RF,HD_DMOA          Start of TOTAL HRS/DAY Array                 
HD       USING HD_DMOA,RF                                                       
         LA    R1,TT_DHVAL                                                      
         LA    R0,TT_DAYS                                                       
CPYONE02 OC    0(L'TIMETDTE,R1),0(R1) Is a date present?                        
         JZ    CPYONE04             No - then no further entries to get         
         AP    HD.HD_EDHRS,L'TIMETDTE(L'TIMEHRS,R1)                             
         LA    R1,L'TIMEDAY(R1)    Bump tsar record                             
         AHI   RF,HD_DMOAL         Bump TOTAL array pointer                     
         JCT   R0,CPYONE02                                                      
         DROP  HD                                                               
*&&                                                                             
CPYONE04 DS    0H                                                               
CPYONE06 AP    TOTHOURS,TT_HRS     Yes - add into total hours                   
                                                                                
* copy account and other time line information for situations where             
* row info hasn't changed but materials or day narrative have                   
                                                                                
         MVC   TL_ULA,TT_AULA      Set SJ/1N account                            
         MVC   TL_TSK,TT_TSK       Set work code                                
         MVC   TL_SJOFF,TT_OFF                                                  
         MVC   TL_MED,TT_MED                                                    
         CLC   PRODUL,TL_ULA       It's either SJ or 1N                         
         JNE   *+10                                                             
         MVC   TL_1CULA,TT_CAULA   SJ so contra is 1C                           
         MVC   TL_INULA,TT_INULA   Set income account                           
                                                                                
CPYONE08 OI    TT_BSTAT,TT_BSRCO   Set record copied from old buffer            
         GOTOR BUFTIM,DMCB,('TSAADD',NEWBUF),0                                  
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Copy old time buffer to new time buffer and flag records as copied  *         
***********************************************************************         
                                                                                
CPYALL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CPYALL*'                                                      
                                                                                
         XC    TT_D(TT_KEYL),TT_D                                               
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         J     CPYALL04                                                         
CPYALL02 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF),0                                  
CPYALL04 TM    TIMERR,TSEEOF                                                    
         JNZ   EXITY                                                            
                                                                                
         OC    TT_ITEM#,TT_ITEM#   Is this a time row?                          
         JNZ   CPYALL12            No                                           
         OC    TT_NAR#,TT_NAR#     Skip day narrative                           
         JNZ   CPYALL12                                                         
*&&US                                                                           
         LA    RF,HD_DMOA          Start of TOTAL HRS/DAY Array                 
HD       USING HD_DMOA,RF                                                       
         LA    R1,TT_DHVAL                                                      
         LA    R0,TT_DAYS                                                       
CPYALL06 OC    0(L'TIMETDTE,R1),0(R1) Is a date present?                        
         JZ    CPYALL10             No - then no further entries to get         
         AP    HD.HD_EDHRS,L'TIMETDTE(L'TIMEHRS,R1)                             
         LA    R1,L'TIMEDAY(R1)    Bump tsar record                             
         AHI   RF,HD_DMOAL         Bump TOTAL array pointer                     
         JCT   R0,CPYALL06                                                      
         DROP  HD                                                               
*&&                                                                             
CPYALL10 AP    TOTHOURS,TT_HRS     Add into total hours                         
CPYALL12 OI    TT_BSTAT,TT_BSRCO   Set record copied from old buffer            
         GOTOR BUFTIM,DMCB,('TSAADD',NEWBUF),0                                  
         JE    CPYALL02            Get next record to add                       
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Create status change elements and set approval elements             *         
* determine what the status should be for the time sheet              *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6             New time record                              
DOSTAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOSTAT*'                                                      
         LA    R6,TT_NEW                                                        
         XC    SVMOA,SVMOA                                                      
                                                                                
         TM    HD_IND,FW_INOPR     Test no previous time                        
         JNZ   DOST0150            Yes                                          
                                                                                
         GOTOR BLDAPP              Build table of approvers                     
                                                                                
         MVI   APPSTAT,0           Initialise status                            
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JO    *+2                 Yes die as it shouldn't be empty             
         J     DOST0015                                                         
                                                                                
DOST0010 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   DOST0140            Yes                                          
                                                                                
DOST0015 DS    0H                                                               
*&&UK                                                                           
         CLC   PRODUL,TT_AULA                                                   
         JNE   DOST0020                                                         
         CLC   SVMOA,TT_MOA        Basic optimisation - only check              
         JE    DOST0020                     if different from last row          
         MVC   SVMOA,TT_MOA                                                     
*                                                                               
K        USING PHIRECD,IOKEY       Read payroll history record                  
         MVC   K.PHIKEY,SPACES                                                  
         MVI   K.PHIKTYP,PHIKTYPQ                                               
         MVI   K.PHIKSUB,PHIKSUBQ                                               
         MVC   K.PHIKCPY,CUXCPY                                                 
         MVC   K.PHIKOFC,HD_1ROFF                                               
         MVC   K.PHIKDPT,HD_1RDEP                                               
         MVC   K.PHIKSBD,HD_1RSUB                                               
         MVC   K.PHIKPER,HD_1RPER                                               
         SR    R1,R1                                                            
         ICM   R1,3,TT_MOA                                                      
         LNR   R1,R1                                                            
         STCM  R1,3,K.PHIKMOA      MOA 2's complement                           
         XC    K.PHIKSEQ,K.PHIKSEQ Clear sequence number                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    DOST0020                                                         
         DROP  K                                                                
         MVC   WORK(L'TT_MOA),TT_MOA                                            
         MVI   WORK+L'TT_MOA,X'01'                                              
         GOTOR VDATCON,DMCB,(1,WORK),(9,WORK+3)                                 
         MVC   ROUERRV,=AL2(AE$00558)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(5,WORK+3)                                   
*&&                                                                             
DOST0020 OC    NEW.TT_ITEM#,NEW.TT_ITEM# Skip for materials and                 
         JNZ   DOST0030            narrative                                    
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   DOST0030                                                         
         GOTOR RESAPP              Update time if materials changed             
         NI    TR_IND,FF-TR_IAPTT  Remove approved this time bit                
         MVI   APPSTAT,0           Initialise status                            
                                                                                
DOST0030 MVC   TIME#,NEW.TT_TIME#  Save row number (for RESAPP call)            
         XC    TT_D(TT_KEYL),TT_D                                               
         MVC   TT_#,NEW.TT_#       Read old buffer record                       
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         JNE   DOST0060            Not found  - must be a new row               
                                                                                
         MVI   BYTE1,0             Set nothing has changed                      
                                                                                
***********************************************************************         
* Test changes to materials                                           *         
***********************************************************************         
                                                                                
DOST0038 OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JZ    DOST0040                                                         
         CP    TT_IMULT,NEW.TT_IMULT                                            
         JE    *+8                                                              
         OI    BYTE1,STCMMULT      Set number of items changed                  
         CP    TT_ITOT,NEW.TT_ITOT                                              
         JE    *+8                                                              
         OI    BYTE1,STCMTOT       Set total is different                       
         CP    TT_IPRCE,NEW.TT_IPRCE                                            
         JE    *+8                                                              
         OI    BYTE1,STCMPRCE      Set price has changed                        
         CLC   TT_INUM,NEW.TT_INUM                                              
         JE    *+8                                                              
         OI    BYTE1,STCMITMS      Set item code has changed                    
         CLC   TT_NARRL(L'TT_NARRL+L'TT_NARR),NEW.TT_NARRL                      
         JE    *+8                                                              
         OI    BYTE1,STCMTEXT      Set description has changed                  
         J     DOST0050                                                         
***********************************************************************         
* Test changes to time                                                *         
***********************************************************************         
                                                                                
DOST0040 OC    NEW.TT_NAR#,NEW.TT_NAR# Day narrative change?                    
         JNZ   DOST0045                                                         
         MVC   NEW.TT_ADAT,TT_ADAT                                              
         MVC   ANYACCNT,NEW.TT_AULA                                             
         GOTOR GETCAP,NEW.TT_D     Get approver for client account              
         TM    NEW.TT_BSTAT,TT_BSRCO Test record copied from old buffer         
         JZ    DOST0041                                                         
         GOTOR ADDAPP,DMCB,NEW.TT_CLIAP,NEW.TT_EPST1                            
*                                  Add approver table entry                     
DOST0041 LA    R1,TT_DHVAL         Test change to hours                         
ODT      USING TIMETDT1,R1                                                      
         LA    RF,NEW.TT_DHVAL                                                  
NDT      USING TIMETDT1,RF                                                      
         LHI   R0,TT_DAYS                                                       
         BASR  RE,0                                                             
         CLC   ODT.TIMEHRS1,NDT.TIMEHRS1                                        
         JE    *+8                                                              
         OI    BYTE1,STCTHRS       Set hours have changed                       
         AHI   R1,L'TIMETDT1+L'TIMEHRS1                                         
         AHI   RF,L'TIMETDT1+L'TIMEHRS1                                         
         BCTR  R0,RE                                                            
         DROP  ODT,NDT                                                          
                                                                                
         CLC   TT_CAULA,NEW.TT_CAULA                                            
         JE    *+8                                                              
         OI    APPSTAT,APPSACCH    Set account has changed                      
         CLC   TT_AULA,NEW.TT_AULA                                              
         JE    *+12                                                             
         OI    BYTE1,STCTACC       Set account is different                     
         OI    APPSTAT,APPSACCH    Set account has changed                      
         CLC   TT_TTYP,NEW.TT_TTYP                                              
         JE    *+8                                                              
         OI    BYTE1,STCTTTYP      Set type of time changed                     
         CLC   TT_TSK,NEW.TT_TSK   Has the work code changed?                   
         JE    DOST0044            No                                           
         CLC   TT_TSK,SPACES       Maybe - but are we comparing binary          
         JNE   DOST0042            zero to spaces                               
         OC    NEW.TT_TSK,NEW.TT_TSK                                            
         JZ    DOST0044            Yes we are so don't set changed              
DOST0042 OI    BYTE1,STCTTSK       Set work code changed                        
DOST0044 CLC   TT_NARRL(L'TT_NARRL+L'TT_NARR),NEW.TT_NARRL                      
         JE    *+8                                                              
         OI    BYTE1,STCTTEXT      Set text has changed                         
         CLC   TT_ORD,NEW.TT_ORD                                                
         JE    *+8                                                              
         OI    BYTE1,STCTTORD      Set order has changed                        
         CLC   TT_INTRF,NEW.TT_INTRF                                            
         JE    *+8                                                              
         OI    BYTE1,STCTTINT      Set internal reference has changed           
         CLC   TT_EST,NEW.TT_EST                                                
         JE    *+8                                                              
         OI    BYTE1,STCTTEST      Set estimate has changed                     
         CLC   TT_EST,NEW.TT_EST                                                
         JE    *+8                                                              
         OI    BYTE1,STCTTEST      Set estimate has changed                     
         J     DOST0050                                                         
                                                                                
***********************************************************************         
* Test changes to narrative                                           *         
***********************************************************************         
DOST0045 OC    TT_DNARL,TT_DNARL   Any change to narrative?                     
         JZ    DOST0048                                                         
         OC    NEW.TT_DNARL,NEW.TT_DNARL                                        
         JZ    DOST0048                                                         
         CLC   TT_DNARL,NEW.TT_DNARL Compare lengths                            
         JE    DOST0046                                                         
         OI    BYTE1,STCTTEXT      Set text has changed                         
         J     DOST0048                                                         
*                                                                               
DOST0046 LLC   RF,TT_DNARL         If lengths same check text matches           
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   TT_DNAR(0),NEW.TT_DNAR                                           
         EX    RF,0(RE)                                                         
         JE    DOST0048                                                         
         OI    BYTE1,STCTTEXT      Set text has changed                         
         J     DOST0048                                                         
***********************************************************************         
* Check for change to this row                                        *         
***********************************************************************         
                                                                                
DOST0048 DS    0H                                                               
                                                                                
DOST0050 CLI   BYTE1,0             Has anything changed                         
         JE    DOST0070            No                                           
                                                                                
***********************************************************************         
* Changes to this row                                                 *         
***********************************************************************         
                                                                                
         GOTOR AUDCHA              Log changes for audit record                 
                                                                                
         OC    TT_NAR#,TT_NAR#                                                  
         JNZ   DOST0052                                                         
                                                                                
         CLC   TT_MOA,HD_DMMOA     Is it higher than the new one?               
         JH    *+10                                                             
         MVC   TT_MOA,HD_DMMOA                                                  
                                                                                
DOST0052 OI    NEW.TT_BSTAT,TT_BSRCR                                            
         TM    APPSTAT,APPSACCH    If we change time line acc ensure            
         JZ    DOST0080            We delete and re-add record                  
         NI    NEW.TT_BSTAT,FF-TT_BSRCR                                         
         OI    NEW.TT_BSTAT,TT_BSRNR                                            
         J     DOST0090                                                         
                                                                                
***********************************************************************         
* New row added                                                       *         
***********************************************************************         
                                                                                
DOST0060 GOTOR AUDADD              Log additions to audit record                
         OI    NEW.TT_BSTAT,TT_BSRNR                                            
         J     DOST0100                                                         
                                                                                
***********************************************************************         
* No changes to this row                                              *         
***********************************************************************         
                                                                                
DOST0070 OC    TT_NAR#,TT_NAR#     Skip for day narrative                       
         JNZ   DOST0072                                                         
         GOTOR AUDAPP              Log approvals                                
         J     DOST0080                                                         
*                                                                               
DOST0072 DS    0H                                                               
         TM    APPSTAT,APPSACCH    If we change time line acc ensure            
         JZ    DOST0080            We delete and re-add day narrative           
         NI    NEW.TT_BSTAT,FF-TT_BSRCR                                         
         OI    NEW.TT_BSTAT,TT_BSRNR                                            
         J     DOST0090                                                         
                                                                                
DOST0080 OI    TT_BSTAT,TT_BSROB   Set we found this one                        
         MVC   NEW.TT_CKSBR,TT_CKSBR  Set original rec sequence number          
         J     *+8                     on new buffer                            
DOST0090 OI    TT_BSTAT,TT_BSRNR   Set treat as new                             
         GOTOR BUFTIM,DMCB,('TSAWRT',OLDBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   NEW.TT_LOC,TT_LOC   Set data location                            
                                                                                
DOST0100 OC    NEW.TT_ITEM#,NEW.TT_ITEM# Skip for materials/day narr            
         JNZ   DOST0130                                                         
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   DOST0135                                                         
         OC    NEW.TT_CLIAP,NEW.TT_CLIAP Do we have an approver                 
         JZ    DOST0110                  No - treat as approved                 
         TM    NEW.TT_EPST1,TIMESAPR     Yes - is it approved                   
         JNZ   DOST0120                  Yes                                    
         TM    NEW.TT_EPST1,TIMESREJ     is it rejected                         
         JZ    *+8                       No                                     
         OI    TR_IND,TR_IRLCL     Set rejected client approval                 
         OI    TR_IND,TR_IMCLI     Set missing client approval                  
         J     DOST0130                                                         
                                                                                
DOST0110 OI    TR_IND,TR_INCLI     No client approval required                  
         J     DOST0130                                                         
                                                                                
DOST0120 OI    TR_IND,TR_ICLIA     Some time is client approved                 
                                                                                
DOST0130 TM    TR_IND,TR_IAPTT     Approved/rejected by client manager          
         JZ    *+12                                                             
         OI    NEW.TT_BSTAT,TT_BSRIA                                            
         OI    TR_IND,TR_ICLAT     Set we have at least one client appr         
                                                                                
DOST0135 GOTOR BUFTIM,DMCB,('TSAWRT',NEWBUF),NEW.TT_D                           
         JE    DOST0010                                                         
         DC    H'0'                                                             
                                                                                
DOST0140 GOTOR RESAPP              Call RESAPP for last time line               
                                                                                
         GOTOR AUDDEL              Audit deleted time rows                      
         GOTOR RBLCLS              Set to rebuild clusters where                
         J     DOST0250              there are day narrative additions          
*                                      and changes                              
***********************************************************************         
* Create status elements and update approvals for new timesheets      *         
***********************************************************************         
                                                                                
DOST0150 XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   DOST0170            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
DOST0160 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   DOST0230            Yes - add line manager approval el           
                                                                                
DOST0170 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
TIM      USING TIMELD,R4                                                        
         OC    NEW.TT_ITEM#,NEW.TT_ITEM# If materials skip                      
         JNZ   DOST0180            building timel for audit                     
         OC    NEW.TT_NAR#,NEW.TT_NAR# If narrative                             
         JNZ   DOST0180            Skip if narrative                            
         XC    TIM.TIMELD(TIMALNQ),TIM.TIMELD                                   
         MVI   TIM.TIMEL,TIMELQ                                                 
         MVI   TIM.TIMLN,TIMALNQ                                                
         MVI   TIM.TIMETYP,TIMEARIN                                             
         MVC   TIM.TIMAPULA,NEW.TT_AULA                                         
         MVC   TIM.TIMAPIDB,NEW.TT_CLIAP                                        
         MVC   TIM.TIMAIDNO,NEW.TT_TIME#                                        
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    DOST0172            Yes                                          
         CLC   HD_PPID#,NEW.TT_CLIAP Is client approver the user?               
         JNE   DOST0172            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   DOST0172            No                                           
         OI    TIM.TIMASTAT,TIMASAPR   Set item as approved                     
         OI    NEW.TT_EPST1,TIMESAPR Set item as approved                       
         OI    TR_IND,TR_ICLAT     Set we have at least one client appr         
         MVC   TIM.TIMAPDAC,HD_PPID#                                            
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIM.TIMASTAT,TIMAMOBL Set this has been done by mobile           
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIM.TIMASTAT,TIMAAURA Set this has been done by aura             
         MVC   TIM.TIMADATE,HD_TODP                                             
         MVC   TIM.TIMAUSER,LP_USRID                                            
         GOTOR GETTIM,TIM.TIMATIME                                              
*                                                                               
DOST0172 GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    DOST0180                                                         
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
         DROP  TIM                                                              
*                                                                               
         USING STCELD,R4                                                        
DOST0180 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTRWAD                                                  
         OC    NEW.TT_TOFFI,NEW.TT_TOFFI                                        
         JZ    *+8                                                              
         MVI   STCTSTA3,STCTTIMO   Mark as timeoff                              
         MVC   STCTROW,NEW.TT_TIME#                                             
         MVC   STCTMOA,NEW.TT_MOA                                               
         OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JZ    DOST0184                                                         
         MVI   STCTTYP,STCTMRAD                                                 
         MVC   STCTMROW,NEW.TT_ITEM#                                            
         ZAP   STCMCTOT,NEW.TT_ITOT                                             
         ZAP   STCMCPRC,NEW.TT_IPRCE                                            
         ZAP   STCMCMUL,NEW.TT_IMULT                                            
         MVC   STCMCCOD,NEW.TT_INUM                                             
         XR    RF,RF                                                            
         ICM   RF,1,NEW.TT_NARRL   any item text                                
         JZ    DOST0182            no                                           
         SHI   RF,1                                                             
         CHI   RF,L'STCMCTXT-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCMCTXT-1                                                  
         BASR  RE,0                                                             
         MVC   STCMCTXT(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
DOST0182 LA    R2,STCMCTXT                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    DOST0210                                                         
         J     DOST018A                                                         
*                                  Day narrative audit element                  
DOST0184 OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JZ    DOST0185                                                         
         MVI   STCTTYP,STCTDNRA                                                 
         J     DOST0188                                                         
                                                                                
DOST0185 MVC   STCTCULA,NEW.TT_AULA                                             
         MVC   STCTCTSK,NEW.TT_TSK                                              
         MVC   STCTCORD,NEW.TT_ORD                                              
         MVC   STCTEST#,NEW.TT_EST                                              
         MVC   STCTCINT,NEW.TT_INTRF                                            
         MVC   STCTCTTY,NEW.TT_TTYP                                             
         ZAP   STCTCHRS,NEW.TT_HRS                                              
         XR    RF,RF                                                            
         ICM   RF,1,NEW.TT_NARRL                                                
         JZ    DOST0186                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCTCNAR-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCTCNAR-1                                                  
         BASR  RE,0                                                             
         MVC   STCTCNAR(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
DOST0186 LA    R2,STCTCNAR                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
*                                                                               
DOST0188 GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    DOST0189                                                         
*                                                                               
DOST018A TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
*                                                                               
DOST0189 OC    NEW.TT_NAR#,NEW.TT_NAR#   Skip approval check for narr           
         JNZ   DOST0160                                                         
         OC    NEW.TT_CLIAP,NEW.TT_CLIAP Do we have an approver                 
         JZ    DOST0190                  No - treat as approved                 
         TM    NEW.TT_EPST1,TIMESAPR     Yes - is it approved                   
         JNZ   DOST0200                  Yes                                    
         TM    NEW.TT_EPST1,TIMESREJ     Is it rejected                         
         JZ    *+8                       No                                     
         OI    TR_IND,TR_IRLCL     Set rejected client approval                 
         OI    TR_IND,TR_IMCLI     Set missing client approval                  
         J     DOST0160                                                         
                                                                                
DOST0190 OI    TR_IND,TR_INCLI     No client approval required                  
         J     DOST0160                                                         
                                                                                
DOST0200 OI    TR_IND,TR_ICLIA     Some time is client approved                 
         J     DOST0220                                                         
                                                                                
DOST0210 CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    DOST0160            Yes                                          
         CLC   HD_PPID#,NEW.TT_CLIAP   Is client approver the user?             
         JNE   DOST0160            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   DOST0160            No                                           
         OI    NEW.TT_EPST1,TIMISAPR   Set item as approved                     
DOST0220 GOTOR BUFTIM,DMCB,('TSAWRT',NEWBUF),NEW.TT_D                           
         JE    DOST0160                                                         
         DC    H'0'                                                             
                                                                                
TIM      USING TIMELD,R4                                                        
DOST0230 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         XC    TIM.TIMELD(TIMALNQ),TIM.TIMELD                                   
         MVI   TIM.TIMEL,TIMELQ    Add manager TIMEL                            
         MVI   TIM.TIMLN,TIMALN1Q                                               
         MVI   TIM.TIMETYP,TIMEARIN                                             
         MVC   TIM.TIMAPULA,HD_1RULA                                            
         MVC   TIM.TIMAPIDB,HD_MANAP                                            
         MVC   TIM.TIMAPSDT,HD_DMDAT                                            
         MVC   TIM.TIMAPER#,HD_PERNO                                            
         CLI   QH_STAT,QH_SSAVQ    Test saving                                  
         JE    DOST0240                                                         
         CLC   HD_MANAP,HD_PPID#   Is line manager approver the user?           
         JNE   DOST0240            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   DOST0240            No                                           
         OI    TIM.TIMASTAT,TIMASAPR   Set approved                             
         MVC   TIM.TIMAPDAC,HD_PPID#                                            
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIM.TIMASTAT,TIMAMOBL Set this has been done via mobile          
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIM.TIMASTAT,TIMAAURA Set this has been done by aura             
         MVC   TIM.TIMADATE,HD_TODP                                             
         MVC   TIM.TIMAUSER,LP_USRID                                            
         GOTOR GETTIM,TIM.TIMATIME                                              
*                                                                               
DOST0240 GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    DOST0242                                                         
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
                                                                                
DOST0242 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTTSAD                                                  
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    DOST0250                                                         
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
         DROP  R4                                                               
***********************************************************************         
* Look at approvals and work out new overall status for time sheet    *         
***********************************************************************         
                                                                                
* Time sheet was saved previously                                               
                                                                                
DOST0250 CLI   HD_TSSTO,0          Time sheet saved before                      
         JNE   DOST0260                                                         
         CLI   QH_STAT,QH_SSUBQ    Are we now submitting?                       
         JE    DOST0280                                                         
         CLI   QH_STAT,QH_SSAVQ    Or are we saving again?                      
         JE    *+6                                                              
         DC    H'0'                Anything else from save should die           
         MVI   TR_TSSTN,0                                                       
         J     DOST0350                                                         
                                                                                
* Time sheet was previously submitted, part approved, rejected                  
                                                                                
DOST0260 TM    HD_TSSTO,TIMSSUBM   Time sheet submitted before                  
         JZ    DOST0270                                                         
         TM    HD_TSSTO,TIMSPAPP   Time sheet part approved                     
         JZ    DOST0270                                                         
         TM    HD_TSSTO,TIMSMAAP   Time sheet line manager approved             
         JZ    DOST0270                                                         
         TM    HD_TSSTO,TIMSREJE   Time sheet was rejected                      
         JZ    DOST0270                                                         
         TM    HD_TSSTO,TIMSFAPP   Time sheet was fully approved                
         JZ    DOST0270                                                         
         TM    HD_TSSTO,TIMSAWAP   Awaiting line manager approval               
         JZ    DOST0270                                                         
         DC    H'0'                                                             
                                                                                
DOST0270 CLI   QH_STAT,QH_SSUBQ    Are we now submitting?                       
         JE    DOST0280            Yes                                          
         CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JNE   DOST0340            No                                           
         TM    TR_IND,TR_IRLCL     Are any client approvals rejected            
         JZ    DOST0280            No                                           
         MVI   TR_TSSTN,TIMSREJE   Yes - mark timesheet as rejected             
         J     DOST0350                                                         
                                                                                
DOST0280 CLI   HD_COACS,C'C'       Are we approving sequential?                 
         JE    DOST0310            No - concurrently                            
         TM    TR_IND,TR_IMCLI     Are there any client approvals reqd?         
         JZ    DOST0290            No                                           
         MVI   TR_TSSTN,TIMSSUBM   Yes - set as submitted                       
         TM    TR_IND,TR_ICLIA     Have we got any client approvals?            
         JZ    DOST0350            No                                           
         MVI   TR_TSSTN,TIMSPAPP   Yes  - must be part approved                 
         J     DOST0350                                                         
                                                                                
DOST0290 TM    TR_IND,TR_ILNMG+TR_ILMGP Have we got line manager app?           
         JZ    DOST0300            No                                           
         MVI   TR_TSSTN,TIMSFAPP   Yes - must be fully approved                 
         J     DOST0350                                                         
                                                                                
DOST0300 MVI   TR_TSSTN,TIMSAWAP+TIMSSUBM Set submitted and awaiting            
         TM    TR_IND,TR_ICLIA     Have we got any client approval?             
         JZ    DOST0350            Yes - so time sheet is part approved         
         MVI   TR_TSSTN,TIMSAWAP+TIMSPAPP Awaiting line man approval            
         J     DOST0350                                                         
                                                                                
DOST0310 TM    TR_IND,TR_IMCLI     Are we missing some cli approvals?           
         JZ    DOST0330            No                                           
         TM    TR_IND,TR_ILNMG+TR_ILMGP Have we got line manager app            
         JZ    DOST0320            No                                           
         MVI   TR_TSSTN,TIMSMAAP+TIMSPAPP Yes - line manager approved           
         J     DOST0350                                                         
                                                                                
DOST0320 MVI   TR_TSSTN,TIMSAWAP+TIMSSUBM Awaiting line man approval            
         TM    TR_IND,TR_ICLIA     Have we got any client approval              
         JZ    DOST0350            No - so time sheet sub and awaiting          
         MVI   TR_TSSTN,TIMSAWAP+TIMSPAPP Yes - ts part approved n wait         
         J     DOST0350                                                         
                                                                                
DOST0330 MVI   TR_TSSTN,TIMSFAPP   Set fully approved if line man app           
         TM    TR_IND,TR_ILNMG+TR_ILMGP Have we got line manager app            
         JNZ   DOST0350            Yes                                          
         J     DOST0320                                                         
                                                                                
DOST0340 CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   *+12                                                             
         MVI   TR_TSSTN,TIMSREJE                                                
         J     DOST0350                                                         
                                                                                
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TR_TSSTN,0                                                       
                                                                                
DOST0350 TM    TR_TSSTN,TIMSSUBM   Is the time sheet status submitted?          
         JZ    DOST0370            No                                           
         CLI   HD_TSSTO,0          Was it saved before?                         
         JE    DOST0360            Yes                                          
         TM    HD_TSSTO,TIMSREJE   Was it rejected before?                      
*&&UK*&& JZ    DOST0390                                                         
*&&US                                                                           
         JNZ   DOST0360                                                         
         MVC   HD_SUBCL,HD_TODP                                                 
         MVC   HD_SUBLM,HD_TODP                                                 
         J     DOST0390                                                         
*&&                                                                             
                                                                                
DOST0360 GOTOR NOTCLI              Set cli approvers for notification           
         MVC   HD_SUBCL,HD_TODP                                                 
         CLI   HD_COACS,C'C'       Is it concurrent approval?                   
         JNE   DOST0390                                                         
         GOTOR SETMGR              Set manager for notification                 
         MVC   HD_SUBLM,HD_TODP                                                 
         J     DOST0390                                                         
                                                                                
DOST0370 TM    TR_TSSTN,TIMSAWAP   Test awaiting line manager approval          
         JZ    DOST0390            No                                           
         CLI   HD_TSSTO,0          Previous just saved?                         
         JE    DOST0380                                                         
         TM    HD_TSSTO,TIMSREJE   Was it rejected before?                      
         JNZ   DOST0380                                                         
         TM    HD_TSSTO,TIMSSUBM   Was it submitted before?                     
         JNZ   DOST0380                                                         
         TM    HD_TSSTO,TIMSPAPP   Was it part approved before?                 
         JZ    DOST0390                                                         
DOST0380 GOTOR SETMGR              Set manager for notification                 
         MVC   HD_SUBLM,HD_TODP                                                 
                                                                                
DOST0390 CLC   TR_TSSTN,HD_TSSTO   Is the time sheet status different?          
         JE    EXITY                                                            
         USING STCELD,R4                                                        
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTTIMS                                                  
         MVC   STCDTFR,HD_TSSTO                                                 
         MVC   STCDTTO,TR_TSSTN                                                 
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    EXITY                                                            
*                                                                               
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITY                                                            
         DROP  R4,NEW                                                           
         EJECT                                                                  
***********************************************************************         
* Build table of approvers - used for when adjusters make changes     *         
***********************************************************************         
                                                                                
BLDAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAPP*'                                                      
                                                                                
         L     R3,AGENAREA                                                      
         XC    0(L'TIMEPIDC,R3),0(R3)                                           
         XC    TT_D(TT_KEYL),TT_D                                               
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JZ    BLDAPP02            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
BLDAPP02 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF),0                                  
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDAPPX             Yes                                          
                                                                                
         OC    TT_ITEM#,TT_ITEM#   Ignore materials                             
         JNZ   BLDAPP02                                                         
         OC    TT_NAR#,TT_NAR#     Ignore narrative                             
         JNZ   BLDAPP02                                                         
         OC    TT_CLIAP,TT_CLIAP   Test we have an approver                     
         JZ    BLDAPP02                                                         
         TM    TT_EPST1,TIMESAPR   Is it approved?                              
         JZ    BLDAPP02                                                         
         MVC   0(L'TIMEPIDC,R3),TT_CLIAP                                        
         AHI   R3,L'TIMEPIDC                                                    
         XC    0(L'TIMEPIDC,R3),0(R3)                                           
         J     BLDAPP02                                                         
                                                                                
BLDAPPX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Log changes made and ensure approver info is correct                *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6             New time record                              
AUDCHA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDCHA*'                                                      
         LA    R6,TT_NEW                                                        
                                                                                
         CLC   HD_MANAP,CCTPID     Is it approver?                              
         JE    AUDCHA02                                                         
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDCHA02            No                                           
         NI    TR_IND,FF-(TR_ILNMG+TR_ILMGP)                                    
                                                                                
         USING STCELD,R4                                                        
AUDCHA02 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTRWAM                                                  
         OC    NEW.TT_TOFFI,NEW.TT_TOFFI                                        
         JZ    *+8                                                              
         MVI   STCTSTA3,STCTTIMO   Mark as timeoff                              
         MVC   STCTMOA,NEW.TT_MOA                                               
*                                                                               
         OC    NEW.TT_NAR#,NEW.TT_NAR# Test day narrative amendment             
         JZ    AUDCHA04                                                         
         MVC   STCTROW,NEW.TT_TIME#                                             
         MVI   STCTTYP,STCTDNRM    Set as day row narrative amendment           
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDALLX                                                          
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDCHA04 OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JZ    AUDCHA10                                                         
         MVC   STCTROW,NEW.TT_TIME#                                             
         MVI   STCTTYP,STCTMRAM    Set as row amendment                         
         MVC   STCTSTA2,BYTE1      Set type of changes                          
         MVC   STCTMROW,NEW.TT_ITEM#                                            
         ZAP   STCMCTOT,NEW.TT_ITOT                                             
         ZAP   STCMCPRC,NEW.TT_IPRCE                                            
         ZAP   STCMCMUL,NEW.TT_IMULT                                            
         MVC   STCMCCOD,NEW.TT_INUM                                             
         XR    RF,RF                                                            
         TM    STCTSTA2,STCMTEXT                                                
         JZ    AUDCHA06                                                         
         ICM   RF,1,NEW.TT_NARRL                                                
         JZ    AUDCHA06                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCMCTXT-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCMCTXT-1                                                  
         BASR  RE,0                                                             
         MVC   STCMCTXT(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
AUDCHA06 LA    R2,STCMCTXT                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDCHA07                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDCHA07 OI    APPSTAT,APPSMTCH    Set material change                          
         TM    TR_IND,TR_IAPTT     Approved this time                           
         JZ    AUDCHA08                                                         
         OI    NEW.TT_EPST1,TIMISAPR   Set item as approved                     
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   *+8                                                              
         OI    NEW.TT_EPST1,TIMISREJ   Set item as rejected                     
         J     AUDALLX             Set we found row but changed                 
                                                                                
AUDCHA08 TM    APPSTAT,APPSNTLC    Could we have been approved prev             
         JZ    AUDALLX             No as time line changed                      
         MVC   NEW.TT_EPST1,APPSTAT                                             
         NI    NEW.TT_EPST1,X'FF'-(APPSWAPP+APPSWREJ)                           
         J     AUDALLX                                                          
                                                                                
AUDCHA10 MVC   STCTROW,NEW.TT_TIME#                                             
         MVC   STCTSTAT,BYTE1      Set type of changes for time line            
         MVC   STCTCULA,NEW.TT_AULA                                             
         MVC   STCTCTSK,NEW.TT_TSK                                              
         MVC   STCTCORD,NEW.TT_ORD                                              
         MVC   STCTEST#,NEW.TT_EST                                              
         MVC   STCTCINT,NEW.TT_INTRF                                            
         MVC   STCTCTTY,NEW.TT_TTYP                                             
         ZAP   STCTCHRS,NEW.TT_HRS                                              
         XR    RF,RF                                                            
         TM    STCTSTAT,STCTTEXT                                                
         JZ    AUDCHA14                                                         
         ICM   RF,1,NEW.TT_NARRL                                                
         JZ    AUDCHA14                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCTCNAR-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCTCNAR-1                                                  
         BASR  RE,0                                                             
         MVC   STCTCNAR(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
AUDCHA14 LA    R2,STCTCNAR                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDCHA15                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
         USING TIMEL,R4                                                         
AUDCHA15 XC    TIMELD(TIMALNQ),TIMELD                                           
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,NEW.TT_AULA                                             
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVC   TIMAPIDB,NEW.TT_CLIAP                                            
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    AUDCHA16            Yes                                          
         CLC   HD_PPID#,NEW.TT_CLIAP Is client approver the user?               
         JNE   AUDCHA16            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDCHA16            No                                           
         OI    NEW.TT_EPST1,TIMESAPR                                            
         OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    TR_IND,TR_IAPTT     Set approved this time                       
         MVC   TIMAPDAC,HD_PPID#                                                
         MVC   TIMADATE,HD_TODP                                                 
         MVC   TIMAUSER,LP_USRID                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         J     AUDCHA26                                                         
                                                                                
AUDCHA16 CLC   HD_PPID#,CCTPID     Is it user?                                  
         JE    AUDCHA24            Yes                                          
         CLC   NEW.TT_CLIAP,CCTPID Is it approver for this item?                
         JE    AUDCHA18            Yes                                          
         CLI   QH_BUAPR,C'Y'       No - could it be back up approver?           
         JNE   AUDCHA24            No - must be adjuster                        
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,NEW.TT_CLIBA     Search backups approvers for match           
AUDCHA17 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    AUDCHA24                                                         
         CLC   CCTPID,0(R1)        Is user a back up client approver?           
         JE    AUDCHA18            Yes                                          
         LA    R1,L'PIDNO(R1)      Check other entries                          
         JCT   R0,AUDCHA17                                                      
         J     AUDCHA24                                                         
                                                                                
                                                                                
AUDCHA18 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    *+12                                                             
         CLI   QH_STAT,QH_SSUBQ    Are we submitting (self approval)?           
         JNE   AUDCHA20                                                         
         CLC   CCTPID,NEW.TT_CLIAP Is connected user the client app?            
         JE    *+12                                                             
         CLI   QH_BUAPR,C'Y'                                                    
         JNE   AUDCHA19                                                         
         GOTOR SETAPP,NEW.TT_CLIAP Set approved by client approver              
AUDCHA19 OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    NEW.TT_EPST1,TIMESAPR   Set item as approved                     
         OI    TR_IND,TR_IAPTT     Approved this time                           
         MVC   TIMAPDAC,CCTPID                                                  
         J     AUDCHA22                                                         
                                                                                
AUDCHA20 CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   AUDCHA24                                                         
         OI    TIMASTAT,TIMASREJ   Set item as rejected                         
         OI    NEW.TT_EPST1,TIMESREJ   Set item as rejected                     
         OI    TR_IND,TR_IAPTT     Approved this time                           
         MVC   TIMAPDAC,CCTPID                                                  
                                                                                
AUDCHA22 MVC   TIMADATE,HD_TODP                                                 
         MVC   TIMAUSER,LP_USRID                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDCHA23                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDCHA23 CLI   QH_COMSL,0          Test any rejection comments                  
         JE    AUDCHA28                                                         
         GOTOR BLDCOM,NEW.TT_TIME# Build comments element if required           
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDCHA28                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDCHA24 GOTOR NOTAPP,TIMAPULA     Notify approver as changed                   
                                                                                
AUDCHA26 GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDCHA28                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
*                                                                               
AUDCHA28 MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMDLNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,FF                                                       
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDALLX                                                          
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
         DROP  NEW                                                              
         EJECT                                                                  
***********************************************************************         
* Log additions made and ensure approver info is correct              *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6                                                          
AUDADD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDADD*'                                                      
         LA    R6,TT_NEW                                                        
                                                                                
         CLC   HD_MANAP,CCTPID     Is it approver?                              
         JE    AUDADD02                                                         
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDADD02            No                                           
         NI    TR_IND,FF-(TR_ILNMG+TR_ILMGP) Remove manager approval            
                                                                                
         USING STCELD,R4                                                        
AUDADD02 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTRWAD                                                  
         OC    NEW.TT_TOFFI,NEW.TT_TOFFI                                        
         JZ    *+8                                                              
         MVI   STCTSTA3,STCTTIMO   Mark as timeoff                              
         MVC   STCTMOA,NEW.TT_MOA                                               
         MVC   STCTROW,NEW.TT_TIME#                                             
*                                                                               
         OC    NEW.TT_NAR#,NEW.TT_NAR# Test day narrative addition              
         JZ    AUDADD06                                                         
         MVC   STCTROW,NEW.TT_TIME#                                             
         MVI   STCTTYP,STCTDNRA    Set as day row narrative addition            
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDALLX                                                          
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDADD06 OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JZ    AUDADD12                                                         
         MVI   STCTTYP,STCTMRAD                                                 
         MVC   STCTMROW,NEW.TT_ITEM#                                            
         ZAP   STCMCTOT,NEW.TT_ITOT                                             
         ZAP   STCMCPRC,NEW.TT_IPRCE                                            
         ZAP   STCMCMUL,NEW.TT_IMULT                                            
         MVC   STCMCCOD,NEW.TT_INUM                                             
         XR    RF,RF                                                            
         ICM   RF,1,NEW.TT_NARRL       any item text                            
         JZ    AUDADD08            no                                           
         SHI   RF,1                                                             
         CHI   RF,L'STCMCTXT-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCMCTXT-1                                                  
         BASR  RE,0                                                             
         MVC   STCMCTXT(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
AUDADD08 LA    R2,STCMCTXT                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDADD09                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDADD09 OI    APPSTAT,APPSMTAD    Set material addition                        
         TM    TR_IND,TR_IAPTT     Approved this time                           
         JZ    AUDADD10                                                         
         OI    NEW.TT_EPST1,TIMISAPR Set item as approved                       
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   *+8                                                              
         OI    NEW.TT_EPST1,TIMISREJ Set item as rejected                       
         J     AUDALLX             Set we found row but changed                 
                                                                                
AUDADD10 TM    APPSTAT,APPSNTLC    Could we have been approved prev             
         JZ    AUDALLX             No as time line changed                      
         MVC   NEW.TT_EPST1,APPSTAT                                             
         NI    NEW.TT_EPST1,X'FF'-(APPSWAPP+APPSWREJ)                           
         J     AUDALLX                                                          
                                                                                
AUDADD12 MVC   STCTCULA,NEW.TT_AULA                                             
         MVC   STCTCTSK,NEW.TT_TSK                                              
         MVC   STCTCORD,NEW.TT_ORD                                              
         MVC   STCTEST#,NEW.TT_EST                                              
         MVC   STCTCINT,NEW.TT_INTRF                                            
         MVC   STCTCTTY,NEW.TT_TTYP                                             
         ZAP   STCTCHRS,NEW.TT_HRS                                              
         XR    RF,RF                                                            
         ICM   RF,1,NEW.TT_NARRL                                                
         JZ    AUDADD14                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCTCNAR-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCTCNAR-1                                                  
         BASR  RE,0                                                             
         MVC   STCTCNAR(0),NEW.TT_NARR                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
AUDADD14 LA    R2,STCTCNAR                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDADD16                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
         USING TIMEL,R4                                                         
AUDADD16 XC    TIMELD(TIMALNQ),TIMELD                                           
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,NEW.TT_AULA                                             
         OC    NEW.TT_ITEM#,NEW.TT_ITEM#  Skip item                             
         JNZ   AUDADD18                                                         
         MVC   TIMAPIDB,NEW.TT_CLIAP                                            
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    AUDADD18            Yes                                          
         CLC   HD_PPID#,NEW.TT_CLIAP Is client approver the user?               
         JNE   AUDADD18            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDADD18            No                                           
         OI    NEW.TT_EPST1,TIMESAPR                                            
         OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    TR_IND,TR_IAPTT     Approved this time                           
         MVC   TIMAPDAC,HD_PPID#                                                
         MVC   TIMADATE,HD_TODP                                                 
         MVC   TIMAUSER,LP_USRID                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         J     AUDADD38                                                         
                                                                                
AUDADD18 CLC   HD_PPID#,CCTPID     Is it user?                                  
         JE    AUDADD36            Yes                                          
         CLC   NEW.TT_CLIAP,CCTPID Is it approver for this item?                
         JE    AUDADD22            Yes                                          
         CLI   QH_BUAPR,C'Y'       No - could it be back up approver            
         JNE   AUDADD28            No - must be adjuster                        
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,NEW.TT_CLIBA     Search backups approvers for match           
AUDADD20 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    AUDADD28                                                         
         CLC   CCTPID,0(R1)        Is user a back up client approver?           
         JE    AUDADD22            Yes                                          
         LA    R1,L'PIDNO(R1)      Check other entries                          
         JCT   R0,AUDADD20                                                      
         J     AUDADD28                                                         
                                                                                
AUDADD22 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    *+12                                                             
         CLI   QH_STAT,QH_SSUBQ    Are we submitting (self approval)?           
         JNE   AUDADD26                                                         
         CLC   CCTPID,NEW.TT_CLIAP Is connected user the client app?            
         JE    *+12                                                             
         CLI   QH_BUAPR,C'Y'       Backup approval?                             
         JNE   AUDADD24                                                         
         GOTOR SETAPP,NEW.TT_CLIAP Set approved by client approver              
AUDADD24 OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    NEW.TT_EPST1,TIMESAPR                                            
         MVC   TIMAPDAC,CCTPID                                                  
         J     AUDADD32                                                         
                                                                                
AUDADD26 CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   AUDADD28                                                         
         OI    TIMASTAT,TIMASREJ   Set item as rejected                         
         OI    NEW.TT_EPST1,TIMESREJ                                            
         MVC   TIMAPDAC,CCTPID                                                  
         J     AUDADD32                                                         
                                                                                
AUDADD28 MVC   ANYACCNT,TIMAPULA                                                
         OC    NEW.TT_CLIAP,NEW.TT_CLIAP  Any client approver?                  
         JZ    AUDADD30            For audit treat as approved                  
         GOTOR CHKAPP              Find whether it's already cli app            
         JNE   AUDADD36            No                                           
         OI    NEW.TT_EPST1,TIMESAPR                                            
*                                                                               
AUDADD30 OI    TIMASTAT,TIMASAPR   Set item as approved                         
         MVC   TIMAPDAC,CCTPID                                                  
                                                                                
AUDADD32 MVC   TIMADATE,HD_TODP                                                 
         MVC   TIMAUSER,LP_USRID                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDADD34                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDADD34 OI    TR_IND,TR_IAPTT     Set approved this time                       
         CLI   QH_COMSL,0          Test any rejection comments                  
         JE    AUDALLX                                                          
         GOTOR BLDCOM,NEW.TT_TIME# Build comments element if required           
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDALLX                                                          
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
                                                                                
AUDADD36 GOTOR NOTAPP,TIMAPULA     Notify approver as changed                   
*                                                                               
AUDADD38 GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDADD40                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
AUDADD40 J     AUDALLX                                                          
         DROP  NEW                                                              
         EJECT                                                                  
***********************************************************************         
* No changes but time could be approved - need to log this for audit  *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6                                                          
AUDAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDAPP*'                                                      
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         LA    R6,TT_NEW                                                        
                                                                                
         OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JZ    AUDAPP06                                                         
         TM    TR_IND,TR_IAPTT     Approved this time                           
         JZ    AUDAPP02                                                         
         OI    NEW.TT_EPST1,TIMISAPR   Set item as approved                     
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   *+8                                                              
         OI    NEW.TT_EPST1,TIMISREJ   Set item as rejected                     
         J     AUDAPP04            Set we found row but changed                 
                                                                                
AUDAPP02 TM    APPSTAT,APPSNTLC    Could we have been approved prev             
         JZ    AUDAPP04            No as time line changed                      
         MVC   NEW.TT_EPST1,APPSTAT                                             
         NI    NEW.TT_EPST1,X'FF'-(APPSWAPP+APPSWREJ)                           
AUDAPP04 MVC   NEW.TT_IIND,TT_IIND                                              
*        J     AUDALLX                                                          
                                                                                
AUDAPP06 CLI   QH_UTYP,QH_UAPRQ    Are we approving/rejecting only?             
         JNE   AUDAPP08            No                                           
         CLI   QH_STAT,QH_SSUBQ    With Aura we can submit from list            
         JE    AUDAPP08            so treat as non approval                     
         CLI   HD_COTUP,COSAVED    Were postings made when saved?               
         JE    AUDAPP20            Yes - don't need to validate                 
         CLI   HD_COTUP,COSUBMD    Were postings made when submitted?           
         JE    AUDAPP20            Yes - don't need to validate again           
         CLI   HD_COTUP,COFUAPR    Are postings made at fully approved          
         JNE   AUDAPP07            No                                           
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JNE   AUDAPP07            No - must be approving                       
         TM    HD_TSSTO,TIMSFAPP   If time sheet was fully approved             
         JZ    AUDAPP20            previously - we could post again             
*                                  if rejecting and must validate line          
AUDAPP07 GOTOR CHKTMD              Check details                                
         J     AUDAPP20                                                         
                                                                                
AUDAPP08 CLI   HD_COTUP,COFUAPR    Postings made at fully approved?             
         JNE   AUDAPP12            No                                           
         TM    HD_TSSTO,TIMSFAPP   If time sheet was fully approved             
         JZ    AUDAPP10            previously - we could delete                 
         CLI   QH_STAT,QH_SDELQ    Are we deleting?                             
         JE    AUDAPP18            Yes - always check time lines                
         J     AUDAPP20            No as won't be changing lines                
                                                                                
AUDAPP10 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    AUDAPP18                                                         
         CLI   HD_COTUP,COFUAPR    No - but we could be self approver           
         JE    AUDAPP14            submitting and therefore check               
                                                                                
AUDAPP12 CLI   HD_COTUP,COSUBMD    Postings created at submitted                
         JNE   AUDAPP20                                                         
AUDAPP14 CLI   QH_STAT,QH_SSUBQ    Are we now submitting and therefore          
         JNE   AUDAPP20            creating postings                            
                                                                                
AUDAPP18 GOTOR CHKTMD              Check details                                
                                                                                
AUDAPP20 CLI   HD_COTUP,COSAVED    Is TUP saved?                                
         JE    *+12                                                             
         CLI   HD_COTUP,COSUBMD    Postings created at submitted                
         JNE   AUDAPP21                                                         
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   AUDAPP2A                                                         
         MVC   NEW.TT_MOA,TT_MOA   then nothing changed and no postings         
         MVC   NEW.TT_OMOA,TT_OMOA needed                                       
         J     AUDAPP2A                                                         
*                                                                               
AUDAPP21 CLC   NEW.TT_MOA,TT_MOA   Is it higher than new?                       
         JNL   *+10                                                             
         MVC   NEW.TT_MOA,TT_MOA   As nothing has changed set as before         
                                                                                
AUDAPP2A OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JNZ   *+8                                                              
         OI    APPSTAT,APPSNTLC    Set no time line change                      
         CLC   TT_CLIAP,NEW.TT_CLIAP Is the approver the same as before         
         JE    AUDAPP22                                                         
         OI    NEW.TT_BSTAT,TT_BSRCR no - set change status                     
         OI    TR_IND,TR_ICLAT     and ensure time records updated              
AUDAPP22 CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    AUDAPP24            Yes                                          
         CLC   HD_PPID#,NEW.TT_CLIAP   Is client approver the user?             
         JNE   AUDAPP24            No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDAPP24            No                                           
         OC    NEW.TT_ITEM#,NEW.TT_ITEM# Skip item and narrative                
         JNZ   AUDALLX                                                          
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   AUDALLX                                                          
         USING TIMEL,R4                                                         
         OI    NEW.TT_EPST1,TIMESAPR                                            
         XC    TIMELD(TIMALNQ),TIMELD                                           
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,ANYACCNT                                                
         MVC   TIMAPIDB,NEW.TT_CLIAP                                            
         OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    TR_IND,TR_IAPTT     Approved this time                           
         MVC   TIMAPDAC,HD_PPID#                                                
         MVC   TIMADATE,HD_TODP                                                 
         MVC   TIMAUSER,LP_USRID                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDAPP42                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDAPP24 CLC   HD_PPID#,CCTPID     Is it user?                                  
         JE    AUDAPP28            Yes                                          
         CLC   NEW.TT_CLIAP,CCTPID Is it approver for this item?                
         JE    AUDAPP26            Yes                                          
         CLI   QH_BUAPR,C'Y'       No - could it be back up approver?           
         JNE   AUDAPP28            No - must be adjuster                        
         LA    R0,HD_BAMAX         Max Backup Approvers                         
         LA    R1,NEW.TT_CLIBA     Search backups approvers for match           
AUDAPP25 OC    0(L'PIDNO,R1),0(R1) Any Pids left in table?                      
         JZ    AUDAPP28                                                         
         CLC   CCTPID,0(R1)        Is user a back up client approver?           
         JE    AUDAPP26            Yes                                          
         LA    R1,L'PIDNO(R1)      Check other entries                          
         JCT   R0,AUDAPP25                                                      
         J     AUDAPP28                                                         
                                                                                
                                                                                
AUDAPP26 CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    AUDAPP30                                                         
         CLI   QH_STAT,QH_SSUBQ    Are we submitting (self approval)?           
         JE    AUDAPP32                                                         
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JE    AUDAPP34                                                         
                                                                                
AUDAPP28 MVC   NEW.TT_EPST1,TT_EPST1                                            
         TM    NEW.TT_BSTAT,TT_BSRCO Was record copied?                         
         JZ    *+12                                                             
         TM    NEW.TT_EPST1,TIMESREJ Was it previously rejected?                
         JZ    AUDAPP29                                                         
         CLI   QH_STAT,QH_SSUBQ    Are we submitting?                           
         JNE   AUDAPP29            No                                           
         NI    NEW.TT_EPST1,X'FF'-TIMESREJ                                      
         OI    NEW.TT_BSTAT,TT_BSRCR                                            
AUDAPP29 OC    APPSTAT,NEW.TT_EPST1                                             
         J     AUDALLX                                                          
                                                                                
AUDAPP30 CLI   QH_BUAPR,C'Y'       Backup approval?                             
         JNE   AUDAPP34                                                         
AUDAPP32 GOTOR SETAPP,NEW.TT_CLIAP Set approved by client approver              
         J     *+14                                                             
                                                                                
         USING TIMEL,R4                                                         
AUDAPP34 OC    NEW.TT_ITEM#,NEW.TT_ITEM# Skip item and narrative                
         JNZ   AUDALLX                                                          
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   AUDALLX                                                          
         XC    TIMELD(TIMALNQ),TIMELD                                           
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,ANYACCNT                                                
         MVC   TIMAPIDB,NEW.TT_CLIAP                                            
         MVC   TIMAUSER,LP_USRID                                                
         CLI   QH_STAT,QH_SREJQ    Are we rejecting?                            
         JE    AUDAPP36                                                         
         OI    TIMASTAT,TIMASAPR   Set item as approved                         
         OI    TR_IND,TR_IAPTT     Approved this time                           
         OI    NEW.TT_EPST1,TIMESAPR                                            
         MVC   TIMAPDAC,CCTPID                                                  
         J     AUDAPP38                                                         
                                                                                
AUDAPP36 OI    TIMASTAT,TIMASREJ   Set item as rejected                         
         OI    NEW.TT_EPST1,TIMESREJ                                            
         OI    TR_IND,TR_IAPTT     Approved/rejected this time                  
         MVC   TIMAPDAC,CCTPID                                                  
                                                                                
AUDAPP38 MVC   TIMADATE,HD_TODP                                                 
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMASTAT,TIMAAURA Set this has been done by aura                 
         GOTOR GETTIM,TIMATIME                                                  
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDAPP40                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDAPP40 CLI   QH_COMSL,0          Test any rejection comments                  
         JE    AUDAPP42                                                         
         GOTOR BLDCOM,TT_TIME#     Build comments element if necessary          
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDAPP42                                                         
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
*                                                                               
AUDAPP42 MVI   TIMEL,TIMELQ        Set to delete old TIMEL for app info         
         MVI   TIMLN,TIMDLNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,FF                                                       
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDALLX                                                          
         CLI   RUNMODE,RVALREQQ    Are we online?                               
         JE    *+6                 yes                                          
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$TMROW) too many rows                             
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     AUDALLX                                                          
         DROP  NEW                                                              
         EJECT                                                                  
***********************************************************************         
* Build rejection/approval comments element                           *         
*                                                                     *         
* Ntry:- R0=Current time                                              *         
*        R1=Time line row number or zero                              *         
*        R4=A(Where to build comments element)                        *         
***********************************************************************         
                                                                                
BLDCOM   LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         XC    TIMELD(256),TIMELD                                               
         CLI   QH_COMSL,0          Test any rejection comments                  
         BER   RE                                                               
         MVI   TIMEL,TIMELQ                                                     
         LTR   R1,R1               Test time row number passed                  
         JZ    *+10                                                             
         MVC   TIMRIDNO,0(R1)                                                   
         MVI   TIMETYP,TIMERJAP                                                 
         MVI   TIMRTYP,TIMRTCLI                                                 
         STCM  R0,15,TIMRRTME                                                   
         MVC   TIMRRDTE,HD_TODP                                                 
         MVC   TIMRUSER,LP_USRID                                                
         MVC   TIMRRPID,CCTPID                                                  
         MVI   TIMRRSTA,TIMRRAPP                                                
         CLI   QH_STAT,QH_SAPRQ    Are we approving?                            
         JE    *+8                 Yes                                          
         MVI   TIMRRSTA,TIMRRREJ   No - must be rejecting                       
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    TIMRRSTA,TIMRMOBL   Set this has been done via mobile            
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    TIMRRSTA,TIMRAURA   Set this has been done by aura               
         LLC   RF,QH_COMSL                                                      
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   TIMREJAP(0),QH_COMS                                              
         EX    RF,0(R1)                                                         
         AHI   RF,TIMRLN1Q+1                                                    
         STC   RF,TIMLN                                                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Log time and material lines that are deleted                        *         
***********************************************************************         
                                                                                
         USING TI_RECD,R2                                                       
AUDDEL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDDEL*'                                                      
                                                                                
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         XC    TT_D(TT_KEYL),TT_D                                               
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         J     AUDDEL04                                                         
AUDDEL02 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF),0                                  
AUDDEL04 TM    TIMERR,TSEEOF       Have we reached end of this buffer           
         JNZ   AUDALLX                                                          
         TM    TT_BSTAT,TT_BSROB   Test this exists in the new buffer           
         JNZ   AUDDEL02                                                         
         TM    TT_BSTAT,TT_BSRNR   or is to be treated as a new record          
         JNZ   AUDDEL02                                                         
                                                                                
         CLC   HD_MANAP,CCTPID     Is it approver?                              
         JE    AUDDEL06                                                         
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   AUDDEL06            No                                           
         NI    TR_IND,FF-(TR_ILNMG+TR_ILMGP) Remove manager approval            
                                                                                
         USING STCELD,R4                                                        
AUDDEL06 LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTRWDL                                                  
         OC    TT_TOFFI,TT_TOFFI                                                
         JZ    *+8                                                              
         MVI   STCTSTA3,STCTTIMO   Mark as timeoff                              
         MVC   STCTROW,TT_TIME#                                                 
         OC    TT_NAR#,TT_NAR#     Test day narrative deletion                  
         JZ    *+8                                                              
         MVI   STCTTYP,STCTDNRD    Set as day row narrative deletion            
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         OC    TT_ITEM#,TT_ITEM#                                                
         JZ    AUDDEL10                                                         
         MVC   STCTMROW,TT_ITEM#                                                
         MVI   STCTTYP,STCTMRDL                                                 
         MVI   STCLN,STCLN4Q                                                    
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDDEL02                                                         
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   AUDALLX                                                          
         DC    H'0'                                                             
                                                                                
         USING TIMELD,R4                                                        
AUDDEL10 MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMDLNQ                                                    
         MVC   TIMAIDNO,TT_TIME#                                                
         MVI   TIMETYP,FF                                                       
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    AUDDEL02                                                         
         TM    TSARGENL+(TSERRS-TSARD),TSEEOF  Buffer full!                     
         JNZ   AUDALLX                                                          
         DC    H'0'                                                             
*                                                                               
AUDALLX  XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Reset approval on time if materials change                          *         
***********************************************************************         
         USING TI_RECD,R2                                                       
NEW      USING TT_D,R6                                                          
RESAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*RESAPP*'                                                      
                                                                                
         LA    R6,TT_NEW                                                        
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         TM    TR_IND,TR_IAPTT     Approved this time?                          
         JNZ   RESAPPX             Yes - therefore no reseting to do            
         TM    APPSTAT,APPSNTLC    Any time line change?                        
         JZ    RESAPPX             Yes - will already reset approver            
         TM    APPSTAT,APPSMTAD+APPSMTCH Any material changes?                  
         JZ    RESAPPX             No - so nothing to do                        
         TM    APPSTAT,APPSWAPP    Previously approved?                         
         JNZ   RESAPP02                                                         
         TM    APPSTAT,APPSWREJ    Previously rejected?                         
         JZ    RESAPPX                                                          
                                                                                
RESAPP02 ICM   R0,15,NEW.TT_TIME#  Save key of current record                   
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         MVC   NEW.TT_TIME#,TIME#  Read for time record for this line           
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   RESAPP06            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
RESAPP04 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   RESAPPX             Yes                                          
                                                                                
RESAPP06 OC    NEW.TT_NAR#,NEW.TT_NAR#  Skip day narr lines                     
         JNZ   RESAPP04                                                         
         CLC   NEW.TT_TIME#,TIME#  Is this the same time line?                  
         JNE   RESAPP12            No                                           
         OC    NEW.TT_ITEM#,NEW.TT_ITEM# Is it a materials record?              
         JZ    RESAPP08                                                         
                                                                                
         MVI   NEW.TT_EPST1,0      Reset status on materials                    
         J     RESAPP10                                                         
                                                                                
RESAPP08 TM    APPSTAT,APPSMTCH    Any material changes or additions?           
         JNZ   *+14                No                                           
         CLC   HD_PPID#,CCTPID     Is it user?                                  
         JNE   RESAPP12            No - adjuster keep status as before          
                                                                                
         MVI   NEW.TT_EPST1,0      Reset status                                 
                                                                                
         USING TIMEL,R4                                                         
         XC    TIMELD(TIMALNQ),TIMELD                                           
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,ANYACCNT                                                
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVC   TIMAPIDB,NEW.TT_CLIAP                                            
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMDLNQ                                                    
         MVC   TIMAIDNO,NEW.TT_TIME#                                            
         MVI   TIMETYP,FF                                                       
         GOTOR BUFGEN,DMCB,('TSAADD',TIMBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR NOTAPP,TIMAPULA     Notify approver as changed                   
                                                                                
RESAPP10 GOTOR BUFTIM,DMCB,('TSAWRT',NEWBUF),NEW.TT_D                           
         JE    RESAPP04                                                         
         DC    H'0'                                                             
                                                                                
RESAPP12 STCM  R0,15,NEW.TT_#      Re-read caller's last record                 
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         JE    RESAPPX                                                          
         DC    H'0'                                                             
                                                                                
RESAPPX  J     EXIT                                                             
         EJECT                                                                  
         DROP  NEW                                                              
***********************************************************************         
* Set TSAR buffered record to rebuild element clusters on time records*         
* where time lines have had changes or additions to daily comments    *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6                                                          
RBLCLS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*RBLCLS*'                                                      
                                                                                
         CLI   QH_UTYP,QH_UAPRQ    Are we approving/rejecting only?             
         JE    RBLCLSX             Yes - nothing will change other              
         MVI   RBLIND,0                                                         
         LA    R6,TT_NEW                  than status                           
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   RBLCLS06            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
RBLCLS04 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JZ    RBLCLS06            No                                           
         OI    RBLIND,RBLIEOF      Set end of buffer                            
         TM    RBLIND,RBLICHG+RBLINCH   Do we have a mix of                     
         JO    RBLCLS20                   changes/additions and no chng         
         J     RBLCLS32                                                         
                                                                                
RBLCLS06 OC    NEW.TT_NAR#,NEW.TT_NAR#  Is it day narr lines                    
         JNZ   RBLCLS10                 Yes                                     
         OC    NEW.TT_ITEM#,NEW.TT_ITEM# Is it material lines                   
         JNZ   RBLCLS10                 Yes                                     
         MVC   SVTT_#,NEW.TT_TIME#                                              
         TM    RBLIND,RBLICHG+RBLINCH   Do we have a mix of                     
         JO    RBLCLS20                   changes/additions and no chng         
         MVC   TIME#,NEW.TT_TIME#  Save row number for time line                
         MVC   TIMCAULA,NEW.TT_CAULA Save contra account                        
         MVC   TIMCOFF,NEW.TT_COFF  Save contra office                          
         NI    RBLIND,X'FF'-(RBLICHG+RBLINCH)                                   
                                                                                
RBLCLS10 TM    NEW.TT_BSTAT,TT_BSRNR+TT_BSRCR+TT_BSRIA                          
         JNZ   RBLCLS12                                                         
         OI    RBLIND,RBLINCH      Set no change/addtion                        
         J     RBLCLS04                                                         
RBLCLS12 OI    RBLIND,RBLICHG      Set change/addition                          
         J     RBLCLS04                                                         
                                                                                
RBLCLS20 XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         MVC   NEW.TT_TIME#,TIME#  Read for row number                          
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   RBLCLS24            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
RBLCLS22 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   RBLCLS30            Yes                                          
                                                                                
RBLCLS24 CLC   NEW.TT_CAULA,TIMCAULA  Match on contra and office as             
         JNE   RBLCLS22                these fields are on the key              
         CLC   NEW.TT_COFF,TIMCOFF      of the time record                      
         JNE   RBLCLS22                                                         
         TM    NEW.TT_BSTAT,TT_BSRNR+TT_BSRCR+TT_BSRIA Is buffer rec            
         JNZ   RBLCLS22                    already set as change/add            
         OI    NEW.TT_BSTAT,TT_BSRCR  Set as change to cause rebuild            
         GOTOR BUFTIM,DMCB,('TSAWRT',NEWBUF),NEW.TT_D                           
         JE    RBLCLS22                                                         
         DC    H'0'                                                             
                                                                                
RBLCLS30 TM    RBLIND,RBLIEOF           Have we reached end                     
         JNZ   RBLCLS32                 Yes                                     
         MVI   RBLIND,0                                                         
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         MVC   NEW.TT_#,SVTT_# Re-read caller's last record                     
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         JE    RBLCLS06                                                         
         DC    H'0'                                                             
                                                                                
* Read old buffer to find deletes and ensure we refresh sequential recs         
* with same contra/office                                                       
                                                                                
RBLCLS32 MVI   RBLIND,0                                                         
         XC    TT_D(TT_KEYL),TT_D                                               
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   RBLCLS36            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
                                                                                
RBLCLS34 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF),0                                  
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JZ    RBLCLS36            No                                           
         OI    RBLIND,RBLIEOF      Set end of buffer                            
         TM    RBLIND,RBLIDEL      Do we have a delete                          
         JO    RBLCLS50             yes rebuld sequential recs                  
         J     RBLCLSX                                                          
                                                                                
RBLCLS36 OC    TT_NAR#,TT_NAR#          Is it day narr lines                    
         JNZ   RBLCLS40                 Yes                                     
         OC    TT_ITEM#,TT_ITEM#        Is it material lines                    
         JNZ   RBLCLS40                 Yes                                     
         MVC   SVTT_#,TT_TIME#                                                  
         TM    RBLIND,RBLIDEL       If deletes present need to rebuild          
         JO    RBLCLS50               sequential records to avoid gaps          
         MVC   TIME#,TT_TIME#  Save row number for time line                    
         MVC   TIMCAULA,TT_CAULA Save contra account                            
         MVC   TIMCOFF,TT_COFF  Save contra office                              
         NI    RBLIND,X'FF'-(RBLICHG+RBLIDEL)                                   
                                                                                
RBLCLS40 TM    TT_BSTAT,TT_BSROB   Does line still exist                        
         JNZ   RBLCLS42            Yes                                          
         OI    RBLIND,RBLIDEL      Set deletion on timeline                     
         J     RBLCLS34                                                         
RBLCLS42 OI    RBLIND,RBLICHG      Set change                                   
         J     RBLCLS34                                                         
                                                                                
RBLCLS50 XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         MVC   NEW.TT_TIME#,TIME#  Read for row number                          
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JZ    RBLCLS54            No                                           
         J     RBLCLSX             Yes nothing more to do                       
                                                                                
RBLCLS52 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   RBLCLS60            Yes                                          
                                                                                
RBLCLS54 CLC   NEW.TT_CAULA,TIMCAULA  Match on contra and office as             
         JNE   RBLCLS52                these fields are on the key              
         CLC   NEW.TT_COFF,TIMCOFF      of the time record                      
         JNE   RBLCLS52                                                         
         TM    NEW.TT_BSTAT,TT_BSRNR+TT_BSRCR+TT_BSRIA Is buffer rec            
         JNZ   RBLCLS52                    already set as change/add            
         OI    NEW.TT_BSTAT,TT_BSRCR  Set as change to cause rebuild            
         GOTOR BUFTIM,DMCB,('TSAWRT',NEWBUF),NEW.TT_D                           
         JE    RBLCLS52                                                         
         DC    H'0'                                                             
                                                                                
RBLCLS60 TM    RBLIND,RBLIEOF           Have we reached end                     
         JNZ   RBLCLSX                  Yes                                     
         MVI   RBLIND,0                                                         
         XC    TT_D(TT_KEYL),TT_D                                               
         MVC   TT_#,SVTT_# Re-read caller's last record                         
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF),0                                  
         JE    RBLCLS36                                                         
         DC    H'0'                                                             
RBLCLSX  J     EXIT                                                             
         EJECT                                                                  
         DROP  NEW                                                              
***********************************************************************         
* Build basic status element - R1=STCTTYP value, R4=A(Element)        *         
***********************************************************************         
                                                                                
         USING STCELD,R4                                                        
BLDSTC   NTR1  LABEL=NO                                                         
         XC    STCELD(256),STCELD                                               
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCLN3Q                                                    
         STC   R1,STCTTYP                                                       
         MVI   STCIND,STCITIME                                                  
         MVC   STCTDTE,HD_TODP5                                                 
         MVC   STCTUSR,LP_USRID                                                 
         GOTOR GETTIM,STCTTIM                                                   
         MVC   STCTPID,CCTPID                                                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   *+8                                                              
         OI    STCTSTA3,STCTMOBL   Set this has been done via the               
         CHI   RF,XPRODIKQ                                                      
         JNE   *+8                                                              
         OI    STCTSTA3,STCTAURA   Set this has been done by aura               
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Get current time in packed decimal                                  *         
*                                                                     *         
* Ntry:- R1=A(Where to store time value)                              *         
* Exit:- R0=Current time (0hhmmssC)                                   *         
***********************************************************************         
                                                                                
GETTIM   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         OILL  GR0,X'000C'          'OR' in packed sign value                   
         STCM  R0,15,0(R2)                                                      
*&&US*&& AP    0(4,R2),=P'60000'   Adjust to real (EST) time in US              
*&&US*&& ICM   R0,15,0(R2)         and update R0                                
         XIT1  REGS=(R0)           Exit with R0 intact                          
         EJECT                                                                  
***********************************************************************         
* Build a new time buffer record and add it to new time buffer        *         
***********************************************************************         
                                                                                
NEWTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*NEWTIM*'                                                      
                                                                                
         GOTOR CLRTTD              Clear time record                            
                                                                                
         MVC   TT_TIME#,QT_TIME#   Set time line row number                     
         MVC   TT_ITEM#,QI_ITEM#   Set item row rumber                          
         MVC   TT_CTIME,TT_TIME#   Set time row number for sorting              
         MVC   TT_CITEM,TT_ITEM#   Set item row number for sorting              
                                                                                
         TM    HD_IND,FW_INOPR     No previous time records found               
         JZ    *+8                                                              
         OI    TT_BSTAT,TT_BSRNR   All records must be adds                     
                                                                                
         MVI   TT_SEQ,FF           Set high sequence number for adds            
         MVI   TT_CKSBR,FF         Set high record sequence number              
         MVC   TT_AULA,TL_ULA      Set SJ/1N account                            
         MVC   TT_TSK,TL_TSK       Set work code                                
         MVC   TT_OFF,TL_SJOFF                                                  
         MVC   TT_MED,TL_MED                                                    
         GOTOR GETCAP,TT_D                                                      
         CLC   TL_SJOFF,SPACES                                                  
         JH    NEWTIM02                                                         
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    NEWTIM02                                                         
*        CLI   TL_TTYPE,QT_TNTIM   Was time type N                              
*        JNE   *+10                                                             
         MVC   TT_OFF,HD_1ROFF                                                  
NEWTIM02 OC    TT_OFF,SPACES       Make sure it's valid for key                 
         MVC   TT_MOA,HD_DMMOA                                                  
         MVC   TT_OMOA,HD_DMMOA    Store original month of service              
         MVI   TT_STAT,TIMSMCS                                                  
         MVC   TT_ADAT,HD_TODP                                                  
         MVC   TT_CAULA,TL_ULA                                                  
         CLC   PRODUL,TL_ULA       It's either SJ or 1N                         
         JNE   *+10                                                             
         MVC   TT_CAULA,TL_1CULA   SJ so contra is 1C                           
         MVC   TT_INULA,TL_INULA   Set income account                           
                                                                                
         MVC   TT_NARR,SPACES      Preset description to spaces                 
         ICM   R1,15,QX_DESCI      Test any narrative                           
         JZ    NEWTIM04                                                         
         LLC   RF,LW_DATA1-LW_D(R1)                                             
         STC   RF,TT_NARRL                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TT_NARR(0),LW_DATA1+1-LW_D(R1)                                   
         EX    RF,0(RE)                                                         
                                                                                
NEWTIM04 MVC   TT_ORD,QT_ORDER                                                  
         MVC   TT_EST,QT_EST#                                                   
         ICM   R1,15,QT_TOFFI      Test any narrative                           
         JZ    NEWTIM05                                                         
         LLC   RF,LW_DATA1-LW_D(R1)                                             
         STC   RF,TT_TOFFL                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TT_TOFFI(0),LW_DATA1+1-LW_D(R1)                                  
         EX    RF,0(RE)                                                         
*                                                                               
NEWTIM05 MVC   TT_INTRF,QT_INTRF                                                
         CLI   RECTYPE,RECTTIMQ    Test time record                             
         JNE   NEWTIM20                                                         
         CLI   TL_TTYPE,QT_TBTIM   Set type of time                             
         JNE   *+8                                                              
         MVI   TT_TTYP,TIMTCB                                                   
         CLI   TL_TTYPE,QT_TETIM                                                
         JNE   *+8                                                              
         MVI   TT_TTYP,TIMTEM                                                   
         CLI   TL_TTYPE,QT_TRTIM                                                
         JNE   *+8                                                              
         MVI   TT_TTYP,TIMTCR                                                   
         CLI   TL_TTYPE,QT_TNTIM                                                
         JNE   NEWTIM06                                                         
         MVI   TT_TTYP,TIMTCN                                                   
         CLC   PRODUL,TT_AULA                                                   
         JE    NEWTIM06                                                         
         MVI   TT_TTYP,TIMTNC                                                   
                                                                                
NEWTIM06 OC    TT_TTYP,TT_TTYP     check we have a time type                    
         JNZ   NEWTIM07                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPMOBILQ         Check mobile app connected                   
         JE    *+6                 If not dump as they should always            
         DC    H'0'                 pass a type                                 
         MVC   ROUERRV,=AL2(AE$DTTMO)                                           
         J     EXITN                                                            
*                                                                               
NEWTIM07 LA    R1,TL_EHRS1                                                      
         LHI   R0,TT_DAYS                                                       
         ZAP   TT_HRS,PZERO                                                     
NEWTIM08 OC    0(L'TL_EHRS1,R1),0(R1)                                           
         JZ    NEWTIM10                                                         
         AP    TT_HRS,0(L'TL_EHRS1,R1)                                          
         AP    TOTHOURS,0(L'TL_EHRS1,R1)                                        
NEWTIM10 AHI   R1,TL_HDMNL                                                      
         JCT   R0,NEWTIM08                                                      
                                                                                
         MVC   TT_CANAM,TL_CANAM                                                
         CLC   PRODUL,TL_ULA       It's either SJ or 1N                         
         JNE   NEWTIM12                                                         
         MVC   TT_SJNAM,TL_CLNAM                                                
         CLC   TL_PRNAM,SPACES                                                  
         JNH   *+10                                                             
         MVC   TT_SJNAM,TL_PRNAM                                                
         CLC   TL_JBNAM,SPACES                                                  
         JNH   *+10                                                             
         MVC   TT_SJNAM,TL_JBNAM                                                
         MVC   TT_12ULA,TL_12ULA                                                
         MVC   TT_12NAM,TL_12NAM                                                
                                                                                
NEWTIM12 MVC   TT_COFF,SPACES      Set contra-office                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    NEWTIM14                                                         
         MVC   TT_COFF,TL_SJOFF                                                 
         CLC   TL_SJOFF,SPACES                                                  
         JH    NEWTIM14                                                         
*        CLI   TL_TTYPE,QT_TETIM                                                
*        JE    *+12                                                             
*        CLI   TL_TTYPE,QT_TNTIM   Was time type N?                             
*        JNE   *+10                                                             
         MVC   TT_COFF,HD_1ROFF                                                 
         OC    TT_COFF,SPACES      Make sure it's valid for key                 
                                                                                
NEWTIM14 CLI   RECTYPE,RECTMATQ    Are we dealing with materials?               
         JE    NEWTIM20                                                         
                                                                                
         ZAP   TT_RATE,PZERO                                                    
*&&UK*&& ZAP   TT_CRATE,PZERO                                                   
         ZAP   TT_AMNT,PZERO                                                    
         CLC   TL_ULA,SPACES                                                    
         JNH   NEWTIM18                                                         
*&&UK                                                                           
         CLI   TT_TTYP,TIMTCN                                                   
         JE    *+20                                                             
*&&                                                                             
         CLI   TT_TTYP,TIMTCR                                                   
         JE    *+12                                                             
         CLI   TT_TTYP,TIMTCB                                                   
         JNE   NEWTIM18                                                         
                                                                                
         L     R3,ABLOCK                                                        
         USING X_RATED,R3          Look up cost rate                            
         XC    0(X_RATEDQ,R3),0(R3)                                             
         MVC   X_RATCTRY,CUCTRY                                                 
         MVC   X_RAT1RACT,HD_1RACT                                              
         MVC   X_RAT1ROFF,HD_1ROFF Person office                                
         MVC   X_RAT1RDPT,HD_1RDEP                                              
         MVC   X_RAT1RSUB,HD_1RSUB                                              
         MVC   X_RAT1RPER,HD_1RPER                                              
         MVC   X_RATSJCPJ,TL_ACT                                                
         MVC   X_RATSJOFF,SJOFFC   Client client office                         
         MVC   X_RATSJTSK,TL_TSK                                                
         MVC   X_RATCRDTE,HD_PEDT                                               
         OC    HD_LEDT,HD_LEDT     Do we have a location end date               
         JZ    *+20                                                             
         CLC   HD_PEDT,HD_LEDT     Compare period end date with loc end         
         JL    *+10                date and take whichever is lower             
         MVC   X_RATCRDTE,HD_LEDT                                               
         MVC   X_RDATAMGR,XDATAMGR                                              
         MVC   X_RCOMFACS,ACOMFACS                                              
         MVC   X_RCASHVAL,VCASHVAL                                              
         MVC   X_RAT1CMPY,CUXCPY                                                
*&&US                                                                           
         TM    TL_ICPJ,TL_EADJ     Job Eliglible for rate Adjustment            
         JNO   *+8                                                              
         OI    X_RATSTAT,X_RATEADJ   Get adjustment rates for US                
*&&                                                                             
*&&UK                                                                           
         CLI   TT_TTYP,TIMTCN                                                   
         JNE   *+8                                                              
         OI    X_RATSTAT,X_RATSCOST  get cost rate only                         
         MVC   X_RATCSTAT,CPYSTAT7                                              
         MVC   X_RATCSTA4,CPYSTAT4                                              
         MVI   X_RATCFLG,0         Currency flag                                
         MVC   X_RTOBACCO,VTOBACCO                                              
*&&                                                                             
         GOTOR VGETRTE,X_RATED                                                  
         CLI   TT_TTYP,TIMTCN                                                   
         JE    *+22                                                             
         TM    X_RATSTAT2,X_RATEFND                                             
         JNZ   *+14                                                             
         MVC   ROUERRV,=AL2(AE$MRATE)                                           
         J     EXITN                                                            
*&&UK                                                                           
         TM    X_RATSTAT2,X_RATEFNDC   was cost rate found                      
         JNZ   NEWTIM16                                                         
         CLI   TT_TTYP,TIMTCN                                                   
         JNE   *+12                                                             
         TM    X_RATSTAT2,X_RATEHIST   was history record found                 
         JNZ   NEWTIM16                                                         
         MVC   ROUERRV,=AL2(AE$00558)                                           
         J     EXITN                                                            
NEWTIM16 CLI   TT_TTYP,TIMTCN                                                   
         JE    NEWTIM18                                                         
*&&                                                                             
*&&US                                                                           
         TM    X_RATSTAT2,X_RATEWADJ   Was rate adjusted?                       
         JNO   *+8                                                              
         OI    TT_RBSTA,TIMRBADJ                                                
*&&                                                                             
         ZAP   TT_RATE,X_RATEAMNT  Set billing rate                             
         MVC   TT_REFF,X_RATEEFFD  Set rate effective date                      
*&&UK*&& ZAP   TT_CRATE,X_RATEAMTC Set cost rate                                
*&&UK*&& MVC   TT_CREFF,X_RATEEFFC Set cost rate effective date                 
         ZAP   DUB,X_RATEAMNT                                                   
         MP    DUB,TT_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   TT_AMNT,DUB         Set billing amount                           
         DROP  R3                                                               
                                                                                
NEWTIM18 MVC   TT_ETPDT,HD_LEDT    Set period end date                          
         MVC   TT_DHVAL,TL_HDMA    Set dates/hours                              
                                                                                
         GOTOR ADDAPP,DMCB,TT_CLIAP,TT_EPST1                                    
         J     NEWTIM34            Build approver table entry                   
                                                                                
NEWTIM20 CLI   RECTYPE,RECTMATQ    Test materials                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   TT_TTYP,TT_TMATR    Set materials                                
         MVC   TT_12ULA,TL_12ULA                                                
         MVC   TT_12NAM,TL_12NAM                                                
                                                                                
         MVC   TT_COFF,SPACES      Set contra-office code                       
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    NEWTIM22                                                         
         MVC   TT_COFF,TL_SJOFF                                                 
         CLC   TL_SJOFF,SPACES                                                  
         JH    NEWTIM22                                                         
         MVC   TT_COFF,HD_1ROFF                                                 
                                                                                
NEWTIM22 MVC   TT_INUM,SPACES                                                   
         ZAP   TT_IMULT,PZERO                                                   
         ZAP   TT_IPRCE,PZERO                                                   
         MVI   TT_IIND,TIMISNIT    Set not a production item                    
         MVC   TT_ISEQ,QI_ITEMS                                                 
         MVC   TT_INUM,QI_ITEMC                                                 
         MVI   TT_IIND,TIMISNPR    Set no price given                           
         OC    QI_ITEMP,QI_ITEMP                                                
         JZ    NEWTIM32                                                         
         MVI   TT_IIND,0                                                        
         ZAP   TT_IPRCE,QI_ITEMP                                                
         CLI   QI_ITEMO,C'Y'       Test price given                             
         JNE   NEWTIM30                                                         
                                                                                
K        USING PASRECD,IOKEY                                                    
         XC    K.PASKEY,K.PASKEY   Read article passive                         
         MVI   K.PASKTYP,PASKTYPQ                                               
         MVI   K.PASKSUB,PASKSQ                                                 
         MVC   K.PASKCPY,CUXCPY                                                 
         MVC   K.PASKSEQ,TT_ISEQ                                                
         DROP  K                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,ARTRFST-ARTRECD                                               
         USING PRIELD,R2                                                        
         SR    R0,R0                                                            
NEWTIM24 CLI   PRIEL,0                                                          
         JE    NEWTIM30                                                         
         CLI   PRIEL,PRIELQ                                                     
         JE    *+14                                                             
         IC    R0,PRILN                                                         
         AR    R2,R0                                                            
         J     NEWTIM24                                                         
                                                                                
         LLC   RE,PRICNTR                                                       
         LA    R2,PRINTRY                                                       
         USING PRINTRY,R2          R2=A(Effective date/price array)             
NEWTIM26 CLC   PRIDAT,HD_DMDAT     Test price effective date                    
         JH    NEWTIM28                                                         
         CP    TT_IPRCE,PRIAMT     Is the price different?                      
         JE    NEWTIM30                                                         
         OI    TT_IIND,TIMISPOV    Yes - set price override                     
         J     NEWTIM30                                                         
NEWTIM28 AHI   R2,PRINTRQ          Bump to next date/price entry                
         JCT   RE,NEWTIM26         Do for number of entries                     
         DROP  R2                                                               
                                                                                
NEWTIM30 ZAP   TT_IMULT,QI_ITEMX   Set item multiplier                          
                                                                                
NEWTIM32 ZAP   TT_ITOT,QI_ITEMT    Set item total (will die if empty)           
                                                                                
NEWTIM34 MVC   TT_DA,EFFS          Set high disk address                        
*                                                                               
         GOTOR BUFTIM,DMCB,('TSAADD',NEWBUF),0                                  
         JE    NEWTIM36                                                         
         TM    TIMERR,TSEEOF       Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                Duplicate key                                
         MVC   ROUERRV,=AL2(AE$MAX#)                                            
         J     EXITN                                                            
*                              **  Add day narrative array **                   
NEWTIM36 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   EXITN                                                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',TT_D),TT_LN2Q                         
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Build a new day narrative buffer record and add it to buffer        *         
***********************************************************************         
                                                                                
NEWDNR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*NEWDNR*'                                                      
                                                                                
         GOTOR CLRTTD              Clear time record                            
         MVI   TT_TTYP,TT_TDNAR    Set day narrative type                       
         MVC   TT_TIME#,QD_TIME#   Set time line row number                     
         MVC   TT_CTIME,TT_TIME#   Set time row number for sorting              
         MVC   TT_NAR#,QD_SEQ#     Set narr sequence number for sorting         
         MVC   TT_CNAR#,QD_SEQ#                                                 
                                                                                
         MVI   TT_SEQ,FF           Set high sequence number for adds            
         MVI   TT_CKSBR,FF         Set high record sequence number              
         MVC   TT_DDTE,QD_DATE                                                  
*                                                                               
         TM    HD_IND,FW_INOPR     No previous time records found               
         JZ    *+8                                                              
         OI    TT_BSTAT,TT_BSRNR   All records must be adds                     
                                                                                
         ICM   R1,15,QX_DESCI      Test any narrative                           
         JZ    NEWDNR06                                                         
         LLC   RF,LW_DATA1-LW_D(R1)                                             
         LTR   RF,RF                                                            
         JZ    NEWDNR06            No narrative don't add element               
         STC   RF,TT_DNARL                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TT_DNAR(0),LW_DATA1+1-LW_D(R1)                                   
         EX    RF,0(RE)                                                         
*                                                                               
NEWDNR00 MVC   TT_AULA,TL_ULA      Set SJ/1N account                            
         MVC   TT_TSK,TL_TSK       Set work code                                
         MVC   TT_OFF,TL_SJOFF                                                  
         MVC   TT_MED,TL_MED                                                    
*        GOTOR GETCAP,TT_D         don't think we need to do this               
         CLC   TL_SJOFF,SPACES                                                  
         JH    NEWDNR02                                                         
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    NEWDNR02                                                         
         MVC   TT_OFF,HD_1ROFF                                                  
NEWDNR02 OC    TT_OFF,SPACES       Make sure it's valid for key                 
         MVC   TT_MOA,HD_DMMOA                                                  
         MVC   TT_OMOA,HD_DMMOA    Store original month of service              
         MVC   TT_CAULA,TL_ULA                                                  
         CLC   PRODUL,TL_ULA       It's either SJ or 1N                         
         JNE   *+10                                                             
         MVC   TT_CAULA,TL_1CULA   SJ so contra is 1C                           
         MVC   TT_INULA,TL_INULA   Set income account                           
                                                                                
         MVC   TT_NARR,SPACES      Preset description to spaces                 
         MVC   TT_ORD,QT_ORDER                                                  
         MVC   TT_EST,QT_EST#                                                   
         MVC   TT_INTRF,QT_INTRF                                                
         MVC   TT_COFF,SPACES      Set contra-office code                       
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    NEWDNR03                                                         
         MVC   TT_COFF,TL_SJOFF                                                 
         CLC   TL_SJOFF,SPACES                                                  
         JH    NEWDNR03                                                         
         MVC   TT_COFF,HD_1ROFF                                                 
*                                                                               
NEWDNR03 MVC   TT_DA,EFFS          Set high disk address                        
         GOTOR BUFTIM,DMCB,('TSAADD',NEWBUF),0                                  
         JE    NEWDNR04                                                         
         TM    TIMERR,TSEEOF       Test end of buffer                           
         JNZ   *+6                                                              
         DC    H'0'                Duplicate key                                
         MVC   ROUERRV,=AL2(AE$MAX#)                                            
         J     EXITN                                                            
                                                                                
NEWDNR04 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   NEWDNR06                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',TT_D),TT_LN2Q                         
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
*                                                                               
NEWDNR06 J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Build old time buffer from existing time records                    *         
***********************************************************************         
                                                                                
OLDTIM   NTR1  LABEL=NO,WORK=(RC,OTWORKL)                                       
         J     *+12                                                             
         DC    C'*OLDTIM*'                                                      
         USING OTWORKD,RC                                                       
         GOTOR CLRWRK,OTWORKL      Clear work area                              
*&&US                                                                           
         LLC   R6,HD_DATEN         R6=Number of dates                           
         LA    R2,HD_DMOA                                                       
HD       USING HD_DMOA,R2          R2=A(Date/MOA array)                         
OLDTIM02 ZAP   HD.HD_EDHRS,PZERO   Clear hours                                  
         AHI   R2,HD_DMOAL         Bump date/MOA pointer                        
         JCT   R6,OLDTIM02         Do for number of dates                       
         DROP  HD                                                               
*&&                                                                             
K        USING TSWRECD,IOKEY       Read time sheet weekly passives              
         XC    K.TSWKEY,K.TSWKEY                                                
         MVI   K.TSWKTYP,TSWKTYPQ                                               
         MVI   K.TSWKSUB,TSWKSUBQ                                               
         MVC   K.TSWKCPY,CUXCPY                                                 
         MVC   K.TSWKPER,HD_1RPER                                               
         MVC   K.TSWKEND,HD_PEDTC                                               
         LLC   RF,ONERL3L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.TSWKODS(0),HD_1RACT                                            
         EX    RF,0(RE)                                                         
         OC    K.TSWKODS,SPACES                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   K.TSWKEY(TSWKULC-TSWKEY),IOKEYSAV                                
         JNE   EXITH               No data for this Person/Week/ODS             
                                                                                
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   OLDTIM06            No                                           
         MVC   HD_TSSTO,K.TSWKSTAT Save status of first TSW passive             
                                                                                
OLDTIM06 CLC   HD_TSSTO,K.TSWKSTAT Test status same as first/saved              
         JE    *+6                                                              
         DC    H'0'                No - bad TSW passive status                  
         MVC   CSVKEY1,K.TSWKEY                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,IOADDR                                                        
         USING TIMRECD,R4          R4=A(Time record)                            
                                                                                
         MVC   WORK(L'TIMKEY),TIMKEY                                            
                                                                                
         AHI   R4,TIMRFST-TIMRECD                                               
         USING TIMELD,R4                                                        
                                                                                
OLDTIM08 CLI   TIMEL,0             Test end of record                           
         JE    OLDTIM42                                                         
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   OLDTIM10                                                         
         CLI   TIMETYP,TIMEARIN    Test approval row information                
         JE    OLDTIM12                                                         
         CLI   TIMETYP,TIMEINP     Test time type                               
         JE    OLDTIM14                                                         
         CLI   TIMETYP,TIMEITMS    Test item type                               
         JE    OLDTIM14                                                         
         CLI   TIMETYP,TIMEDNAR    Test new day narrative                       
         JE    OLDTIM14                                                         
                                                                                
OLDTIM10 LLC   R0,TIMLN            Bump to next input element                   
         AR    R4,R0                                                            
         J     OLDTIM08                                                         
                                                                                
         USING APPTABD,RF                                                       
OLDTIM12 L     RF,AAPPTAB                                                       
         MVI   APPRLN,APPKLNQ                                                   
         MVI   APPRTYP,APPRTMAN    Build manager entry in table                 
         MVC   APPRAST,TIMASTAT    Approval status                              
         AHI   RF,APPKLNQ                                                       
         MVI   APPRLN,0                                                         
         ST    RF,AAPPTN           Set A(Next approver entry)                   
         J     OLDTIM10                                                         
         DROP  R4,RF                                                            
                                                                                
OLDTIM14 GOTOR CLRTTD              Clear time record                            
                                                                                
         MVC   TT_COFF,WORK+(TIMKOFF-TIMKEY)                                    
         MVC   TT_CAULA,WORK+(TIMKULC-TIMKEY)                                   
         MVC   TT_CKSBR,WORK+(TIMKSBR-TIMKEY)                                   
                                                                                
         MVC   TT_NARR,SPACES      Preset description to spaces                 
                                                                                
         LR    R2,R4               R2=A(Current detail item)                    
         USING TIMELD,R2                                                        
                                                                                
         LA    R0,TIMELD                                                        
         S     R0,AIO2                                                          
         STCM  R0,3,TT_SCLST       Set disp. to start of time cluster           
                                                                                
         MVC   TT_SEQ,TIMSEQ       Set current sequence number                  
                                                                                
         CLI   TIMETYP,TIMEITMS    Test materials                               
         JNE   OLDTIM15                                                         
         MVI   TT_TTYP,TT_TMATR    Set materials record                         
         MVC   TT_TIME#,TIMIIDNO   Set time row number                          
         MVC   TT_ITEM#,TIMIIIDN   Set item sequence number                     
         MVC   TT_CTIME,TT_TIME#   Set time row number for sorting              
         MVC   TT_CITEM,TT_ITEM#   Set item row number for sorting              
         MVC   TT_MOA,TIMIMOA      Set month of activity                        
         MVC   TT_OMOA,TIMIMOA     Set original month of activity               
         MVC   TT_AULA,TIMIULA     Set SJ account                               
         MVC   TT_INULA,TIMINULA   Set income account                           
         MVC   TT_TSK,TIMITSK      Set work code                                
         MVC   TT_OFF,TIMIOFF      Set client office code                       
         MVC   TT_EPST1,TIMISTAT   Set status                                   
         MVC   TT_CLIAP,TIMIPIDC   Set client approver PID                      
         MVC   TT_ISEQ,TIMISEQ     Set sequence number                          
         MVC   TT_INUM,TIMINUM     Set item number                              
         MVC   TT_IMULT,TIMIMULT   Set item mulitplier                          
         MVC   TT_IPRCE,TIMIPRCE   Set price per item                           
         MVC   TT_IIND,TIMIIND     Set item indicator                           
         MVC   TT_ITOT,TIMITOT     Set total price                              
         LLC   R1,TIMLN                                                         
         SHI   R1,TIMITLNQ         Get length of narrative                      
         JNP   OLDTIM22                                                         
         STC   R1,TT_NARRL                                                      
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   TT_NARR(0),TIMITEXT Set item description                         
         EX    R1,0(RE)                                                         
         J     OLDTIM22                                                         
*                                                                               
OLDTIM15 CLI   TIMETYP,TIMEDNAR    Test new day narrative                       
         JNE   OLDTIM16                                                         
         MVC   TT_TIME#,TIMDIDNO   Populate narrative row no.                   
         MVC   TT_NAR#,TIMDNIDN    Set narrative sequence                       
         MVC   TT_CTIME,TT_TIME#   Set time row number for sorting              
         MVC   TT_CITEM,TT_ITEM#   Set item row number for sorting              
         MVC   TT_CNAR#,TT_NAR#    Set day narrative row for sorting            
         MVI   TT_TTYP,TT_TDNAR    Set day narrative type                       
         MVC   TT_DDTE,TIMDTDT1    Extract date                                 
         LLC   RF,TIMLN                                                         
         SHI   RF,1+TIMDNLNQ                                                    
         BASR  RE,0                                                             
         MVC   TT_DNAR(0),TIMDNARR Extract narrative                            
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STC   RF,TT_DNARL         Set narrative length                         
         J     OLDTIM26                                                         
                                                                                
OLDTIM16 CLI   TIMETYP,TIMEINP     Test time                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         XC    TL_INTRF,TL_INTRF                                                
         XC    TL_ORD,TL_ORD                                                    
         XC    TL_EST,TL_EST                                                    
         XC    TL_TOFFI,TL_TOFFI                                                
         MVC   TT_AULA,TIMACC      Set SJ/1N account code                       
         MVC   TT_TSK,TIMTSK       Set task code                                
         MVC   TT_OFF,TIMOFF       Set client/1R office code                    
         MVC   TT_MED,TIMMED       Set media code                               
         MVC   TT_TTYP,TIMTTYP     Set type of time                             
         OC    TT_TTYP,TT_TTYP     Check if time type is set                    
         JNZ   OLDTIM18                                                         
         MVI   TT_TTYP,TIMTNC      If empty set as non client non bill          
         CLC   PRODUL,TIMACC       If production account                        
         JNE   OLDTIM18                                                         
         MVI   TT_TTYP,TIMTCN      Set as client non bill                       
OLDTIM18 MVC   TT_IND,TIMIND       Set time indicator                           
         MVC   TT_MOA,TIMMOA       Set month of activity                        
         MVC   TT_OMOA,TIMMOA      Set original month of activity               
         TM    TIMSTAT,TIMLOCK     Is the time locked from flexibill            
         JZ    OLDTIM20            No                                           
         MVC   ROUERRV,=AL2(AE$ACPEN) Yes - error                               
         GOTOR SAVERR,DMCB,ROUERRV,(L'TT_AULA,TT_AULA)                          
OLDTIM20 CLI   TIMLN,TIMILNQ                                                    
         JNH   OLDTIM26                                                         
         MVC   TT_STAT,TIMSTAT     Set status                                   
         MVC   TT_ADAT,TIMADAT     Set activty date                             
         MVC   TT_HRS,TIMHRS       Set hours                                    
         ZAP   TT_RATE,PZERO       Initialise rate                              
*&&UK*&& ZAP   TT_CRATE,PZERO      and cost rate                                
         ZAP   TT_AMNT,PZERO       and billing amount                           
         CLI   TIMLN,TIMBLNQ+TIMILN1Q                                           
         JNH   OLDTIM26                                                         
         MVC   TT_RATE,TIMRATE     Set billing rate                             
         MVC   TT_RBSTA,TIMRBSTA   Set billable time status                     
         MVC   TT_REFF,TIMREFF     Set rate effective date                      
         MVC   TT_INULA,TIMINC     Set income account                           
*&&UK*&& MVC   TT_CRATE,TIMCRATE   Set cost rate                                
*&&UK*&& MVC   TT_CREFF,TIMCREFF   Set cost rate effecitve date                 
         MVC   TT_AMNT,TIMAMNT     Set billing amount                           
                                                                                
OLDTIM22 CLI   TT_TTYP,TIMTCB      Only concerned with Billable Time            
         JE    *+12                                                             
         CLI   TT_TTYP,TT_TMATR    or materials record                          
         JNE   OLDTIM26                                                         
         CLC   TT_INULA,SPACES     Protect against In Progress                  
         JNH   OLDTIM26                                                         
V        USING IN_D,OTRBAREA       Resolve income account values                
         GOTOR GETINC,DMCB,TT_INULA,V.IN_D                                      
         JE    OLDTIM24            Exit on error                                
         GOTOR SAVERR,DMCB,ROUERRV,(L'TT_INULA,ERRTXT)                          
OLDTIM24 MVC   TT_12ULA,V.IN_12ULA                                              
         MVC   TT_12NAM,V.IN_12NAM                                              
         DROP  V                                                                
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
                                                                                
OLDTIM26 LLC   R0,TIMLN            Bump to next element                         
         AR    R2,R0                                                            
         CLI   TIMEL,TIMELQ        Test end of cluster/record                   
         JNE   OLDTIM38                                                         
         CLI   TIMETYP,TIMEINP     Test time element                            
         JE    OLDTIM38                                                         
         CLI   TIMETYP,TIMEITMS    Test materials element                       
         JE    OLDTIM38                                                         
         CLI   TIMETYP,TIMEDNAR    Test new day narrative                       
         JE    OLDTIM38                                                         
         CLI   TIMETYP,TIMENAR     Test narrative element                       
         JE    OLDTIM30                                                         
         CLI   TIMETYP,TIMEINRF    Test internal reference element              
         JE    OLDTIM32                                                         
         CLI   TIMETYP,TIMEORDR    Test order element                           
         JE    OLDTIM34                                                         
         CLI   TIMETYP,TIMEEST     Test estimate element                        
         JE    OLDTIM36                                                         
         CLI   TIMETYP,TIMETOFF    Test timeoff id element                      
         JE    OLDTIM37                                                         
         CLI   TIMETYP,TIMETIME    Time element                                 
         JNE   OLDTIM26                                                         
         MVC   TT_TIME#,TIMEIDNO   Set time row number                          
         MVC   TT_CTIME,TT_TIME#   Set time row number for sorting              
         MVC   TT_EPST1,TIMEPST1   Set client approval status                   
         MVC   TT_CLIAP,TIMEPIDC   Set time approver                            
         MVC   TT_ETPDT,TIMETPDT   Set period end date                          
*                                                                               
         LA    RE,TIMEDAY                                                       
         LA    R1,TT_DHVAL                                                      
         LLC   R0,TIMLN                                                         
         AR    R0,R2                                                            
OLDTIM28 CR    RE,R0               Have we reached end of element               
         JNL   OLDTIM26            Yes                                          
         OC    0(L'TIMETDTE,RE),0(RE) Is a date present?                        
         JZ    OLDTIM26             No - then no further entries to get         
         MVC   0(L'TIMEDAY,R1),0(RE) Set date and hours                         
         LA    R1,L'TIMEDAY(R1)    Bump tsar record                             
         LA    RE,L'TIMEDAY(RE)    Bump day/hours entry on TIMEL                
         J     OLDTIM28                                                         
*                                                                               
OLDTIM30 LLC   R1,TIMLN            Time description element                     
         SHI   R1,TIMNARR-TIMELD                                                
         STC   R1,TT_NARRL         Set length of description                    
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   TT_NARR(0),TIMNARR  Set description                              
         EX    R1,0(RE)                                                         
         J     OLDTIM26                                                         
                                                                                
OLDTIM32 MVC   TL_INTRF,TIMJINRF   set internal reference                       
         J     OLDTIM26                                                         
                                                                                
OLDTIM34 MVC   TL_ORD,TIMOORDR     Set order number                             
         J     OLDTIM26                                                         
                                                                                
OLDTIM36 MVC   TL_EST,TIMSESNM     Set estimate number                          
         J     OLDTIM26                                                         
                                                                                
OLDTIM37 LLC   R1,TIMLN                                                         
         SHI   R1,(TIMFIDN-TIMELD)                                              
         STC   R1,TL_TOFFL                                                      
         LTR   R1,R1                                                            
         JZ    OLDTIM26                                                         
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   TL_TOFFI(0),TIMFIDN    Set timeoff id                            
         EX    R1,0(RE)                                                         
         OC    TL_TOFFI,SPACES                                                  
         J     OLDTIM26                                                         
                                                                                
OLDTIM38 MVC   TT_ORD,TL_ORD                                                    
         MVC   TT_EST,TL_EST                                                    
         MVC   TT_TOFFI,TL_TOFFI                                                
         MVC   TT_TOFFL,TL_TOFFL                                                
         MVC   TT_INTRF,TL_INTRF                                                
OLDTIM40 S     R2,AIO2                                                          
         STCM  R2,3,TT_ECLST       Set disp. to end of time cluster             
         MVC   TT_DA,IODA          Set disk address of record                   
         GOTOR BUFTIM,DMCB,('TSAADD',OLDBUF),0                                  
         JE    OLDTIM10                                                         
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
OLDTIM42 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         CLC   K.TSWKEY(TSWKULC-TSWKEY),IOKEYSAV                                
         JE    OLDTIM06                                                         
         J     EXITY               Set buffer built                             
         DROP  K,RC                                                             
OTWORKD  DSECT                     ** OLDTIM local w/s **                       
OTRBAREA DS    XL(OB_LNQ)                                                       
OTWORKL  EQU   *-OTWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Add entry to approver table for sending                             *         
* On NTRY P1=A(APPROVER)                                              *         
*         P2=approval status                                          *         
***********************************************************************         
         SPACE 1                                                                
ADDAPP   L     R2,0(R1)                                                         
         OC    0(L'APPRPID,R2),0(R2)                                            
         BZR   RE                                                               
         XR    RF,RF                                                            
         ICM   RF,15,AAPPTN         AAPPTN will be zero if off-line             
         BZR   RE                                                               
         ST    RE,SAVERE                                                        
         USING APPTABD,RF                                                       
         XC    APPRLN(APPJLNQ),APPRLN  Clear ioareas as we go                   
         MVI   APPRTYP,APPRTCLI                                                 
         MVC   APPRPID,0(R2)                                                    
         L     R2,4(R1)                                                         
         MVC   APPRAST,0(R2)                                                    
         MVC   APPRULA,ANYACCNT                                                 
         MVC   APPRCLNM,TL_CLNAM   1N/SJ (client) name                          
         LHI   R1,APPCLNQ                                                       
         CLC   PRODUL,ANYACCNT     Test production ledger                       
         JNE   ADDAPP02                                                         
         CLC   TL_PRNAM,SPACES                                                  
         JE    ADDAPP02                                                         
         MVC   APPRPRNM,TL_PRNAM   Set product name if present                  
         LHI   R1,APPPLNQ                                                       
         CLC   TL_JBNAM,SPACES                                                  
         JE    ADDAPP02                                                         
         MVC   APPRJBNM,TL_JBNAM   Set job name if present                      
         LHI   R1,APPJLNQ                                                       
ADDAPP02 STC   R1,APPRLN           Set length of this entry                     
                                                                                
TAB      USING APPTABD,RE                                                       
         XR    RE,RE                                                            
         ICM   RE,15,AAPPTAB       Look for same entry already in table         
         XR    R0,R0                                                            
ADDAPP06 CR    RE,RF                                                            
         JE    ADDAPP10            EOT, add the new one                         
         CLC   TAB.APPRLN(L'APPRLN+L'APPRTYP+L'APPRPID),APPRLN                  
         JNE   ADDAPP08                                                         
         CLC   TAB.APPRULA,APPRULA                                              
         JE    ADDAPP12            existing entry, don't add again              
ADDAPP08 IC    R0,TAB.APPRLN                                                    
         AR    RE,R0                                                            
         J     ADDAPP06                                                         
                                                                                
ADDAPP10 AR    RF,R1                                                            
         ST    RF,AAPPTN           Set address of next entry                    
         J     *+10                                                             
ADDAPP12 XC    APPRLN(APPJLNQ),APPRLN  Clear ioareas as we go                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Check to see if approver for this adjustment has approved before    *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6                                                          
CHKAPP   L     RF,AGENAREA                                                      
CHKAP02  OC    0(L'TIMEPIDC,RF),0(RF)                                           
         JNZ   *+8                                                              
         LTR   RE,RE               End of list - set CC not equal               
         BR    RE                                                               
         CLC   0(L'TIMEPIDC,RF),NEW.TT_CLIAP                                    
         BER   RE                                                               
         AHI   RF,L'TIMEPIDC                                                    
         J     CHKAP02                                                          
         EJECT                                                                  
         DROP  NEW                                                              
***********************************************************************         
* Check to see TIMEL is correct and update approver table             *         
***********************************************************************         
                                                                                
CHKTMD   NTR1  LABEL=NO,WORK=(RC,CTWORKL)                                       
         J     *+12                                                             
         DC    C'*CHKTMD*'                                                      
                                                                                
         USING CTWORKD,RC                                                       
         GOTOR CLRWRK,CTWORKL      Clear work area                              
NEW      USING TT_D,R6             so whole DSECT is addressible                
         LA    R6,TT_NEW                                                        
                                                                                
         LA    R0,TL_VALS          Clear time line values                       
         LHI   R1,TL_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   NEW.TT_TTYP,TT_TMATR                                             
         JNE   CHKTMD02                                                         
         MVI   TL_TTYPE,QT_TBTIM   Default to B-time if mats (DSBO-822)         
         J     CHKTMD04                                                         
*                                                                               
CHKTMD02 CLI   NEW.TT_TTYP,TT_TDNAR Narrative type                              
         JNE   CHKTMD03                                                         
         CLC   NEW.TT_DNARL,TT_DNARL Check for change in narrative              
         JNE   CHKTMD2A                                                         
         CLC   NEW.TT_DNAR,TT_DNAR                                              
         JNE   CHKTMD2A                                                         
         OI    NEW.TT_BSTAT,TT_BSRCO Else set copied                            
         J     CHKTMDX                                                          
*                                                                               
CHKTMD2A OI    NEW.TT_BSTAT,TT_BSRCR Narrative has been amended                 
         USING STCELD,R4                                                        
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTDNRA     Build STCEL for day narrative change         
         MVC   STCTROW,NEW.TT_TIME#                                             
         LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     CHKTMDX                                                          
*                                                                               
CHKTMD03 CLI   NEW.TT_TTYP,TIMTCB                                               
         JNE   *+8                                                              
         MVI   TL_TTYPE,QT_TBTIM                                                
         CLI   NEW.TT_TTYP,TIMTCR                                               
         JNE   *+8                                                              
         MVI   TL_TTYPE,QT_TRTIM                                                
         CLI   NEW.TT_TTYP,TIMTCN                                               
         JNE   *+8                                                              
         MVI   TL_TTYPE,QT_TNTIM                                                
         CLI   NEW.TT_TTYP,TIMTNC                                               
         JNE   *+8                                                              
         MVI   TL_TTYPE,QT_TNTIM                                                
         CLI   NEW.TT_TTYP,TIMTEM  Skip check if empty row                      
         JE    CHKTMD04                                                         
         GOTOR CHKTTY,TL_TTYPE     Check time type is OK for locations          
         JE    CHKTMD04                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACT,HD_1RACT)                        
                                                                                
CHKTMD04 CLC   TT_MOA,HD_DMMOA     Test change of MOA                           
         JE    CHKTMD12                                                         
                                                                                
         CLC   NEW.TT_MOA,HD_DMMOA Is it higher than the new one?               
         JH    *+10                                                             
         MVC   NEW.TT_MOA,HD_DMMOA                                              
         CLC   TT_MOA,HD_DMMOA     Is it higher than the new one?               
         JH    *+10                                                             
         MVC   TT_MOA,HD_DMMOA                                                  
         OI    NEW.TT_BSTAT,TT_BSRCR                                            
         OI    TR_IND,TR_ICLAT     ensure TIMRECs are updated                   
                                                                                
         USING STCELD,R4                                                        
         LAY   R4,I_GEN+(TI_ELEM-TI_RECD)                                       
         GOTOR BLDSTC,STCTRWAM     Build STCEL for MOA change                   
         OC    NEW.TT_TOFFI,NEW.TT_TOFFI                                        
         JZ    *+8                                                              
         MVI   STCTSTA3,STCTTIMO   Mark as timeoff                              
         OI    STCTSTA3,STCTSMOA                                                
         MVC   STCTMOA,TT_MOA                                                   
         MVC   STCTROW,TT_TIME#                                                 
         OC    TT_ITEM#,TT_ITEM#                                                
         JZ    CHKTMD08                                                         
         MVI   STCTTYP,STCTMRAM                                                 
         MVC   STCTMROW,TT_ITEM#                                                
         ZAP   STCMCTOT,TT_ITOT                                                 
         ZAP   STCMCPRC,TT_IPRCE                                                
         ZAP   STCMCMUL,TT_IMULT                                                
         MVC   STCMCCOD,TT_INUM                                                 
         XR    RF,RF                                                            
         ICM   RF,1,TT_NARRL                                                    
         JZ    CHKTMD06                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCMCTXT-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCMCTXT-1                                                  
         BASR  RE,0                                                             
         MVC   STCMCTXT(0),TT_NARR                                              
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
                                                                                
CHKTMD06 LA    R2,STCMCTXT                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     CHKTMD12                                                         
                                                                                
CHKTMD08 MVC   STCTCULA,TT_AULA                                                 
         MVC   STCTCTSK,TT_TSK                                                  
         MVC   STCTCORD,TT_ORD                                                  
         MVC   STCTEST#,TT_EST                                                  
         MVC   STCTCINT,TT_INTRF                                                
         MVC   STCTCTTY,TT_TTYP                                                 
         ZAP   STCTCHRS,TT_HRS                                                  
         XR    RF,RF                                                            
         ICM   RF,1,TT_NARRL                                                    
         JZ    CHKTMD10                                                         
         SHI   RF,1                                                             
         CHI   RF,L'STCTCNAR-1     For audit we just capture first              
         JNH   *+8                 60 chars                                     
         LHI   RF,L'STCTCNAR-1                                                  
         BASR  RE,0                                                             
         MVC   STCTCNAR(0),TT_NARR                                              
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
                                                                                
CHKTMD10 LA    R2,STCTCNAR                                                      
         AR    RF,R2                                                            
         SR    RF,R4                                                            
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
*                                                                               
CHKTMD12 CLC   PRODUL,ANYACCNT                                                  
         JE    CHKTMD14                                                         
         GOTOR VALNCA,ANYACCNT+L'ACTKUNT+L'ACTKLDG                              
         JE    CHKTMD22                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'ANYACCNT,ANYACCNT)                        
         J     CHKTMD22                                                         
                                                                                
CHKTMD14 GOTOR VALJOB,ANYACCNT     Validate production job etc.                 
         JE    CHKTMD16                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'ANYACCNT,ANYACCNT)                        
                                                                                
CHKTMD16 GOTOR VALWCD,NEW.TT_TSK   Validate work code                           
         JE    CHKTMD18                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QT_TASK,NEW.TT_TSK)                       
*                                  Resolve income account values                
CHKTMD18 CLI   NEW.TT_TTYP,TIMTCB                                               
         JE    *+12                                                             
         CLI   NEW.TT_TTYP,TT_TMATR                                             
         JNE   CHKTMD22            No - we won't be posting to SI/SK            
         MVC   TL_ULA,ANYACCNT                                                  
         GOTOR VALINC              Re-validate/update SI/SK account             
         JE    CHKTMD20            Exit on error:                               
         GOTOR SAVERR,DMCB,ROUERRV,(L'TT_INULA,ERRTXT)                          
                                                                                
CHKTMD20 CLC   NEW.TT_INULA,TL_INULA  Test income account changed               
         JE    *+12                                                             
         OI    TR_IND,TR_ICLAT     Ensure TIMRECs are updated                   
         OI    NEW.TT_BSTAT,TT_BSRCR    if account changes                      
         MVC   NEW.TT_INULA,TL_INULA                                            
         MVC   NEW.TT_12ULA,TL_12ULA                                            
         MVC   NEW.TT_12NAM,TL_12NAM                                            
                                                                                
CHKTMD22 MVC   NEW.TT_CANAM,TL_CANAM                                            
                                                                                
         CLC   PRODUL,ANYACCNT     It's either SJ or 1N                         
         JNE   CHKTMD24                                                         
         MVC   NEW.TT_SJNAM,TL_CLNAM                                            
         CLC   TL_PRNAM,SPACES                                                  
         JE    CHKTMD24                                                         
         MVC   NEW.TT_SJNAM,TL_PRNAM                                            
         CLC   TL_JBNAM,SPACES                                                  
         JE    CHKTMD24                                                         
         MVC   NEW.TT_SJNAM,TL_JBNAM                                            
*                                  Add approver table entry                     
CHKTMD24 GOTOR ADDAPP,DMCB,NEW.TT_CLIAP,NEW.TT_EPST1                            
*                                                                               
CHKTMDX  LHI   RE,1                                                             
         CHI   RE,1                                                             
         XIT1  REGS=(R4)                                                        
         DROP  RC,R4,NEW                                                        
                                                                                
CTWORKD  DSECT                     ** CHKTMD local w/s **                       
CTRBAREA DS    XL(OB_LNQ)                                                       
CTWORKL  EQU   *-CTWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Set line manager for notification                                   *         
***********************************************************************         
                                                                                
SETMGR   ST    RE,SAVERE                                                        
         MVI   BYTE4,SE#TIME                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,HD_1ROFF,BYTE4                            
         JNE   SETMGR02                                                         
         L     RF,AAPPTAB          Manager is always first entry                
         MVI   APPRNTFY-APPTABD(RF),C'Y'                                        
SETMGR02 L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set client approves for notification                                *         
***********************************************************************         
                                                                                
NOTCLI   ST    RE,SAVERE                                                        
         MVI   BYTE4,SE#TIME                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,HD_1ROFF,BYTE4                            
         L     RE,SAVERE                                                        
         BNER  RE                                                               
         L     RF,AAPPTAB                                                       
         USING APPTABD,RF                                                       
         SR    R0,R0                                                            
NOTCLI02 IC    R0,APPRLN                                                        
         AR    RF,R0                                                            
         CLI   APPRLN,0            Test end of table                            
         BER   RE                  Yes                                          
         MVI   APPRNTFY,C'Y'                                                    
         J     NOTCLI02                                                         
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Set to notify approver of changes                                   *         
***********************************************************************         
                                                                                
NOTAPP   CLI   QH_STAT,QH_SSAVQ    Are we only saving the record?               
         BER   RE                  Yes                                          
                                                                                
         ST    RE,SAVERE                                                        
         MVI   BYTE4,SE#TIME                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,HD_1ROFF,BYTE4                            
         L     RE,SAVERE                                                        
         BNER  RE                                                               
                                                                                
         L     RF,AAPPTAB          Could be submitting, approving               
         USING APPTABD,RF                                                       
NOTAPP02 CLI   APPRLN,0            End of table                                 
         BER   RE                                                               
         CLI   APPRTYP,APPRTCLI    Test client approver                         
         JNE   NOTAPP04                                                         
         CLC   APPRULA,0(R1)       Match account code                           
         JE    NOTAPP06                                                         
NOTAPP04 LLC   R0,APPRLN           Bump to next table entry                     
         AR    RF,R0                                                            
         J     NOTAPP02                                                         
                                                                                
NOTAPP06 MVI   APPRNTFY,C'Y'       Set notify approver as changed               
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Set to update manager/client approval status in APPTAB              *         
* On NTRY P1 = A(PID) for approval                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APPTABD,RF                                                       
SETAPP   ST    RE,SAVERE                                                        
         L     RF,AAPPTAB          Could be submitting, approving               
*                                                                               
SETAPP02 CLI   APPRLN,0            End of table                                 
         BER   RE                                                               
         CLC   APPRPID,0(R1)       Match account code                           
         JE    SETAPP06                                                         
         LLC   R0,APPRLN           Bump to next table entry                     
         AR    RF,R0                                                            
         J     SETAPP02                                                         
                                                                                
SETAPP06 CLI   QH_STAT,QH_SAPRQ    Approving this time?                         
         JE    *+12                                                             
         CLI   QH_STAT,QH_SSUBQ    Submitting this time?                        
         JNE   SETAPP08                                                         
         MVI   APPRAST,TIMASAPR                                                 
         BR    RE                                                               
*                                                                               
SETAPP08 CLI   QH_STAT,QH_SREJQ    Rejecting this time?                         
         BNER  RE                                                               
         MVI   APPRAST,TIMASREJ                                                 
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Get person record and resolve time values                           *         
***********************************************************************         
                                                                                
GETPER   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETPER*'                                                      
                                                                                
         XC    AEMPEL,AEMPEL       Clear A(EMPEL)                               
         XC    ALOCEL,ALOCEL       Clear A(LOCEL)                               
                                                                                
K        USING PERRECD,IOKEY                                                    
         MVC   K.PERKEY,SPACES                                                  
         MVI   K.PERKTYP,PERKTYPQ                                               
         MVC   K.PERKCPY,CUXCPY                                                 
         MVC   K.PERKCODE,HD_1RPER                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    GETPER02                                                         
         MVC   ROUERRV,=AL2(AE$IVPER)                                           
         J     EXITN                                                            
         DROP  K                                                                
                                                                                
GETPER02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,PERRFST-PERRECD  Locate elements                              
         USING EMPELD,R2                                                        
         SR    R0,R0                                                            
                                                                                
GETPER04 CLI   EMPEL,0             Test end of record                           
         JE    GETPER16                                                         
         CLI   EMPEL,EMPELQ        Test employee element                        
         JE    GETPER08                                                         
         CLI   EMPEL,LOCELQ        Test location element                        
         JE    GETPER12                                                         
         CLI   EMPEL,PIDELQ        Test employee PID element                    
         JE    GETPER14                                                         
                                                                                
GETPER06 IC    R0,EMPLN                                                         
         AR    R2,R0                                                            
         J     GETPER04                                                         
                                                                                
GETPER08 ST    R2,AEMPEL           Save EMPEL address                           
         CLC   HD_PSTR,EMPHIR      Validate EMPEL                               
         JL    GETPER10                                                         
         OC    EMPTRM,EMPTRM                                                    
         JZ    GETPER06                                                         
         CLC   HD_PEND,EMPTRM                                                   
         JNH   GETPER06                                                         
                                                                                
GETPER10 MVC   ROUERRV,=AL2(AE$OHIRE)                                           
         J     EXITN                                                            
                                                                                
         USING LOCELD,R2                                                        
GETPER12 CLC   HD_PSTR,LOCSTART    Find location for current date               
         JL    GETPER06                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    *+14                                                             
         CLC   HD_PEND,LOCEND                                                   
         JH    GETPER06                                                         
         ST    R2,ALOCEL                                                        
         J     GETPER06                                                         
                                                                                
         USING PIDELD,R2                                                        
GETPER14 MVC   HD_PPID#,PIDNO      Extract PID                                  
         J     GETPER06                                                         
                                                                                
GETPER16 OC    ALOCEL,ALOCEL       Test location element found                  
         JNZ   *+14                                                             
         MVC   ROUERRV,=AL2(AE$IVLDT)                                           
         J     EXITN                                                            
                                                                                
         OC    HD_PPID#,HD_PPID#   Test PID element found                       
         JNZ   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$NCPID)                                           
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Set person values and period end date from person record            *         
***********************************************************************         
                                                                                
SETEDT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETEDT*'                                                      
                                                                                
         L     R1,AEMPEL           Set person status                            
         MVC   HD_EMPST,EMPSTAT-EMPELD(R1)                                      
                                                                                
         L     R2,ALOCEL           Set person location values                   
         USING LOCELD,R2                                                        
         MVC   HD_1ROFF,LOCOFF                                                  
         MVC   HD_1RDEP,LOCDEPT                                                 
         MVC   HD_1RSUB,LOCSUB                                                  
         MVC   HD_LEDT,LOCEND      Set location end date                        
                                                                                
SETEDT02 OC    HD_LEDT,HD_LEDT     Test period end date set                     
         JZ    SETEDT08                                                         
         CLC   HD_LEDT,HD_PEDT     Is loc end date after period end?            
         JH    SETEDT08                                                         
                                                                                
         GOTOR VDATCON,DMCB,(1,HD_LEDT),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'D',WORK),WORK+6,1                                 
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
                                                                                
         LA    R1,LOCELD                                                        
NXT      USING LOCELD,R1           Look further down record                     
         SR    R0,R0               for new location end date                    
SETEDT04 IC    R0,NXT.LOCLN        Bumo to next element on record               
         AR    R1,R0                                                            
         CLI   NXT.LOCEL,0         Test end of record                           
         JE    SETEDT10                                                         
         CLI   NXT.LOCEL,LOCELQ    Test location element                        
         JNE   SETEDT04                                                         
         CLC   WORK(L'LOCSTART),NXT.LOCSTART                                    
         JL    SETEDT04                                                         
         OC    NXT.LOCEND,NXT.LOCEND                                            
         JZ    SETEDT06                                                         
         CLC   WORK(L'LOCEND),NXT.LOCEND                                        
         JH    SETEDT04                                                         
                                                                                
SETEDT06 CLC   NXT.LOCOFF(LOCLOCK-LOCOFF),LOCOFF                                
         JNE   SETEDT10                                                         
         MVC   HD_LEDT,NXT.LOCEND  Set location end date                        
         J     SETEDT02            Go look for another one                      
                                                                                
SETEDT08 MVC   HD_LEDT,HD_PEDT     Set location end date as period end          
                                                                                
SETEDT10 SR    R0,R0               Set complemented location end date           
         ICM   R0,7,HD_LEDT                                                     
         LNR   R0,R0                                                            
         STCM  R0,7,HD_LEDTC                                                    
                                                                                
                                                                                
         J     EXITY                                                            
         DROP  R2,NXT                                                           
         EJECT                                                                  
***********************************************************************         
* Build job key from client, product and job codes                    *         
*                                                                     *         
* Ntry:- P1=A(Area to build key into)                                 *         
*        P2=A(Client code)                                            *         
*        P3=A(Product code)                                           *         
*        P4=A(Job code)                                               *         
***********************************************************************         
                                                                                
BLDJOB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDJOB*'                                                      
                                                                                
         LM    R2,R5,0(R1)                                                      
         USING ACTKULA,R2                                                       
         MVC   ACTKULA(L'PRODUL),PRODUL                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),0(R3)                                                 
         EX    RF,0(RE)            Move client code to key                      
         LA    R1,ACTKACT+1(RF)                                                 
         LLC   RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R4)       Move product code to key                     
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,PPROLEN                                                       
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,L'GOSELJOB       Job can't be longer than GOSELJOB            
         JNH   *+8                                                              
         LHI   RF,L'GOSELJOB                                                    
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R5)       Move job code to key                         
         EX    RF,0(RE)                                                         
         OC    ACTKACT,SPACES      (ensure space filled key)                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Build person account from office, department, sub-department and    *         
* person codes                                                        *         
*                                                                     *         
* Ntry:- P1=A(Area to build account into)                             *         
*        P2=A(Office code)                                            *         
*        P3=A(Department code)                                        *         
*        P4=A(Sub-department code)                                    *         
*        P5=A(Person code)                                            *         
***********************************************************************         
                                                                                
BLDPER   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDPER*'                                                      
                                                                                
         LM    R2,R6,0(R1)                                                      
         USING ACTKUNT,R2                                                       
         MVC   ACTKUNT(L'LEDGER1R),LEDGER1R                                     
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),0(R3)                                                 
         EX    RF,0(RE)            Move office code to key                      
         LA    R1,ACTKACT+1(RF)                                                 
         LLC   RE,ONERL1L                                                       
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R4)       Move department to key                       
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,ONERL2L                                                       
         IC    RF,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R5)       Move sub-department code to key              
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,ONERL3L                                                       
         IC    RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R6)       Move person code to key                      
         EX    RF,0(RE)                                                         
         OC    ACTKACT,SPACES                                                   
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Validate job code                                                   *         
*                                                                     *         
* Ntry:- R1=A(ULCliProJob)                                            *         
***********************************************************************         
                                                                                
VALJOB   NTR1  LABEL=NO,WORK=(RC,VJWORKL)                                       
         J     *+12                                                             
         DC    C'*VALJOB*'                                                      
                                                                                
         USING VJWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(Job code)                               
         GOTOR CLRWRK,VJWORKL      Clear work area                              
         USING OB_D,VJRBAREA                                                    
                                                                                
         MVC   TL_SJOFF,SPACES                                                  
         MVC   TL_MED,SPACES                                                    
         MVC   TL_CLNAM,SPACES                                                  
         MVC   TL_PRNAM,SPACES                                                  
         MVC   TL_JBNAM,SPACES                                                  
         MVC   TL_1CULA,SPACES                                                  
                                                                                
P        USING ACTKULA,R2                                                       
K        USING ACTRECD,IOKEY       Build key of client                          
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),P.ACTKUNT                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),P.ACTKACT                                           
         EX    RF,0(RE)                                                         
                                                                                
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALJOB02                                                         
         JH    EXITN                                                            
                                                                                
         MVC   TL_CLNAM,OB_NAME                                                 
         MVC   TL_SJOFF,OB_SJOFF                                                
         MVC   TL_1CULA,OB_1CAC                                                 
         OI    TL_ICPJ,TL_ICLI     Set client level                             
         J     VALJOB08                                                         
                                                                                
VALJOB02 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$CLILK)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSCLOS                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALJOBN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TL_ICPJ,TL_ICLI     Set client level                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   VALJOBN                                                          
         LTR   R1,R1                                                            
         JZ    VALJOB04                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALJOB04                                                         
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VALJOB04                                                         
         MVC   ROUERRV,=AL2(AE$CLKTS)                                           
         OI    TL_ICPJ,TL_IERR     Set found error                              
         GOTOR SAVERR,DMCB,ROUERRV,(L'ANYACCNT,ANYACCNT)                        
*                                                                               
VALJOB04 GOTOR GETNAM,OB_NAME      Set client name                              
         MVC   TL_CLNAM,OB_NAME                                                 
         GOTOR GETELA,PPRELQ       Locate production profile element            
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PPRELD,R1                                                        
         MVC   TL_SJOFF,PPRGAOFF                                                
         OC    TL_SJOFF,SPACES                                                  
         MVC   OB_SJOFF,TL_SJOFF                                                
         CLC   PPRCOST,SPACES                                                   
         JNH   VALJOB06                                                         
         MVC   TL_1CULA,PPRCOSTU                                                
         OC    TL_1CULA,SPACES                                                  
         MVC   OB_1CAC,TL_1CULA                                                 
         DROP  R1                                                               
                                                                                
VALJOB06 GOTOR ADDBUF,OB_D                                                      
         XC    OB_D(OB_LNQ),OB_D                                                
                                                                                
VALJOB08 MVC   TL_SJCLI,K.ACTKACT  Save client code                             
         LLC   RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         LA    R1,P.ACTKACT(RE)                                                 
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JH    VALJOB10                                                         
         LLC   RF,PJOBLEN          Clear the rest of anyaccnt                   
         LLC   RE,PCLILEN          to ensure any job code from web app          
         SR    RF,RE               is cleared if there is no product            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         CLI   TL_TTYPE,TL_TBILQ   Billable time needs product                  
         JNE   VALJOB32                                                         
         MVC   ROUERRV,=AL2(AE$MSPRD)                                           
         J     EXITN                                                            
                                                                                
VALJOB10 MVC   TL_SJPRO,SPACES                                                  
         BASR  RE,0                                                             
         MVC   TL_SJPRO(0),0(R1)   Save product code                            
         EX    RF,0(RE)                                                         
         LLC   RF,PPROLEN          Build key of product                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),P.ACTKACT                                           
         EX    RF,0(RE)                                                         
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALJOB12                                                         
         JH    EXITN                                                            
                                                                                
         MVC   TL_PRNAM,OB_NAME                                                 
         CLC   OB_SJOFF,SPACES                                                  
         JNH   *+10                                                             
         MVC   TL_SJOFF,OB_SJOFF                                                
         CLC   OB_1CAC,SPACES                                                   
         JNH   *+10                                                             
         MVC   TL_1CULA,OB_1CAC                                                 
         OI    TL_ICPJ,TL_IPRO     Set product level                            
         J     VALJOB20                                                         
                                                                                
VALJOB12 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSCLOS                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALJOBN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TL_ICPJ,TL_IPRO     Set product level                            
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   VALJOBN                                                          
         LTR   R1,R1                                                            
         JZ    VALJOB14                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALJOB14                                                         
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VALJOB14                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALJOBN                                                          
VALJOB14 GOTOR GETNAM,OB_NAME      Set product name                             
         MVC   TL_PRNAM,OB_NAME                                                 
         GOTOR GETELA,PPRELQ       Locate production profile element            
         JNE   VALJOB18                                                         
         USING PPRELD,R1                                                        
         MVC   OB_SJOFF,PPRGAOFF                                                
         CLI   PPRGAOFF,C' '                                                    
         JNH   VALJOB16                                                         
         MVC   TL_SJOFF,PPRGAOFF                                                
         OC    TL_SJOFF,SPACES                                                  
VALJOB16 MVC   OB_1CAC,PPRCOSTU                                                 
         CLC   PPRCOST,SPACES                                                   
         JNH   VALJOB18                                                         
         MVC   TL_1CULA,PPRCOSTU                                                
         OC    TL_1CULA,SPACES                                                  
                                                                                
VALJOB18 GOTOR ADDBUF,OB_D                                                      
         XC    OB_D(OB_LNQ),OB_D                                                
                                                                                
VALJOB20 LLC   RE,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         LA    R1,P.ACTKACT(RE)                                                 
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   0(0,R1),SPACES                                                   
         EX    RF,0(RE)                                                         
         JH    VALJOB22                                                         
         CLI   TL_TTYPE,TL_TBILQ   Billable time needs job                      
         JNE   VALJOB32                                                         
         MVC   ROUERRV,=AL2(AE$MSJOB)                                           
         J     EXITN                                                            
                                                                                
VALJOB22 CHI   RF,L'TL_SJJOB-1                                                  
         JNH   *+8                                                              
         LHI   RF,L'TL_SJJOB-1                                                  
         MVC   TL_SJJOB,SPACES                                                  
         BASR  RE,0                                                             
         MVC   TL_SJJOB(0),0(R1)   Save job code                                
         EX    RF,0(RE)                                                         
         MVC   TL_MED,0(R1)                                                     
         MVC   K.ACTKACT,P.ACTKACT                                              
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALJOB24                                                         
         JH    EXITN                                                            
                                                                                
         MVC   TL_JBNAM,OB_NAME                                                 
         CLC   OB_SJOFF,SPACES                                                  
         JNH   *+10                                                             
         MVC   TL_SJOFF,OB_SJOFF                                                
         CLC   OB_1CAC,SPACES                                                   
         JNH   *+10                                                             
         MVC   TL_1CULA,OB_1CAC                                                 
*&&US                                                                           
         TM    OB_JSTA1,JOBSART                                                 
         JZ    *+8                                                              
         OI    TL_ICPJ,TL_EADJ     Job Eliglible for rate Adjustment            
         TM    OB_JSTA1,JOBSXJOB   Is this and expense job?                     
         JNO   VALJOB23                                                         
         CLI   TL_TTYPE,TL_TBILQ   Billable time cant post to x-job             
         JNE   VALJOB23                                                         
         MVC   ROUERRV,=AL2(AE$CUEXJ)                                           
         J     EXITN                                                            
*&&                                                                             
VALJOB23 OI    TL_ICPJ,TL_IJOB     Set job level                                
         J     VALJOB32                                                         
                                                                                
VALJOB24 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSDRFT                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSCLOS                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$JOBCL)                                           
         J     VALJOBN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TL_ICPJ,TL_IJOB     Set job level                                
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   VALJOBN                                                          
         LTR   R1,R1                                                            
         JZ    VALJOB26                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALJOB26                                                         
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VALJOB25                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALJOBN                                                          
VALJOB25 TM    RSTLSTAT,RSTLSTIQ   No to timesheets                             
         JZ    VALJOB26                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALJOBN                                                          
VALJOB26 GOTOR GETNAM,OB_NAME      Set job name                                 
         MVC   TL_JBNAM,OB_NAME                                                 
         GOTOR GETELA,PPRELQ                                                    
*&&UK*&& JNH   VALJOB30                                                         
*&&US*&& JNH   VALJOB29                                                         
         USING PPRELD,R1                                                        
         MVC   OB_SJOFF,PPRGAOFF                                                
         CLI   PPRGAOFF,C' '                                                    
         JNH   VALJOB28                                                         
         MVC   TL_SJOFF,PPRGAOFF                                                
         OC    TL_SJOFF,SPACES                                                  
                                                                                
VALJOB28 MVC   OB_1CAC,PPRCOSTU                                                 
         CLC   PPRCOST,SPACES                                                   
*&&UK*&& JNH   VALJOB30                                                         
*&&US*&& JNH   VALJOB29                                                         
         MVC   TL_1CULA,PPRCOSTU                                                
         OC    TL_1CULA,SPACES                                                  
         DROP  R1                                                               
                                                                                
*&&US                                                                           
VALJOB29 GOTOR GETELA,JOBELQ                                                    
         JNE   VALJOB30                                                         
         USING JOBELD,R1                                                        
         CLI   JOBLN,JOBLN3Q                                                    
         JL    VALJOB30                                                         
         MVC   OB_JSTA1,JOBSTA1    Save off status byte for later test          
*&&US                                                                           
         TM    JOBSTA1,JOBSART                                                  
         JZ    *+8                                                              
         OI    TL_ICPJ,TL_EADJ     Job Eliglible for rate Adjustment            
*&&                                                                             
         TM    JOBSTA1,JOBSXJOB    Is this an expense job?                      
         JNO   VALJOB30                                                         
         CLI   TL_TTYPE,TL_TBILQ   Billable time cant post to x-job             
         JNE   VALJOB30                                                         
         MVC   ROUERRV,=AL2(AE$CUEXJ)                                           
         J     EXITN                                                            
*&&                                                                             
                                                                                
VALJOB30 GOTOR ADDBUF,OB_D                                                      
         XC    OB_D(OB_LNQ),OB_D                                                
                                                                                
VALJOB32 CLI   OFFIND,FULLYQ       Test office checking                         
         JE    *+12                                                             
         CLI   OFFIND,PANDLQ                                                    
         JNE   VALJOB34                                                         
         CLC   TL_SJOFF,SPACES                                                  
         JNE   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INVPO)                                           
         J     EXITN                                                            
                                                                                
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,TL_SJOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL              Validate office                              
         JE    VALJOB34                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
         DROP  R1                                                               
                                                                                
VALJOB34 CLC   TL_1CULA,SPACES                                                  
         JH    *+14                                                             
         MVC   ROUERRV,=AL2(AE$COSTG)                                           
         J     VALJOBN                                                          
                                                                                
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKULA,TL_1CULA                                               
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALJOB36                                                         
         MVC   TL_CANAM,OB_NAME                                                 
         J     EXITY                                                            
                                                                                
VALJOB36 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ILCAC)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSDRFT                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     VALJOBN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ILCAC)                                           
         J     VALJOBN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   VALJOBN                                                          
         GOTOR GETNAM,OB_NAME      Set job name                                 
         MVC   TL_CANAM,OB_NAME                                                 
         GOTOR ADDBUF,OB_D                                                      
         J     EXITY                                                            
                                                                                
VALJOBN  OC    OB_KEY,OB_KEY       Test have record key                         
         JZ    EXITN                                                            
         MVC   OB_ERROR,ROUERRV    Add record in error to buffer                
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
         DROP  RC,K,P                                                           
                                                                                
VJWORKD  DSECT                     ** VALJOB local w/s **                       
VJRBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VJRBAREA+(OB_OTHER-OB_D)                                         
OB_SJOFF DS    CL(L'TL_SJOFF)      SJ office code                               
OB_1CAC  DS    CL(L'TL_1CULA)      Costing account code                         
*&&US                                                                           
OB_JSTA1 DS    CL(L'JOBSTA1)       Job status 1                                 
*&&                                                                             
         ORG                                                                    
VJWORKL  EQU   *-VJWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate 1N account                                                 *         
***********************************************************************         
                                                                                
VALNCA   NTR1  LABEL=NO,WORK=(RC,V1WORKL)                                       
         J     *+12                                                             
         DC    C'*VALNCA*'                                                      
                                                                                
         USING V1WORKD,RC                                                       
         LR    R2,R1               R2=A(1N account code)                        
         GOTOR CLRWRK,V1WORKL      Clear work area                              
         USING OB_D,V1RBAREA                                                    
                                                                                
A        USING ACTKUNT,ANYACCNT                                                 
         MVC   A.ACTKUNT(L'ACTKUNT+L'ACTKLDG),LEDGER1N                          
         MVC   A.ACTKACT,0(R2)                                                  
                                                                                
K        USING LDGRECD,IOKEY                                                    
         MVC   K.LDGKEY,SPACES                                                  
         MVC   K.LDGKCPY,CUXCPY                                                 
         MVC   K.LDGKUNT(L'LDGKUNT+L'LDGKLDG),A.ACTKUNT                         
         MVC   OB_KEY(L'LDGKEY),K.LDGKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALNCA02                                                         
         JH    EXITN                                                            
         MVC   V1OPOS,OB_DATA                                                   
         J     VALNCA04                                                         
                                                                                
VALNCA02 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         MVC   OB_ERROR,ROUERRV    Set error if any                             
         GOTOR GETELA,LDGELQ       Locate ledger element on record              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   V1OPOS,LDGOPOS-LDGELD(R1)                                        
         MVC   OB_DATA(L'V1OPOS),V1OPOS                                         
                                                                                
         GOTOR ADDBUF,OB_D                                                      
         OC    OB_ERROR,OB_ERROR                                                
         JNZ   EXITN                                                            
         XC    OB_D(OB_LNQ),OB_D                                                
                                                                                
VALNCA04 CLI   OFFIND,NONEQ        No office - no checking                      
         JE    VALNCA06                                                         
         CLI   V1OPOS,0            OFFPOS=T scenarios are OK                    
         JE    VALNCA06                                                         
         CLI   V1OPOS,LDGOTRAN                                                  
         JE    VALNCA06                                                         
                                                                                
         NI    V1OPOS,FF-LDGOKEY2                                               
         LLC   RF,V1OPOS                                                        
         LA    RF,A.ACTKACT-1(RF)                                               
                                                                                
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC+0(1),0(RF)                                              
         MVI   OFFAOFFC+1,C' '                                                  
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFFAOFFC+1(1),1(RF)                                              
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL              Validate office                              
         JE    VALNCA06                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
K        USING ACTRECD,IOKEY                                                    
VALNCA06 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),LEDGER1N                          
         MVC   K.ACTKACT,A.ACTKACT                                              
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALNCA08                                                         
         JH    EXITN                                                            
         MVC   TL_CANAM,OB_NAME                                                 
         MVC   TL_RST7,OB_RST7                                                  
         OI    TL_ICPJ,TL_INON     Set non-client found                         
         CLI   OB_RSAF2,C'T'                                                    
         JNE   EXITY                                                            
         OI    TL_ICPJ,TL_1NHOL    is holiday                                   
         J     EXITY                                                            
                                                                                
VALNCA08 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALNCAN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALNCAN                                                          
                                                                                
         TM    K.ACTKSTAT,ACTSABLP                                              
         JNZ   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     VALNCAN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   VALNCAN                                                          
         LTR   R1,R1                                                            
         JZ    VALNCA10                                                         
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALNCA10                                                         
         MVC   OB_RST7,RSTSTAT7    Save future time byte                        
         MVC   TL_RST7,RSTSTAT7    Save future time byte                        
         L     RF,AIO3                                                          
         MVC   OB_RSAF2,ACTRSAF2-ACTRECD(RF)                                    
VALNCA10 GOTOR GETNAM,OB_NAME      Set 1N account name                          
         MVC   TL_CANAM,OB_NAME                                                 
         GOTOR ADDBUF,OB_D                                                      
         OI    TL_ICPJ,TL_INON     Non-client found                             
         CLI   OB_RSAF2,C'T'                                                    
         JNE   EXITY                                                            
         OI    TL_ICPJ,TL_1NHOL    is holiday                                   
         J     EXITY                                                            
                                                                                
VALNCAN  MVC   OB_ERROR,ROUERRV    Add record in error to buffer                
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
         DROP  RC,A,K                                                           
                                                                                
V1WORKD  DSECT                     ** VALNCA local w/s **                       
V1OPOS   DS    XL(L'LDGOPOS)                                                    
V1RBAREA DS    XL(OB_LNQ)                                                       
         ORG   V1RBAREA+(OB_OTHER-OB_D)                                         
OB_RST7  DS    CL(L'RSTSTAT7)      Record status byte 7                         
OB_RSAF2 DS    CL(L'ACTRSAF2)      Filter 2                                     
         ORG                                                                    
V1WORKL  EQU   *-V1WORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate time work code                                             *         
***********************************************************************         
                                                                                
VALWCD   NTR1  LABEL=NO,WORK=(RC,VWWORKL)                                       
         J     *+12                                                             
         DC    C'*VALWCD*'                                                      
                                                                                
         USING VWWORKD,RC                                                       
         MVC   TL_TSK,0(R1)                                                     
         GOTOR CLRWRK,VWWORKL      Clear work area                              
         USING OB_D,VWRBAREA                                                    
                                                                                
         CLC   TL_TSK,SPACES       Test work code present                       
         JH    VALWCD02                                                         
         TM    TL_ICPJ,TL_IJOB                                                  
         JZ    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$WCREQ) "Workcode is required"                    
         CLI   TL_TTYPE,TL_TBILQ                                                
         JNE   EXITN                                                            
         MVC   ROUERRV,=AL2(AE$WCNIB)                                           
         J     EXITN                                                            
                                                                                
VALWCD02 CLC   TL_TSK,=C'99'       Can't be special types                       
         JE    *+14                                                             
         CLC   TL_TSK,=C'**'                                                    
         JNE   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      Type of work code OK?                        
         JNE   VALWCD04                                                         
         CLI   TL_TSK,C'0'         Disallow external work codes                 
         JL    VALWCD04                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
*&&                                                                             
K        USING WCORECD,IOKEY                                                    
VALWCD04 MVC   K.WCOKEY,SPACES      Build key of work code record               
         MVI   K.WCOKTYP,WCOKTYPQ                                               
         MVC   K.WCOKCPY,CUXCPY                                                 
         MVC   K.WCOKUNT(L'PRODUL),PRODUL                                       
         MVC   K.WCOKWRK,TL_TSK                                                 
         MVC   OB_KEY(L'WCOKEY),K.WCOKEY                                        
         DROP  K                                                                
                                                                                
         GOTOR GETBUF,OB_D         Read buffer record                           
*&&UK*&& JNL   EXIT                Exit if found or bad                         
*&&US                                                                           
         JL    VALWCD06                                                         
         JH    EXITN                                                            
         TM    OB_WSTA2,WCOSART    Rate Eligible for adjustment                 
         JZ    EXITY                                                            
         OI    TL_ICPJ,TL_EADJ                                                  
         J     EXITY               Exit if found or bad                         
*&&                                                                             
                                                                                
VALWCD06 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     VALWCDN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETELA,WCOELQ       Locate work code element                     
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     VALWCDN                                                          
                                                                                
         USING WCOELD,R1                                                        
         MVC   OB_WSTA2,WCOSTAT2   Save off status byte for later test          
         TM    WCOSTAT2,WCOSLPST   work code locked for postings?               
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     VALWCDN                                                          
                                                                                
*&&US                                                                           
         TM    WCOSTAT2,WCOSART    Rate Eligible for adjustment                 
         JZ    *+8                                                              
         OI    TL_ICPJ,TL_EADJ                                                  
*&&                                                                             
                                                                                
         TM    WCOSTAT,WCOSHCOE    Hours carried in original est?               
*&&UK*&& JNZ   *+14                                                             
*&&US*&& J     *+14                                                             
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     VALWCDN                                                          
         DROP  R1                                                               
                                                                                
         GOTOR ADDBUF,OB_D         Add valid entry to buffer                    
         J     EXITY                                                            
                                                                                
VALWCDN  MVC   OB_ERROR,ROUERRV    Add invalid entry buffer                     
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
         DROP  RC                                                               
                                                                                
VWWORKD  DSECT                     ** VALWCD local w/s **                       
VWRBAREA DS    XL(OB_LNQ)                                                       
         ORG   VWRBAREA+(OB_OTHER-OB_D)                                         
OB_WSTA2 DS    CL(L'WCOSTAT2)      Workcode Status Byte 2                       
         ORG                                                                    
VWWORKL  EQU   *-VWWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate income account                                             *         
***********************************************************************         
                                                                                
VALINC   NTR1  LABEL=NO,WORK=(RC,VIWORKL)                                       
         J     *+12                                                             
         DC    C'*VALINC*'                                                      
                                                                                
         USING VIWORKD,RC                                                       
         GOTOR CLRWRK,VIWORKL      Clear work area                              
         USING OB_D,VIRBAREA                                                    
                                                                                
         MVI   OB_OTYP,OB_OTYPQ    Build key of buffer record                   
         MVI   OB_OSUB,OB_OSUBQ                                                 
         MVC   OB_OJOB,TL_ULA                                                   
         MVC   OB_OWCD,TL_TSK                                                   
         GOTOR GETBUF,OB_D                                                      
         JH    EXITN                                                            
         JE    VALINC02            If found have all values set                 
                                                                                
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
         MVC   GOADM,XDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
         MVC   GOSELCLI,TL_SJCLI                                                
         MVC   GOSELPRO,TL_SJPRO                                                
         MVC   GOSELJOB,TL_SJJOB                                                
         MVC   GOSELWC,TL_TSK                                                   
                                                                                
         GOTOR VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
*&&UK*&& MVC   OB_TTALL,GOTTALLW                                                
*&&UK*&& MVC   OB_TFNAR,GOTFNARR                                                
*&&UK*&& MVC   OB_TNOJB,GOTNOJOB                                                
                                                                                
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
         MVC   OB_TOT,GOTOT                                                     
         MVC   OB_FPT,GOFPT                                                     
         MVC   OB_FJT,GOFJT                                                     
*&&US*&& MVC   OB_TFNAR,GOTFNARR                                                
*&&US*&& MVC   OB_INACT,GOICA                                                   
*&&US*&& MVC   OB_TTALL,GOTTALLW                                                
*&&US*&& MVC   OB_TNOJB,GOTNOJOB                                                
*&&UK                                                                           
         L     R3,AGOBBLCK                                                      
         USING GOBBLKD,R3                                                       
         MVC   OB_INACT,GOINCAC                                                 
*&&                                                                             
         GOTOR ADDBUF,OB_D         Add option buffer record                     
         DROP  R3                                                               
                                                                                
VALINC02 MVC   TL_TFNAR,OB_TFNAR   Set narrative required for time              
         CLI   TL_TTYPE,TL_TDEFQ                                                
         JNE   VALINC06                                                         
         MVC   TL_TTYPE,OB_TOT                                                  
         CLI   OB_TOT,C' '         Do we have a default type of time            
         JH    VALINC06                  from client prod job                   
         CLI   HD_CODTM,C' '       No - then look at cost profile value         
         JNH   VALINC06                  for default type of time               
         CLI   HD_CODTM,CONODEFL                                                
         JE    VALINC06                                                         
         CLI   HD_CODTM,COSETB                                                  
         JNE   *+8                                                              
         MVI   TL_TTYPE,TL_TBILQ                                                
         CLI   HD_CODTM,COSETR                                                  
         JNE   *+8                                                              
         MVI   TL_TTYPE,TL_TRELQ                                                
         CLI   HD_CODTM,COSETN                                                  
         JNE   *+8                                                              
         MVI   TL_TTYPE,TL_TNONQ                                                
*                                                                               
VALINC06 DS    0H                                                               
*&&UK                                                                           
         LA    R1,OB_TTALL         Test type of time is valid                   
         LHI   R0,L'OB_TTALL                                                    
VALINC08 CLC   TL_TTYPE,0(R1)                                                   
         JE    VALINC10                                                         
         AHI   R1,1                                                             
         JCT   R0,VALINC08                                                      
         MVC   ROUERRV,=AL2(AE$INTTM)                                           
         J     EXITN               Wrong type of time specified                 
*&&                                                                             
VALINC10 CLI   HD_1RFPT,YESQ       Is the user forced to enter product          
         JE    VALINC12            Yes                                          
         CLI   TL_TTYPE,TL_TBILQ   If billable should have product              
         JE    VALINC12                                                         
*&&UK                                                                           
         CLI   TL_TTYPE,TL_TRELQ   If relisation time should have prod          
         JE    VALINC12                                                         
*&&                                                                             
         CLI   OB_FPT,C' '         Any profile entered                          
         JNH   VALINC16                                                         
         CLI   OB_FPT,C'X'         Prod to be excluded                          
         JE    VALINC14                                                         
         CLI   OB_FPT,C'A'         Prod required for all types of time          
         JE    VALINC12                                                         
         CLC   OB_FPT,TL_TTYPE     Required for this type?                      
         JNE   VALINC16                                                         
                                                                                
VALINC12 OC    TL_SJPRO,TL_SJPRO   Test product given                           
         JNZ   VALINC16                                                         
         MVC   ROUERRV,=AL2(AE$MSPRD)                                           
         J     EXITN               Error - product required                     
                                                                                
VALINC14 OC    TL_SJPRO,TL_SJPRO   Test product given                           
         JZ    VALINC16                                                         
         MVC   ROUERRV,=AL2(AE$PRNAL)                                           
         J     EXITN               Error - product not allowed                  
                                                                                
VALINC16 CLI   HD_CONJB,YESQ       Is the user not allowed to enter job         
         JE    VALINC20            Yes                                          
         CLI   HD_1RFJT,YESQ       Is the user forced to enter job              
         JE    VALINC18            Yes                                          
         CLI   TL_TTYPE,TL_TBILQ   If billable should have job                  
         JE    VALINC18                                                         
*&&UK                                                                           
         CLI   TL_TTYPE,TL_TRELQ   If relisation time should have job           
         JE    VALINC18                                                         
*&&                                                                             
         CLI   OB_FJT,C' '         Any profile entered                          
         JNH   VALINC26                                                         
         CLI   OB_FJT,C'X'         Job not required for all types               
         JE    VALINC20                                                         
         CLI   OB_FJT,C'A'         Job required for all types of time           
         JE    VALINC18                                                         
         CLC   OB_FJT,TL_TTYPE     Required for this type?                      
         JNE   VALINC26                                                         
                                                                                
VALINC18 OC    TL_SJJOB,TL_SJJOB   Test job given                               
         JNZ   VALINC24                                                         
         MVC   ROUERRV,=AL2(AE$MSJOB)                                           
         J     EXITN               Error - job required                         
                                                                                
VALINC20 OC    TL_SJJOB,TL_SJJOB   Test job given                               
         JZ    VALINC26                                                         
         MVC   ROUERRV,=AL2(AE$JBNAL)                                           
         J     EXITN               Error - job input not allowed                
                                                                                
VALINC24 OC    TL_TSK,TL_TSK       Any workcode                                 
         JNZ   VALINC26            Yes                                          
         MVC   ROUERRV,=AL2(AE$INWRK) Should have one present if                
         CLI   TL_TTYPE,TL_TBILQ             job is quoted                      
         JNE   EXITN                                                            
         MVC   ROUERRV,=AL2(AE$WCNIB)                                           
         J     EXITN                                                            
                                                                                
VALINC26 MVC   TL_INULA,HD_INULA   Set income from override                     
         OC    TL_INULA,SPACES                                                  
         CLC   TL_INULA,SPACES     Test override income account set             
         JH    *+10                Yes - don't use Opt/Maint one                
         MVC   TL_INULA,OB_INACT+L'ACTKCPY                                      
*&&UK                                                                           
         CLI   HD_COIOS,COSUSP     Suspense account override?                   
         JNE   *+10                                                             
         MVC   TL_INULA(L'ACTKUNT+L'ACTKLDG),LEDGERSK                           
         CLI   HD_COIOS,COINCOME   Income account override?                     
         JNE   *+10                                                             
         MVC   TL_INULA(L'ACTKUNT+L'ACTKLDG),LEDGERSI                           
*&&                                                                             
         CLI   HD_1RFJT,YESQ       Force job input                              
*&&US*&& JE    VALINC28            Yes - then ignore opt maint                  
*&&UK*&& JE    VALINC30                                                         
         CLI   OB_TNOJB,NOQ        Is job input allowed                         
*&&US*&& JNE   VALINC28            Yes                                          
*&&UK*&& JNE   VALINC30                                                         
         OC    TL_SJJOB,TL_SJJOB   No - test job given                          
*&&US*&& JZ    VALINC28                                                         
*&&UK*&& JZ    VALINC30                                                         
         MVC   ROUERRV,=AL2(AE$JBNAL)                                           
         J     EXITN               Error - job input not allowed                
                                                                                
*&&US                                                                           
VALINC28 CLC   TL_INULA,SPACES     Return income account or SKIPS               
         JH    VALINC30                                                         
         CLI   TL_TTYPE,TL_TBILQ   Is it billable or materials?                 
         JNE   VALINC30                                                         
         MVC   TL_INULA,SPACES                                                  
         MVC   TL_INULA(L'LEDGERSK),LEDGERSK                                    
         MVC   TL_INULA+L'LEDGERSK(3),=C'IPS'                                   
*&&                                                                             
VALINC30 CLI   TL_TTYPE,TL_TBILQ   Is it billable or materials?                 
         JE    *+12                                                             
         CLI   HD_COMAT,C'Y'                                                    
         JNE   EXITY               No - we won't be posting to SI/SK            
                                                                                
V        USING IN_D,VIRBAREA       Resolve income account values                
         GOTOR GETINC,DMCB,TL_INULA,V.IN_D                                      
         JNE   EXIT                Exit on error                                
                                                                                
         MVC   TL_12ULA,V.IN_12ULA                                              
         MVC   TL_12NAM,V.IN_12NAM                                              
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
VIWORKD  DSECT                     ** VALINC local w/s **                       
VIRBAREA DS    XL(OB_LNQ)          GETOPT buffer record                         
         ORG   VIRBAREA                                                         
OB_OTYP  DS    X                   ** Buffer key **                             
OB_OTYPQ EQU   X'FF'               Buffer type                                  
OB_OSUB  DS    C                                                                
OB_OSUBQ EQU   C'O'                Buffer sub-type                              
OB_OJOB  DS    CL(L'ACTKULA)       Job code                                     
OB_OWCD  DS    CL(L'TL_TSK)        Work code                                    
         ORG   VIRBAREA+(OB_DATA-OB_D)                                          
OB_TOT   DS    CL(L'GOTOT)         Default type of time                         
OB_FPT   DS    CL(L'GOFPT)         Force product on time sheet                  
OB_FJT   DS    CL(L'GOFJT)         Force job on time sheet                      
OB_INACT DS    CL(L'GOICA)         Income account                               
OB_TFNAR DS    CL(L'GOTFNARR)      Force narrative for time types               
OB_TNOJB DS    CL(L'GOTNOJOB)      Job allowed                                  
OB_TTALL DS    CL(L'GOTTALLW)      Type of time allowed                         
         ORG                                                                    
VIWORKL  EQU   *-VIWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Resolve income account values (IN_D)                                *         
***********************************************************************         
                                                                                
GETINC   NTR1  LABEL=NO,WORK=(RC,GIWORKL)                                       
         J     *+12                                                             
         DC    C'*GETINC*'                                                      
                                                                                
         USING GIWORKD,RC          RC=A(Local working storage)                  
         LM    R2,R3,0(R1)         R2=A(Income account code)                    
         USING IN_D,R3             R3=A(Buffer record)                          
K        USING ACTRECD,IOKEY       Build full account key                       
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKULA,0(R2)     Set unit, ledger and account                 
         XC    IN_KEY,IN_KEY       Set buffer key                               
         MVC   IN_KCULA,K.ACTKEY                                                
         GOTOR GETBUF,IN_D         Exit if found or invalid                     
         JNL   EXIT                                                             
*&&UK                                                                           
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    GETINC0A                                                         
         TM    CPXSTATA,CPXLACAC                                                
         JZ    GETINC0A                                                         
         CLI   INCOPOS,0           Test Inc ledger offpos already saved         
         JH    GETINC0A                                                         
         MVC   K.ACTKACT,SPACES    Read the ledger record                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     GETINC10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETELA,LDGELQ                                                    
         JNE   GETINC0A            ?                                            
         MVC   INCOPOS,LDGOPOS-LDGELD(R1)                                       
         MVC   K.ACTKULA,0(R2)     Reset account key                            
*&&                                                                             
GETINC0A MVC   ERRTXT(L'ACTKULA),0(R2)                                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     GETINC10                                                         
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK                                              
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     GETINC10                                                         
                                                                                
         TM    K.ACTKSTAT,ACTSABLP                                              
         JNZ   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     GETINC10                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         JNE   GETINC10                                                         
         MVI   IN_INCST,0                                                       
         LTR   R1,R1               R1=(Status element)                          
         JZ    *+10                                                             
         MVC   IN_INCST,RSTCOSTG-RSTELD(R1)                                     
*&&UK                                                                           
         CLI   CUACCS,0            Test any user limit access                   
         JE    GETINC00                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         TM    OFFACST4,CPYSOFF2                                                
         JZ    GETINC00                                                         
         TM    OFFAXSTA,CPXLACAC   Test using Limit Account access              
         JZ    GETINC00                                                         
         MVC   OFFAOPOS,INCOPOS                                                 
         L     RF,IOADDR                                                        
         ST    RF,OFFAREC          A(low-lvl a/c)                               
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JE    GETINC00                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)    Security lock out                      
         J     GETINC10                                                         
         DROP  R1                                                               
*&&                                                                             
GETINC00 GOTOR GETNAM,IN_INNAM     Set income account name                      
         MVC   IN_INANA,SPACES     Initialise other values                      
         MVC   IN_12ULA,SPACES                                                  
         MVC   IN_12NAM,SPACES                                                  
                                                                                
         GOTOR GETELA,SPAELQ       Get special posting element                  
         JNE   GETINC06                                                         
         SR    R0,R0                                                            
         USING SPAELD,R1                                                        
GETINC02 CLI   SPATYPE,SPATANAL    Test analysis account                        
         JE    GETINC04            Yes                                          
         IC    R0,SPALN            Bump to next element                         
         AR    R1,R0                                                            
         CLI   SPAEL,SPAELQ        Test special posting element                 
         JE    GETINC02            Yes                                          
         J     GETINC06                                                         
                                                                                
GETINC04 MVC   IN_INANA,SPAAANAL   Set analysis code                            
         DROP  R1                                                               
                                                                                
GETINC06 CLC   LEDGERSI,0(R2)      Test income ledger                           
         JNE   GETINC08                                                         
                                                                                
K        USING ACTKULA,WORK        Build key of 12 account record               
         MVC   K.ACTKULA,SPACES                                                 
         MVC   K.ACTKUNT(L'LEDGER12),LEDGER12                                   
         MVC   K.ACTKACT(L'IN_INCST),IN_INCST                                   
         CLC   IN_INANA,SPACES     Test analysis code present                   
         JNH   *+10                                                             
         MVC   K.ACTKACT(L'IN_INANA),IN_INANA                                   
         CLI   K.ACTKACT,C' '      Test valid costing/analysis                  
         JH    *+6                                                              
         DC    H'0'                                                             
         MVC   IN_12ULA,K.ACTKULA                                               
                                                                                
R        USING OB_D,GIRBAREA                                                    
         GOTOR GETACC,DMCB,('GATSTPST',K.ACTKULA),R.OB_D                        
         JNE   GETINC10                                                         
         MVC   IN_12NAM,R.OB_NAME  Set 12 account name                          
                                                                                
GETINC08 GOTOR ADDBUF,IN_D         Add valid entry to buffer                    
         MVC   ERRTXT,SPACES                                                    
         J     EXITY                                                            
                                                                                
GETINC10 MVC   IN_ERROR,ROUERRV    Add invalid entry to buffer                  
         GOTOR ADDBUF,IN_D                                                      
         J     EXITN                                                            
         DROP  R3                                                               
                                                                                
GIWORKD  DSECT                     ** GETINC local w/s **                       
GIRBAREA DS    XL(OB_LNQ)          GETACC buffer record                         
GIWORKL  EQU   *-GIWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Resolve account values (OB_D)                                       *         
***********************************************************************         
                                                                                
GATSTPST EQU   X'80'               Test if account can be posted to             
                                                                                
GETACC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETACC*'                                                      
                                                                                
         LR    R4,R1               R4=A(Calling parameter list)                 
         LM    R2,R3,0(R4)         R2=A(Unit, ledger and account)               
         USING OB_D,R3             R3=A(Buffer record)                          
K        USING ACTKEY,IOKEY                                                     
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKULA,0(R2)     Set unit, ledger and account                 
         MVC   ERRTXT(L'ACTKULA),0(R2)                                          
                                                                                
         XC    OB_D(OB_LNQ),OB_D   Clear Buffer                                 
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D         Get record from buffer                       
         JNL   EXIT                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     GETACC04                                                         
                                                                                
         TM    K.ACTKSTAT,ACTSLOCK Test locked                                  
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     GETACC04                                                         
                                                                                
         TM    0(R4),GATSTPST      Test check if can be posted to               
         JZ    GETACC02            No                                           
         TM    K.ACTKSTAT,ACTSABLP                                              
         JNZ   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     GETACC04                                                         
                                                                                
GETACC02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETNAM,OB_NAME      Set account name                             
         GOTOR ADDBUF,OB_D         Add valid entry to buffer                    
         MVC   ERRTXT,SPACES                                                    
         J     EXITY                                                            
                                                                                
GETACC04 MVC   OB_ERROR,ROUERRV    Add invalid entry to buffer                  
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Check for month lock                                                *         
***********************************************************************         
                                                                                
MTHLCK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*MTHLCK*'                                                    
                                                                                
         L     R2,0(R1)                                                         
         MVC   HD_CDAT(2),0(R2)    Period month                                 
         L     R2,4(R1)            Future month limit                           
MTHLCK02 MVI   HD_CDAT+2,1         Set first of month                           
         GOTOR VDATCON,DMCB,(X'81',HD_CDAT),(6,DUB)                             
         IC    R0,4(R1)                                                         
***      GOTOR BMONVAL,DMCB,((R0),DUB),(49,ACOMFACS),(CULANG,WORK),    +        
               (CUXCPY,CUACCS)                                                  
*&&US                                                                           
         GOTOR BMONVAL,DMCB,((R0),DUB),(49,ACOMFACS),(CULANG,WORK),    +        
               (CUXCPY,0)                                                       
*&&                                                                             
*&&UK                                                                           
         GOTOR BMONVAL,DMCB,((R0),DUB),(49,ACOMFACS),(CULANG,WORK),    +        
               (CUXCPY,CUUSER)                                                  
*&&                                                                             
         CLI   WORK+(BMOERR-BMONVALD),BMOEOKQ                                   
         JE    EXITY                                                            
         GOTOR VDATCON,DMCB,(1,HD_CDAT),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'M',WORK),WORK+6,1                                 
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,HD_CDAT)                              
         CLC   HD_CDAT(2),0(R2)    Test if future limit exceeded                
         JL    MTHLCK02                                                         
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Get latest time sheet/revision # on 1R account record               *         
***********************************************************************         
                                                                                
GETTSN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETTSN*'                                                      
                                                                                
K        USING ACTRECD,IOKEY                                                    
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKULA,HD_1RULA                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     EXITN                                                            
         TM    K.ACTKSTAT,ACTSLOCK   Test account is locked                     
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     EXITN                                                            
         DROP  K                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETNAM,HD_1RNAM     Set person account name                      
                                                                                
         GOTOR GETELA,RSTELQ       Locate status element                        
         JE    *+6                                                              
         DC    H'0'                Status element not found                     
                                                                                
         USING RSTELD,R1                                                        
         CLI   RSTLN,RSTLN2Q       Must not be a short element                  
         JNL   *+6                                                              
         DC    H'0'                                                             
         TM    RSTSTAT1,RSTSACIL   Test account is locked                       
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     EXITN                                                            
                                                                                
         CLI   RSTLN,RSTLN3Q       Must not be a short element                  
         JL    GETTSN02                                                         
         TM    RSTSTAT7,RSTLCKTS   Test if account has locks                    
         JZ    *+8                                                              
         OI    HD_IND,FW_SLOCK     Submitter locked                             
         TM    RSTSTAT7,RSTLCKAP                                                
         JZ    *+8                                                              
         OI    HD_IND,FW_ALOCK     Approver locked                              
         TM    RSTSTAT5,RSTSPROD                                                
         JZ    *+8                                                              
         MVI   HD_1RFPT,YESQ                                                    
         TM    RSTSTAT5,RSTSPRJB                                                
         JZ    *+8                                                              
         MVI   HD_1RFJT,YESQ                                                    
                                                                                
GETTSN02 MVC   HD_1RCST,RSTCOSTG   Set costing group                            
                                                                                
         GOTOR GETELA,SPAELQ       Get special posting element                  
         JNE   GETTSN08                                                         
         SR    R0,R0                                                            
         USING SPAELD,R1                                                        
GETTSN04 CLI   SPATYPE,SPATINCO    Test income account                          
         JE    GETTSN06            Yes                                          
         IC    R0,SPALN            Bump to next element                         
         AR    R1,R0                                                            
         CLI   SPAEL,SPAELQ        Test special posting element                 
         JE    GETTSN04            Yes                                          
         J     GETTSN08                                                         
                                                                                
GETTSN06 MVC   HD_INULA,SPAAULA    Set override income account                  
         DROP  R1                                                               
                                                                                
GETTSN08 J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* unlock record to recovery file                                      *         
***********************************************************************         
                                                                                
UPDTSN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDTSN*'                                                      
                                                                                
         GOTOR BLDLOK,HD_PPID#     Build LOCKET key in WORK                     
         GOTOR DMGRITRN,DMCB,$LOKREC,WORK                                       
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Get edit hours amount for time sheet period                         *         
***********************************************************************         
                                                                                
CHKHRS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKHRS*'                                                      
                                                                                
         OI    TL_IEDT,TL_IOFFQ+TL_IDPTQ+TL_ISUBQ+TL_IPERQ                      
                                                                                
K        USING EDTRECD,IOKEY                                                    
CHKHRS02 MVC   K.EDTKEY,SPACES                                                  
         MVI   K.EDTKTYP,EDTKTYPQ                                               
         MVI   K.EDTKSUB,EDTKSUBQ                                               
         MVC   K.EDTKCPY,CUXCPY                                                 
         TM    TL_IEDT,TL_IOFFQ                                                 
         JZ    CHKHRS04                                                         
         MVC   K.EDTKOFC,HD_1ROFF                                               
         NI    TL_IEDT,FF-TL_IOFFQ                                              
CHKHRS04 TM    TL_IEDT,TL_IDPTQ                                                 
         JZ    CHKHRS06                                                         
         MVC   K.EDTKDPT(L'HD_1RDEP),HD_1RDEP                                   
         NI    TL_IEDT,FF-TL_IDPTQ                                              
         OI    TL_IEDT,TL_IOFFQ                                                 
CHKHRS06 TM    TL_IEDT,TL_ISUBQ                                                 
         JZ    CHKHRS08                                                         
         MVC   K.EDTKSBD(L'HD_1RSUB),HD_1RSUB                                   
         NI    TL_IEDT,FF-TL_ISUBQ                                              
         OI    TL_IEDT,TL_IDPTQ                                                 
CHKHRS08 TM    TL_IEDT,TL_IPERQ                                                 
         JZ    CHKHRS10                                                         
         MVC   K.EDTKPER,HD_1RPER                                               
         NI    TL_IEDT,FF-TL_IPERQ                                              
         OI    TL_IEDT,TL_ISUBQ                                                 
CHKHRS10 XC    K.EDTKSEQ,K.EDTKSEQ                                              
         MVC   K.EDTKYR,HD_CSTR                                                 
         MVI   K.EDTKKSTA,EDTKSDAY Set daily time                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    CHKHRS12                                                         
         MVC   K.EDTKEY,IOKEYSAV                                                
         CLC   K.EDTKOFC,SPACES    Have we read lowest level                    
         JE    CHKHRS30            Yes - exit                                   
         J     CHKHRS02                                                         
                                                                                
CHKHRS12 CLC   K.EDTKSTDT,HD_DMDAT                                              
         JNH   *+6                                                              
         DC    H'0'                                                             
         CLC   K.EDTKENDT,HD_DMDAT                                              
         JNL   CHKHRS13                                                         
         GOTO1 VDATCON,DMCB,(1,HD_CSTR),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'+1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,HD_CSTR)                              
         J     CHKHRS10                                                         
                                                                                
CHKHRS13 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
                                                                                
         LA    R2,HD_DMDAT                                                      
         LHI   R3,TT_DAYS                                                       
                                                                                
CHKHRS14 OC    0(L'TL_ETDT1,R2),0(R2)                                           
         JZ    CHKHRS30                                                         
         ZAP   DUB,PZERO                                                        
         GOTOR VDATCON,DMCB,(1,0(R2)),(0,WORK)                                  
         GOTOR VGETDAY,DMCB,WORK,DUB1                                           
         CLC   DUB1(3),SPACES                                                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,DAYTAB                                                        
CHKHRS18 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RF),0(R1)                                                    
         JE    CHKHRS20                                                         
         LA    RF,DAYTABL(RF)                                                   
         J     CHKHRS18                                                         
*                                                                               
CHKHRS20 MVC   BYTE1,1(RF)                                                      
                                                                                
         L     R4,IOADDR                                                        
         AHI   R4,EDTRFST-EDTRECD                                               
         USING DEDELD,R4                                                        
         SR    R0,R0                                                            
                                                                                
CHKHRS22 CLI   DEDEL,0                                                          
         JE    CHKHRS28                                                         
         CLI   DEDEL,DEDELQ                                                     
         JNE   CHKHRS26                                                         
         CLI   DEDLN,DEDLN1Q                                                    
         JNE   CHKHRS24                                                         
         CLC   DEDIND,BYTE1        Match day                                    
         JNE   CHKHRS26                                                         
         ZAP   DUB,DEDHRS                                                       
         J     CHKHRS26                                                         
                                                                                
CHKHRS24 CLC   DEDDATE,0(R2)       Match date                                   
         JNE   CHKHRS26                                                         
         ZAP   DUB,DEDHRS                                                       
         J     CHKHRS28                                                         
                                                                                
CHKHRS26 IC    R0,DEDLN                                                         
         AR    R4,R0                                                            
         J     CHKHRS22                                                         
                                                                                
CHKHRS28 AP    TR_EDHRS,DUB        Add into total hours                         
*&&US                                                                           
         CLI   QH_STAT,QH_SSAVQ    Are we saving?                               
         JE    CHKHRS29            Yes - skip hours check                       
         CP    HD_EDHRS-HD_DMOA(L'HD_EDHRS,R2),DUB   Enough Hours?              
         JNL   CHKHRS29                                                         
         CURED (P8,DUB),(10,ERRTXT),2,ZERO=YES,ALIGN=LEFT                       
         MVC   ROUERRV,=AL2(AE$MDHRS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,((R0),ERRTXT)                                
*&&                                                                             
CHKHRS29 AHI   R2,HD_DMOAL                                                      
         JCT   R3,CHKHRS14                                                      
                                                                                
CHKHRS30 J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Establish approver and back-up approver PID for time                *         
***********************************************************************         
                                                                                
GETCAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETCAP*'                                                      
                                                                                
         LR    R7,R1                                                            
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    GETCAP02                                                         
         TM    TT_BSTAT,TT_BSRCO   If data was copied - approver needs          
         JZ    EXITY                to be refreshed on both passes              
P        USING ACTKULA,TT_AULA     R2=A(Account code)                           
GETCAP02 XC    TT_CLIAP,TT_CLIAP   Client approver                              
         XC    TT_CLIBA,TT_CLIBA   Client back up approver                      
         CLC   PRODUL,P.ACTKUNT    Test production ledger                       
         JNE   GETCAP20                                                         
                                                                                
         LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY       Find job, product or client approver         
GETCAP03 XC    SJ.JOBPAS,SJ.JOBPAS                                              
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
         CLC   TT_MED,SPACES                                                    
         JNH   GETCAP14                                                         
         MVC   SJ.JOBPCMED,TT_MED                                               
                                                                                
GETCAP10 TM    APRSTAT,APROFF                                                   
         JZ    GETCAP12                                                         
         CLC   TT_OFF,SPACES                                                    
         JNH   GETCAP14                                                         
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    GETCAP14                                                         
         MVC   SJ.JOBPCOFF,TT_OFF                                               
                                                                                
GETCAP12 OC    SJ.JOBPCODE,SPACES  make sure all spaces before read             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),IOKEYSAV                             
         JE    GETCAP18                                                         
GETCAP14 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   GETCAP03                                                         
         J     GETCAPN                                                          
                                                                                
GETCAP18 MVC   TT_CLIAP,SJ.JOBPPIDB                                             
         J     GETCAP26                                                         
         DROP  SJ                                                               
                                                                                
NC       USING NCTPASD,IOKEY       Non-client time approver                     
GETCAP20 XC    NC.NCTPAS,NC.NCTPAS                                              
         MVI   NC.NCTPTYP,NCTPTYPQ                                              
         MVI   NC.NCTPSUB,NCTPSUBQ                                              
         MVC   NC.NCTPCPY,CUXCPY                                                
         MVI   NC.NCTPAPPL,NCTPATIM                                             
         MVC   NC.NCTPNCC,P.ACTKACT                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GETCAP24                                                         
         DC    H'0'                                                             
                                                                                
GETCAP22 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GETCAP24                                                         
         DC    H'0'                                                             
                                                                                
GETCAP24 CLC   NC.NCTPAS(NCTPPIDB-NCTPASD),IOKEYSAV                             
         JNE   GETCAPN                                                          
         MVC   TT_CLIAP,NC.NCTPPIDB                                             
         DROP  NC,P                                                             
                                                                                
GETCAP26 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETCAP28 CLI   LIDEL,0             Test end of record                           
         JE    EXITY                                                            
         CLI   LIDEL,LIDELQ        Test end of record                           
         JE    GETCAP32                                                         
GETCAP30 IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GETCAP28                                                         
                                                                                
GETCAP32 CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GETCAP30                                                         
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
         LA    RF,TT_CLIBA                                                      
         XR    RE,RE                                                            
GETCAP34 TM    LIDLAPPL,LIDLTIME   Is this entry for timesheets                 
         JZ    GETCAP36            No                                           
         MVC   0(L'PIDNO,RF),LIDLPID  Yes - Get back up approver PID            
         LA    RF,L'PIDNO(RF)                                                   
         AHI   RE,1                                                             
                                                                                
GETCAP36 CHI   RE,HD_BAMAX         Did we fill up the whole table?              
         JE    EXITY                                                            
         LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETCAP34            No - check next entry                        
         J     EXITY               Yes - finish                                 
                                                                                
GETCAPN  J     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Get approver and back-up approver PID for time                      *         
***********************************************************************         
                                                                                
GETMAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETMAP*'                                                      
                                                                                
         LR    R2,R1                                                            
P        USING ACTKACT,R2                                                       
                                                                                
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
         MVC   HD_MANAP,MN.DPAPPIDB                                             
         J     GETMAP10                                                         
                                                                                
GETMAP08 MVC   MN.DPAPAS,IOKEYSAV  Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETMAP02         Do for number of 1R levels                   
         J     GETMAPN                                                          
                                                                                
GETMAP10 CLI   MN.DPAPSEQ,0        MAIN APPROVER RECORD?                        
         JNE   GETMAP12                                                         
         XC    CSVKEY1,CSVKEY1                                                  
         MVC   CSVKEY1(L'DPAPAS),MN.DPAPAS                                      
         J     GETMAP18                                                         
         DROP  MN,P                                                             
                                                                                
AP       USING APPRECD,IOKEY       NO, READ MAIN APPROVER RECORD                
GETMAP12 XC    AP.APPKEY,AP.APPKEY                                              
         MVI   AP.APPKTYP,APPKTYPQ                                              
         MVI   AP.APPKSUB,APPKSUBQ                                              
         MVC   AP.APPKCPY,CUXCPY                                                
         MVC   AP.APPKPIDB,HD_MANAP                                             
         XC    CSVKEY1,CSVKEY1                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    GETMAP18                                                         
         DC    H'0'                MAIN APPROVER RECORD MISSING                 
*                                                                               
GETMAP14 OC    CSVKEY1,CSVKEY1                                                  
         JZ    GETMAP16                                                         
         MVC   IOKEY,CSVKEY1       (RE-)ESTABLISH SEQUENCE                      
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETMAP16 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                MAIN APPROVER RECORD MISSING                 
         CLC   AP.APPKEY(APPKSEQ-APPKEY),IOKEYSAV                               
         JNE   GETMAPY                                                          
         DROP  AP                                                               
*                                                                               
GETMAP18 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,IOADDR                                                        
         OC    CSVKEY1,CSVKEY1                                                  
         JZ    *+10                                                             
         MVC   CSVKEY1(L'APPKEY),0(R1)  SAVE APPKEY                             
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING LIDELD,R1                                                        
GETMAP20 CLI   LIDEL,0             Test end of record                           
         JE    GETMAP14            Yes, any more?                               
         CLI   LIDEL,LIDELQ                                                     
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
         LA    RF,HD_MANBA                                                      
         XR    RE,RE                                                            
GETMAP26 TM    LIDLAPPL,LIDLTIME   Is this entry for timesheets                 
         JZ    GETMAP28            No                                           
         MVC   0(L'PIDNO,RF),LIDLPID  Yes - Get back up approver PID            
         LA    RF,L'PIDNO(RF)                                                   
         AHI   RE,1                                                             
                                                                                
GETMAP28 CHI   RE,HD_BAMAX         Did we fill up the whole table?              
         JE    GETMAPY             Hope it's enough...                          
         LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R3,R4               Check we haven't reached end of el           
         JH    GETMAP26            No - check next entry                        
         J     GETMAPY             Yes - finish                                 
         DROP  R4                                                               
                                                                                
GETMAPN  MVC   ROUERRV,=AL2(AE$INAPP)                                           
         J     EXITN                                                            
                                                                                
GETMAPY  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
***********************************************************************         
                                                                                
SAVERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
                                                                                
         CLI   RUNMODE,RVALREQQ    Can't handle errors if live update           
         JE    *+6                                                              
         DC    H'0'                                                             
         USING ET_D,RF                                                          
         LLC   R0,4(R1)            R0=Length of text                            
         LM    R2,R3,0(R1)         R2=A(Error message), R3=A(text)              
         LA    RF,ERRTAB           Point to start of table                      
SAVERR02 CLI   ET_D,ET_EOTQ        Are we at end of table                       
         JE    SAVERR10            Yes                                          
         LLC   RE,ET_LN            RE=length of table entry                     
         CLC   ET_ERRNO,0(R2)      Is the error same as previous                
         JNE   SAVERR06            No - go to next entry                        
         SHI   RE,ET_LN1Q                                                       
         CR    RE,R0               Is the extra text the same length            
         JNE   SAVERR04            No - must be different                       
         LTR   R4,R0               Is there any extra text                      
         JZ    EXITY               No - can exit as message same                
         SHI   R4,1                                                             
         BASR  RE,0                                                             
         CLC   ET_EXTRA(0),0(R3)   Is the extra text the same                   
         EX    R4,0(RE)                                                         
         JE    EXITY               Yes - then skip this again                   
SAVERR04 LLC   RE,ET_LN                                                         
SAVERR06 AR    RF,RE               RF=A(next entry in table)                    
         J     SAVERR02                                                         
*                                                                               
SAVERR10 L     RF,ANXTERR          Address of next error in table               
         CLI   ERRTAB,ET_EOTQ      If table is empty                            
         JNE   *+8                                                              
         LA    RF,ERRTAB           Point to start of table                      
         LLC   R0,4(R1)                                                         
         AHI   R0,ET_LN1Q          R0=Length of new entry                       
         AR    R0,RF                                                            
         LA    RE,ERRTAB+L'ERRTAB                                               
         OI    TWAMODE,TWAMERP     Set we have validation error                 
         CR    R0,RE               Skip if full not good to die here            
         JH    EXITY                                                            
         SR    R0,RF                                                            
         STC   R0,ET_LN            Set length of new entry                      
         MVC   ET_ERRNO,0(R2)                                                   
         SHI   R0,ET_LN1Q                                                       
         LTR   R1,R0               Test any extra text to be added              
         JZ    SAVERR20            No                                           
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ET_EXTRA(0),0(R3)   Move extra text into entry                   
         EX    R1,0(RE)                                                         
SAVERR20 LA    RF,ET_D+ET_LN1Q     Point to next error slot                     
         AR    RF,R0               Add length of extra text                     
         ST    RF,ANXTERR          Set A(Next entry to be added)                
         MVI   ET_D,ET_EOTQ        Set new end of table                         
         MVC   ERRTXT,SPACES       Clear extra text                             
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Clear TSAR record                                                   *         
***********************************************************************         
                                                                                
CLRTTD   STM   RE,R1,12(RD)                                                     
         LA    R0,TT_D                                                          
         LHI   R1,TT_LN2Q                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
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
* Extract name of record pointed to by IOADDR                         *         
*                                                                     *         
* Ntry:- R1=A(Output area)                                            *         
***********************************************************************         
                                                                                
GETNAM   STM   RE,R2,12(RD)                                                     
         LR    R2,R1               R2=A(Output area)                            
         MVC   0(L'NAMEREC,R2),SPACES                                           
         GOTOR GETELA,NAMELQ       Locate name element on record                
         JNE   GETNAMX                                                          
         USING NAMELD,R1                                                        
         CLI   NAMLN,NAMLN1Q       Test good length                             
         JNH   GETNAMX                                                          
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         CHI   RF,L'NAMEREC-1                                                   
         JNH   *+8                                                              
         LHI   RF,L'NAMEREC-1                                                   
         BASR  RE,0                                                             
         MVC   0(0,R2),NAMEREC     Move name to output area                     
         EX    RF,0(RE)                                                         
GETNAMX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
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
* Test security for an account record                                 *         
*                                                                     *         
* Ntry:- R1=A(Account status element) or 0 to locate status element   *         
*        on current record                                            *         
* Exit:- CC=Equal if okay, not equal if security lockout with ROUERRV *         
*        set to message number                                        *         
***********************************************************************         
                                                                                
TSTSEC   XC    ROUERRV,ROUERRV     Preset no error                              
         LTR   R1,R1               Test pointing to status element              
         JNZ   TSTSEC02                                                         
         ST    RE,12(RD)                                                        
         GOTOR GETELA,RSTELQ       Locate status element on record              
         L     RE,12(RD)                                                        
         JE    TSTSEC02                                                         
         CR    RE,RE               No status element - set OK                   
         BR    RE                  (Element pointer will be zero)               
                                                                                
         USING RSTELD,R1                                                        
TSTSEC02 CLI   CUAUTH+1,0          Allow if zero                                
         BER   RE                                                               
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   *+12                                                             
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         BR    RE                                                               
         CR    RE,RE               Set CC=Equal                                 
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Add time line/materials TSAR record if passed                       *         
***********************************************************************         
                                                                                
DOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOTSAR*'                                                      
                                                                                
         ICM   RE,15,0(R1)         RE=Index to work pool element                
         JZ    EXITN               CC=Low if element not found                  
         SR    RF,RF                                                            
         ICM   RF,3,LW_LN-LW_D(RE)                                              
         SHI   RF,LW_LN1Q          RF=L'TSAR record                             
         AHI   RE,LW_LN1Q          RE=A(TSAR record)                            
         LA    R0,TT_D             To address                                   
         LR    R1,RF               To length                                    
         MVCL  R0,RE               Copy record to I/O area 1                    
         GOTOR BUFTIM,DMCB,('TSAADD',NEWBUF),0                                  
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR for time buffers                                  *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
***********************************************************************         
                                                                                
BUFTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFTIM*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSAROLDT         Point to correct TSAR block                  
         CLI   3(R2),OLDBUF                                                     
         JNE   *+8                                                              
         LA    R3,TSARNEWT                                                      
                                                                                
         USING TSARD,R3            R3=A(TSAR block)                             
         LA    R0,TT_D                                                          
         ST    R0,TSAREC           Set A(Record)                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   BUFTIM02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,2*ONEK                                                        
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRTSAB1                                                  
         CLI   3(R2),OLDBUF                                                     
         JNE   *+8                                                              
         MVI   TSRECI,TSRTSAB2                                                  
         OI    TSRECI,TSRXTN                                                    
         MVI   TSKEYL,TT_KEYL      Set key length                               
         LHI   R0,TT_LN2Q                                                       
         STCM  R0,3,TSRECL         Set record length                            
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFTIM02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    BUFTIM04                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
         OC    4(L'TSAREC,R2),4(R2)                                             
         JZ    *+10                                                             
         MVC   TSAREC,4(R2)        Set A(record) if passed                      
         GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   TIMERR,TSERRS       Return TSERRS in TIMERR                      
         J     BUFTIMX                                                          
                                                                                
BUFTIM04 MVI   TIMERR,TSEEOF       Set EOF if not initialised                   
                                                                                
BUFTIMX  CLI   TIMERR,0            Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR                                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,ONEK                                                          
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
* Manage a TSAR buffer of ACCMST records that are to be updated       *         
*                                                                     *         
* This routine will add an entry to the buffer for the first read     *         
* of the record, provided that the record is found and not deleted.   *         
* The caller may add new records to the buffer (with a disk address   *         
* of zero) and records which are to replace deleted records (which    *         
* are not added automatically).  When the routine is called for the   *         
* last time (R1=0) the buffer records are read back and the internal  *         
* DATAMGR routine is called to add the records to the output FACWRK   *         
* recovery file - any records with a zero disk address will cause     *         
* an ADDREC in SRUPD60 else a PUTREC will be done instead.            *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows or zero if last     *         
*        time call (to flush buffer and write recovery records)       *         
*                                                                     *         
*        P1/0   - Action/read flags                                   *         
*        P2/1-3 - A(Record)                                           *         
*        P3     - A(Caller hook routine for buffer add)               *         
***********************************************************************         
                                                                                
BUFGET   EQU   1                   Get/create a buffer entry                    
BUFGDR   EQU   BUFGET+X'08'        Get/create - read for deletes                
BUFPUT   EQU   2                   Update a buffer entry                        
BUFADD   EQU   3                   Add a new buffer entry                       
BUFRDH   EQU   4                   Read high for a record key                   
BUFNXT   EQU   5                   Get next record (after read high)            
                                                                                
BUFREC   NTR1  LABEL=NO,WORK=(RC,BRWORKL)                                       
         J     *+12                                                             
         DC    C'*BUFREC*'                                                      
                                                                                
         USING BRWORKD,RC          RC=A(local working storage)                  
TB       USING TSARD,TSARRECS      BUFREC TSAR block                            
         LTR   R2,R1               Test last time call                          
         JZ    BUFREC16                                                         
         MVC   BRACTN,0(R2)                                                     
         MVC   BRINDS,0(R2)                                                     
         NI    BRACTN,B'00000111'  Isolate action number                        
         NI    BRINDS,B'11111000'  Isolate read flags                           
                                                                                
         TM    RUNINDS,RUNIBUFF    Test first time                              
         JNZ   BUFREC02                                                         
         MVI   TB.TSACTN,TSAINI    Yes - initialise TSAR buffer                 
         MVI   TB.TSRECI,TSRXTN+TSRMINB1+TSRVAR                                 
         MVI   TB.TSKEYL,L'BR_KEY                                               
         LHI   R0,BR_LNQ                                                        
         STCM  R0,3,TB.TSRECL      Set maximum record length                    
         LHI   R0,2*ONEK           Default to 2MB off-line                      
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL     Set buffer length if not set                 
         MVC   TB.TSACOM,ACOMFACS                                               
         GOTOR VTSAR,TB.TSARD      Initialise TSAR buffer                       
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    RUNINDS,RUNIBUFF    Set not first time                           
                                                                                
BUFREC02 L     R4,4(R2)            R4=A(Record)                                 
         SHI   R4,BR_HL            Back up by header length                     
         ST    R4,TB.TSAREC        Set A(TSAR record)                           
R        USING BR_D,R4                                                          
         MVC   BRSAVE(BR_HL),0(R4) Save what's there                            
         XC    R.BR_D(BR_HL),R.BR_D                                             
         MVI   BRTSARER,0          Clear TSAR return flags                      
                                                                                
         CLI   BRACTN,BUFGET       Test get record from buffer                  
         JNE   BUFREC04                                                         
         MVC   R.BR_KEY,IOKEY                                                   
         MVI   TB.TSACTN,TSARDH                                                 
         GOTOR VTSAR,TB.TSARD      See if we have record already                
         JE    BUFREC12                                                         
         MVI   TB.TSACTN,TSAADD    Set action to add record                     
         SR    R0,R0               Set buffer record length                     
         ICM   R0,3,R.BR_REC+(ACCRLEN-ACCRECD)                                  
         AHI   R0,BR_REC-BR_D                                                   
         STCM  R0,3,R.BR_LEN                                                    
         MVC   R.BR_KEY,IOKEY      Set record key                               
                                                                                
         MVC   IOKEYSAV,IOKEY      Save record key (emulate IOEXEC)             
         GOTOR DATAMGR,PARM,(BRINDS,DMREAD),ACCDIR,IOKEY,IOKEY                  
         MVC   IODA,IOKEY+(ACCKDA-ACCRECD)                                      
         MVC   IOERR,8(R1)         Save directory return                        
         JNE   EXITN               Exit on any errors                           
                                                                                
         MVC   R.BR_DA,IODA        Set disk address                             
         GOTOR (RF),(R1),(BRINDS,DMGETR),ACCMST,R.BR_DA,R.BR_REC,      +        
               R.BR_WORK                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR VTSAR,TB.TSARD      Add record to TSAR buffer                    
         JE    BUFREC12                                                         
         DC    H'0'                                                             
                                                                                
BUFREC04 CLI   BRACTN,BUFPUT       Test put record                              
         JE    *+12                                                             
         CLI   BRACTN,BUFADD       Test add record                              
         JNE   BUFREC06                                                         
         SR    R0,R0               Set buffer record length                     
         ICM   R0,3,R.BR_REC+(ACCRLEN-ACCRECD)                                  
         AHI   R0,BR_REC-BR_D                                                   
         STCM  R0,3,R.BR_LEN                                                    
         MVC   R.BR_KEY,IOKEY      Set record key                               
         MVC   R.BR_ERRS,IOERR     Set error flags                              
         MVC   R.BR_DA,IODA        Set disk address                             
         MVC   R.BR_WORK,IOWORK    Set PUTREC work area                         
         MVI   TB.TSACTN,TSAADD    Set action to add                            
         CLI   BRACTN,BUFPUT                                                    
         JNE   *+8                                                              
         MVI   TB.TSACTN,TSAPUT    Set action to put                            
         GOTOR VTSAR,TB.TSARD                                                   
         JE    BUFREC14                                                         
         DC    H'0'                                                             
                                                                                
BUFREC06 CLI   BRACTN,BUFRDH       Test read high for a record key              
         JNE   BUFREC08                                                         
         MVC   IOKEYSAV,IOKEY      Save caller's key                            
         MVC   R.BR_KEY,IOKEY      Set read high key                            
         MVI   TB.TSACTN,TSARDH    Set action to 'read high'                    
         J     BUFREC10                                                         
                                                                                
BUFREC08 CLI   BRACTN,BUFNXT       Get next record for a record key             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TB.TSACTN,TSANXT    Set action to 'get next'                     
                                                                                
BUFREC10 GOTOR VTSAR,TB.TSARD      Read high/get next                           
         MVC   BRTSARER,TB.TSERRS                                               
         TM    BRTSARER,TSEEOF     Test end of file                             
         JNZ   BUFREC14                                                         
         MVI   BRTSARER,0          Clear error for read high                    
                                                                                
BUFREC12 MVC   IODA,R.BR_DA        Set disk address                             
         MVC   IOWORK,R.BR_WORK    Set PUTREC work area                         
K        USING ACCRECD,IOKEY       Build directory record                       
         MVC   K.ACCKEY,R.BR_REC                                                
         MVC   K.ACCKSTA,R.BR_REC+(ACCRSTA-ACCRECD)                             
         MVC   K.ACCKDA,IODA                                                    
                                                                                
BUFREC14 MVC   R.BR_D(BR_HL),BRSAVE                                             
         CLI   BRTSARER,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R                                                                
                                                                                
BUFREC16 TM    RUNINDS,RUNIBUFF    Test first time flag set                     
         JZ    EXITY               No - buffer must be empty                    
                                                                                
         MVI   TB.TSACTN,TSARDH                                                 
         L     R2,AIO1             Use IO1 for these calls                      
         ST    R2,TB.TSAREC                                                     
         USING BR_D,R2                                                          
         XC    BR_KEY,BR_KEY                                                    
                                                                                
BUFREC18 GOTOR VTSAR,TB.TSARD      Get first/next buffer record                 
         MVI   TB.TSACTN,TSANXT                                                 
         TM    TB.TSERRS,TSEEOF    Test end of buffer                           
         JNZ   EXITY                                                            
                                                                                
         OC    BR_DA,BR_DA         Test new record                              
         JZ    BUFREC20                                                         
         GOTOR DMGRITRN,DMCB,DMPUTF,ACCMST,BR_DA,BR_REC,BR_WORK                 
                                                                                
         GOTOR DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,BR_KEY,IOKEYSAV               
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   K.ACCKEY,BR_REC     Build directory record                       
         MVC   K.ACCKSTA,BR_REC+(ACCRSTA-ACCRECD)                               
         MVC   K.ACCKDA,BR_DA                                                   
         CLC   K.ACCRECD(ACCKLEN),IOKEYSAV                                      
         JE    BUFREC18            Done if no change to it                      
         GOTOR DMGRITRN,DMCB,DMWRTD,ACCDIR,K.ACCKEY,K.ACCKEY                    
         J     BUFREC18                                                         
                                                                                
BUFREC20 GOTOR DMGRITRN,DMCB,DMADDF,ACCMST,BR_DA,BR_REC,BR_WORK                 
         J     BUFREC18                                                         
         DROP  RC,K,R2                                                          
                                                                                
BRWORKD  DSECT                     ** BUFREC local working storage **           
BRACTN   DS    X                   Action                                       
BRINDS   DS    X                   Read flags                                   
BRTSARER DS    XL(L'TSERRS)        TSAR error                                   
BRSAVE   DS    XL(BR_HL)           Save area                                    
BRWORKL  EQU   *-BRWORKD                                                        
                                                                                
BR_D     DSECT                     ** Record buffer **                          
BR_LEN   DS    AL2                 Buffer record length                         
                                                                                
BR_KEY   DS    XL(L'ACCKEY)        Record key                                   
                                                                                
BR_ERRS  DS    XL(L'IOERR)         Error flags                                  
BR_DA    DS    XL(L'IODA)          Disk address                                 
BR_WORK  DS    XL(L'IOWORK)        PUTREC work area                             
BR_HL    EQU   *-BR_D              Length of header                             
                                                                                
BR_REC   DS    XL(IOLENQ)          ACCMST record (including key)                
BR_LNQ   EQU   *-BR_D              Maximum record size                          
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR for general use buffer                            *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
***********************************************************************         
                                                                                
BUFGEN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFGEN*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         CLI   0(R2),TSAADD                                                     
         JNE   *+8                                                              
         OI    RUNINDS,RUNIUPD     Set we have something to update              
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    EXITY                                                            
                                                                                
TR       USING TSARD,TSARGENL      R3=A(TSAR block)                             
         USING TI_RECD,R3                                                       
         LAY   R3,I_GEN                                                         
         ST    R3,TR.TSAREC        Set A(Record)                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   BUFGEN04                                                         
         XC    TR.TSARD(TSPNEWL),TR.TSARD                                       
         MVC   TR.TSACTN,0(R2)        Action                                    
         MVC   TR.TSACOM,ACOMFACS     A(COMFACS)                                
         LHI   R0,2*ONEK                                                        
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TR.TSBUFFL        Set require 1MB off-line                  
         MVI   TR.TSRECI,TSRXTN+TSRMINB2                                        
         MVI   TR.TSKEYL,TI_KEYL      Set key length                            
         LHI   R0,TI_LENQ                                                       
         STCM  R0,3,TR.TSRECL         Set max record length                     
         XC    TIMSEQ#,TIMSEQ#                                                  
*                                                                               
BUFGEN02 MVI   TR.TSINDS,TSINODSK     Set no disk writes (save/restore)         
         GOTOR VTSAR,TR.TSARD                                                   
         TM    TR.TSINDS,TSIINIOK                                               
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFGEN04 TM    TR.TSINDS,TSIINIOK     Test initialised                          
         JZ    BUFGEN06                                                         
                                                                                
         MVC   TR.TSACTN,0(R2)        Set action                                
         CLI   TR.TSACTN,TSAADD                                                 
         JNE   BUFGEN05                                                         
         OI    RUNINDS,RUNIUPD        Set we have something to update           
         LLH   RF,TIMSEQ#             Set sequence no. and update the           
         STH   RF,TI_KSEQ             sequence no.                              
         AHI   RF,1                                                             
         STH   RF,TIMSEQ#                                                       
*                                                                               
BUFGEN05 OC    4(L'TSAREC,R2),4(R2)                                             
         JZ    *+10                                                             
         MVC   TR.TSAREC,4(R2)        Set A(record) if passed                   
                                                                                
         GOTOR VTSAR,TR.TSARD         Call TSAR                                 
         MVC   TSAERR,TR.TSERRS       Return TSERRS in INVERR                   
         J     BUFGENX                                                          
                                                                                
BUFGEN06 MVI   TSAERR,TSEEOF       Set EOF if not initialised                   
*                                                                               
BUFGENX  CLI   TSAERR,0            Set condition code for caller                
         J     EXIT                                                             
         DROP  TR                                                               
         EJECT                                                                  
***********************************************************************         
* Create transactions and/or update/create buckets                    *         
***********************************************************************         
                                                                                
NEW      USING TT_D,R6                                                          
TIMTRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TIMTRN*'                                                      
                                                                                
         XC    SVMOA,SVMOA                                                      
         LA    R6,TT_NEW                                                        
*&&UK                                                                           
         MVI   ATPROF1,ATPR1SAL    Default to use sales rate                    
         CLI   HD_COBRT,COOPTYQ    Use cost rate not sales (B-Time)             
         JE    *+8                                                              
         MVI   ATPROF1,ATPR1CST    Use cost rate                                
         MVI   ATPROF2,ATPR1SAL    Default to use sales rate                    
         CLI   HD_CORRT,COOPTYQ    Use cost rate not sales (R-Time)             
         JE    *+8                                                              
         MVI   ATPROF2,ATPR1CST    Use cost rate                                
*&&                                                                             
* USE A TABLE HERE INSTEAD                                                      
                                                                                
         MVI   ATTRSST4,0          Clear status                                 
         TM    TR_TSSTN,TIMSFAPP   Is time fully approved?                      
         JNZ   TIMTRN02            Yes - status to set                          
         CLI   TR_TSSTN,0          Any status                                   
         JNE   *+12                Yes                                          
         MVI   ATTRSST4,TRSSSAVT   None must be saved                           
         J     TIMTRN02                                                         
         TM    TR_TSSTN,TIMSMAAP   Is time line manager approved?               
         JZ    *+8                 No                                           
         OI    ATTRSST4,TRSSMAAP   Yes - set status accordingly                 
         TM    TR_TSSTN,TIMSREJE   Is time line manager rejecter?               
         JZ    *+8                 No                                           
*&&UK*&& MVI   ATTRSST4,TRSSREJT   Yes - set status accordingly                 
*&&US*&& MVI   ATTRSST4,TRSSREJD   Yes - set status accordingly                 
         TM    TR_TSSTN,TIMESAPR   Test approved                                
         JZ    *+8                                                              
         OI    ATTRSST4,TRSSSJAT   Yes - set further status byte                
         TM    TR_TSSTN,TIMESREJ   Test rejected                                
         JZ    *+8                                                              
*&&UK*&& MVI   ATTRSST4,TRSSREJT                                                
*&&US*&& MVI   ATTRSST4,TRSSREJD                                                
         CLI   ATTRSST4,0                                                       
         JNE   *+12                                                             
         MVI   ATTRSST4,TRSSSUBT   If neither, assume submitted                 
         J     TIMTRN02                                                         
         TM    ATTRSST4,TRSSSJAT+TRSSMAAP                                       
         JNO   TIMTRN02                                                         
         MVI   ATTRSST4,0          Clear as fully approved                      
                                                                                
TIMTRN02 XC    OLD.TT_#,OLD.TT_#   Get first old time buffer record             
         GOTOR NXTBUF,OLDBUF                                                    
         XC    NEW.TT_#,NEW.TT_#   Get first new time buffer record             
         GOTOR NXTBUF,NEWBUF                                                    
                                                                                
TIMTRN04 MVI   STPOSTNG,0          Initialise action controls                   
                                                                                
         CLC   OLD.TT_#,EFFS       Test old buffer is empty                     
         JNE   *+8                                                              
         OI    STPOSTNG,STADDNEW   Yes - create new postings                    
                                                                                
         CLC   NEW.TT_#,EFFS       Test new buffer is empty                     
         JNE   *+8                                                              
         OI    STPOSTNG,STBCKOUT   Yes - back out old postings                  
                                                                                
         TM    STPOSTNG,STADDNEW+STBCKOUT                                       
         JO    TIMTRNX             Back out old and create new postings         
         TM    STPOSTNG,STBCKOUT                                                
         JNZ   TIMTRN16            Back out old postings                        
         TM    STPOSTNG,STADDNEW                                                
         JNZ   TIMTRN18            Create new postings                          
                                                                                
         CLC   OLD.TT_#,NEW.TT_#   Compare keys                                 
         JL    TIMTRN16            Line deleted - back out old postings         
         JH    TIMTRN18            Line added - create new postings             
                                                                                
***********************************************************************         
* Old and new records are present - check for pertinent data changes  *         
***********************************************************************         
                                                                                
         LA    R0,OLD.TT_AVALS                                                  
         LHI   R1,TT_AVALL                                                      
         LA    RE,NEW.TT_AVALS                                                  
         LHI   RF,TT_AVALL                                                      
         CLCL  R0,RE               Any critical data                            
         JNE   TIMTRN10            changed                                      
         OC    OLD.TT_ITEM#,OLD.TT_ITEM# Check whether time or material         
         JNZ   TIMTRN06                                                         
         CLC   OLD.TT_TPST(TT_TPSTL),NEW.TT_TPST  Any time info changed         
         JNE   TIMTRN10                                                         
         CP    OLD.TT_HRS,NEW.TT_HRS Have hours changed                         
         JE    TIMTRN08                                                         
         SP    NEW.TT_HRS,OLD.TT_HRS   Yes work out difference                  
         SP    NEW.TT_AMNT,OLD.TT_AMNT and post it                              
         J     TIMTRN07                                                         
                                                                                
TIMTRN06 CLC   OLD.TT_IPST(TT_IPSTL),NEW.TT_IPST  Any material info             
         JNE   TIMTRN10                  changed                                
         CP    OLD.TT_IMULT,NEW.TT_IMULT Has multiplier changed                 
         JE    TIMTRN08                                                         
         SP    NEW.TT_IMULT,OLD.TT_IMULT  Yes work out difference               
         SP    NEW.TT_ITOT,OLD.TT_ITOT and post it                              
TIMTRN07 CLC   HD_TSSTO,TR_TSSTN   Test time sheet status has changed           
         JNE   *+14                                                             
         CLC   OLD.TT_EPST1,NEW.TT_EPST1                                        
         JE    TIMTRN12            Nothing changed - get next pair              
         OI    STPOSTNG,STAUPTRX   Ensure existing transactions are             
         J     TIMTRN12                                updated                  
                                                                                
TIMTRN08 CLC   HD_TSSTO,TR_TSSTN   Test time sheet status has changed           
         JNE   *+14                                                             
         CLC   OLD.TT_EPST1,NEW.TT_EPST1                                        
         JE    TIMTRN14            Nothing changed - get next pair              
                                                                                
         OI    STPOSTNG,STA1CBUC   Set skipping buckets                         
         J     TIMTRN12                                                         
                                                                                
***********************************************************************         
* Changed entry - back out old postings and create new postings       *         
***********************************************************************         
                                                                                
TIMTRN10 OI    STPOSTNG,STNEGATE                                                
         GOTOR BLDTRN,OLD.TT_D     Back out old postings                        
                                                                                
TIMTRN12 GOTOR BLDTRN,NEW.TT_D     Create new postings/buckets                  
                                                                                
TIMTRN14 GOTOR NXTBUF,OLDBUF       Get next old time record                     
         GOTOR NXTBUF,NEWBUF       Get next new time record                     
         J     TIMTRN04                                                         
                                                                                
***********************************************************************         
* Deleted entry - back out old postings                               *         
***********************************************************************         
                                                                                
TIMTRN16 OI    STPOSTNG,STNEGATE                                                
*   update MOA  (DOSTAT doesn't handle on delete)                               
         CLC   TT_MOA,HD_DMMOA         Is it higher than the new one?           
         JH    *+10                                                             
         MVC   TT_MOA,HD_DMMOA                                                  
*                                                                               
         GOTOR BLDTRN,OLD.TT_D     Back out old postings                        
         GOTOR NXTBUF,OLDBUF       Get next old time record                     
         J     TIMTRN04                                                         
                                                                                
***********************************************************************         
* New entry added - create new postings                               *         
***********************************************************************         
                                                                                
TIMTRN18 GOTOR BLDTRN,NEW.TT_D     Create new postings                          
         GOTOR NXTBUF,NEWBUF       Get next new time record                     
         J     TIMTRN04                                                         
                                                                                
TIMTRNX  J     EXITY                                                            
         EJECT                                                                  
         DROP  NEW                                                              
***********************************************************************         
* Get first/next record from old or new time buffer                   *         
*                                                                     *         
* Ntry:- R1=TSAR buffer number (old or new)                           *         
* Exit:- Record returned in T_OLD or TT_NEW - TT_# is set to EFFS if  *         
*        the buffer is empty or exhausted                             *         
***********************************************************************         
                                                                                
NXTBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*NXTBUF*'                                                      
                                                                                
         LR    R0,R1               Save calling buffer number                   
         LA    R7,TT_OLD           Point to correct output record               
         CHI   R0,OLDBUF                                                        
         JE    *+8                                                              
         LA    R7,TT_NEW                                                        
         CLC   TT_#,EFFS           Shouldn't be here if EOF is set              
         JNE   NXTBUF02                                                         
         DC    H'0'                                                             
                                                                                
NXTBUF02 LHI   RF,TSARDH           Set to do read high (first time)             
         OC    TT_#,TT_#           Test record read previously                  
         JZ    *+8                                                              
         LHI   RF,TSANXT           Yes - get next record                        
         GOTOR BUFTIM,DMCB,((RF),(R0)),TT_D                                     
         TM    TIMERR,TSEEOF       Test end of file                             
         JZ    *+14                                                             
         MVC   TT_#,EFFS           Set buffer is empty/exhausted                
         J     NXTBUFX                                                          
         OC    TT_NAR#,TT_NAR#     Skip day narrative                           
         JNZ   NXTBUF02                                                         
                                                                                
         CLI   HD_COTUP,COSAVED    Test postings made when saved                
         JE    NXTBUFX                                                          
         CLI   HD_COTUP,COFUAPR    Postings hit file at fully approved          
         JNE   NXTBUF06                                                         
         CHI   R0,OLDBUF           Build before buffer?                         
         JE    NXTBUF04                                                         
         TM    TR_TSSTN,TIMSFAPP   No - is time now fully approved?             
         JO    NXTBUFX             Yes                                          
         TM    HD_TSSTO,TIMSFAPP   Was it previously fully approved?            
         JNZ   NXTBUF02            i.e. adjust functionality                    
         J     NXTBUF24            Post contra headers                          
                                                                                
NXTBUF04 TM    HD_TSSTO,TIMSFAPP   Was time previously fully approved?          
         JNO   NXTBUF24            No                                           
                                                                                
NXTBUF06 CLI   HD_COTUP,COLMAPR    Postings hit file at line manager            
         JNE   NXTBUF10            approval                                     
         CHI   R0,OLDBUF           Test before buffer?                          
         JE    NXTBUF08                                                         
         TM    TR_TSSTN,TIMSFAPP   No - is time now fully approved              
         JO    NXTBUFX             Yes                                          
         TM    TR_TSSTN,TIMSMAAP   Is time now line manager approved?           
         JO    NXTBUFX             Yes                                          
         J     NXTBUF24            Post contra headers                          
                                                                                
NXTBUF08 TM    HD_TSSTO,TIMSFAPP   Was previous time fully approved?            
         JO    NXTBUFX             Yes                                          
         TM    HD_TSSTO,TIMSMAAP   Previous time line manager approved?         
         JNO   NXTBUF24            No - not interested                          
                                                                                
NXTBUF10 CLI   HD_COTUP,COSUBMD    Postings hit file at submitted               
         JE    *+12                                                             
         CLI   HD_COTUP,COCLAPR    Postings hit file at client approval         
         JNE   NXTBUFX                                                          
         CHI   R0,OLDBUF           Test before buffer?                          
         JE    NXTBUF16                                                         
         TM    TR_TSSTN,TIMSFAPP   No - is time now fully approved              
         JO    NXTBUFX             Yes                                          
         CLI   HD_COACS,C'C'       Is it concurrent approval?                   
         JNE   NXTBUF12                                                         
         TM    TR_TSSTN,TIMSMAAP   Is time now line manager approved?           
         JO    NXTBUF22            Yes                                          
         J     NXTBUF14                                                         
                                                                                
NXTBUF12 TM    TR_TSSTN,TIMSMAAP   Test line manager approved                   
         JO    NXTBUFX                                                          
                                                                                
NXTBUF14 TM    TR_TSSTN,TIMSPAPP   Is time now part approved?                   
         JO    NXTBUF22            Yes                                          
         TM    TR_TSSTN,TIMSSUBM   Is time now submitted?                       
         JO    NXTBUF22            Yes                                          
         J     NXTBUF24            Post contra headers                          
                                                                                
NXTBUF16 TM    HD_TSSTO,TIMSFAPP   Was previous time fully approved?            
         JO    NXTBUFX             Yes                                          
         CLI   HD_COACS,C'C'       Is it concurrent approval?                   
         JNE   NXTBUF18                                                         
         TM    HD_TSSTO,TIMSMAAP   Is time now line manager approved?           
         JO    NXTBUF22            Yes                                          
         J     NXTBUF20                                                         
                                                                                
NXTBUF18 TM    HD_TSSTO,TIMSMAAP   Test line manager approved                   
         JO    NXTBUFX                                                          
                                                                                
NXTBUF20 TM    HD_TSSTO,TIMSMAAP   Previous time line manager approved?         
         JO    NXTBUF22            Yes                                          
         TM    HD_TSSTO,TIMSPAPP   Is time now part approved?                   
         JO    NXTBUF22            Yes                                          
         TM    HD_TSSTO,TIMSSUBM   Is time now submitted?                       
         JNO   NXTBUF24            No - not interested                          
                                                                                
NXTBUF22 CLI   HD_COTUP,COCLAPR    Postings hit file at client approval         
         JNE   NXTBUFX                                                          
         TM    TT_EPST1,TIMESAPR   Is the time client approved?                 
         JNZ   NXTBUFX             Yes                                          
                                                                                
NXTBUF24 OC    TT_ITEM#,TT_ITEM#   Test materials                               
         JNZ   NXTBUF02            Yes - time will create contras               
                                                                                
         OI    TT_BSTAT,TT_BSHDR   Set we only want contra-headers              
         ZAP   TT_HRS,PZERO        Clear hours                                  
         ZAP   TT_RATE,PZERO       Clear sales rate                             
*&&UK*&& ZAP   TT_CRATE,PZERO      Clear cost rate                              
                                                                                
NXTBUFX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Build transactions                                                  *         
***********************************************************************         
                                                                                
BLDTRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDTRN*'                                                      
                                                                                
         LR    R7,R1               Point to TT_D                                
         OC    TT_NAR#,TT_NAR#     Skip narrative type                          
         JNZ   EXITY                                                            
*&&UK                                                                           
         TM    TT_BSTAT,TT_BSHDR   Test creating contra-headers only            
         JNZ   BLDTRN02                                                         
         CLI   TT_TTYP,TIMTNC      Test non-client time                         
         JE    BLDTRN02                                                         
         CLI   TT_TTYP,TT_TMATR    If materials no buckets to add               
         JE    BLDTRN02            For on-line update                           
         TM    STPOSTNG,STA1CBUC   Are we skipping buckets?                     
         JNZ   BLDTRN02            Yes                                          
         CLC   TT_MOA,SVMOA        Is the MOA different?                        
         JE    BLDTRN02            No - therefore current table OK              
                                                                                
         GOTOR BLDMTH              Build 14 contra and method table             
*&&                                                                             
BLDTRN02 OC    TT_ITEM#,TT_ITEM#   Test materials                               
         JZ    BLDTRN04                                                         
         ZAP   ATAMOUNT,TT_ITOT    Materials amount                             
         J     BLDTRN08                                                         
                                                                                
BLDTRN04 CLI   TT_TTYP,TIMTNC      Test non-client time                         
         JE    *+12                                                             
         CLI   TT_TTYP,TIMTCN      Test nonbillable-client time                 
         JNE   BLDTRN06                                                         
         ZAP   ATAMOUNT,PZERO      Yes - posting amount is zero                 
         J     BLDTRN08                                                         
                                                                                
BLDTRN06 ZAP   PACK16,TT_RATE      Calculate time posting amount                
*&&UK                                                                           
         LA    RE,ATPROF1          Profile 1 for B-Time                         
         CLI   TT_TTYP,TIMTCB      Are we doing B-Time?                         
         JE    *+8                                                              
         LA    RE,ATPROF2          Profile 2 for N-Time and R-Time              
         TM    0(RE),ATPR1CST      Test cost rate instead of sales rate         
         JNO   *+10                                                             
         ZAP   PACK16,TT_CRATE     Yes - set cost rate                          
*&&                                                                             
         MP    PACK16,TT_HRS       Calculate amount based on rate               
         SRP   PACK16,64-2,5                                                    
         ZAP   ATAMOUNT,PACK16                                                  
                                                                                
BLDTRN08 CLC   TT_AULA,SPACES                                                   
         JNH   BLDTRNX                                                          
         MVC   ATREF,SPACES        Create batch reference #                     
         MVI   ATREF,C'T'          Time                                         
         CLI   TT_TTYP,TT_TMATR    Are we doing materials?                      
         JNE   BLDTRN10            No                                           
         MVI   ATREF,C'M'          Yes set reference for materials              
                                                                                
BLDTRN10 LLC   R1,HD_PERNO         Period number into chars 2-4                 
         CVD   R1,DUB              of batch reference                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  ATREF+1(3),DUB                                                   
                                                                                
         CLI   TT_TTYP,TT_TMATR    If materials skip time postings              
         JE    BLDTRN44                                                         
         TM    STPOSTNG,STA1CBUC   Are we skipping buckets?                     
         JNZ   BLDTRN32            Yes                                          
                                                                                
*        *** DR 1R/1C - only update buckets (Client time) ***                   
*        *** DR 1R/1N - only update buckets (Non-client time) ***               
                                                                                
         OI    POSTSTAT,POSTS1R    1R posting time record buckets               
                                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,HD_1RULA    1R account                                   
         MVC   TRNKOFF,TT_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_CAULA    Costing account                              
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
         MVI   TRNRSTYP,49         Assume type 49                               
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
         GOTOR EL_TRS                                                           
                                                                                
         OI    STPOSTNG,STBUCKET   Only update buckets                          
         GOTOR GOATRN,TT_CANAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS1R                                              
*&&UK                                                                           
         TM    TT_BSTAT,TT_BSHDR   Test creating contra headers only            
         JNZ   BLDTRN32            Yes - don't do method buckets                
         CLI   TT_TTYP,TIMTNC      Test non-client time                         
         JE    BLDTRN32            Yes - don't do method buckets                
                                                                                
         LA    R5,METHTAB          R5=A(Method table)                           
         USING MT_D,R5                                                          
BLDTRN12 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN18            No - finished 1R/1C and 1C/14                
                                                                                
         LHI   R0,MT_BCNT          There 3 possible buckets                     
         LA    R3,MT_PRATE         Start at type 3                              
BLDTRN14 CP    0(MT_RLNQ,R3),PZERO Is this accumulator zero?                    
         JE    BLDTRN16            Yes - ignore                                 
         ZAP   PACK16,0(MT_RLNQ,R3)                                             
         MP    PACK16,TT_HRS                                                    
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-8,5       Shift 8                                      
         ZAP   PACK8,PACK16                                                     
         TM    STPOSTNG,STNEGATE                                                
         JZ    *+10                                                             
         MP    PACK8,PNEGONE                                                    
         ZAP   DRAMOUNT,PACK8                                                   
         ZAP   CRAMOUNT,PZERO                                                   
         MVC   BUCKMTHD,MT_METHD   Set method                                   
         STC   R0,BUCKSLRY                                                      
         OI    BUCKSLRY,X'F0'      Set salary                                   
         OI    POSTSTAT,POSTSCA    CA type buckets                              
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,HD_1RULA    1R account                                   
         MVC   TRNKOFF,TT_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_CAULA    1C account                                   
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,SPACES                                                   
         MVC   TRNKREF(L'MT_METHD),MT_METHD                                     
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
                                                                                
         GOTOR GOATRN,TT_CANAM     Put transaction record                       
                                                                                
         ZAP   CRAMOUNT,PACK8                                                   
         ZAP   DRAMOUNT,PZERO                                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_CAULA    1C account                                   
         MVC   TRNKOFF,TT_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,AT142ULA    14 account 2 level                           
         CLI   MT_STR,MT_S3LVL     Is it 3 level structure?                     
         JNE   *+10                                                             
         MVC   TRNKULC,AT143ULA    14 account 3 level                           
         TM    METHSTAT,METSNANL   Are we using profile setting?                
         JZ    *+10                No                                           
         MVC   TRNKCACT(L'MT_ANAL),MT_ANAL Set analysis from profile            
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,SPACES                                                   
         MVC   TRNKREF(L'MT_METHD),MT_METHD                                     
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
                                                                                
         LA    R1,HD_142NM                                                      
         CLI   MT_STR,MT_S3LVL                                                  
         JNE   *+8                                                              
         LA    R1,HD_143NM                                                      
         GOTOR GOATRN              Put transaction record                       
                                                                                
BLDTRN16 AHI   R3,MT_RLNQ          Yes - go to next accumulator                 
         JCT   R0,BLDTRN14         Subtract type and repeat                     
         AHI   R5,MT_LNQ           End of accumulators get next method          
         J     BLDTRN12            in table and read that                       
                                                                                
BLDTRN18 LA    R5,METHTAB          R5=A(Method table)                           
BLDTRN20 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN22            No - finished 1R/1C and 1C/14                
         GOTOR UPDCPR              Update payroll history record                
         AHI   R5,MT_LNQ           Bump to next method                          
         J     BLDTRN20                                                         
                                                                                
BLDTRN22 LA    R5,METHTAB          R5=A(Method table)                           
BLDTRN24 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN30            No - finished 1R/1C and 1C/14                
                                                                                
         LHI   R0,MT_BCNT          There 3 possible buckets                     
         LA    R3,MT_PRATE         Start at type 3                              
BLDTRN26 CP    0(MT_RLNQ,R3),PZERO Is this accumulator zero?                    
         JE    BLDTRN28            Yes - ignore                                 
         ZAP   PACK16,0(MT_RLNQ,R3)                                             
         MP    PACK16,TT_HRS                                                    
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-8,5       Shift 8                                      
         ZAP   PACK8,PACK16                                                     
         TM    STPOSTNG,STNEGATE   Is it back out?                              
         JZ    *+10                                                             
         MP    PACK8,PNEGONE       Yes                                          
         STC   R0,BUCKSLRY                                                      
         OI    BUCKSLRY,X'F0'      Convert into display number                  
                                                                                
K        USING PLDRECD,IOKEY                                                    
         XC    K.PLDKEY(ACCKLEN),K.PLDKEY                                       
         MVI   K.PLDKTYP,PLDKTYPQ                                               
         MVI   K.PLDKSUB,PLDKSUBQ                                               
         MVC   K.PLDKCPY,CUXCPY                                                 
         MVC   K.PLDKMTHD,MT_METHD                                              
         MVC   K.PLDKCACT,TT_CAULA+L'TRNKLDG+L'TRNKUNT                          
         MVC   K.PLDKRACT,HD_1RACT                                              
         MVC   K.PLDKANAL,AT142ACT                                              
         TM    METHSTAT,METSNANL   Did analysis exist in 1R                     
         JZ    *+10                Yes                                          
         MVC   K.PLDKANAL,MT_ANAL  No - use profile value for analysis          
         MVC   K.PLDKYYMM,TT_MOA   Year/month                                   
         MVC   K.PLDKPTYP,BUCKSLRY Payroll type                                 
         ZAP   K.PLDKAMT,PACK8                                                  
         GOTOR DMGRITRN,DMCB,$PLDREC,K.PLDRECD                                  
                                                                                
BLDTRN28 AHI   R3,MT_RLNQ          Bump to next accumulator                     
         JCT   R0,BLDTRN26         Subtract type and repeat                     
         AHI   R5,MT_LNQ           End of accumulators get next method          
         J     BLDTRN24            in table and read that                       
         DROP  R5                                                               
                                                                                
BLDTRN30 NI    POSTSTAT,FF-POSTSCA                                              
*&&                                                                             
*              *** DR SJ/1R ***                                                 
                                                                                
BLDTRN32 NI    STPOSTNG,FF-STBUCKET                                             
         CLI   TT_TTYP,TIMTCN      Continue if client time                      
         JH    BLDTRNX                                                          
         TM    TT_BSTAT,TT_BSHDR   Test creating contra headers only            
         JNZ   BLDTRNX             Yes - don't update postings                  
         LLC   RE,PPROLEN          Test if a job specified                      
         LA    RE,TT_AACT(RE)                                                   
         CLC   0(5,RE),SPACES                                                   
         JNH   BLDTRNX                                                          
         TM    STPOSTNG,STA1CBUC+STAUPTRX Are we skipping buckets?              
         JZ    BLDTRN34            No - add records                             
         GOTOR UPDSTA              Update existing postings                     
         TM    STPOSTNG,STA1CBUC   Are skipping buckets                         
         JNZ   BLDTRNX             Yes don't add records                        
                                                                                
BLDTRN34 OI    POSTSTAT,POSTSSJ    SJ posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_AULA     SJ account                                   
         MVC   TRNKWORK,TT_TSK     Task code                                    
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,HD_1RULA                                                 
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SPA                                                           
         GOTOR EL_PRT                                                           
         GOTOR EL_SPD                                                           
*&&UK*&& GOTOR EL_SCI1                                                          
*&&UK*&& GOTOR EL_SCI2                                                          
*&&UK*&& GOTOR EL_SCI4                                                          
*&&UK*&& GOTOR EL_SCI5                                                          
         GOTOR EL_TRS                                                           
         GOTOR EL_ANO                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM1                                                          
         GOTOR EL_TIM2                                                          
         GOTOR EL_TIM3                                                          
         GOTOR EL_TIM4                                                          
         GOTOR EL_TIM5                                                          
                                                                                
         GOTOR GOATRN,HD_1RNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSJ                                              
                                                                                
*              *** CR SI/SJ ***                                                 
                                                                                
BLDTRN36 CLI   TT_TTYP,TIMTCB      Exit if not B-Time                           
         JNE   BLDTRNX                                                          
                                                                                
         OI    POSTSTAT,POSTSSI    SI posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_INULA    Income account                               
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_AULA     SJ account                                   
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
*&&US*&& GOTOR EL_MDT              Media interface element                      
*&&UK*&& GOTOR EL_SCI3                                                          
         GOTOR EL_TRS                                                           
*&&UK*&& GOTOR EL_SOR                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_RFL                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM1                                                          
         GOTOR EL_TIM2                                                          
         GOTOR EL_TIM3                                                          
         GOTOR EL_TIM4                                                          
         GOTOR EL_TIM5                                                          
                                                                                
         GOTOR GOATRN,TT_SJNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSI                                              
         CLC   LEDGERSI,TT_INULA                                                
         JNE   BLDTRN42            Posting to SI - skip for u/l=SK              
                                                                                
*              *** DR 1C/12 ***                                                 
                                                                                
BLDTRN38 OI    POSTSTAT,POSTS1C                                                 
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_CAULA    Costing account                              
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_12ULA    Analysis account                             
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,TT_12NAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS1C                                              
                                                                                
*              *** CR 12/1C ***                                                 
                                                                                
BLDTRN40 OI    POSTSTAT,POSTS12    12 posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_12ULA    Analysis account                             
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_CAULA    Costing account                              
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,TT_CANAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS12                                              
                                                                                
BLDTRN42 J     BLDTRNX                                                          
                                                                                
*              *** Materials postings ***                                       
*              *** DR SJ/credit account ***                                     
                                                                                
BLDTRN44 TM    STPOSTNG,STA1CBUC+STAUPTRX just updating existing recs?          
         JZ    BLDTRN46            No - add records                             
         TM    TT_BSTAT,TT_BSHDR   Test creating contra headers only            
         JNZ   BLDTRNX             Yes - don't update postings                  
         GOTOR UPDSTA              Update exsiting SJ posting                   
         TM    STPOSTNG,STA1CBUC                                                
         JNZ   BLDTRNX                                                          
                                                                                
BLDTRN46 OI    POSTSTAT,POSTSSJ    SJ posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_AULA     SJ account                                   
         MVC   TRNKWORK,TT_TSK     Task code                                    
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,HD_1RULA    1R account                                   
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
         MVI   TRNRSTYP,8          Assume type 8                                
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SPA                                                           
*&&UK*&& GOTOR EL_ART                                                           
         GOTOR EL_SPD                                                           
         GOTOR EL_TRS                                                           
*&&US*&& GOTOR EL_UNP                                                           
         GOTOR EL_ANO                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM1                                                          
         GOTOR EL_TIM2                                                          
         GOTOR EL_TIM3                                                          
         GOTOR EL_TIM4                                                          
         GOTOR EL_TIM5                                                          
                                                                                
         GOTOR GOATRN,HD_1RNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSJ                                              
                                                                                
*              *** CR SI/SJ ***                                                 
                                                                                
BLDTRN48 OI    POSTSTAT,POSTSSI    SI posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_INULA    Income account                               
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_AULA     SJ account                                   
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
*&&US*&& GOTOR EL_MDT                                                           
         GOTOR EL_TRS                                                           
*&&UK*&& GOTOR EL_ART                                                           
*&&UK*&& GOTOR EL_SOR                                                           
*&&US*&& GOTOR EL_UNP                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_RFL                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM1                                                          
         GOTOR EL_TIM2                                                          
         GOTOR EL_TIM3                                                          
         GOTOR EL_TIM4                                                          
         GOTOR EL_TIM5                                                          
                                                                                
         GOTOR GOATRN,TT_SJNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSI                                              
         CLC   LEDGERSI,TT_INULA                                                
         JNE   BLDTRNX             Posting to SI - skip for u/l=SK              
                                                                                
*              *** DR 1C/12 ***                                                 
                                                                                
BLDTRN50 OI    POSTSTAT,POSTS1C                                                 
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_CAULA    Costing account                              
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_12ULA    Analysis account                             
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,TT_12NAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS1C                                              
                                                                                
*              *** CR 12/1C ***                                                 
                                                                                
BLDTRN52 OI    POSTSTAT,POSTS12    12 posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,TT_12ULA    Analysis account                             
         MVC   TRNKOFF,TT_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,TT_CAULA    Costing account                              
         MVC   TRNKDATE,HD_PEDT                                                 
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,TT_CANAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS12                                              
                                                                                
BLDTRNX  NI    STPOSTNG,FF-(STNEGATE)                                           
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'1A' Media transfer element                                        *         
***********************************************************************         
*&&US                                                                           
EL_MDT   NTR1  LABEL=NO                                                         
         CLC   LEDGERSI,TT_INULA                                                
         JNE   EXITY               Don't add to SK posting                      
                                                                                
N        USING MDTELD,ELEMENT                                                   
         XC    N.MDTELD(MDTLNQ),N.MDTELD                                        
         MVI   N.MDTEL,MDTELQ                                                   
         MVI   N.MDTLN,MDTLNQ                                                   
         MVI   N.MDTSYS,MDTSPROD   C'J' - production                            
         LLC   R1,PPROLEN                                                       
         LA    R1,TT_AACT(R1)                                                   
         MVC   N.MDTMED,0(R1)                                                   
         MVC   N.MDTCLI(L'TT_AACT),TT_AACT                                      
         MVC   N.MDTMOS,TT_MOA     MOA                                          
         MVC   N.MDTDSCP,TT_SJNAM  SJ account name                              
         ZAP   DUB,ATAMOUNT                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,N.MDTCOM      Income (commission)                          
         STCM  R0,15,N.MDTINTL     Income (internal)                            
         J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'23' Others element                                                *         
***********************************************************************         
                                                                                
EL_OTH   NTR1  LABEL=NO                                                         
N        USING OTHELD,ELEMENT                                                   
         XC    N.OTHELD(OTHLN2Q),N.OTHELD                                       
         MVI   N.OTHEL,OTHELQ                                                   
         MVI   N.OTHLN,OTHLN1Q                                                  
         MVC   N.OTHNUM(L'OTHNUM+L'OTHPROF),SPACES                              
         MVC   N.OTHNUM(L'TT_AACT),TT_AACT                                      
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'2C' Special posting element                                       *         
***********************************************************************         
                                                                                
EL_SPA   NTR1  LABEL=NO                                                         
N        USING SPAELD,ELEMENT                                                   
         XC    N.SPAELD(SPALNQ),N.SPAELD                                        
         MVI   N.SPAEL,SPAELQ                                                   
         MVI   N.SPALN,SPALNQ                                                   
         MVI   N.SPATYPE,SPATCCST  Costing account                              
         MVC   N.SPAAULA,TT_CAULA                                               
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'40' Personnel rate element                                        *         
***********************************************************************         
                                                                                
EL_PRT   NTR1  LABEL=NO                                                         
N        USING PRTELD,ELEMENT                                                   
         XC    N.PRTELD(PRTLNQ),N.PRTELD                                        
         MVI   N.PRTEL,PRTELQ                                                   
         MVI   N.PRTLN,PRTLNQ                                                   
         MVC   N.PRTSTRT,TT_REFF   Effective date for rate                      
         ZAP   N.PRTRATE,TT_RATE   Rate                                         
         ZAP   N.PRTHOUR,TT_HRS    Hours                                        
*&&US*&& MVC   N.PRTLINE#,TT_TIME# TMS line #                                   
         TM    STPOSTNG,STNEGATE   Making negative posting                      
         JNO   EL_PRT02                                                         
         ZAP   DUB,N.PRTHOUR                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.PRTHOUR,DUB                                                    
                                                                                
EL_PRT02 DS    0H                                                               
*&&US                                                                           
         MVI   N.PRTSTAT,PRTSNOTQ  N-Time                                       
         CLI   TT_TTYP,TIMTCR      R-Time                                       
         JNE   *+8                                                              
         MVI   N.PRTSTAT,PRTSRTEQ                                               
         CLI   TT_TTYP,TIMTCB      B-Time                                       
         JNE   *+8                                                              
         MVI   N.PRTSTAT,PRTSBILQ                                               
         TM    TT_RBSTA,TIMRBADJ                                                
         JNO   *+8                                                              
         OI    N.PRTSTAT,PRTSADJ   Rate was adjusted                            
*&&                                                                             
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'44' Transaction element                                           *         
***********************************************************************         
                                                                                
EL_TRN   NTR1  LABEL=NO                                                         
N        USING TRNELD,ELEMENT                                                   
         XC    N.TRNELD(TRNLN1Q),N.TRNELD                                       
         MVI   N.TRNEL,TRNELQ                                                   
         MVC   N.TRNDATE,HD_PEDT   Set transaction date to today                
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we posting CA type buckets?              
         JZ    EL_TRN02            No                                           
         MVC   N.TRNREF,SPACES     Yes                                          
         MVC   N.TRNREF(L'BUCKMTHD),BUCKMTHD                                    
         J     EL_TRN04                                                         
*&&                                                                             
EL_TRN02 MVC   N.TRNREF,ATREF                                                   
         MVI   N.TRNTYPE,49        Type 49                                      
         CLI   TT_TTYP,TT_TMATR    If materials it's a batch type 8             
         JNE   EL_TRN04                                                         
         MVI   N.TRNTYPE,8         Type 8                                       
                                                                                
EL_TRN04 MVC   N.TRNMOS,TT_MOA     Convert YYMM -> YM ebcdic                    
         OI    N.TRNMOS,X'F0'                                                   
         LLC   R1,N.TRNMOS+1                                                    
         LHI   RF,X'F0'                                                         
         TM    N.TRNMOS+1,X'10'                                                 
         JNO   *+8                                                              
         LHI   RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,N.TRNMOS+1                                                    
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we posting CA type buckets?              
         JZ    EL_TRN06            No                                           
         ZAP   N.TRNAMNT,PZERO     Zero amount                                  
         SR    R1,R1                                                            
         J     EL_TRN10                                                         
*&&                                                                             
EL_TRN06 LLC   R1,HD_PERNO         Period number into last 4 chars              
         CVD   R1,DUB              of batch reference                           
         OI    DUB+7,X'0F'                                                      
         UNPK  N.TRNBREF(4),DUB+5(3)                                            
                                                                                
         TM    POSTSTAT,POSTSSJ    SJ posting                                   
         JZ    *+8                                                              
         OI    N.TRNSTAT,TRNSAUTH  Only authorised users                        
         ZAP   N.TRNAMNT,PZERO                                                  
         OI    N.TRNSTAT,TRNSNOCM                                               
         TM    POSTSTAT,POSTS1R+POSTSSJ+POSTS1C                                 
         JZ    *+8                                                              
         OI    N.TRNSTAT,TRNSDR    Debit posting for 1R/SJ/1C                   
         TM    POSTSTAT,POSTS1R                                                 
         JO    EL_TRN10                                                         
         CLI   TT_TTYP,TIMTCB      Only put amount in billable postings         
         JE    EL_TRN08                                                         
         CLI   TT_TTYP,TT_TMATR    and materials                                
         JNE   EL_TRN10                                                         
EL_TRN08 ZAP   N.TRNAMNT,ATAMOUNT  Amount                                       
         TM    STPOSTNG,STNEGATE   Making negative posting                      
         JNO   EL_TRN10                                                         
         ZAP   DUB,N.TRNAMNT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.TRNAMNT,DUB                                                    
                                                                                
EL_TRN10 MVC   N.TRNOFFC,TT_OFF    Client office                                
         TM    POSTSTAT,POSTSSJ                                                 
         JZ    *+10                                                             
         MVC   N.TRNOFFC,TT_TSK    SJ posting uses work code                    
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    EL_TRN12                                                         
         CLC   N.TRNOFFC,SPACES                                                 
         JH    EL_TRN12                                                         
         DC    H'0'                                                             
*                                                                               
EL_TRN12 SR    R1,R1                                                            
         ICM   R1,1,TT_NARRL                                                    
         JZ    EL_TRN14                                                         
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   N.TRNNARR(0),TT_NARR                                             
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
                                                                                
EL_TRN14 LA    RF,TRNLN1Q(R1)                                                   
         STC   RF,N.TRNLN                                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'4C' Subsidiary posting element                                    *         
***********************************************************************         
                                                                                
EL_SPD   NTR1  LABEL=NO                                                         
*&&US                                                                           
         CLI   TT_TTYP,TIMTCB      Only add for Billable time                   
         JE    *+12                                                             
         CLI   TT_TTYP,TT_TMATR    Or Materials posting                         
         JNE   EXITY               All others-skip                              
*&&                                                                             
         CLC   TT_INULA,SPACES     Don't add memo if no income                  
         JNH   EXITY                                                            
N        USING SPDELD,ELEMENT                                                   
         XC    N.SPDELD(SPDLN1Q),N.SPDELD                                       
         MVI   N.SPDEL,SPDELQ                                                   
         MVC   N.SPDACCS,TT_INULA  Income account                               
         MVI   N.SPDLN,SPDLN1Q+L'TT_INULA                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (hours)                               *         
***********************************************************************         
                                                                                
EL_SCI1  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITHOUR  Hours                                        
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we creating CA type buckets?             
         JZ    EL_SCI12            No                                           
         MVI   N.SCILN,SCILN3Q     Yes                                          
         MVI   N.SCITYPE,SCITNULL                                               
         ZAP   N.SCIAMNT,DRAMOUNT  Debit amount                                 
         ZAP   N.SCIADMN,CRAMOUNT  Credit amount                                
         MVC   N.SCISUBTY,SPACES                                                
         MVC   N.SCISUBTY(L'BUCKTYPE),BUCKTYPE                                  
         J     EL_SCI18                                                         
                                                                                
EL_SCI12 TM    POSTSTAT,POSTSSJ                                                 
         JZ    EL_SCI14                                                         
         MVI   N.SCITYPE,SCITSJHR                                               
         ZAP   N.SCIAMNT,TT_HRS    Hours                                        
         J     EL_SCI16                                                         
*&&                                                                             
EL_SCI14 ZAP   N.SCIAMNT,TT_HRS                                                 
                                                                                
EL_SCI16 TM    STPOSTNG,STNEGATE                                                
         JNO   EL_SCI18                                                         
         ZAP   DUB,N.SCIAMNT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI18 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (sales or cost rate memo)             *         
***********************************************************************         
*&&UK                                                                           
EL_SCI2  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITCRAT  Always use type C even if its not            
                                                                                
         ZAP   DUB,TT_RATE                                                      
         CLI   TT_TTYP,TIMTCB      Are we doing B - time?                       
         JE    *+12                Check ATPROF1 and take opposite              
         TM    ATPROF2,ATPR1CST    ATPROF2 for N and R time                     
         J     *+8                 Check ATPR1SAL and post as stated            
         TM    ATPROF1,ATPR1SAL    (this is OK this way round)                  
         JNO   *+10                                                             
         ZAP   DUB,TT_CRATE                                                     
         MP    DUB,TT_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
         TM    STPOSTNG,STNEGATE   Reverse amount if backing out                
         JNO   EL_SCI22                                                         
         ZAP   DUB,N.SCIAMNT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI22 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (gross dollars bucket)                *         
***********************************************************************         
                                                                                
EL_SCI3  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITGRSS  Gross amount                                 
         ZAP   N.SCIAMNT,PZERO                                                  
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (memo sales amount)                   *         
***********************************************************************         
                                                                                
EL_SCI4  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITMSRT  Memo sales rates                             
                                                                                
         ZAP   DUB,TT_RATE                                                      
         MP    DUB,TT_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
         TM    STPOSTNG,STNEGATE   Reverse amount if backing out                
         JNO   EL_SCI42                                                         
         ZAP   DUB,N.SCIAMNT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI42 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (memo cost amount)                    *         
***********************************************************************         
                                                                                
EL_SCI5  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITMCRT  Memo cost rates                              
                                                                                
         ZAP   DUB,TT_CRATE                                                     
         MP    DUB,TT_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
         TM    STPOSTNG,STNEGATE   Reverse amount if backing out                
         JNO   EL_SCI52                                                         
         ZAP   DUB,N.SCIAMNT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI52 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'59' Article transaction data element                              *         
***********************************************************************         
                                                                                
EL_ART   NTR1  LABEL=NO                                                         
         OC    TT_ITEM#,TT_ITEM#   Do we have an items element                  
         JZ    EXITY               No                                           
         OC    TT_ISEQ,TT_ISEQ     Was an item added                            
         JZ    EXITY                                                            
N        USING ARTELD,ELEMENT                                                   
         LA    R2,ELEMENT                                                       
         XC    N.ARTELD(ARTLN2Q),N.ARTELD                                       
         MVI   N.ARTEL,ARTELQ                                                   
         MVI   N.ARTLN,ARTLN2Q                                                  
         MVC   N.ARTWC,TT_TSK      work code/task code                          
         MVC   N.ARTSEQ,TT_ISEQ    sequence                                     
         MVC   N.ARTNUM,TT_INUM    Number                                       
         ZAP   N.ARTPRICE,TT_IPRCE Price                                        
         ZAP   N.ARTMULT,TT_IMULT  Multiplier                                   
         TM    STPOSTNG,STNEGATE   Making negative posting                      
         JNO   EL_ART02                                                         
         ZAP   DUB,N.ARTMULT                                                    
         MP    DUB,PNEGONE                                                      
         ZAP   N.ARTMULT,DUB                                                    
                                                                                
EL_ART02 TM    TT_IIND,TIMISNPR                                                 
         JZ    *+8                                                              
         OI    N.ARTSTAT,ARTSTXQ                                                
         TM    TT_IIND,TIMISPOV                                                 
         JZ    *+8                                                              
         OI    N.ARTSTAT,ARTSTPQ                                                
         J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'60' Transaction status element                                    *         
***********************************************************************         
                                                                                
EL_TRS   NTR1  LABEL=NO                                                         
N        USING TRSELD,ELEMENT                                                   
         XC    N.TRSEL(TRSLNQ),N.TRSELD                                         
         MVI   N.TRSEL,TRSELQ                                                   
         MVI   N.TRSLN,TRSLNQ                                                   
         MVC   N.TRSDATE,HD_TODC                                                
         MVC   N.TRSEFDT,N.TRSDATE Effective date                               
         MVC   N.TRSPMOS,TT_MOA                                                 
         MVC   N.TRSUSER,CUUSER    User id                                      
         TM    TT_IND,TIMIADJ      Time sheet adjusted                          
         JNO   *+8                                                              
         OI    N.TRSSTAT2,TRSSTADJ                                              
         TM    N.TRSSTAT2,TRSSTMSS+TRSSTADJ                                     
         JNZ   *+8                                                              
         OI    N.TRSSTAT2,TRSSTIME Regular                                      
         MVI   N.TRSSTAT3,TRSSMCS  Source is for MCS posting                    
         TM    POSTSTAT,POSTSSJ+POSTSSI+POSTS12+POSTS1C                         
         JZ    EL_TRS16                                                         
                                                                                
         TM    STPOSTNG,STNEGATE   Test creating new posting                    
         JNZ   EL_TRS04            No                                           
         TM    TR_TSSTN,TIMSFAPP   Is time fully approved?                      
         JNZ   EL_TRS16            Yes - status to set                          
         CLI   TR_TSSTN,0          Any status                                   
         JNE   EL_TRS02            Yes                                          
         MVI   N.TRSSTAT4,TRSSSAVT None must be saved                           
         J     EL_TRS16                                                         
                                                                                
EL_TRS02 TM    TR_TSSTN,TIMSMAAP   Is time line manager approved?               
         JZ    *+8                 No                                           
         OI    N.TRSSTAT4,TRSSMAAP Yes - set status accordingly                 
         TM    TR_TSSTN,TIMSREJE   Is time line manager rejecter?               
         JZ    *+8                 No                                           
*&&UK*&& MVI   N.TRSSTAT4,TRSSREJT Yes - set status accordingly                 
*&&US*&& MVI   N.TRSSTAT4,TRSSREJD Yes - set status accordingly                 
         J     EL_TRS12                                                         
                                                                                
EL_TRS04 TM    HD_TSSTO,TIMSFAPP   Is time fully approved?                      
         JNZ   EL_TRS16            Yes - status to set                          
*        TM    TR_TSSTN,TIMSDELT   Are we deleting time now?                    
*        JZ    EL_TRS06            No                                           
         OC    HD_TSSTO,HD_TSSTO   Yes - was there status before                
*        JZ    EL_TRS08                                                         
*L_TRS06 OC    TR_TSSTN,TR_TSSTN   Any status                                   
         JNZ   EL_TRS10            Yes                                          
EL_TRS08 MVI   N.TRSSTAT4,TRSSSAVT None must be saved                           
         J     EL_TRS16                                                         
                                                                                
EL_TRS10 TM    HD_TSSTO,TIMSMAAP   Is time line manager approved?               
         JZ    *+8                 No                                           
         OI    N.TRSSTAT4,TRSSMAAP Yes - set status accordingly                 
         TM    HD_TSSTO,TIMSREJE   Is time line manager rejecter?               
         JZ    *+8                 No                                           
*&&UK*&& MVI   N.TRSSTAT4,TRSSREJT Yes - set status accordingly                 
*&&US*&& MVI   N.TRSSTAT4,TRSSREJD Yes - set status accordingly                 
                                                                                
EL_TRS12 TM    TT_EPST1,TIMESAPR   Test approved                                
         JZ    *+8                                                              
         OI    N.TRSSTAT4,TRSSSJAT Set further status byte                      
         TM    TT_EPST1,TIMESREJ   Test rejected                                
         JZ    *+8                                                              
*&&UK*&& MVI   N.TRSSTAT4,TRSSREJT                                              
*&&US*&& MVI   N.TRSSTAT4,TRSSREJD                                              
         CLI   N.TRSSTAT4,0                                                     
         JNE   EL_TRS14                                                         
         MVI   N.TRSSTAT4,TRSSSUBT If neither, assume submitted                 
         J     EL_TRS16                                                         
                                                                                
EL_TRS14 TM    N.TRSSTAT4,TRSSSJAT+TRSSMAAP                                     
         JNO   EL_TRS16                                                         
         MVI   N.TRSSTAT4,0        Clear as fully approved                      
                                                                                
EL_TRS16 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'65' Analyzed office element                                       *         
***********************************************************************         
                                                                                
EL_ANO   NTR1  LABEL=NO                                                         
*&&UK*&& CLI   TT_OFF+1,C' '       One byte or two byte office code             
*&&UK*&& JE    EXITY               Only adding elem for two byte off            
N        USING ANOELD,ELEMENT                                                   
         MVI   N.ANOEL,ANOELQ      Office element                               
         MVI   N.ANOLN,ANOLNQ                                                   
         MVI   N.ANOTYPE,ANOTCLI   Client office                                
         MVC   N.ANOOFFC,TT_OFF    Client office code                           
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'7B' Source element                                                *         
***********************************************************************         
*&&UK                                                                           
EL_SOR   NTR1  LABEL=NO                                                         
         CLC   LEDGERSI,TT_INULA                                                
         JNE   EXITY               Don't add to SK posting                      
N        USING SORELD,ELEMENT                                                   
         XC    N.SORELD(SORALNQ),N.SORELD                                       
         MVI   N.SOREL,SORELQ                                                   
         MVI   N.SORLN,SORALNQ                                                  
         MVI   N.SORSYS,SORSACC    C'A' - account system                        
         MVC   N.SORAULA,TT_AULA   SJ U/L/CLI/PRD/JOB                           
         J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'7C' Unit Pricing Element                                          *         
***********************************************************************         
                                                                                
EL_UNP   NTR1  LABEL=NO                                                         
         OC    TT_ITEM#,TT_ITEM#   Do we have an items element                  
         JZ    EXITY               No                                           
         OC    TT_ISEQ,TT_ISEQ     Was an item added                            
         JZ    EXITY                                                            
N        USING UNPELD,ELEMENT                                                   
         XC    N.UNPEL(UNPLNQ),N.UNPELD                                         
         MVI   N.UNPEL,UNPELQ                                                   
         MVI   N.UNPLN,UNPLNQ                                                   
         OI    N.UNPSTAT,UNPSQTRH  Set quarter hours                            
         MVC   N.UNPSTRT,HD_TODP   Today's Date                                 
         ZAP   N.UNPRICE,TT_IPRCE  Price                                        
         ZAP   N.UNPUNIT,TT_IMULT  Multiplier                                   
                                                                                
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'8B' Time element - order number                                   *         
***********************************************************************         
                                                                                
EL_TIM1  NTR1  LABEL=NO                                                         
         CLC   TT_ORD,SPACES                                                    
         JNH   EXITY                                                            
N        USING TIMELD,ELEMENT                                                   
         XC    N.TIMELD(TIMOLNQ),N.TIMELD                                       
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIMOLNQ                                                  
         MVI   N.TIMETYP,TIMEORDR                                               
         MVC   N.TIMOIDNO,TT_TIME#                                              
         MVC   N.TIMOORDR,TT_ORD                                                
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'8B' Time element - internal reference                             *         
***********************************************************************         
                                                                                
EL_TIM2  NTR1  LABEL=NO                                                         
         CLC   TT_INTRF,SPACES                                                  
         JNH   EXITY                                                            
N        USING TIMELD,ELEMENT                                                   
         XC    N.TIMELD(TIMJLNQ),N.TIMELD                                       
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIMJLNQ                                                  
         MVI   N.TIMETYP,TIMEINRF                                               
         MVC   N.TIMJIDNO,TT_TIME#                                              
         MVC   N.TIMJINRF,TT_INTRF                                              
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'8B' Time element - estimate number (global)                       *         
***********************************************************************         
                                                                                
EL_TIM3  NTR1  LABEL=NO                                                         
         CLC   TT_EST,SPACES                                                    
         JNH   EXITY                                                            
N        USING TIMELD,ELEMENT                                                   
         XC    N.TIMELD(TIMJLNQ),N.TIMELD                                       
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIMSLNQ                                                  
         MVI   N.TIMETYP,TIMEEST                                                
         MVC   N.TIMSIDNO,TT_TIME#                                              
         MVC   N.TIMSESNM,TT_EST                                                
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'8B' Time element - time and material row number                   *         
***********************************************************************         
                                                                                
EL_TIM4  NTR1  LABEL=NO                                                         
N        USING TIMELD,ELEMENT                                                   
         XC    N.TIMELD(TIMJLNQ),N.TIMELD                                       
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIM#LNQ                                                  
         MVI   N.TIMETYP,TIMEROW#                                               
         MVC   N.TIM#IDNO,TT_TIME#                                              
         MVC   N.TIM#MID#,TT_ITEM#                                              
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'8B' Time element - timeoff id                                     *         
***********************************************************************         
                                                                                
EL_TIM5  NTR1  LABEL=NO                                                         
N        USING TIMELD,ELEMENT                                                   
         LLC   RF,TT_TOFFL                                                      
         LTR   RF,RF               Anything to add?                             
         JZ    EXITY                                                            
*                                                                               
         XC    N.TIMELD(TIMHLNQ+(TIMFIDN-TIMELD)),N.TIMELD                      
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIMHLNQ                                                  
         MVI   N.TIMETYP,TIMETOFF                                               
         MVC   N.TIMFLID,TT_TIME#                                               
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   N.TIMFIDN(0),TT_TOFFI                                            
         EX    RF,0(RE)                                                         
         AHI   RF,1+(TIMFIDN-TIMELD)                                            
         STC   RF,N.TIMLN                                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'C0' Analysis pointer element                                      *         
***********************************************************************         
                                                                                
EL_APE   NTR1  LABEL=NO,WORK=(RC,APEACTL)                                       
         USING APEACTD,RC                                                       
         LR    R3,R2                                                            
         USING TRNRECD,R3          R3=A(Transaction record)                     
                                                                                
         XC    APEACTD(APEACTSL),APEACTD                                        
         MVC   APE1RAA,HD_1RULA                                                 
         MVI   APE1RAS,APENSDR                                                  
         MVC   APEULAA,TT_CAULA                                                 
         MVI   APEULAS,APENSDR                                                  
         MVC   APEINAA,TT_INULA                                                 
         CLC   LEDGERSI,TT_INULA   Test posting to SI                           
         JNE   EL_APE02                                                         
         MVC   APE12AA,TT_12ULA                                                 
         MVC   APECAAA,TT_CAULA                                                 
         MVI   APECAAS,APENSDR                                                  
                                                                                
EL_APE02 LA    R2,ELEMENT                                                       
         USING APEELD,R2           R2=A(Element)                                
         MVI   APEEL,APEELQ                                                     
                                                                                
         LA    R4,APENTRYS                                                      
A        USING APENTRYS,R4         R4=A(Analysis account list)                  
         LHI   R0,APEACTSN         R0=Number of accounts in list                
                                                                                
         MVI   APENUM,0            Set no analysis accounts                     
         LA    R5,APENTRY                                                       
         USING APENTRY,R5          R5=A(Analysis account entry)                 
                                                                                
EL_APE04 CLC   A.APEACTA,SPACES    Test any account here                        
         JNH   EL_APE06                                                         
         CLC   A.APEACTA,TRNKULA   Is it the account?                           
         JE    EL_APE06                                                         
         CLC   A.APEACTA,TRNKULC   Is it the contra-account?                    
         JE    EL_APE06                                                         
         CLC   LEDGERSI,A.APEACTA  Is it SI posting?                            
         JNE   *+12                No - OK                                      
         CLI   TT_TTYP,TIMTCB                                                   
         JNE   EL_APE06            Don't add SI posting if not B-Time           
                                                                                
         MVC   APENSTAT,A.APEACTS  Set status                                   
         MVC   APENACT,A.APEACTA   Set account                                  
         LA    R1,APENACT+L'APENACT-1                                           
         BASR  RE,0                                                             
         CLI   0(R1),C' '          Locate end of account code                   
         JH    *+6                                                              
         BCTR  R1,RE                                                            
         AHI   R1,1                                                             
         SR    R1,R5                                                            
         STC   R1,APENLEN          Set length of entry                          
         AR    R5,R1               Point to next entry                          
         IC    R1,APENUM           Bump number of entries                       
         AHI   R1,1                                                             
         STC   R1,APENUM                                                        
                                                                                
EL_APE06 AHI   R4,APENTRYL         Bump to next account                         
         JCT   R0,EL_APE04                                                      
                                                                                
         CLI   APENUM,0            Test any entries added                       
         JE    EXITY                                                            
         SR    R5,R2                                                            
         STC   R5,APELN            Set length of element                        
         J     ADDELE                                                           
         DROP  R2,R3,R5,RC,A                                                    
                                                                                
APEACTD  DSECT                     ** Analysis account list **                  
APENTRYS DS    0X                                                               
APEACTA  DS    0XL(L'APENACT)                                                   
APE1RAA  DS    CL(L'HD_1RULA)                                                   
APEACTS  DS    0XL(L'APENSTAT)                                                  
APE1RAS  DS    X                                                                
APENTRYL EQU   *-APENTRYS                                                       
APEULAA  DS    CL(L'TT_AULA)                                                    
APEULAS  DS    X                                                                
APE12AA  DS    CL(L'TT_12ULA)                                                   
APE12AS  DS    X                                                                
APEINAA  DS    CL(L'TT_INULA)                                                   
APEINAS  DS    X                                                                
APECAAA  DS    CL(L'TT_CAULA)                                                   
APECAAS  DS    X                                                                
APEACTSL EQU   *-APEACTD                                                        
APEACTSN EQU   APEACTSL/APENTRYL                                                
APEACTL  EQU   *-APEACTD                                                        
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* X'C5' Freeform data element (to carry work code)                    *         
***********************************************************************         
                                                                                
EL_RFL   NTR1  LABEL=NO                                                         
N        USING RFLELD,ELEMENT                                                   
         XC    N.RFLELD(RFLLNQ+L'TT_TSK),N.RFLELD                               
         MVI   N.RFLEL,RFLELQ                                                   
         MVI   N.RFLLN,RFLLNQ+L'TT_TSK                                          
         MVI   N.RFLTYPE,RFLWC                                                  
         MVC   N.RFLDATA(L'TT_TSK),TT_TSK                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'D8' Person ID element                                             *         
***********************************************************************         
                                                                                
EL_PID   NTR1  LABEL=NO                                                         
         OC    HD_PPID#,HD_PPID#                                                
         JZ    EXITY                                                            
N        USING PIDELD,ELEMENT                                                   
         XC    N.PIDEL(PIDLNQ),N.PIDEL                                          
         MVI   N.PIDEL,PIDELQ                                                   
         MVI   N.PIDLN,PIDLNQ                                                   
         MVC   N.PIDNO,HD_PPID#     User's PID #                                
         J     ADDELE                                                           
         EJECT                                                                  
***********************************************************************         
* Add an element to end of record pointed to by ATRNREC               *         
***********************************************************************         
                                                                                
ADDELE   L     RE,ATRNREC          RE=A(Record)                                 
         USING TRNRECD,RE                                                       
         SR    R0,R0                                                            
         ICM   R0,3,TRNRLEN        Get current record length                    
         JNZ   *+8                                                              
         LHI   R0,TRNRFST+1-TRNRECD                                             
         LLC   RF,ELEMENT+1        RF=L'element to be added                     
         AR    R0,RF               Update record length                         
         STCM  R0,3,TRNRLEN-TRNRECD(RE)                                         
         CHI   R0,MAXRECLN                                                      
         JNH   *+6                                                              
         DC    H'0'                Record became too big                        
         SR    R0,RF                                                            
         BCTR  R0,0                Decrement for EOR                            
         AR    R0,RE               R0=A(Element insertion point)                
         LR    R1,R0                                                            
         AR    R1,RF                                                            
         MVI   0(R1),0             Set new EOR                                  
         LR    R1,RF               Set 'to' length                              
         LA    RE,ELEMENT          Set 'from' address                           
         MVCL  R0,RE               Move element to record                       
         J     EXITY                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* Build 14 contra account code and hourly rate method table           *         
***********************************************************************         
*&&UK                                                                           
BLDMTH   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDMTH*'                                                      
                                                                                
         MVC   SVMOA,TT_MOA        Save current MOA value                       
         MVI   METHSTAT,0          Initialise status flags                      
                                                                                
         CLC   HD_1RCST,SPACES     Test 1R analysis set                         
         JH    *+8                                                              
         OI    METHSTAT,METSNANL   Set no analysis found on 1R                  
                                                                                
         MVC   AT142ULA,SPACES                                                  
         MVC   AT142ULA(L'LEDGER14),LEDGER14                                    
         MVC   AT142ACT(L'HD_1RCST),HD_1RCST                                    
         LLC   RF,ONERL2L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   AT142ACT+L'HD_1RCST(0),HD_1RACT                                  
         EX    RF,0(RE)                                                         
         MVC   AT143ULA,SPACES                                                  
         MVC   AT143ULA(L'LEDGER14),LEDGER14                                    
         MVC   AT143ACT(L'HD_1RCST),HD_1RCST                                    
         LLC   RF,ONERL3L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   AT143ACT+L'HD_1RCST(0),HD_1RACT                                  
         EX    RF,0(RE)                                                         
                                                                                
         LA    R5,METHTAB          R5=A(Method table)                           
         USING MT_D,R5                                                          
         MVI   MT_METHD,MT_EOTQ                                                 
                                                                                
K        USING CAPRECD,IOKEY       Read cost allocation profile record          
         MVC   K.CAPKEY,SPACES                                                  
         MVI   K.CAPKTYP,CAPKTYPQ                                               
         MVI   K.CAPKSUB,CAPKSUBQ                                               
         MVC   K.CAPKCPY,CUXCPY    Company code                                 
         MVI   K.CAPKMTHD,C'1'     Start with method 1                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    BLDMTH06                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH04 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    BLDMTH06                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH06 CLC   K.CAPKEY(CAPKMTHD-CAPKEY),IOKEYSAV                               
         JNE   BLDMTH30            No more records                              
         CLI   MT_METHD,MT_EOTQ    Test first time                              
         JE    BLDMTH08                                                         
         CLC   MT_METHD,K.CAPKMTHD Is it the same method as before?             
         JE    BLDMTH08            Yes                                          
         AHI   R5,MT_LNQ           No - bump to next entry                      
         MVI   MT_METHD,MT_EOTQ                                                 
                                                                                
BLDMTH08 CLC   K.CAPKOFC,SPACES                                                 
         JH    *+12                                                             
         OI    METHSTAT,METSHLVL   High level method record                     
         J     BLDMTH12                                                         
         CLC   K.CAPKOFC,HD_1ROFF                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKDPT,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKDPT,HD_1RDEP                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKSDT,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKDPT,HD_1RSUB                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKPER,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKPER,HD_1RPER                                               
         JNE   BLDMTH04                                                         
BLDMTH10 NI    METHSTAT,FF-METSHLVL                                             
                                                                                
BLDMTH12 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR                                                        
         AHI   R3,CAPRFST-CAPRECD  Point R3 to first element of record          
         USING OPDELD,R3                                                        
         SR    R0,R0                                                            
         TM    METHSTAT,METSHLVL   Only find contra at high level rec           
         JZ    BLDMTH14                                                         
         MVI   MT_STR,MT_S2LVL     Structure of 14 as 2 level default           
         ZAP   MT_PCT,P1MILL       100% is the default allocation               
         MVI   MT_ANAL,C'Z'        'Z' is the standard default                  
                                                                                
BLDMTH14 CLI   OPDEL,0             Test end of record                           
         JE    BLDMTH04            Yes - get next record                        
         CLI   OPDEL,OPDELQ        Test option data element                     
         JNE   BLDMTH24                                                         
                                                                                
         CLI   OPDNUM,X'1C'        On-line update for allocation                
         JNE   BLDMTH16                                                         
         CLI   OPDDATA,C'Y'        Test on-line update                          
         JNE   BLDMTH26            No                                           
         MVC   MT_METHD,K.CAPKMTHD Save method number in table                  
         J     BLDMTH24                                                         
                                                                                
BLDMTH16 CLI   OPDNUM,X'0C'        Percentage of salary to allocate             
         JNE   BLDMTH18            No                                           
         ZAP   MT_PCT,OPDDATA(4)   Save % of salary to allocate                 
         CP    MT_PCT,PZERO        If zero not interested in this               
         JNE   BLDMTH24            method otherwise read other profiles         
         J     BLDMTH26            Get next sequential record                   
                                                                                
BLDMTH18 CLI   OPDNUM,X'0A'        Include executive in allocation opt          
         JNE   BLDMTH20            No - see if other type                       
         TM    HD_EMPST,EMPSEXEC   Is this person an executive?                 
         JZ    BLDMTH24            No - no need for test                        
         CLI   OPDDATA,C'N'        Do we include                                
         JNE   BLDMTH24            Yes - get next element                       
         J     BLDMTH26            No - clear method number                     
                                                                                
BLDMTH20 CLI   OPDNUM,X'05'        Structure of 14 account option ele           
         JNE   BLDMTH22            No - get next element                        
         CLI   OPDDATA,C'3'        If it 3 level or 2                           
         JNE   BLDMTH24            No - it's 2 as default                       
         MVI   MT_STR,MT_S3LVL     Set struture of 14 as 3 level                
         J     BLDMTH24            Branch back to read rest of elements         
                                                                                
BLDMTH22 CLI   OPDNUM,X'18'        Get default analysis code                    
         JNE   BLDMTH24            No - get next element                        
         MVC   MT_ANAL,OPDDATA     Save this default                            
                                                                                
BLDMTH24 IC    R0,OPDLN            Bump to next element                         
         AR    R3,R0                                                            
         J     BLDMTH14                                                         
                                                                                
BLDMTH26 MVI   MT_METHD,MT_EOTQ    This method is invalid                       
         J     BLDMTH04            Get next record                              
                                                                                
BLDMTH30 LA    R5,METHTAB          R5=A(Method table)                           
         CLI   MT_METHD,MT_EOTQ    Have we got method                           
         JE    BLDMTHX             No                                           
                                                                                
***********************************************************************         
* Read payroll history and payroll detail records and set method      *         
* costing rates                                                       *         
***********************************************************************         
                                                                                
K        USING PHIRECD,IOKEY       Read payroll history record                  
         MVC   K.PHIKEY,SPACES                                                  
         MVI   K.PHIKTYP,PHIKTYPQ                                               
         MVI   K.PHIKSUB,PHIKSUBQ                                               
         MVC   K.PHIKCPY,CUXCPY                                                 
         MVC   K.PHIKOFC,HD_1ROFF                                               
         MVC   K.PHIKDPT,HD_1RDEP                                               
         MVC   K.PHIKSBD,HD_1RSUB                                               
         MVC   K.PHIKPER,HD_1RPER                                               
         SR    R1,R1                                                            
         ICM   R1,3,TT_MOA                                                      
         LNR   R1,R1                                                            
         STCM  R1,3,K.PHIKMOA      MOA 2's complement                           
         XC    K.PHIKSEQ,K.PHIKSEQ Clear sequence number                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                No history must error                        
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    BLDMTH32                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH32 ZAP   MT_SRATE,PZERO      Zero accumulators                            
         ZAP   MT_BRATE,PZERO                                                   
         ZAP   MT_PRATE,PZERO                                                   
         ZAP   MT_TRATE,PZERO                                                   
                                                                                
         L     R3,IOADDR                                                        
         AHI   R3,PHIRFST-PHIRECD  R3=A(Payroll detail element)                 
         USING PDEELD,R3                                                        
BLDMTH34 CLI   PDEEL,0             Test end of record                           
         JE    BLDMTH54            Yes - next method                            
         CLI   PDEEL,PDEELQ        Payroll detail element                       
         JNE   BLDMTH52            No                                           
         CLC   TT_MOA,PDEDTE       Is pay date in same month?                   
         JNE   BLDMTH52            No - ignore                                  
         TM    PDESTAT2,PDESHRTE   Test hourly rate type                        
         JZ    BLDMTH52                                                         
                                                                                
K        USING CAHRECD,IOKEY       Read cost allocation history record          
         MVC   K.CAHKEY,SPACES                                                  
         MVI   K.CAHKTYP,CAHKTYPQ                                               
         MVI   K.CAHKSUB,CAHKSUBQ                                               
         MVC   K.CAHKCPY,CUXCPY                                                 
         MVC   K.CAHKMTHD,MT_METHD Set method                                   
         XC    K.CAHKOFC,K.CAHKOFC                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,CAHRFST-CAHRECD  R2=A(First element on record)                
         USING PATELD,R2                                                        
BLDMTH44 CLI   PATEL,0             Test end of record                           
         JE    BLDMTH52            Yes - get next payroll detail                
         CLI   PATEL,PATELQ        Payroll type element                         
         JNE   BLDMTH50            No                                           
         CLC   PDENUM,PATNUM       Is payroll number equal?                     
         JNE   BLDMTH50            Yes                                          
                                                                                
         CLI   PATTYPE,PATTSAL     Is it salary type?                           
         JNE   BLDMTH46            No check for other types                     
         AP    MT_SRATE,PDEAMT     Add up salary types for method               
         AP    MT_SRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_SRATE                                                
         J     BLDMTH52            Get next payroll detail                      
                                                                                
BLDMTH46 CLI   PATTYPE,PATTBEN     Is it benefit type?                          
         JNE   BLDMTH48            No check for other types                     
         AP    MT_BRATE,PDEAMT     Add up benefit types for method              
         AP    MT_BRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_BRATE                                                
         J     BLDMTH52            Get next payoll detail                       
                                                                                
BLDMTH48 CLI   PATTYPE,PATTPEN     Is it pension salary type?                   
         JNE   BLDMTH52            No                                           
         AP    MT_PRATE,PDEAMT     Add up pension types for method              
         AP    MT_PRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_PRATE                                                
         J     BLDMTH52            Get next payroll detail                      
                                                                                
BLDMTH50 IC    R0,PATLN            Bump to next payroll type element            
         AR    R2,R0                                                            
         J     BLDMTH44                                                         
                                                                                
BLDMTH52 IC    R0,PDELN            Bump to next payroll detail element          
         AR    R3,R0                                                            
         J     BLDMTH34                                                         
                                                                                
BLDMTH54 AHI   R5,MT_LNQ           Bump to next method                          
         CLI   MT_METHD,MT_EOTQ    Test end of table                            
         JNE   BLDMTH32            No                                           
                                                                                
BLDMTHX  J     EXITY                                                            
         DROP  K,R2,R3,R5                                                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Call DMGRITRN to put ADDTRN transaction record to output file       *         
***********************************************************************         
                                                                                
GOATRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOATRN*'                                                      
                                                                                
         LTR   R2,R1               Test last time call                          
         JZ    GOATRN04                                                         
         TM    RUNINDS,RUNIATRF    Test first time call                         
         JNZ   GOATRN02                                                         
         OI    RUNINDS,RUNIATRF    Set not first time/Put header                
         GOTOR DMGRITRN,DMCB,('FW_AAHDR',$ADDTRN)                               
                                                                                
GOATRN02 GOTOR DMGRITRN,DMCB,('FW_AATRN',$ADDTRN),(R2)                          
         J     EXITY                                                            
                                                                                
GOATRN04 TM    RUNINDS,RUNIATRF    Test any transactions put                    
         JZ    EXITY                                                            
         GOTOR DMGRITRN,DMCB,('FW_AAEND',$ADDTRN)                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Update costing personal rates record                                *         
***********************************************************************         
*&&UK                                                                           
         USING MT_D,R5             R5=A(Method table entry)                     
UPDCPR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDCPR*'                                                      
                                                                                
K        USING CPRRECD,IOKEY                                                    
         MVC   K.CPRKEY,SPACES     Build key of costing rates record            
         MVI   K.CPRKTYP,CPRKTYPQ                                               
         MVI   K.CPRKSUB,CPRKSUBQ                                               
         MVC   K.CPRKCPY,CUXCPY                                                 
         MVC   K.CPRKUNT(L'HD_1RULA),HD_1RULA                                   
                                                                                
         MVI   BYTE1,BUFPUT        Set need to put buffer entry                 
         GOTOR BUFREC,DMCB,('BUFGDR',0),(0,ACPRREC)                             
         JE    UPDCPR02                                                         
         MVI   BYTE1,BUFADD        Set need to add buffer entry                 
         TM    IOERR,IOERNF        Test record not found                        
         JNZ   *+12                Yes - must add new record                    
         TM    IOERR,IOEDEL        Test record is deleted                       
         JNZ   *+10                No                                           
         XC    IODA,IODA           Indicate new record (for BUFREC)             
                                                                                
         L     RF,ACPRREC          Build virgin record                          
         USING CPRRECD,RF                                                       
         XC    CPRRECD(256),CPRRECD                                             
         MVC   CPRKEY,IOKEYSAV                                                  
         MVC   IOKEY,IOKEYSAV                                                   
         LHI   R0,CPRRFST-CPRRECD+1                                             
         STCM  R0,3,CPRRLEN                                                     
         DROP  RF                                                               
                                                                                
N        USING PHRELD,ELEMENT      Initialise new element                       
UPDCPR02 XC    N.PHRELD(256),N.PHRELD                                           
         MVI   N.PHREL,PHRELQ                                                   
         MVI   N.PHRLN,PHRLN1Q                                                  
         MVC   N.PHRPER,TT_MOA                                                  
         MVC   N.PHRMTH,MT_METHD                                                
                                                                                
         L     RF,ACPRREC                                                       
         AHI   RF,CPRRFST-CPRRECD                                               
         USING PHRELD,RF                                                        
         SR    R0,R0                                                            
         SR    R2,R2                                                            
UPDCPR04 CLI   PHREL,0             Test end of record                           
         JE    UPDCPR10                                                         
         CLI   PHREL,PHRELQ                                                     
         JNE   UPDCPR06                                                         
         LTR   R2,R2                                                            
         JNZ   *+8                                                              
         LA    R2,PHRELD           R2=A(First PHREL on record)                  
         CLC   PHRPER(L'PHRPER+L'PHRMTH),N.PHRPER                               
         JE    UPDCPR08                                                         
UPDCPR06 IC    R0,PHRLN                                                         
         AR    RF,R0                                                            
         J     UPDCPR04                                                         
                                                                                
UPDCPR08 MVI   PHREL,FF            Delete PHDEL if found on record              
         GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',ACPRREC),0                       
                                                                                
UPDCPR10 ST    R2,FULL1            Save A(First PHREL)                          
                                                                                
         LHI   R3,MT_EBCNT         Number of hourly rate buckets                
         LA    R2,MT_TRATE         Total rate bucket                            
         LA    RE,N.PHRNTRY        Point to first sub-element                   
         USING PHRNTRY,RE                                                       
                                                                                
UPDCPR12 CHI   R3,MT_EBCNT         Is this the total hourly rate?               
         JNE   *+12                Yes - put out even if zero                   
         MVI   N.PHRTYPE,PHRTTOT                                                
         J     UPDCPR14                                                         
                                                                                
         CP    0(MT_RLNQ,R2),PZERO                                              
         JE    UPDCPR18            Ignore other zero counters                   
         STC   R3,PHRTYPE                                                       
         OI    PHRTYPE,X'F0'       Convert to EBCDIC                            
                                                                                
UPDCPR14 ZAP   DUB,0(MT_RLNQ,R2)   Hourly rate                                  
         MP    DUB,P1HUND          Decimal places                               
         CP    MT_PCT,P1MILL       If percentage 100% no amending of            
         JE    UPDCPR16            rates is necessary                           
         ZAP   PACK16,DUB                                                       
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-6,5                                                    
         ZAP   DUB,PACK16                                                       
UPDCPR16 ZAP   PHRRATE,DUB                                                      
         LLC   R0,N.PHRLN          Increment element length                     
         AHI   R0,PHRLN2Q                                                       
         STC   R0,N.PHRLN                                                       
         LLC   R0,N.PHRNUM         Increment number of sub-elements             
         AHI   R0,1                                                             
         STC   R0,N.PHRNUM                                                      
         AHI   RE,PHRLN2Q                                                       
UPDCPR18 AHI   R2,MT_RLNQ          Bump to next rate                            
         JCT   R3,UPDCPR12                                                      
         DROP  RE                                                               
                                                                                
UPDCPR20 L     RF,ACPRREC                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CPRRLEN-CPRRECD(RF)                                         
         LLC   R0,N.PHRLN          RE=L'New element to add                      
         AR    RE,R0               Add to current record length                 
         CHI   RE,MAXRECLN         Test new element fits into record            
         JNH   UPDCPR24            Yes                                          
         ICM   RF,15,FULL1         Point to oldest (first) element              
         JNZ   UPDCPR22                                                         
         DC    H'0'                There isn't one to delete                    
                                                                                
UPDCPR22 MVI   PHREL,FF            Delete this element                          
         IC    R0,PHRLN                                                         
         SR    RE,R0               Decrement length of this one                 
         CHI   RE,MAXRECLN         Does new one fit now?                        
         JNH   *+10                                                             
         AR    RF,R0               No - delete another one                      
         J     UPDCPR22                                                         
         GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',ACPRREC),0                       
                                                                                
UPDCPR24 GOTOR VHELLO,DMCB,(C'P',ACCMST),ACPRREC,N.PHRELD,0                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                Can't add new element                        
                                                                                
         GOTOR BUFREC,DMCB,(BYTE1,0),ACPRREC                                    
         J     EXITY                                                            
         DROP  R5,RF                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Update SJ/SI/12 postings approval status                            *         
***********************************************************************         
                                                                                
UPDSTA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDSTA*'                                                      
                                                                                
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,SPACES                                                  
         MVC   K.TRNKCPY,CUXCPY                                                 
         MVC   K.TRNKULA,TT_AULA   SJ account                                   
         MVC   K.TRNKWORK,TT_TSK   Task code                                    
         MVC   K.TRNKCCPY,CUXCPY                                                
         MVC   K.TRNKULC,HD_1RULA  1R account                                   
         MVC   K.TRNKDATE,HD_PEDT                                               
         MVC   K.TRNKREF,ATREF                                                  
         MVI   K.TRNKSBR,0                                                      
                                                                                
         GOTOR UPDTRS                                                           
                                                                                
         MVC   K.TRNKEY,SPACES                                                  
         MVC   K.TRNKCPY,CUXCPY                                                 
         MVC   K.TRNKULA,TT_INULA  Income account                               
         MVC   K.TRNKOFF,TT_OFF    Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   K.TRNKOFF,SPACES    Old offices = no office in key               
         MVC   K.TRNKCCPY,CUXCPY                                                
         MVC   K.TRNKULC,TT_AULA   SJ account                                   
         MVC   K.TRNKDATE,HD_PEDT                                               
         MVC   K.TRNKREF,ATREF                                                  
         MVI   K.TRNKSBR,0                                                      
                                                                                
         GOTOR UPDTRS                                                           
                                                                                
         CLC   TT_INULA(L'LEDGERSI),LEDGERSI                                    
         JNE   UPDSTAX             Posting to SI - skip for u/l=SK              
                                                                                
         MVC   K.TRNKEY,SPACES                                                  
         MVC   K.TRNKCPY,CUXCPY                                                 
         MVC   K.TRNKULA,TT_CAULA  Costing account                              
         MVC   K.TRNKOFF,TT_OFF    Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   K.TRNKOFF,SPACES    Old offices = no office in key               
         MVC   K.TRNKCCPY,CUXCPY                                                
         MVC   K.TRNKULC,TT_12ULA  Analysis account                             
         MVC   K.TRNKDATE,HD_PEDT                                               
         MVC   K.TRNKREF,ATREF                                                  
         MVI   K.TRNKSBR,0                                                      
                                                                                
         GOTOR UPDTRS                                                           
                                                                                
         MVC   K.TRNKEY,SPACES                                                  
         MVC   K.TRNKCPY,CUXCPY                                                 
         MVC   K.TRNKULA,TT_12ULA  Analysis account                             
         MVC   K.TRNKOFF,TT_OFF    Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   K.TRNKOFF,SPACES    Old offices = no office in key               
         MVC   K.TRNKCCPY,CUXCPY                                                
         MVC   K.TRNKULC,TT_CAULA  Costing account                              
         MVC   K.TRNKDATE,HD_PEDT                                               
         MVC   K.TRNKREF,ATREF                                                  
         MVI   K.TRNKSBR,0                                                      
                                                                                
         GOTOR UPDTRS                                                           
                                                                                
UPDSTAX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Update TRSSTAT4 on existing postings if necessary                   *         
***********************************************************************         
                                                                                
UPDTRS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDTRS*'                                                      
                                                                                
K        USING TRNRECD,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    UPDTRS04                                                         
         DC    H'0'                                                             
                                                                                
UPDTRS02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    UPDTRS04                                                         
         DC    H'0'                                                             
                                                                                
UPDTRS04 CLC   K.TRNKEY(TRNKSBR-TRNRECD),IOKEYSAV                               
         JNE   UPDTRSX                                                          
         TM    K.TRNKSTAT,TRNSREVS Not interested in reversals                  
         JNZ   UPDTRS02                                                         
         LHI   R1,IOGET+IOMST+IO1  get record from master                       
*&&UK                                                                           
         TM    K.TRNKSTAT,TRNSARCH Archive transaction ?                        
         JZ    UPDTRS05            no, continue                                 
         LHI   R1,IOGET+IOARC+IO1  yes, get record from Archive file            
*&&                                                                             
UPDTRS05 GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,IOADDR                                                        
         CLC   TRNKULC-TRNRECD(L'TRNKULC,R1),HD_1RULA  1R account               
         JE    UPDTRS40                                                         
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING TIMELD,R1                                                        
UPDTRS06 CLI   TIMEL,0             End of record                                
         JE    UPDTRS10            Couldn't find element - try others           
         CLI   TIMEL,TIMELQ        Time element                                 
         JNE   UPDTRS08                                                         
         CLI   TIMETYP,TIMEROW#    Check type is correct                        
         JNE   UPDTRS08                                                         
         CLC   TT_TIME#,TIM#IDNO   Does the time row match                      
         JNE   UPDTRS02            No - not the correct posting                 
         CLC   TT_ITEM#,TIM#MID#   Does the material row match                  
         JNE   UPDTRS02            No - not the correct posting                 
         J     UPDTRS40            Yes - update this posting                    
                                                                                
UPDTRS08 LLC   R0,TIMLN            Bump to next element on record               
         AR    R1,R0                                                            
         J     UPDTRS06                                                         
                                                                                
UPDTRS10 L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
         USING APEELD,R1                                                        
UPDTRS12 CLI   APEEL,0             End of record                                
         JE    UPDTRS02            Couldn't find element - try others           
         CLI   APEEL,APEELQ        Time element                                 
         JE    UPDTRS16                                                         
UPDTRS14 LLC   R0,APELN            Bump to next element on record               
         AR    R1,R0                                                            
         J     UPDTRS12                                                         
                                                                                
UPDTRS16 LLC   RF,APENUM                                                        
         LA    R2,APENTRY                                                       
T        USING APENTRY,R2                                                       
UPDTRS18 LLC   R3,T.APENLEN                                                     
         SHI   R3,L'APENLEN+L'APENSTAT+1                                        
         BASR  RE,0                                                             
         CLC   T.APENACT(0),HD_1RULA                                            
         EX    R3,0(RE)                                                         
         JE    UPDTRS40                                                         
         LLC   R3,T.APENLEN                                                     
         AR    R2,R3                                                            
         JCT   RF,UPDTRS18                                                      
         J     UPDTRS14                                                         
                                                                                
UPDTRS40 GOTOR GETELA,TRSELQ       Locate status element                        
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TRSELD,R1                                                        
         CLC   TRSSTAT4,ATTRSST4   Test status changed                          
         JE    UPDTRS02            No                                           
         MVC   TRSSTAT4,ATTRSST4   Set new status and write back                
*                                                                               
         LHI   R1,IOPUTREC+IOMST+IO1 update master record                       
*&&UK                                                                           
         TM    K.TRNKSTAT,TRNSARCH Archive transaction ?                        
         JZ    UPDTRS60            no, continue                                 
         LHI   R1,IOADFREC+IOMST+IO1 yes, update archive record                 
*&&                                                                             
UPDTRS60 GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPDTRS02                                                         
         DC    H'0'                Died writing a record                        
                                                                                
UPDTRSX  J     EXIT                                                             
         DROP  R1,K                                                             
         EJECT                                                                  
***********************************************************************         
* Time sheet upload header record                                     *         
***********************************************************************         
                                                                                
HDRREC   LKREQ H,A#THDR,NEWREC=Y                                                
                                                                                
HDRPER   LKREQ F,01,(D,B#SAVED,QH_1RPER),CHAR,TEXT=AC#PRSN                      
HDRPEDt  LKREQ F,02,(D,B#SAVED,QH_PEDT),PDAT,TEXT=AC#RSPED                      
HDRDate  LKREQ F,03,(I,B#SAVED,QH_DATI),PDAT,TEXT=AC#DATE,             +        
               LIST=(F,NOSORT),OLEN=L'TL_ETDT1                                  
HDRType  LKREQ F,04,(D,B#SAVED,QH_UTYP),CHAR,TEXT=AC#ACT                        
HDRStat  LKREQ F,05,(D,B#SAVED,QH_STAT),CHAR,TEXT=AC#STT                        
HDRBapr  LKREQ F,06,(D,B#SAVED,QH_BUAPR),CHAR,TEXT=AC#BCAPP                     
HDRRcom  LKREQ F,07,(D,B#SAVED,QH_COMSL),(R,VALTXT),TEXT=AC#RJCT,      +        
               OLEN=L'QH_COMSL+L'QH_COMS,LOWERCASE=Y                            
PCToken  LKREQ F,08,(D,B#SAVED,QH_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
Tempflg  LKREQ F,09,(D,B#SAVED,QH_TEMPL),CHAR,TEXT=AC#TMPLT                     
                                                                                
HDRVals  LKREQ F,64,(I,B#SAVED,QH_AHEAD),CHAR,TEXT=(*,HEADLIT),OLEN=1           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Time sheet upload time line record                                  *         
***********************************************************************         
                                                                                
TSLREC   LKREQ H,A#TTIM,NEWREC=Y                                                
                                                                                
PCToken  LKREQ F,01,(D,B#SAVED,QX_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
TIMHours LKREQ F,02,(I,B#SAVED,QT_HRSI),SPAK,TEXT=AC#HOURS,            +        
               LIST=(F,NOSORT),OLEN=L'TL_EHRS1                                  
TIMType  LKREQ F,03,(D,B#SAVED,QT_TTYPE),CHAR,TEXT=AC#TYPE                      
TIMRow#  LKREQ F,04,(D,B#SAVED,QT_TIME#),LBIN,TEXT=AC#ROW                       
TIM1NAct LKREQ F,05,(D,B#SAVED,QT_1NACT),CHAR,TEXT=AC#1N                        
TIMCli   LKREQ F,06,(D,B#SAVED,QT_SJCLI),CHAR,TEXT=AC#CLIC                      
TIMPro   LKREQ F,07,(D,B#SAVED,QT_SJPRO),CHAR,TEXT=AC#PROC                      
TIMJob   LKREQ F,08,(D,B#SAVED,QT_SJJOB),CHAR,TEXT=AC#JOBC                      
TIMTask  LKREQ F,09,(D,B#SAVED,QT_TASK),CHAR,TEXT=AC#WC                         
TIMNarr  LKREQ F,10,(I,B#SAVED,QX_DESCI),(R,VALTXT),OLEN=L'TIMNARR+1,  +        
               TEXT=AC#NRTV,LOWERCASE=Y,DELIM=X'FF'                             
TIMChng? LKREQ F,11,(D,B#SAVED,QT_AMEND),CHAR,TEXT=AC#CHGD                      
TIMOrd   LKREQ F,12,(D,B#SAVED,QT_ORDER),CHAR,TEXT=AC#ORNUM                     
TIMInref LKREQ F,13,(D,B#SAVED,QT_INTRF),CHAR,TEXT=AC#INTRF                     
TIMEst   LKREQ F,14,(D,B#SAVED,QT_EST#),CHAR,TEXT=AC#EST                        
Time     LKREQ F,15,(I,B#SAVED,QT_TOFFI),(R,VALTXT),OLEN=L'TIMFIDN+1,  +        
               CHAR,TEXT=AC#TOFFI,DELIM=X'FF'                                   
                                                                                
TSARRec  LKREQ F,64,(I,B#SAVED,QX_TSAR),CHAR,TEXT=(*,TSARLIT),OLEN=1            
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Time sheet upload materials line record                             *         
***********************************************************************         
                                                                                
MATREC   LKREQ H,A#TMAT,NEWREC=Y                                                
                                                                                
PCToken  LKREQ F,01,(D,B#SAVED,QX_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
ITMCode  LKREQ F,02,(D,B#SAVED,QI_ITEMC),CHAR,TEXT=AC#ITMC                      
ITMSeq   LKREQ F,03,(D,B#SAVED,QI_ITEMS),HEXD,TEXT=AC#ITMS                      
ITMRow#  LKREQ F,04,(D,B#SAVED,QI_ITEM#),LBIN,TEXT=AC#ITMR                      
ITMPrice LKREQ F,05,(D,B#SAVED,QI_ITEMP),SPAK,TEXT=AC#ITMP                      
ITMMulti LKREQ F,06,(D,B#SAVED,QI_ITEMX),SPAK,TEXT=AC#ITMM                      
ITMTotal LKREQ F,07,(D,B#SAVED,QI_ITEMT),SPAK,TEXT=AC#ITMT                      
ITMOride LKREQ F,08,(D,B#SAVED,QI_ITEMO),CHAR,TEXT=AC#ITMO                      
ITMNarr  LKREQ F,09,(I,B#SAVED,QX_DESCI),(R,VALTXT),OLEN=L'TIMNARR+1,  +        
               TEXT=AC#NRTV,LOWERCASE=Y,DELIM=X'FF'                             
                                                                                
TSARRec  LKREQ F,64,(I,B#SAVED,QX_TSAR),CHAR,TEXT=(*,TSARLIT),OLEN=1            
                                                                                
         LKREQ E                                                                
***********************************************************************         
* Day narrative record                                                *         
***********************************************************************         
                                                                                
DNAREC   LKREQ H,A#TDNR,NEWREC=Y                                                
PCToken  LKREQ F,01,(D,B#SAVED,QX_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
Date     LKREQ F,02,(D,B#SAVED,QD_DATE),PDAT,TEXT=AC#DATE                       
Changed  LKREQ F,03,(D,B#SAVED,QT_AMEND),CHAR,TEXT=AC#CHGD                      
Row      LKREQ F,04,(D,B#SAVED,QD_TIME#),LBIN,TEXT=AC#ROW                       
Sequence LKREQ F,05,(D,B#SAVED,QD_SEQ#),LBIN,TEXT=AC#ITMS                       
TIMDnarr LKREQ F,06,(I,B#SAVED,QX_DESCI),(R,VALTXT),OLEN=L'TIMNARR+1,  +        
               TEXT=AC#NRTV,LOWERCASE=Y,DELIM=X'FF'                             
                                                                                
TSARRec  LKREQ F,64,(I,B#SAVED,QX_TSAR),CHAR,TEXT=(*,TSARLIT),OLEN=1            
                                                                                
         LKREQ E                                                                
***********************************************************************         
* Time sheet upload trailer record                                    *         
***********************************************************************         
                                                                                
TRLREC   LKREQ H,A#TTRL,NEWREC=Y                                                
                                                                                
TRLIndex LKREQ F,01,(D,B#SAVED,QT_INDEX),HEXD,TEXT=AC#SPCNU                     
                                                                                
TRLVals  LKREQ F,64,(I,B#SAVED,QT_ATRLR),CHAR,TEXT=(*,TRLRLIT),OLEN=1           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* End of request map tables                                           *         
***********************************************************************         
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
* Validate rejection comment/time or materials narrative              *         
***********************************************************************         
                                                                                
VALTXT   LM    R2,R4,LP_AINP-LP_D(R5)                                           
         STC   R3,0(R4)            Returns L'Text,Text string                   
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   1(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
                                                                                
         DROP  RA                  Drop GLOBALS                                 
         EJECT                                                                  
***********************************************************************         
* Data Manager routines                                               *         
*                                                                     *         
* DMGRITRN will be entered for any call to VDATAMGR (internal calls)  *         
* DMGRXTRN will be entered for any call to CDATAMGR (external calls)  *         
***********************************************************************         
                                                                                
DMGRITRN SR    RF,RF               Internal entry point - clear RF              
                                                                                
DMGRXTRN NTR1  LABEL=NO,WORK=(RC,DMWORKL)                                       
         J     *+12                                                             
         DC    C'*BODMGR*'                                                      
                                                                                
         USING DMWORKD,RC          RC=A(Local w/s)                              
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         L     R5,LLP                                                           
         USING LP_D,R5             R5=A(LP_D)                                   
         LM    R2,RB,LP_R2RB                                                    
         USING WORKD,R9            R9=A(WORKD)                                  
         USING SAVED,R8            R8=A(SAVED)                                  
         USING CPYELD,SCPYEL                                                    
         ST    R1,DMGRSVR1         Save A(Caller's parameter list)              
         MVC   DMGRDMCB,0(R1)      Save caller's DMCB                           
         MVI   DMGRERRS,0          Set no errors                                
                                                                                
         MVI   DMGRFLAG,DMGRFEXT   Set external I/O call                        
         LTR   RF,RF               Test called internally                       
         JNZ   *+8                                                              
         MVI   DMGRFLAG,DMGRFINT   Set internal I/O call                        
                                                                                
         LM    R3,R4,DMGRP1        R3=A(Command), R4=A(File)                    
                                                                                
         CLC   $ADDTRN,0(R3)       Test ADDTRN handler record                   
         JE    ADDTRN                                                           
*&&UK*&& CLC   $PLDREC,0(R3)       Test PLDREC handler record                   
*&&UK*&& JE    PLDREC                                                           
         CLC   $TIMREC,0(R3)       Test TIMREC handler record                   
         JE    TIMREC                                                           
         CLC   $LOKREC,0(R3)       Test LOKREC handler record                   
         JE    LOKREC                                                           
                                                                                
         CLC   ACCOUNT,0(R4)       Only interested in Account files             
         JNE   DMGRREAL                                                         
                                                                                
         CLC   DMPUTF,0(R3)        Test updative I/O                            
         JE    DMGR0010                                                         
         CLC   DMWRTD,0(R3)                                                     
         JE    DMGR0010                                                         
         CLC   DMADDD,0(R3)                                                     
         JE    DMGR0010                                                         
*&&UK                                                                           
         CLC   DMADFF,0(R3)                                                     
         JE    DMGR0010                                                         
*&&                                                                             
         CLC   DMADDF,0(R3)                                                     
         JNE   DMGRREAL            No - issue non-updative I/O                  
                                                                                
DMGR0010 LHI   R0,FW_FDIR                                                       
         CLC   ACCDIR,0(R4)        Test ACCDIR I/O                              
         JE    DMGR0020                                                         
         LHI   R0,FW_FMST                                                       
         CLC   ACCMST,0(R4)        Test ACCMST I/O                              
         JE    DMGR0030                                                         
         LHI   R0,FW_FARC                                                       
         CLC   ACCARC,0(R4)        Test ACCARC I/O                              
         JE    DMGR0030                                                         
         DC    H'0'                Disallow other files                         
                                                                                
DMGR0020 L     R2,DMGRP4           ACCDIR updative I/O                          
         MVC   DMGRKEY,0(R2)                                                    
         SHI   R2,FW_DIROL         Back up by overhead length                   
         USING FW_D,R2             Build directory record                       
         MVC   DMGRSAVE(FW_DIRHL),0(R2)                                         
         XC    FW_D(FW_DIRHL),FW_D                                              
         STC   R0,FW_FILE          Set file number                              
         LHI   RF,FW_DIRRL                                                      
         STCM  RF,3,FW_RLEN        Set length of record                         
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         MVI   FW_ACT,FW_AADDD     Set action to DMADD                          
         CLC   DMADDD,0(R3)        Test DMADD                                   
         JE    *+8                                                              
         MVI   FW_ACT,FW_AWRTD     No - set action to DMWRITE                   
         GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_DIRHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGR0030 L     R2,DMGRP3           ACCMST/ACCARC updative I/O                   
         MVC   DMGRDA,0(R2)        Set disk address                             
         L     R2,DMGRP4           Point to I/O area                            
         MVC   DMGRKEY,0(R2)       Save record key                              
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R2)                                         
         AHI   RF,FW_RECOL         Add on length of overhead                    
         SHI   R2,FW_RECOL         Back up by overhead length                   
         USING FW_D,R2             Build file record                            
         MVC   DMGRSAVE(FW_RECHL),0(R2)                                         
         XC    FW_D(FW_RECHL),FW_D                                              
         STC   R0,FW_FILE          Set file number                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         MVI   FW_ACT,FW_APUTR     Set action to PUTREC                         
         MVC   FW_RDA,DMGRDA       Set disk address                             
         CLC   DMPUTF,0(R3)        Test PUTREC                                  
         JE    DMGR0032            yes, continue                                
*&&UK                                                                           
         MVI   FW_ACT,FW_AADFR     Set action to ADDREC - ARCHIVE               
         CLC   DMADFF,0(R3)        Test ADDREC - ARCHIVE ?                      
         JE    DMGR0032            yes, continue                                
*&&                                                                             
         MVI   FW_ACT,FW_AADDR     No - set action to ADDREC                    
         XC    FW_RDA,FW_RDA       and clear disk address                       
         CLI   FW_RKEY+CPRKTYP-CPRRECD,CPRKTYPQ  COSTING PERSONAL RATE?         
         JNE   DMGR0032            NO, CONTINUE                                 
*                                                                               
         CLI   FW_RKEY+CPRKSUB-CPRRECD,CPRKSUBQ  SUB-RECORD TYPE=07 ?           
         JNE   DMGR0032            NO, CONTINUE                                 
         MVI   FW_RTYP,ACRTCPR     YES, SET COSTING PERSONAL RATES REC          
*                                                                               
DMGR0032 GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_RECHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGRREAL GOTOR DATAMGR,DMGRDMCB    Call real DATAMGR                            
                                                                                
DMGREXIT L     R1,DMGRSVR1         Restore caller's parameter list              
         MVC   0(DMGRPL,R1),DMGRDMCB                                            
         CLI   DMGRERRS,0          Set condition code for caller                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Handle special $ADDTRN actions                                      *         
***********************************************************************         
                                                                                
ADDTRN   CLI   DMGRACTN,FW_AAHDR   Test ADDTRN header                           
         JNE   ADDTRN02                                                         
         LA    R2,DMGRSAVE                                                      
         XC    FW_D(FW_HLNQ),FW_D                                               
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AAHDR     Set action                                   
         LHI   RF,FW_HLNQ                                                       
         STCM  RF,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put ADDTRN header record                     
         MVC   FW_D(FW_HLNQ),DMGRSAVE                                           
         J     DMGREXIT                                                         
                                                                                
ADDTRN02 CLI   DMGRACTN,FW_AATRN   Test ADDTRN transaction record               
         JNE   ADDTRN04                                                         
         L     R2,ATRNREC                                                       
         MVC   DMGRKEY,0(R2)       Save transaction record key                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R2)                                         
         JNZ   *+6                                                              
         DC    H'0'                Record length can't be zero                  
         CHI   RF,MAXRECLN                                                      
         JNH   *+6                                                              
         DC    H'0'                Nor greater than the maximum                 
         AHI   RF,FW_ATOLQ         Add on overhead length                       
         SHI   R2,FW_ATOLQ         Back-up by overhead length                   
         MVC   DMGRSAVE(FW_ATHLQ),0(R2)                                         
         XC    FW_D(FW_ATHLQ),FW_D                                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AATRN     Set action                                   
         MVC   FW_RKEY,DMGRKEY     Set transaction key                          
         TM    STPOSTNG,STBUCKET   Test posting buckets only                    
         JZ    *+8                                                              
         MVI   FW_ATTI2,TRNIUBKO+TRNIUOFC                                       
* Commenting this out as TT_D always set to old so TT_MOA is for old            
* MOA see LP_R2RB                                                               
*        MVC   FW_ATMOA,TT_MOA     Set month of activity                        
         L     R1,DMGRP2                                                        
         MVC   FW_ATCAN,0(R1)      Set contra-account name                      
         GOTOR PUTOUT,FW_D         Put ADDTRN transaction record                
         MVC   FW_D(FW_ATHLQ),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
ADDTRN04 CLI   DMGRACTN,FW_AAEND   Test ADDTRN end record                       
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DMGRSAVE                                                      
         XC    FW_D(FW_HLNQ),FW_D                                               
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AAEND     Set action                                   
         MVI   FW_RKEY,FF          Set key to high values                       
         MVC   FW_RKEY+1(L'FW_RKEY-1),FW_RKEY                                   
         LHI   RF,FW_HLNQ                                                       
         STCM  RF,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put ADDTRN end record                        
         J     DMGREXIT                                                         
         EJECT                                                                  
***********************************************************************         
* Handle special $PLDREC actions                                      *         
***********************************************************************         
*&&UK                                                                           
PLDREC   L     R2,DMGRP2           Point to block/record                        
         MVC   DMGRKEY,0(R2)                                                    
         SHI   R2,FW_DIROL         Back up by header length                     
         MVC   DMGRSAVE(FW_DIROL),0(R2)                                         
         USING FW_D,R2             Build directory record                       
         XC    FW_D(FW_DIROL),FW_D                                              
         MVI   FW_FILE,FW_FPLD$    Set use PLDREC handler                       
         LHI   RF,FW_DIRRL                                                      
         STCM  RF,3,FW_RLEN        Set length of record                         
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_DIROL),DMGRSAVE                                          
         J     DMGREXIT                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Handle special $TIMREC actions                                      *         
***********************************************************************         
                                                                                
TIMREC   L     R2,ATIMREC          Point to build area                          
         XC    FW_D(FW_DIRRL),FW_D                                              
         MVI   FW_FILE,FW_FTIM$    Set file                                     
         MVC   FW_TRREC,DMGRACTN   Set record type                              
         MVC   FW_TRULA,HD_1RULA   Set person account                           
         MVC   FW_TRPED,HD_PEDT    Set period end date                          
                                                                                
         CLI   DMGRACTN,FW_TRHDR   Test TIMREC header record                    
         JNE   TIMREC02                                                         
         MVC   FW_TRPER,HD_1RPER   Set person code                              
         MVC   FW_TRSTO,HD_TSSTO   Set old time sheet status                    
         MVC   FW_TRSTN,TR_TSSTN   Set new time sheet status                    
         MVC   FW_TRPID,HD_PPID#   Set users PID                                
         MVC   FW_TRLDT,HD_SUBLM   Set submitted date - line manager            
         MVC   FW_TRCDT,HD_SUBCL   Set submitted date - client manager          
         ZAP   FW_TREDT,TR_EDHRS   Set edit hours                               
         MVC   FW_HDIND,HD_IND     Set header status                            
         LHI   R0,FW_DIRRL                                                      
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put header record to output file             
         J     DMGREXIT                                                         
                                                                                
TIMREC02 CLI   FW_TRREC,FW_TRNEW   Test TIMREC new buffer record                
         JE    *+12                                                             
         CLI   FW_TRREC,FW_TROLD   Test TIMREC old buffer record                
         JNE   TIMREC04                                                         
         LA    R0,FW_TRDTT         Copy TT_D to FW_TRDTT                        
         LHI   R1,TT_LN1Q                                                       
         LA    RE,TT_D                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LHI   R0,FW_TRDTT-FW_D+TT_LN1Q                                         
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put buffer record to output file             
         J     DMGREXIT                                                         
                                                                                
TIMREC04 CLI   DMGRACTN,FW_TRSTA   Test TIMREC status change record             
         JE    *+12                                                             
         CLI   DMGRACTN,FW_TRDEL   Test TIMREC delete record                    
         JNE   TIMREC06                                                         
         LHI   R0,FW_DIRRL                                                      
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put record to output file                    
         J     DMGREXIT                                                         
                                                                                
TIMREC06 CLI   DMGRACTN,FW_TREND   Test TIMREC end record                       
         JE    *+6                                                              
         DC    H'0'                                                             
         LHI   R0,FW_DIRRL                                                      
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put end record to output file                
         J     DMGREXIT                                                         
         EJECT                                                                  
***********************************************************************         
* Handle special $LOKREC actions                                      *         
***********************************************************************         
                                                                                
LOKREC   LA    R2,DMGRSAVE         Point to build area                          
         XC    FW_D(FW_DIRRL),FW_D                                              
         MVI   FW_FILE,FW_FLOK$    Set file                                     
         L     R1,DMGRP2           Point to lock key                            
         MVC   FW_LKKEY,0(R1)      Set LOCKET key to be unlocked                
         LHI   R0,FW_LKRL                                                       
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put LOCKET record to output file             
         J     DMGREXIT                                                         
         DROP  R2,RC                                                            
                                                                                
DMWORKD  DSECT                     ** DMGRXTRN s/r local w/s **                 
DMGRSVR1 DS    A                   A(Caller's parameter list)                   
                                                                                
DMGRDMCB DS    0XL(DMGRPL)         ** Caller's parameter list **                
DMGRACTN DS    0X                  Action code (special records)                
DMGRP1   DS    A                   A(Action)                                    
DMGRP2   DS    A                   A(File)                                      
DMGRERRS DS    0X                  Error return byte                            
DMGRP3   DS    A                   A(Disk address)                              
DMGRP4   DS    A                   A(I/O area)                                  
DMGRP5   DS    A                   A(DMWORK area)                               
DMGRP6   DS    A                   N/D                                          
DMGRPL   EQU   *-DMGRP1                                                         
                                                                                
DMGRFLAG DS    X                   ** DMGRXTRN flag byte **                     
DMGRFINT EQU   X'80'               Internal (VDATAMGR) call                     
DMGRFEXT EQU   X'40'               External (CDATAMGR) call                     
                                                                                
DMGRDA   DS    XL(L'IODA)          Disk address                                 
DMGRKEY  DS    XL(L'ACCKEY)        Key                                          
DMGRSAVE DS    XL128               Save area                                    
DMWORKL  EQU   *-DMWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Put record to FACWRK recovery                                       *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTOUT*'                                                      
                                                                                
         LR    R2,R1                                                            
         USING FW_D,R2             R2=A(Record to put)                          
                                                                                
         TM    LP_FLAG,LP_FDRFT    Test 'draft' (validation) mode               
         JZ    *+6                 No                                           
         DC    H'0'                Updative I/O in draft mode                   
                                                                                
         TM    RUNINDS,RUNIPUTF    Test first time call                         
         JNZ   PUTOUT06                                                         
         OI    RUNINDS,RUNIPUTF    Set not first time call                      
         XC    DMGRSEQ#,DMGRSEQ#   Initialise sequence number                   
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT02            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTCARD,SORTTYPE                                   
                                                                                
PUTOUT02 L     R3,WRKBLKR                                                       
         USING WRKIOD,R3           R3=A(FACWRK WRKIO block)                     
         L     R4,WRKIAREC                                                      
H        USING FW_D,R4             Build FACWRK header record                   
         LA    R0,H.FW_D                                                        
         LHI   R1,FW_HDRL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LHI   R0,FW_HDRL                                                       
         STCM  R0,3,H.FW_RLEN                                                   
         MVC   H.FW_HSTMP,FWHSTAMP Set SRUPDxx file stamp                       
         MVI   H.FW_HOLAY,FW_OBOTU Set to call SRUPD60 in FACPAK                
         MVC   H.FW_HSENA,LP_SENO  Set SE number                                
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    *+8                                                              
         OI    H.FW_FLAG,FW_FOFFL  Set running off-line                         
         TM    LP_OFLG1,LP_OFKFW   Test don't purge =FWK file                   
         JZ    *+8                                                              
         OI    H.FW_FLAG,FW_FKFWK  Set don't purge =FWK file                    
         MVC   H.FW_AGY,LP_AGY     Set agency alpha ID                          
         MVC   H.FW_USER,LP_USRID  Set user-ID                                  
         MVC   H.FW_CPY,CUXCPY     Set company code                             
         MVC   H.FW_CPYS1,CPYSTAT1 Set company status bytes                     
         MVC   H.FW_CPYS2,CPYSTAT2                                              
         MVC   H.FW_CPYS3,CPYSTAT3                                              
         MVC   H.FW_CPYS4,CPYSTAT4                                              
         MVC   H.FW_CPYS5,CPYSTAT5                                              
         MVC   H.FW_CPYS6,CPYSTAT6                                              
         MVC   H.FW_CPYS7,CPYSTAT7                                              
         MVC   H.FW_CPYS8,CPYSTAT8                                              
         MVC   H.FW_CPYS9,CPYSTAT9                                              
         MVC   H.FW_CPYSA,CPYSTATA                                              
*&&UK*&& MVC   H.FW_CPYSB,CPYSTATB                                              
*&&UK*&& MVC   H.FW_CPYSC,CPYSTATC                                              
         MVC   H.FW_CPYGL,CPYGLMOA GL MOA                                       
         MVC   H.FW_CPYSF,CPYSFST  Fiscal Start Date                            
         MVC   H.FW_PERLV,ONERL1L  Set 1R ledger levels                         
         MVC   H.FW_SJALV,PCLILEN  Set SJ ledger levels                         
         MVI   H.FW_BOVLY,FW_BTIME Set time module calling                      
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT04            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTPUT,H.FW_RLEN                                   
         J     PUTOUT06                                                         
                                                                                
PUTOUT04 GOTOR DATAMGR,WRKIPARM,ADD                                             
         JE    PUTOUT06                                                         
         DC    H'0'                                                             
         DROP  R3,H                                                             
                                                                                
PUTOUT06 TM    FW_FILE,FW_DMGR     Test normal DATAMGR I/O                      
         JNO   PUTOUT08            No                                           
         ICM   R0,15,DMGRSEQ#      Bump record sequence number                  
         AHI   R0,1                                                             
         STCM  R0,15,DMGRSEQ#                                                   
         STCM  R0,15,FW_SEQ#       Set sequence number in record                
                                                                                
PUTOUT08 TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT10            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTPUT,FW_D                                        
         J     EXITY                                                            
                                                                                
PUTOUT10 L     R1,WRKBLKR          Put record to FACWRK recovery file           
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,ADD,,,FW_D                                          
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Lock the PID for the duration of the upload.  Exit with CC=equal if *         
* locked this time or previously by me.  Exit with CC=not equal if    *         
* there is a lock failure                                             *         
***********************************************************************         
                                                                                
DOLOCK   NTR1  LABEL=NO                                                         
         USING LLKKEYD,WORK                                                     
                                                                                
         LA    R2,PIDLOCK          Point to PID lock table                      
         XR    R1,R1                                                            
DOLOCK02 OC    0(L'PIDLOCK,R2),0(R2)                                            
         JZ    DOLOCK04            Add new entry if not already locked          
         CLC   HD_PPID#(L'HD_PPID#+L'HD_PEDT),0(R2)                             
         JE    EXITY                                                            
         AHI   R2,L'PIDLOCK        Bump to next entry                           
         AHI   R1,1                                                             
         J     DOLOCK02                                                         
                                                                                
DOLOCK04 MVC   0(L'PIDLOCK,R2),HD_PPID#                                         
         CHI   R1,PIDMAXQ          Check we haven't exceeded table              
         JNH   DOLOCK06            No - fine                                    
         MVC   ROUERRV,=AL2(AE$TMIOU) Error too many items for update           
         J     EXITN                                                            
                                                                                
DOLOCK06 GOTOR BLDLOK,HD_PPID#     Build LOCKET key                             
         GOTOR LOCKET,DMCB,('LLKTESTQ',LLKKEYD),ACOMFACS                        
         JE    DOLOCK08                                                         
         XC    0(L'PIDLOCK,R2),0(R2)                                            
         MVC   ROUERRV,=AL2(AE$P#LOK)                                           
         J     EXITN                                                            
                                                                                
                                                                                
DOLOCK08 GOTOR LOCKET,DMCB,('LLKLOCKQ',LLKKEYD),ACOMFACS                        
         JE    EXIT                                                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Build a LOCKET key - R1=A(PID#)                                     *         
***********************************************************************         
                                                                                
BLDLOK   NTR1  LABEL=NO            Build LOCKET key                             
         LR    R3,R1               Point to PID                                 
         L     R5,LLP                                                           
         MVC   LLOCKSE,LP_SENO                                                  
         MVC   LLOCKAGY,LP_AGY                                                  
         MVC   LLOCKRTY,=C'P#'                                                  
         MVC   LLOCKKEY,SPACES                                                  
         GOTOR VDATCON,DMCB,(1,L'HD_PPID#(R3)),(0,LLOCKKEY+2*L'HD_PPID#+        
               )                                                                
         GOTOR VHEXOUT,DMCB,(R3),LLOCKKEY,L'HD_PPID#,0                          
         OC    LLOCKKEY,SPACES                                                  
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Lock the PID for the duration of the upload.  Exit with CC=equal if *         
* locked this time or previously by me.  Exit with CC=not equal if    *         
* there is a lock failure                                             *         
***********************************************************************         
                                                                                
UNLOCK   NTR1  LABEL=NO                                                         
                                                                                
ULOCK02  LA    R2,PIDLOCK          Unlock everything I locked so far            
ULOCK04  OC    0(L'PIDLOCK,R2),PIDLOCK                                          
         JZ    EXITY                                                            
         GOTOR BLDLOK,(R2)         Build LOCKET key                             
         LHI   R0,LLKUNLKQ         On-line unlock action                        
         TM    LP_FLAG,LP_FOFFL                                                 
         JZ    *+8                                                              
         LHI   R0,LLKUNGLQ         Off-line - running as global                 
         GOTOR LOCKET,DMCB,((R0),LLKKEYD),ACOMFACS                              
         JE    *+6                                                              
         DC    H'0'                Failure to unlock                            
         XC    0(L'PIDLOCK,R2),0(R2)                                            
         AHI   R2,L'PIDLOCK                                                     
         J     ULOCK04                                                          
         EJECT                                                                  
***********************************************************************         
* Update the timesheet template record on file                        *         
***********************************************************************         
         USING TTMRECD,R2                                                       
UPDTPL   NTR1  LABEL=NO                                                         
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVI   TTMKTYP,TTMKTYPQ    Read for time template record                
         MVI   TTMKSUB,TTMKSUBQ                                                 
         MVC   TTMKCPY,CUXCPY                                                   
         MVC   TTMK1RAC,HD_1RACT                                                
         XC    BYTE1,BYTE1                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    UPDTP02                                                          
*                                                                               
         L     RE,AIO2             If doesn't exist build one                   
         LA    RF,2000             Clear io area                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R2,AIO2                                                          
         MVC   TTMKEY,IOKEYSAV                                                  
         MVI   BYTE1,1                                                          
         LA    R3,TTMRFST                                                       
         J     UPDTP12                                                          
*                                                                               
UPDTP02  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R3,TTMRFST                                                       
*                                                                               
         USING LIDELD,R3                                                        
UPDTP04  CLI   LIDEL,0                                                          
         JE    UPDTP10                                                          
         CLI   LIDEL,LIDELQ                                                     
         JE    UPDTP08                                                          
UPDTP06  LLC   R0,LIDLN                                                         
         AR    R3,R0                                                            
         J     UPDTP04                                                          
*                                                                               
UPDTP08  MVI   LIDEL,X'FF'                                                      
         J     UPDTP06                                                          
*                                                                               
UPDTP10  GOTOR VHELLO,DMCB,(C'D',ACCMST),(X'FF',TTMRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING LIDELD,R3                                                        
UPDTP12  XC    ELEMENT,ELEMENT                                                  
         XC    ELEMENT2,ELEMENT2                                                
         LA    R3,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'LITTCLAC+L'LITTIMTY+L'LITTWRKC                         
         MVI   LIDTYPE,LIDTCLIT                                                 
         LHI   RF,LIDLNDQ                                                       
         STC   RF,LIDLN                                                         
         LA    R4,LIDDATA                                                       
         ST    R4,ANXTELE                                                       
         LA    R3,ELEMENT2                                                      
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'ACTKACT                                                
         MVI   LIDTYPE,LIDTNCLC                                                 
         LHI   RF,LIDLNDQ                                                       
         STC   RF,LIDLN                                                         
         LA    R4,LIDDATA                                                       
         ST    R4,ANXTELE2                                                      
                                                                                
T        USING LIDDATA,R4                                                       
         XC    TT_KEY(TT_KEYL),TT_KEY                                           
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),0                                  
         J     UPDTP16                                                          
*                                                                               
UPDTP14  GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),0                                  
*                                                                               
UPDTP16  TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   UPDTP24             Finished                                     
         OC    TT_ITEM#,TT_ITEM#   Skip materials                               
         JNZ   UPDTP14                                                          
         OC    TT_NAR#,TT_NAR#     Skip day narrative                           
         JNZ   UPDTP14                                                          
*                                                                               
         CLC   PRODUL,TT_AULA      Client time                                  
         JNE   UPDTP20             No                                           
         LA    R3,ELEMENT                                                       
         L     R4,ANXTELE                                                       
         CLI   TT_TTYP,TIMTCB                                                   
         JNE   *+8                                                              
         MVI   T.LITTIMTY,C'B'                                                  
         CLI   TT_TTYP,TIMTCR                                                   
         JNE   *+8                                                              
         MVI   T.LITTIMTY,C'R'                                                  
         CLI   TT_TTYP,TIMTCN                                                   
         JNE   *+8                                                              
         MVI   T.LITTIMTY,C'N'                                                  
         MVC   T.LITTWRKC,TT_TSK                                                
         MVC   T.LITTCLAC,TT_AACT                                               
         LHI   RF,L'LITTCLAC+L'LITTIMTY+L'LITTWRKC                              
         AR    R4,RF                                                            
         ST    R4,ANXTELE                                                       
         LLC   R0,LIDLN                                                         
         AR    R0,RF                                                            
         STC   R0,LIDLN                                                         
         AR    R0,RF                                                            
         CHI   R0,X'FF'            Element full?                                
         JL    UPDTP14                                                          
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',ACCMST),TTMRECD,LIDELD                         
         CLI   12(R1),0            Did we add element successfully              
         JE    UPDTP18             Yes                                          
         CLI   12(R1),5            Is record now too big                        
         JE    *+6                 Yes                                          
         DC    H'0'                Die on any other error                       
         MVC   ROUERRV,=AL2(AE$TTBRL)                                           
         J     EXITN                                                            
UPDTP18  XC    ELEMENT,ELEMENT                                                  
         MVI   LIDEL,LIDELQ        Build a new element                          
         MVI   LIDITLN,L'LITTCLAC+L'LITTIMTY+L'LITTWRKC                         
         MVI   LIDTYPE,LIDTCLIT                                                 
         LHI   RF,LIDLNDQ                                                       
         STC   RF,LIDLN                                                         
         LA    R4,LIDDATA                                                       
         ST    R4,ANXTELE                                                       
         J     UPDTP14                                                          
*                                                                               
UPDTP20  LA    R3,ELEMENT2                                                      
         L     R4,ANXTELE2                                                      
         MVC   T.LIDDATA(L'ACTKACT),TT_AACT                                     
         LHI   RF,L'ACTKACT                                                     
         AR    R4,RF                                                            
         ST    R4,ANXTELE2                                                      
         LLC   R0,LIDLN                                                         
         AR    R0,RF                                                            
         STC   R0,LIDLN                                                         
         AR    R0,RF                                                            
         CHI   R0,X'FF'            Element full?                                
         JL    UPDTP14                                                          
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',ACCMST),TTMRECD,LIDELD                         
         CLI   12(R1),0            Did we add element successfully              
         JE    UPDTP22             Yes                                          
         CLI   12(R1),5            Is record now too big                        
         JE    *+6                 Yes                                          
         DC    H'0'                Die on any other error                       
         MVC   ROUERRV,=AL2(AE$TTBRL)                                           
         J     EXITN                                                            
UPDTP22  XC    ELEMENT2,ELEMENT2                                                
         MVI   LIDEL,LIDELQ        Build a new element                          
         MVI   LIDITLN,L'ACTKACT                                                
         MVI   LIDTYPE,LIDTNCLC                                                 
         LHI   RF,LIDLNDQ                                                       
         STC   RF,LIDLN                                                         
         LA    R4,LIDDATA                                                       
         ST    R4,ANXTELE2                                                      
         J     UPDTP14                                                          
*                                                                               
UPDTP24  LA    R3,ELEMENT                                                       
         CLI   LIDLN,LIDLNDQ                                                    
         JNH   UPDTP26                                                          
         GOTOR VHELLO,DMCB,(C'P',ACCMST),TTMRECD,ELEMENT                        
         CLI   12(R1),0            Did we add element successfully              
         JE    UPDTP26             Yes                                          
         CLI   12(R1),5            Is record now too big                        
         JE    *+6                 Yes                                          
         DC    H'0'                Die on any other error                       
         MVC   ROUERRV,=AL2(AE$TTBRL)                                           
         J     EXITN                                                            
*                                                                               
UPDTP26  LA    R3,ELEMENT2                                                      
         CLI   LIDLN,LIDLNDQ                                                    
         JNH   UPDTP36                                                          
         GOTOR VHELLO,DMCB,(C'P',ACCMST),TTMRECD,ELEMENT2                       
         CLI   12(R1),0            Did we add element successfully              
         JE    UPDTP36             Yes                                          
         CLI   12(R1),5            Is record now too big                        
         JE    *+6                 Yes                                          
         DC    H'0'                Die on any other error                       
         MVC   ROUERRV,=AL2(AE$TTBRL)                                           
         J     EXITN                                                            
*                                  Put record back or add one                   
UPDTP36  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    EXITY                if so don't update                          
         CLI   BYTE1,1                                                          
         JE    UPDTP40                                                          
*                                                                               
UPDTP38  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
UPDTP40  GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T,R2,R3                                                          
         EJECT                                                                  
***********************************************************************         
* Remove zero hour lines                                              *         
***********************************************************************         
         SPACE 1                                                                
NEW      USING TT_D,R6             Point to new time buffer record              
REMZHL   NTR1  LABEL=NO                                                         
         LA    R6,TT_NEW                                                        
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Only remove zero timelines when Aura         
         CHI   RF,XPRODIKQ         and on submit/reject                         
         JNE   REMZHX                                                           
         CLI   QH_STAT,QH_SSUBQ    Are we submitting?                           
         JNE   REMZHX                                                           
*                                                                               
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
*                                                                               
REMZH010 GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   REMZH030            No                                           
         DC    H'0'                Yes die as it shouldn't be empty             
*                                                                               
REMZH020 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   REMZHX              Yes                                          
*                                                                               
REMZH030 OC    NEW.TT_ITEM#,NEW.TT_ITEM#                                        
         JNZ   REMZH020            Skip material rows                           
         OC    NEW.TT_NAR#,NEW.TT_NAR#                                          
         JNZ   REMZH020            Also skip narrative rows                     
         CP    NEW.TT_HRS,PZERO    Check if zero hour timeline                  
         JNE   REMZH020                                                         
         CLI   NEW.TT_TTYP,TIMTEM  Processing empty row                         
         JE    REMZH020            Yes don't delete                             
         LA    R1,NEW.TT_DHVAL                                                  
         LA    R0,TT_DAYS                                                       
REMZH032 OC    0(L'TIMETDTE,R1),0(R1) Is a date present?                        
         JZ    REMZH034             No - then no further entries to get         
         CP    L'TIMETDTE(L'TIMEHRS,R1),PZERO                                   
         JNE   REMZH020                                                         
         LA    R1,L'TIMEDAY(R1)                                                 
         JCT   R0,REMZH032                                                      
*                                                                               
REMZH034 MVC   SV_TTKEY,NEW.TT_KEY                                              
         GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF),NEW.TT_D                           
*                                                                               
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JZ    REMZH040            No then check if material                    
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         MVC   NEW.TT_KEY(TT_KEYL),SV_TTKEY                                     
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   *+6                 No                                           
         DC    H'0'                                                             
         GOTOR BUFTIM,DMCB,('TSADEL',NEWBUF),NEW.TT_D                           
         JE    REMZHX                                                           
         DC    H'0'                                                             
*                                                                               
REMZH040 OC    NEW.TT_ITEM#,NEW.TT_ITEM#   Is it a material line?               
         JZ    REMZH050            Ok move to next entry                        
         CP    NEW.TT_IMULT,PZERO        Keep if there is a quantity            
         JNE   REMZH020            otherwise delete                             
         GOTOR BUFTIM,DMCB,('TSADEL',NEWBUF),NEW.TT_D                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                  Not a material line, reread and              
REMZH050 XC    NEW.TT_D(TT_KEYL),NEW.TT_D  delete old line                      
         MVC   NEW.TT_KEY(TT_KEYL),SV_TTKEY                                     
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF+TSERNF Is it the end of the buffer?                
         JZ    *+6                 No                                           
         DC    H'0'                                                             
         GOTOR BUFTIM,DMCB,('TSADEL',NEWBUF),NEW.TT_D                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                  Reset sequence and restart reads             
         XC    NEW.TT_D(TT_KEYL),NEW.TT_D                                       
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF),NEW.TT_D                           
         TM    TIMERR,TSEEOF       Is it the end of the buffer?                 
         JNO   REMZH030            No                                           
*                                                                               
REMZHX   J     EXITY                                                            
         EJECT                                                                  
         DROP  NEW                                                              
***********************************************************************         
* General exits                                                       *         
***********************************************************************         
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
AAUDREC  EQU   AIO1,,C'A'          A(Time audit record)                         
ATRNREC  EQU   AIO2,,C'A'          A(Transaction record)                        
AAPPTAB  EQU   AIO4,,C'A'          A(APPTAB) - uses IO5 I06 IO7 also            
ACPRREC  EQU   AIO8,,C'A'          A(Costing personal rates record)             
ATIMREC  EQU   AIO8,,C'A'          A(Time interface record)                     
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
TIMDLNQ  EQU   TIMHLNQ+L'TIMAIDNO  Length of deleted time element               
                                                                                
OLDBUF   EQU   1                   Old time buffer                              
NEWBUF   EQU   2                   New time buffer                              
                                                                                
TIMBUF   EQU   1                   Audit element buffer                         
                                                                                
D#TOKEN  EQU   1                   PC token map number                          
D#IDN    EQU   2                   New ID number                                
D#STA    EQU   3                   New timesheet status                         
D#ERR    EQU   4                   Error message number                         
D#STAV   EQU   5                   New timesheet status to viewer               
D#CAE    EQU   1                   Type field in TEAP call                      
D#CAEML  EQU   2                   emails required Y/N blank is Y               
                                                                                
TOKNLIT  DC    C'PC Token'                                                      
TSARLIT  DC    C'*** TSAR Record ***'                                           
HEADLIT  DC    C'*** Header Values **'                                          
TRLRLIT  DC    C'*** Trailer Values **'                                         
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(WORKL)                                                       
                                                                                
SRUPD60  DC    C'T10D60  '         Update program phase name                    
SORTCARD DC    C'SORT FIELDS=(5,43,A),FORMAT=BI,WORK=1 '                        
SORTTYPE DC    C'RECORD TYPE=V,LENGTH=(2048,,,100,150) '                        
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
                                                                                
PZERO    DC    P'0'                                                             
PNEGONE  DC    P'-1'                                                            
P1HUND   DC    P'100'                                                           
P1THOU   DC    P'1000'                                                          
P1MILL   DC    P'1000000'                                                       
MAXHOURS DC    P'2400'                                                          
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
LEDGER1R DC    C'1R'                                                            
LEDGER1N DC    C'1N'                                                            
LEDGERSK DC    C'SK'                                                            
LEDGERSI DC    C'SI'                                                            
LEDGER12 DC    C'12'                                                            
LEDGER14 DC    C'14'                                                            
                                                                                
FWHSTAMP DC    X'00',C'UO'          FACWRK header record stamp                  
                                                                                
ADDEND   DC    C'ADD=END'                                                       
RETDATEL DC    AL1(LQ_RDATQ),AL2(LQ_LN1Q)                                       
                                                                                
ACCOUNT  DC    C'ACC'              Account file prefix                          
                                                                                
DMWRTD   DC    C'DMW'              DMWRITE (directory)                          
DMADDD   DC    C'DMA'              DMADD   (directory)                          
DMPUTF   DC    C'PUT'              PUTREC  (file)                               
DMADDF   DC    C'ADD'              ADDREC  (file)                               
*&&UK                                                                           
DMADFF   DC    C'ADF'              ADDREC  (Archive file)                       
*&&                                                                             
                                                                                
DMREAD   DC    C'DMREAD  '         DMREAD  (directory)                          
DMGETR   DC    C'GETREC  '         GETREC  (file)                               
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
$ADDTRN  DC    C'$ATR'             ADDTRN file prefix                           
$PLDREC  DC    C'$PLD'             PLDREC file prefix                           
$TIMREC  DC    C'$TIM'             TIMREC file prefix                           
$LOKREC  DC    C'$LOK'             LOKREC file prefix                           
                                                                                
ADD      DC    C'ADD     '         FACWRK add                                   
CLOSE    DC    C'CLOSE   '         FACWRK close                                 
INDEX    DC    C'INDEX   '         FACWRK index                                 
READ     DC    C'READ    '         FACWRK read                                  
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
FILES    DS    0C                  ** System/File list **                       
         DC    C'ACCOUNT'                                                       
         DC    C'U'                                                             
ACCDIR   DC    C'ACCDIR '                                                       
         DC    C'U'                                                             
ACCMST   DC    C'ACCMST '                                                       
         DC    C'U'                                                             
ACCARC   DC    C'ACCARC '                                                       
         DC    C'U'                                                             
ACCRCV   DC    C'ACCRCV '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'X'                                                             
                                                                                
RECTAB   DS    0XL(RECTABL)        ** Record table **                           
         DC    AL2(A#THDR),AL1(RECTHDRQ)                                        
         DC    AL2(A#TTIM),AL1(RECTTIMQ)                                        
         DC    AL2(A#TMAT),AL1(RECTMATQ)                                        
         DC    AL2(A#TDNR),AL1(RECTDNRQ)                                        
         DC    AL2(A#TTRL),AL1(RECTTRLQ)                                        
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
*                                                                               
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
*                                                                               
FUTTAB   DS    0XL1                                                             
         DC    C'N',AL1(TL_TNONQ,TL_INON)                                       
         DC    C'C',AL1(TL_TNONQ,TL_ICLI)                                       
         DC    C'R',AL1(TL_TRELQ,TL_ICLI)                                       
         DC    C'B',AL1(TL_TBILQ,TL_ICLI)                                       
         DC    X'FF'                                                            
*                                                                               
RECTABD  DSECT                     ** Record table layout **                    
RECTMAP# DS    AL2                 Record map number                            
RECTTYPE DS    AL1                 Internal record type                         
RECTABL  EQU   *-RECTABD           Width of table entry                         
SVRDEF   CSECT                                                                  
                                                                                
LLP      DC    A(0)                A(LP_D)                                      
         EJECT                                                                  
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
***********************************************************************         
* Sacred values                                                       *         
***********************************************************************         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER mode                           
                                                                                
RUNINDS  DS    X                   ** Run indicators **                         
RUNITIMF EQU   X'80'               TIMTRN first time flag                       
RUNIPUTF EQU   X'40'               PUTOUT initialised                           
RUNIATRF EQU   X'20'               GOATRN first time flag                       
RUNIBUFF EQU   X'10'               BUFREC first time flag                       
RUNIUPD  EQU   X'04'               Something to update                          
                                                                                
ASRUPD60 DS    A                   A(SRUPD60)                                   
PROTOFF  DS    A                   A(PROTOFF)                                   
PROTON   DS    A                   A(PROTON)                                    
LOCKET   DS    A                   A(LOCKET)                                    
IDATAMGR DS    A                   A(DMGRITRN)                                  
XDATAMGR DS    A                   A(DMGRXTRN)                                  
MASTC    DS    A                   A(MASTC)                                     
DATAMGR  DS    A                   A(DATAMGR)                                   
WRKBLKR  DS    A                   A(FACWRK WRKIO block)                        
AAPPTN   DS    A                   A(Next APPTAB entry)                         
VSORTER  DS    A                   A(SORTER) from RSORTER                       
                                                                                
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSAROLDT DS    XL(TSPXTNL)         TSAR block for old time buffer               
TSARNEWT DS    XL(TSPXTNL)         TSAR block for new time buffer               
TSARGENL DS    XL(TSPXTNL)         TSAR block for timeld buffer                 
TSAERR   DS    XL(L'TSERRS)        Transaction buffer error return              
                                                                                
COMFACS  DS    XL(COMFACSL)        Local copy of COMFACS                        
                                                                                
SAVEVAR  DS    0F                  ** Variables follow **                       
                                                                                
PIDLOCK  DS    (PIDMAXQ)XL(L'HD_PPID#+L'HD_PEDT) PID lock table                 
PIDMAXQ  EQU   100                                                              
PIDLOCKL EQU   *-PIDLOCK                                                        
                                                                                
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
*              ** Header record **                                              
                                                                                
QH_AHEAD DS    AL4                 A(Passed header values)                      
QH_1RPER DS    CL(L'PERKCODE)      Accounting person code                       
QH_PEDT  DS    XL3                 Period end date of time sheet                
QH_DATI  DS    X                   Date/Hours indicator                         
QH_ADAT  DS    AL3                 A(Date list)                                 
QH_BUAPR DS    C                   Back up approval if Y                        
                                                                                
QH_UTYP  DS    C                   ** Type of update **                         
QH_UAPRQ EQU   C'1'                T/S approval - For Aura used to              
*                                  submit from list                             
QH_UUPDQ EQU   C'2'                T/S update - User/Adjuster/Approver          
                                                                                
QH_STAT  DS    C                   ** To status **                              
QH_SSAVQ EQU   C'1'                Saved                                        
QH_SSUBQ EQU   C'2'                Submitted                                    
QH_SAPRQ EQU   C'3'                Approved                                     
QH_SREJQ EQU   C'4'                Rejected                                     
QH_SDELQ EQU   C'5'                Deleted                                      
QH_TOKEN DS    CL8                 PC Token (echoed in error response)          
                                                                                
QH_COMSL DS    X                   Length of rejection/approval text            
QH_COMS  DS    CL(L'TIMREJAP)      Rejection/approval text                      
                                                                                
QH_TEMPL DS    C                   Template flag Y/N/O                          
QH_TEMPO EQU   C'O'                Template only upload                         
QH_TEMPY EQU   C'Y'                Template + time update                       
QH_TEMPN EQU   C'N'                Standard time upload                         
                                                                                
*              ** Common Time/Materials values **                               
                                                                                
QX_TSAR  DS    AL4                 A(Passed TSAR record)                        
QX_TOKEN DS    CL8                 PC Token (echoed in error response)          
QX_DESCI DS    AL4                 Time/Materials description                   
                                                                                
*              ** Time line record **                                           
                                                                                
QT_HRSI  DS    X                   Hours indicator                              
QT_AHRS  DS    AL3                 A(Hours list)                                
*                                                                               
QT_TIME# DS    XL(L'TT_TIME#)      Row number (copy/change)                     
                                                                                
QT_TTYPE DS    C                   ** Type of time **                           
QT_TBTIM EQU   C'B'                (B Time)                                     
QT_TRTIM EQU   C'R'                (R Time)                                     
QT_TNTIM EQU   C'N'                (N Time)                                     
QT_TETIM EQU   C'E'                (E Time)                                     
                                                                                
QT_1NACT DS    CL(L'ACTKACT)       1N account code (N Time)                     
QT_SJCLI DS    CL(L'GOSELCLI)      Client code                                  
QT_SJPRO DS    CL(L'GOSELPRO)      Product code                                 
QT_SJJOB DS    CL(L'GOSELJOB)      Job code                                     
QT_TASK  DS    CL(L'WCOKWRK)       Task (work code)                             
QT_AMEND DS    C                   Row amended - either add or changed          
QT_ORDER DS    CL(L'TIMOORDR)      Order number                                 
QT_EST#  DS    CL(L'TIMSESNM)      Estimate number                              
QT_INTRF DS    CL(L'TIMJINRF)      Internal reference                           
QT_TOFFI DS    AL4                 Timeoff indicator                            
                                                                                
*              ** Materials record **                                           
                                                                                
QI_VALS  DS    0X                  ** Item values **                            
QI_ITEM# DS    XL2                 Item row number in materials grid            
QI_ITEMC DS    CL(L'TIMINUM)       Item code                                    
QI_ITEMS DS    XL(L'TIMISEQ)       Item sequence number                         
QI_ITEMP DS    PL(L'TIMIPRCE)      Item price                                   
QI_ITEMX DS    PL(L'TIMIMULT)      Item multiplier                              
QI_ITEMT DS    PL(L'TIMITOT)       Item total                                   
QI_ITEMO DS    C                   Item price override if Y                     
QI_VALSL EQU   *-QI_VALS                                                        
                                                                                
*              ** Day narrative record **                                       
                                                                                
QD_VALS  DS    0X                  ** Day narrative values **                   
QD_DATE  DS    PL3                 Day narrative date                           
QD_TIME# DS    XL2                 Day narrative row #                          
QD_SEQ#  DS    XL2                 Narrative seq #                              
QD_VALSL EQU   *-QD_VALS                                                        
                                                                                
*              ** Trailer record **                                             
                                                                                
QT_ATRLR DS    AL4                 A(Passed trailer values)                     
QT_INDEX DS    XL(L'TR_INDX)       Audit index number                           
                                                                                
***********************************************************************         
* Derived header values (passed to update phase)                      *         
***********************************************************************         
                                                                                
HD_VALS  DS    0F                                                               
                                                                                
HD_IND   DS    X                   Header status (see FW_HDIND)                 
                                                                                
HD_TODF  DS    CL6                 Today's date character                       
HD_TODP  DS    XL3                 Today's date packed                          
HD_TODC  DS    XL2                 Today's date compressed                      
HD_TODP5 DS    XL3                 Today's date packed (from Datcon/5)          
HD_OVRDU DS    XL3                 Overdue date packed                          
HD_CSTR  DS    XL3                 Calendar start date MOA                      
HD_CEND  DS    XL3                 Calendar end MOA                             
HD_PSTR  DS    XL3                 Period start date                            
HD_PEND  DS    XL3                 Period end date                              
HD_PPID# DS    XL(L'PIDNO)         Person PID number                            
HD_PEDT  DS    XL3                 Period end date                              
HD_LEDT  DS    XL3                 Location end date                            
HD_LEDTC DS    XL3                 Location end date complemented               
HD_PEDTC DS    XL3                 Period end date complemented                 
HD_MOA   DS    XL(L'TMPMTH)        MOA of time                                  
HD_PERNO DS    XL(L'TMPNUMB)       Period number                                
HD_MANAP DS    XL(L'DPAPPIDB)      Manager approver PID                         
HD_MANBA DS    10XL(L'PIDNO)       Back-up manager approver PID                 
HD_BAMAX EQU   10                  10 Max Backup Approvers                      
                                                                                
HD_TSSTO DS    XL(L'TIMKSTAT)      Previous time status                         
                                                                                
HD_1ROFF DS    CL(L'LOCOFF)        Person office code                           
HD_1RDEP DS    CL(L'LOCDEPT)       Department code                              
HD_1RSUB DS    CL(L'LOCSUB)        Sub-department code                          
HD_1RPER DS    CL(L'PERKCODE)      Person code                                  
                                                                                
HD_1RULA DS    0CL(L'ACTKULA)      ** Person 1R account **                      
HD_1RUNT DS    CL(L'ACTKUNT)       Unit                                         
HD_1RLDG DS    CL(L'ACTKLDG)       Ledger                                       
HD_1RACT DS    CL(L'ACTKACT)       Account code                                 
                                                                                
HD_1RNAM DS    CL(L'NAMEREC)       Person 1R account name                       
HD_1RCST DS    CL(L'RSTCOSTG)      Costing group                                
HD_1RFPT DS    CL1                 Force product time - Y/N                     
HD_1RFJT DS    CL1                 Force job time - Y/N                         
HD_INULA DS    CL(L'ACTKULA)       Person override income account               
HD_142NM DS    CL(L'NAMEREC)       Department name                              
HD_143NM DS    CL(L'NAMEREC)       Sub-Department name                          
HD_EMPST DS    XL(L'EMPSTAT)       Employee status (Executive etc.)             
                                                                                
HD_COATM DS    XL(L'COATM)         Allowable time types                         
HD_CODTM DS    XL(L'CODTM)         Default time types                           
HD_COBRT DS    XL(L'COBRTE)        Post sales rate not cost B-Time              
HD_CORRT DS    XL(L'CORRTE)        Post sales rate not cost R-Time              
HD_COTUP DS    XL(L'COTUP)         When to update postings                      
HD_COACS DS    XL(L'COACS)         Time sheet approval option                   
HD_COFNR DS    XL(L'COFNR)         Force narrative on time sheet                
HD_COMAT DS    CL(L'COMAT)         Materials in use                             
HD_COIOS DS    CL(L'COTBTL)        Income or suspense for B-Time                
HD_CONJB DS    CL(L'CONJB)         No job input allowed - Y/N                   
HD_CONDA DS    PL(L'CONDA)         Number of days allowed in future             
HD_COFAP DS    CL(L'COFAP)         Use account setting for future time          
HD_COFTA DS    CL(L'COFTA)         Type of time allowed for future time         
                                                                                
HD_CDAT  DS    XL3                 MOA date                                     
HD_SUBLM DS    XL3                 Submitted date for line manager              
HD_SUBCL DS    XL3                 Submitted date for client approvers          
                                                                                
HD_DATEN DS    X                   Number of dates                              
                                                                                
HD_DMOA  DS    0X                  ** Date/MOA array **                         
HD_DMDAT DS    XL3                 Date                                         
HD_DMMOA DS    XL2                 Month of activity                            
*&&US                                                                           
HD_EDHRS DS    PL(L'TIMEHRS1)      Daily edit hours                             
*&&                                                                             
HD_DMOAL EQU   *-HD_DMOA           Length of entry                              
         DS    (TT_DAYS-1)XL(HD_DMOAL)                                          
                                                                                
HD_VALSL EQU   *-HD_VALS                                                        
                                                                                
***********************************************************************         
* Derived time line values (cleared for time line, not materials)     *         
***********************************************************************         
                                                                                
TL_VALS  DS    0F                                                               
                                                                                
TL_IEDT  DS    X                   ** Edit hours indicator **                   
TL_IOFFQ EQU   X'80'               Set office in key                            
TL_IDPTQ EQU   X'40'               Set department in key                        
TL_ISUBQ EQU   X'20'               Set sub-department in key                    
TL_IPERQ EQU   X'10'               Set person in key                            
                                                                                
TL_HDMA  DS    0X                  ** Hours/Date array **                       
TL_ETDT1 DS    XL(L'TIMETDT1)      Date (uploaded in header)                    
TL_EHRS1 DS    PL(L'TIMEHRS1)      Hours (uploaded in time sheet line)          
TL_HDMNL EQU   *-TL_HDMA           Length of entry                              
         DS    (TT_DAYS-1)XL(TL_HDMNL)                                          
TL_HDMAL EQU   *-TL_HDMA                                                        
                                                                                
TL_ULA   DS    0CL(L'ACTKULA)      ** SJ/1N account code **                     
TL_UNT   DS    CL(L'ACTKUNT)       Unit code                                    
TL_LGD   DS    CL(L'ACTKLDG)       Ledger code                                  
TL_ACT   DS    CL(L'ACTKACT)       Account code                                 
                                                                                
TL_SJCLI DS    CL(L'GOSELCLI)      Client code                                  
TL_SJPRO DS    CL(L'GOSELPRO)      Product code                                 
TL_SJJOB DS    CL(L'GOSELJOB)      Job code                                     
                                                                                
TL_1CULA DS    CL(L'ACTKULA)       Costing client account                       
TL_INULA DS    CL(L'ACTKULA)       Income account                               
TL_12ULA DS    CL(L'ACTKULA)       Costing revenue account                      
TL_12NAM DS    CL(L'NAMEREC)       Costing revenue account name                 
TL_CANAM DS    CL(L'NAMEREC)       1C/1N account name                           
TL_CLNAM DS    CL(L'NAMEREC)       Client name                                  
TL_PRNAM DS    CL(L'NAMEREC)       Product name                                 
TL_JBNAM DS    CL(L'NAMEREC)       Job name                                     
                                                                                
TL_TTYPE DS    C                   ** Time type **                              
TL_TBILQ EQU   C'B'                Billable time                                
TL_TNONQ EQU   C'N'                Non-billable time                            
TL_TRELQ EQU   C'R'                Realisation time                             
TL_TDEFQ EQU   C'D'                Default time (resource management)           
TL_TEMPQ EQU   C'E'                Empty - for timesheet with no time           
                                                                                
TL_SJOFF DS    CL(L'PPRGAOFF)      SJ office code                               
TL_MED   DS    CL1                 Media code                                   
TL_TSK   DS    CL(L'TIMTSK)        Task (work code)                             
TL_ORD   DS    CL(L'TIMOORDR)      Order number                                 
TL_EST   DS    CL(L'TIMSESNM)      Estimate number                              
TL_INTRF DS    CL(L'TIMJINRF)      Internal reference                           
TL_TOFFI DS    CL(L'TIMFIDN)       Timeoff indicator                            
TL_TOFFL DS    XL1                 Length of timeoff indicator                  
                                                                                
TL_ICPJ  DS    X                   ** SJ/1N indicators **                       
TL_ICLI  EQU   X'80'               Client level found                           
TL_IPRO  EQU   X'40'               Product level found                          
TL_IJOB  EQU   X'20'               Job level found                              
TL_INON  EQU   X'10'               Non-client level found                       
TL_IERR  EQU   X'08'               Client/prod/job error                        
TL_EADJ  EQU   X'04'               Job is eligible for adjustment               
TL_1NHOL EQU   X'02'               1N account is holiday (F2=T)                 
                                                                                
TL_TFNAR DS    CL(L'GOTFNARR)      Narrative required for time types            
TL_RST7  DS    XL(L'RSTSTAT7)      Record status byte 7                         
TL_HIDTE DS    XL(L'TIMETDT1)      Highest date in time line packed             
TL_CHIDT DS    XL2                 Highest date in time line compressed         
         DS    XL30                                                             
                                                                                
TL_VALSL EQU   *-TL_VALS                                                        
                                                                                
***********************************************************************         
* Derived trailer values (passed to update phase)                     *         
***********************************************************************         
                                                                                
TR_VALS  DS    0F                                                               
TR_INDX  DS    H                   Current audit index number                   
TR_INDXN DS    H                   Next audit index number                      
                                                                                
TR_TSSTN DS    XL(L'TIMKSTAT)      Current time status                          
                                                                                
TR_IND   DS    X                   Approval status                              
TR_ILNMG EQU   X'40'               Line manager approval/rejection              
TR_IMCLI EQU   X'20'               Missing client approval                      
TR_IRLCL EQU   X'80'               Rejection client approval                    
TR_ICLIA EQU   X'10'               Client approval has occurred                 
TR_INCLI EQU   X'08'               No client approval is required               
TR_IAPTT EQU   X'04'               Approved this time                           
TR_ILMGP EQU   X'02'               Approved previously by line manager          
TR_ICLAT EQU   X'01'               TIMRECs to be updated (due to eg.            
*                                  Client approval,MOA or Inc a/c chgd)         
TR_EDHRS DS    PL3                 Edit hours                                   
                                                                                
TR_VALSL EQU   *-TR_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      ** Current record type **                    
RECTHDRQ EQU   1                   Header record                                
RECTTIMQ EQU   2                   Time line                                    
RECTMATQ EQU   3                   Materials line (follows time)                
RECTDNRQ EQU   4                   Day narrative                                
RECTTRLQ EQU   5                   Trailer record                               
                                                                                
TIMERR   DS    XL(L'TSERRS)        Time buffer error return                     
                                                                                
ANXTERR  DS    A                   A(Next error entry)                          
AEMPEL   DS    A                   A(Employee element)                          
ALOCEL   DS    A                   A(Location element)                          
ANXTELE  DS    A                   A(element work area 1)                       
ANXTELE2 DS    A                   A(element work area 2)                       
                                                                                
TOTHOURS DS    PL4                 Total time sheet hours                       
                                                                                
DMGRSEQ# DS    XL(L'FW_SEQ#)       DATAMGR record sequence number               
                                                                                
         DS    0H                                                               
TIMSEQ#  DS    XL2                 BUFGEN time element sequence no.             
SVTI_KEY DS    XL(TI_KEYL)         Saved time buffer key                        
                                                                                
TIME#    DS    XL(L'TT_TIME#)      Time row number (for RESAPP calls)           
SVTT_#   DS    XL(TT_ROW#L)        Saved Tsar key (for RBLCLS calls)            
TIMCAULA DS    CL(L'ACTKULA)       Contra a/c code (for RBLCLS calls)           
TIMCOFF  DS    CL2                 Contra office (for RBLCLS calls)             
RBLIND   DS    XL1                 Rebuild cluster element                      
RBLIEOF  EQU   X'80'               End of buffer reached                        
RBLICHG  EQU   X'40'               Change/addition in timeline                  
RBLINCH  EQU   X'20'               No change/addition in timeline               
RBLIDEL  EQU   X'10'               Deletion in timeline                         
                                                                                
ELEMENT2 DS    XL256               2nd element work area                        
SV_TTKEY DS    XL(TT_KEYL)         Saved TSAR key                               
SVMOA    DS    XL2                 Saved MOA value                              
                                                                                
APPSTAT  DS    X                   ** Approval flags **                         
APPSWAPP EQU   TIMESAPR            Previously client approved (X'80')           
APPSWREJ EQU   TIMESREJ            Previously client rejected (X'40')           
APPSNTLC EQU   X'20'               No time line change                          
APPSMTAD EQU   X'10'               Material addition                            
APPSMTCH EQU   X'08'               Material change                              
APPSACCH EQU   X'04'               Account changed on time line                 
                                                                                
ERRTAB   DS    XL256               Saved error messages and data                
ERRTXT   DS    CL20                Extra error text                             
                                                                                
STPOSTNG DS    X                   ** TIMTRN s/r posting status **              
STBCKOUT EQU   X'80'               Back out old posting                         
STADDNEW EQU   X'40'               Add new posting                              
STNEGATE EQU   X'20'               Post negative amount                         
STBUCKET EQU   X'10'               Update buckets only                          
STA1CBUC EQU   X'08'               Don't post 1C/14 and 1R/1C buckets           
STAUPTRX EQU   X'04'               Update existing transactions                 
                                                                                
POSTSTAT DS    X                   ** BLDTRN s/r indicators **                  
POSTS1R  EQU   X'80'               Making the 1R posting - buckets only         
POSTSSJ  EQU   X'40'               Making the SJ posting                        
POSTSSI  EQU   X'20'               Making the SI posting                        
POSTS1C  EQU   X'10'               Making the 1C posting                        
POSTS12  EQU   X'08'               Making the 12 posting                        
*&&UK                                                                           
POSTSCA  EQU   X'04'               Making the 1C/1R posting buckets             
*&&                                                                             
ATAMOUNT DS    PL(L'TRNAMNT)       Posting amount                               
                                                                                
ATTRSST4 DS    XL(L'TRSSTAT4)      TRSSTAT4 value                               
ATREF    DS    CL(L'TRNKREF)       Transaction reference #                      
                                                                                
PACK8    DS    PL8                 For packed arithmetic                        
PACK16   DS    PL16                For packed arithmetic                        
                                                                                
*&&UK                                                                           
METHSTAT DS    X                   ** Method indicators **                      
METSHLVL EQU   X'80'               High level method profile record             
METSNANL EQU   X'40'               No analysis found on 1R use default          
                                                                                
BUCKTYPE DS    0CL2                ** Bucket type **                            
BUCKMTHD DS    C                   Method                                       
BUCKSLRY DS    C                   Salary type                                  
                                                                                
DRAMOUNT DS    PL(L'TRNAMNT)       Debit amount                                 
CRAMOUNT DS    PL(L'TRNAMNT)       Credit amount                                
                                                                                
ATPROF1  DS    X                   Cost/Sales profile 1 (B time)                
ATPROF2  DS    X                   Cost/Sales profile 2 (N and R time)          
ATPR1CST EQU   X'01'               Use cost rate                                
ATPR1SAL EQU   X'02'               Use sales rate                               
                                                                                
AT142ULA DS    0CL(L'ACTKULA)      2 level structure 14 account                 
AT142UNT DS    CL(L'ACTKUNT)                                                    
AT142LDG DS    CL(L'ACTKLDG)                                                    
AT142ACT DS    CL(L'ACTKACT)                                                    
AT143ULA DS    0CL(L'ACTKULA)      3 level structure 14 account                 
AT143UNT DS    CL(L'ACTKUNT)                                                    
AT143LDG DS    CL(L'ACTKLDG)                                                    
AT143ACT DS    CL(L'ACTKACT)                                                    
METHTAB  DS    XL(MT_MAX#*MT_LNQ)  Method table (see MT_D)                      
INCOPOS  DS    CL1                 Income ledger offpos                         
*&&                                                                             
SAVEVARL EQU   *-SAVEVAR                                                        
                                                                                
TT_CUR   DS    0X                  Current time buffer record                   
TT_OLD   DS    (TT_LN2Q)X          Old time buffer record (NXTBUF)              
TT_NEW   DS    (TT_LN2Q)X          New time buffer record (NXTBUF)              
*                                                                               
I_GEN    DS    0X                                                               
         DS    (TI_LENQ)X          Tsar buffer entry                            
I_GENL   EQU   *-I_GEN                                                          
*                                                                               
SVI_GEN  DS    XL(I_GENL)          Saved tsar buffer entry                      
*                                                                               
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* Error table                                                         *         
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
                                                                                
***********************************************************************         
* Method table                                                        *         
***********************************************************************         
*&&UK                                                                           
MT_D     DSECT                                                                  
MT_EOTQ  EQU   0                   End of table indicator                       
                                                                                
MT_METHD DS    CL(L'CAPKMTHD)      Method number                                
MT_PCT   DS    PL6                 Method % of allocation                       
                                                                                
MT_STR   DS    X                   ** Method 14 account structure **            
MT_S2LVL EQU   2                   2 level 14 account                           
MT_S3LVL EQU   3                   3 level 14 account                           
                                                                                
MT_ANAL  DS    C                   Default analysis code                        
MT_RLNQ  EQU   6                   Length of rates                              
MT_TRATE DS    PL(MT_RLNQ)         Total hourly rate                            
MT_PRATE DS    PL(MT_RLNQ)         Pension hourly rate                          
MT_BRATE DS    PL(MT_RLNQ)         Benefit hourly rate                          
MT_SRATE DS    PL(MT_RLNQ)         Salary hourly rate                           
MT_BCNT  EQU   (*-MT_PRATE)/MT_RLNQ                                             
MT_EBCNT EQU   (*-MT_TRATE)/MT_RLNQ                                             
MT_LNQ   EQU   *-MT_D                                                           
MT_MAX#  EQU   9                   Maximum number of table entries              
*&&                                                                             
***********************************************************************         
* Optimisation buffer record layout                                   *         
***********************************************************************         
                                                                                
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL64                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
                                                                                
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
* Future time profile match table DSECT                               *         
***********************************************************************         
                                                                                
FUTTABD  DSECT                                                                  
FUTPROF  DS    XL1                 Profile value                                
FUTPNCL  EQU   C'C'                Non billable client time                     
FUTPNON  EQU   C'N'                Non billable non client time                 
FUTPREL  EQU   C'R'                Memo realisation time                        
FUTPBIL  EQU   C'B'                Billable                                     
FUTTYPE  DS    XL1                 Type of time input                           
FUTICPJ  DS    XL1                 Client or non client                         
FUTTABL  EQU   *-FUTTABD                                                        
                                                                                
***********************************************************************         
* Timeld element buffer DSECT                                         *         
***********************************************************************         
                                                                                
TI_RECD  DSECT ,                                                                
TI_REC   DS    0X                                                               
TI_KEY   DS    0X                                                               
TI_KSEQ  DS    XL2                 Unique serial number                         
TI_KEYL  EQU   *-TI_KEY                                                         
                                                                                
TI_ELEM  DS    XL255               STCELQ element                               
TI_LENQ  EQU   *-TI_REC                                                         
                                                                                
***********************************************************************         
* Income account optimisation buffer record                           *         
***********************************************************************         
                                                                                
IN_D     DSECT                                                                  
IN_KEY   DS    0XL(L'OB_KEY)       ** Buffer key **                             
IN_KCULA DS    XL(L'ACTKEY)        Account code                                 
                                                                                
         ORG   IN_D+(OB_ERROR-OB_D)                                             
IN_ERROR DS    XL(L'OB_ERROR)      Error number                                 
                                                                                
         ORG   IN_D+(OB_NAME-OB_D)                                              
IN_INNAM DS    CL(L'NAMEREC)       Income account name                          
IN_INCST DS    CL(L'RSTCOSTG)      Costing group                                
IN_INANA DS    CL(L'SPAAANAL)      Income analysis account                      
IN_12ULA DS    CL(L'TL_12ULA)      12 account code                              
IN_12NAM DS    CL(L'NAMEREC)       12 account name                              
                                                                                
       ++INCLUDE SRUPD60D                                                       
       ++INCLUDE ACTRAND                                                        
       ++INCLUDE ACTIMED                                                        
                                                                                
TT_D     DSECT                                                                  
         ORG   TT_D+TT_LN1Q                                                     
                                                                                
***********************************************************************         
* Extra data used to build postings in TIMTRN                         *         
***********************************************************************         
                                                                                
TT_12ULA DS    CL(L'ACTKULA)       Costing revenue account code                 
TT_SJNAM DS    CL(L'NAMEREC)       Name of SJ account                           
TT_CANAM DS    CL(L'NAMEREC)       Name of 1C/1N account                        
TT_12NAM DS    CL(L'NAMEREC)       Costing revenue account name                 
TT_LN2Q  EQU   *-TT_D              Length of buffer record                      
                                                                                
* Other included books follow                                                   
*        PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
       ++INCLUDE ACRCVRECD                                                      
*PREFIX=L                                                                       
       ++INCLUDE FALOCKETD                                                      
*PREFIX=                                                                        
LOCKETD  DSECT                                                                  
LOCKETL  EQU   *-LOCKETD                                                        
*        PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076ACBRA1B   02/04/21'                                      
         END                                                                    
