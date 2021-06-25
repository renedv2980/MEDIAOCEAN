*          DATA SET ACBRA00    AT LEVEL 118 AS OF 10/27/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T62400A                                                                  
*                                                                               
         TITLE '- ACCOUNTING/BRA - FALINK/DDLINK INTERFACE'                     
*                                                                               
* LEVEL CHANGE COMMENTS                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 22APR05 LIVE DATE FOR BETA RELEASE OF EBUYER                         
* TKLU 002 28APR05 DDLINK UPLOAD IMPLEMENTATION FOR EBUYER/ACMCS13              
* TKLU 003 04MAY05 ETIME UPLOAD AND DOWNLOAD DEFINITION                         
* TKLU 004 12MAY05 ADD NARRATIVE LIST/DISPLAY DOWNLOAD                          
* TKLU 005 18AUG05 PREPARATIONS FOR ETIME                                       
* TKLU 006 12SEP05 TIMESHEET/TIMELINE DOWNLOAD                                  
* TKLU 007 11OCT05 ANOTHER TIMESHEET/TIMELINE DOWNLOAD (SUMMARY VIEW)           
* TKLU 008 20OCT05 NEW ACMCS03 GENERAL ROUTINES                                 
* TKLU 009 04NOV05 T/S AUDIT TRAIL AND STATUS CHANGE MODULES FOR ETIME          
* TKLU 010 10NOV05 GENIE UKCR00003469 - NEW REQUEST TO BYPASS ADDRESS           
*                  AND CURRENCY ISSUE                                           
* TKLU 011 11NOV05 <LO01-4757> - ADD FIRST SET OF EEXPENSES REQUESTS            
* TKLU 012 14NOV05 DUMMY UPLOAD ADDED TO TEST NEW TECHNIQUES                    
* TKLU 013 29NOV05 USE ATIA FOR WORK REC AND BUFF (REPLACE AIO6 ...)            
* TKLU 014 19DEC05 <DU01-5029> - SINGLE ARTICLE DETAILS REQUEST                 
* TKLU 015 21DEC05 <DU01-4941> - FILTER NAMES/VALUES DOWNLOAD                   
* TKLU 016 21DEC05 <LO01-5021> - JOB LIST AND SEARCH DOWNLOAD                   
* TKLU 017 11JAN06 <LO01-5021> - FALINK @TRACKER DUMMY                          
* TKLU 018 13JAN06 <LO01-5021> - REPLACE T/S STATUS UPD. BY USER FIELDS         
* TKLU 019 24JAN06 <LO01-4063> - NEW APPROVER D/LOAD FOR TIME/EXPENSE           
* TKLU 020 08FEB06 <LO01-4757> - FALINK/DDLINK RECEIVE 2ND PAGE BUG FIX         
* TKLU 021 09FEB06 MAKE ALL REQUESTS DDLINK (WEB SERVER DEFECT)                 
*                  <LO01-5021> - NEW MEDIA LIST DOWNLOAD FOR JOB MODULE         
* NSHE 022 21FEB06 PHASE RENAME TO B VERSION AND RELINK                         
* TKLU 023 28FEB06 <DU01-4941> - PREPARATIONS FOR ESTIMTE REQUESTS              
* TKLU 024 03MAR06 <DU01-4941> - INITIAL CALLS FOR ESTIMATES AND JOBS           
* TKLU 025 09MAR06 GENERAL MCS ROUTINE: ADD ACCOUNT TO LIMIT LIST               
* TKLU 026 16MAR06 <LO01-5021> - NEW JOBS DOWNLOAD SERVER                       
* TKLU 027 27MAR06 <DU01-4941> - ESTIMATE SEARCH DOWNLOAD                       
* TKLU 028 11APR06 IMPLEMENT DDLINK UNWIND AT EXIT PLUS US CHANGES              
* TKLU 029 20APR06 <LO01-5379> PROGRAM NUMBER FROM 22 TO 24                     
* TKLU 030 28APR06 <DU01-4941> ESTIMATE RECONCILE REQUEST                       
* TKLU 031 04MAY06 ADD ACMCS20 GENERAL SERVER + NEW VAT CODE D/LOAD             
* TKLU 032 11MAY06 <DU01-4757> - NEW ESTIMATE APPROVERS REQUEST                 
* TKLU 033 16MAY06 <DU01-5456> - REPORT FORMAT DOWNLOAD                         
* TKLU 034 29MAY06 TRANSFER CURRENCY AND OFFICE D/LOAD FROM 11 TO 20            
* TKLU 035 04JUL06 <DU01-5577> 'APPROVE AN ACCOUNT RECORD' UPLOAD               
* TKLU     10JUL06 PREPARATIONS FOR 'JOB BAG' DOWNLOAD                          
* TKLU 036 28AUG06 <DU01-5733> 'GENERAL APPROVAL OVERVIEW' UPLOAD               
* TKLU 037 04SEP06 <DU01-5768> ADD SERVER/REQUESTS FOR INV. APPROVALS           
* TKLU 038 11SEP06 PREPARATIONS FOR QUICKREPORTS AND GENERAL INIT CALLS         
* NSHE 039 15SEP06 ADD CHANGES NEEDED FOR QA DATA                               
* NSHE 040 22SEP06 CHANGE QA DATE TO 01/07/2006                                 
* TKLU 041 26SEP06 NEW JOB AUDIT DOWNLOAD (JOBS PHASE 2)                        
*          27SEP06 PREPARATIONS FOR MCS -> BRA                                  
* TKLU 042 10OCT06 MERGE IN US CHANGES FROM JIM SHEA                            
* TKLU     12OCT06 <DU01-4757> - NEW ESTIMATE ROW COPY UPLOAD                   
* TKLU 043 20OCT06 CON/SEC PERSON SEARCH MODULE                                 
* TKLU 044 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* TKLU 045 03JAN07 <DU01-5771> - JOB APPROVER AND CREATOR D/LOAD                
* TKLU 046 10JAN07 <DU01-5756> - NEW ORDER UPLOAD (FOR FUTURE USE)              
* TKLU 047 09FEB07 <LO01-6136> - NEW RESOURCES UP- AND DOWNLOAD SERVERS         
* TKLU 048 07MAR07 MERGE IN US CHANGES FROM JIM SHEA                            
* TKLU 049 23APR07 NEW 'ORDER SINGLE DISPLAY D/LOAD' REQUEST                    
*                  and new US merger                                            
* TKLU 050 23MAY07 New 'Limit list download' and 'order allocation #            
*                  list' (latest JNEW style)                                    
* TKLU 051 05JUN07 <DU01-6519> New 'Order Combined Download' + new              
*                  RESOURCES module up/downloads (NSHE)                         
*          11JUN07 US merger (JSHA)                                             
* NSHE 053 22JUN07 Add campaign search and list to server 26                    
*                  Move control person search to server 26                      
* TKLU 054 07AUG07 US merger                                                    
* TKLU 055 04OCT07 <DU01-6579> Switch to new NARRATIVE D/Load (ACBRA26)         
* TKLU 056 09OCT07 <DU01-6342> New Invoices Phase 2 download and upload         
* NSHE 057 17OCT07 Stop zipping data as over LAN                                
* TKLU     14NOV07 <DU01-5915> Suspense a/c call for internal orders            
* JFOS 058 23NOV07 <DU01-5768> Invoices audit download call                     
* NSHE 059 14DEC07 Reinstate zipping                                            
* NSHE 060 04JAN08 Merge US changes                                             
* NSHE 063 31JAN08 Remove 3rd routine module                                    
* TKLU 064 25FEB08 US merger (JSHA)                                             
*          04MAR08 <LO01-7220> New send mail for password request               
* NSHE 066 29JUL08 <LO01-7859> Change expense initial call server               
* TKLU     06AUG08 <DU01-7687> New Job/Status upload request                    
* JFOS 067 21JAN09 <LO01-7636> Update Invoices upload entries                   
* NSHE 068 19MAY09 New calls for person upload and XT logon download            
* NSHE 072 19SEP09 Expenses module to run off line                              
* NSHE 073 26JAN10 Run jobs list offline                                        
* NRAK 074 22JUN10 <PR000235> Add account summary download                      
* JFOS 075 25AUG10 <PR000575> Add (new) Invoice display download                
* NRAK 076 25FEB11 <PR000607> MOVE 81 (NEW LIST/SEARCH) OFFLINE                 
* NRAK 078 23MAR11 <PR000031> Add His/Salary upload                             
* JFOS 080 13APR11 <PR000031> Add 'CSV file uploads' download                   
* SMAN 081 26MAY11 <PR000031> Add Salary Posting upload                         
* NSHE 082 18MAY11 Build 32 changes                                             
* MPEN 083 26JUL11 <BR18306D> MOVE A#ETYL OFFLINE                               
* JFOS 084 15AUG11 <PR002088> Add Hourly Rates download                         
* NSHE     20OCT11 <PR002306> Add calendar download                             
* NSHE 085 01NOV11 <BR45112L> Ensure country code is set correctly              
* MPEN 086 10NOV11 <PR0002361> Estimates combined download                      
* NSHE 087 09MAY12 Use own area for WRKIAREC                                    
* NSHE 088 02OCT12 Change overlay for order audit                               
* NSHE 089 19JUL13 Removing dead calls                                          
* NSHE 090 09AUG13 Run time initial call under runner                           
* NSHE 091 02APR14 Run job initial call under runner                            
* MPEN 092 30MAY14 Make password reminder offline                               
* NSHE 093 03OCT14 <DSRD-2733> Estimate audit download added                    
* TKLU     04Nov14 <RD004703> New server for Aura Orders List & Search          
* MPEN 094 30Jan15 <DSRD-5884> New order approval lookup                        
* NSHE 095 09Jan15 <DSRD-5322> Change orders initial call to be runner          
* JFOS     02Feb15 <PCA-1234> New BACS Email status upload call                 
* TKLU     11Feb15 <RD005066> Add A#OUCA 'Order Uncommitted' call               
* NSHE 096 08Apr15 <DSRD-6823> Run person security list offline                 
* NRAK 097 01May15 <DSMHUB-103> more wmp space (use ddlink default)             
* NSHE 098 08May15 <DSRD-7143> Run estimate display online                      
* NRAK 099 15May15 <DSMHUB-103> more wkfile space (relink)                      
* NSHE 100 01Dec15 <DSRD-9614> New items download for Aura                      
* MPEN 101 04Feb16 <DSPCA-2270> New account/media upload                        
* MPEN     18May16 <DSMU-0101> New PdfGen request                               
* NSHE     20May16 DSRD-11237 New add/delete person to account upload           
* TKLU 102 26Oct16 <PCA02523> New TMS Rates upload via ACBRA2C                  
* NSHE 103 25Oct16 DSRD-13710 New A#MOAD download                               
* NSHE 104 08Nov16 DSRD-13861 Run expense audit online - storage                
*                  corruption offline and need time to review                   
* MPEN 105 29Nov16 <DSRD-14212> Province filter for Canadian tax lis            
* MPEN 106 01Feb17 <DSRD-14484> Copy studio changes from US                     
* NSHE 107 12Jul17 DSRD-13730/1/2 rewrite of downloads for jobs etc             
* TKLU 109 16Jan17 DSPCA-2818 Add GM Accounting Person Upload                   
* NSHE     14Dec17 DSRD-17746 Change to call work code list offline             
* TKLU     14Feb18 DSRD-17967 New ACBRA10 Estimate Electronic Signature         
*                             upload server                                     
*          15Mar18 DSRD-17832 A#TLUPL - may remove on release                   
* ABID 110 21MAY18 <DSRD-18387> NEW STYLE CODE FOR APPROVER LIST                
* NSHE 111 20Aug18 <DSRD-19967> Remove old downloads used in ACBRA11            
* MPEN 112 08Oct18 <DSRD-20383> Re-enable old MF acc list/display/srch          
* ABID 113 11OCT18 <DSPCA-2854> ADD ENTRY FOR RUNNER TO UPDATE WORKER           
*                               FILE POSTINGS FOR ACCOUNTING.                   
* ABID 113 11OCT18              SEQUENCE MAPCODE CORRECTLY AND ADD              
*                               COMMENTS                                        
* NSHE 114 03Jan19 DSRD-21174 Ensure account list/search runs offline           
* MPEN 115 02May19 DSRD-22427 New Order Electronic sig upload                   
* YNGX 116 09Jul20 DSRD-26921 Make InvApp Initial call offline                  
* ABID 117 17JUL20 DSPCA-2854 ADD ENTRY FOR CONTRA REQ FOR CREDITOR TRN         
* MPEN     19AUG20 DSRD-27177 Time day comments                                 
* MPEN     01SEP20 DSRD-26699 Timesheet line manager                            
* MPEN 118 27OCT20 DSRD-26698 Make timesheet line manager offline               
*                                                                               
***********************************************************************         
ACBRA00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**BO00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         OI    GIND1,GIONLINE      SET RUNNING ONLINE                           
                                                                                
         ST    R1,AFAPARM                                                       
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   AACCFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         L     RE,20(R1)           RE=A(FACPAK EXTRA INFORMATION)               
         MVC   CUCTRY,1(RE)        SET AGENCY COUNTRY                           
         OC    CUCTRY,CUCTRY       DO WE HAVE A VALUE                           
         JNZ   INIT00              YES - OTHERWISE SET DEFAULT                  
*&&US*&& MVI   CUCTRY,CTRYUSA      FORCE USA                                    
*&&UK*&& MVI   CUCTRY,CTRYGBR      FORCE GREAT BRITAIN                          
                                                                                
INIT00   MVC   CULANG,3(RE)        LANGUAGE                                     
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         MVC   CUXCPY,ATIOB        SET COMPANY CODE                             
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)        SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)        SET A(ROUTINE OVERLAY 2)                     
*                                                                               
TRAP     L     RF,ACOMFACS                                                      
         OC    CCALLOV-COMFACSD(4,RF),CCALLOV-COMFACSD(RF)                      
         JZ    TRAPDIE                                                          
         ICM   RF,15,CCALLOV-COMFACSD(RF)                                       
         CLI   0(RF),0                                                          
         JNE   TRAPX                                                            
TRAPDIE  DC    H'0'                                                             
TRAPX    DS    0H                                                               
*                                                                               
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         MVI   GIND2,GI2NONE       NO SPECIFIC MODE SELECTED                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
*&&UK                                                                           
         CLI   CUTSYS,X'73'        If QA system                                 
         BNE   INIT02               set fake date for brandocean                
         CLC   CUAALF,=C'BA'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'BB'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'BC'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'AD'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'AA'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'AB'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'AC'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'CA'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'CB'                                                    
         BE    INIT02                                                           
         CLC   CUAALF,=C'CC'                                                    
         BE    INIT02                                                           
         MVC   TEMP(3),=X'A60701'                                               
         GOTOR VDATCON,DMCB,(1,TEMP),(3,TODAYB)                                 
         GOTOR (RF),(R1),,(2,TODAYC)                                            
         GOTOR (RF),(R1),,(1,TODAYP)                                            
         GOTOR (RF),(R1),,(0,TODAYF)                                            
         B     INIT04                                                           
*&&                                                                             
INIT02   GOTOR VDATCON,DMCB,(5,0),(3,TODAYB)                                    
         GOTOR (RF),(R1),,(2,TODAYC)                                            
         GOTOR (RF),(R1),,(1,TODAYP)                                            
         GOTOR (RF),(R1),,(0,TODAYF)                                            
                                                                                
***********************************************************************         
* INITIALISE CONTROL BLOCKS AND CALL FALINK OR DDLINK                 *         
***********************************************************************         
                                                                                
INIT04   LAY   R2,FABLK                                                         
         USING FALINKD,R2                                                       
         ST    R2,AFABLK                                                        
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
*        OI    FALAINDS,FALAINDZ                                                
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         LAY   R0,SVFALINK         A(FALINK SAVED STORAGE)                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMSA)                                           
*        MVC   FALAPGS,=AL4(FALATMSB)                                           
                                                                                
         LR    R1,RD               ACQUIRE W/S FOR DDLINK APPLICATIONS          
         AHI   R1,XWORKL                                                        
         L     RE,4(RD)                                                         
         ST    RE,4(R1)                                                         
         ST    R1,8(RE)                                                         
         LR    R2,RD                                                            
         USING XWORKD,R2                                                        
         LR    RD,R1                                                            
                                                                                
         LA    R0,XWORKD           CLEAR THE ACQUIRED STORAGE                   
         LHI   R1,XWORKL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
                                                                                
         LA    R0,LP_D             SET A(LP_D) IN GLOBAL W/S                    
         ST    R0,ALP                                                           
                                                                                
***********************************************************************         
* INITIALISE ONLINE RUNNER VALUES (RUNFACSD)                          *         
***********************************************************************         
                                                                                
         LAY   R0,SVSERVER                                                      
         STCM  R0,15,RSVRSAVE                                                   
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
***********************************************************************         
* INITIALISE DDLINK CONTROL BLOCK VALUES (LP_D)                       *         
***********************************************************************         
                                                                                
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
*        OI    LP_AIND1,LP_AICOM                                                
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         LAY   R0,SVDDLINK                                                      
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LAY   R0,SECBLK                                                        
         ST    R0,LP_ASECD         SET A(SECRET BLOCK)                          
         LARL  R0,DDNDX2           ASSUME SPECIAL FLAVOR                        
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         JE    INIT006                                                          
         LARL  R0,DDNDX                                                         
INIT006  STCM  R0,15,LP_ANDX                                                    
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LAY   R0,LINKWORK                                                      
         STCM  R0,15,LP_AWORK                                                   
*&&US                                                                           
         LAY   R0,WMP                                                           
         ST    R0,LP_AUWMP         SET A(WMP) NEED >6K ONLINE                   
         LHI   R0,L'WMP                                                         
         STH   R0,LP_WMPL                                                       
*&&                                                                             
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         MVC   LP_AFBLK,AFABLK                                                  
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         ALWAYS PASS A(W/S) AS BLOCK 1                
                                                                                
***********************************************************************         
* INITIALISE WRKIO CONTROL BLOCK VALUES (WRKIOD)                      *         
***********************************************************************         
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIABUF,ATIA                                                    
         LAY   R0,DDWRKIAR                                                      
         STCM  R0,15,WRKIAREC                                                   
         LHI   R0,L'DDWRKIAR                                                    
         STH   R0,LP_RECL                                                       
                                                                                
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
                                                                                
         TM    GIND1,GIUNWIND      TEST UNWIND VIA $ABEND                       
         BZ    EXIT                                                             
         NI    GIND1,FF-GIUNWIND                                                
         DC    H'0',C'$ABEND'                                                   
                                                                                
EXIT     OI    LNKSERVH+(FHAT-FHD),FHATMO                                       
         OI    LNKSERVH+(FHOI-FHD),FHOITR                                       
         OI    LNKINPH+(FHOI-FHD),FHOICU+FHOITR                                 
         XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         MACRO                                                                  
         BUILD_DDNDX &FLAVOR=REGULAR                                            
.*                                                                              
.* A MASTER INDEX TABLE IS GENERATED BY *CALLING* MACRO BUILD_DDNDX             
.*                                                                              
         LKMMI H,ACCSYSQ                    DDLINK MASTER MAP INDEX             
*                                                                               
*  MAPCODE - X'0001'                                                            
         LKMMI D,A#INIT,O#SRVR26,AC#INIDL,RUNNER=B INITIAL ORD D/LOAD           
*  MAPCODE - X'0002'   - ORDER DISPLAY CALL - ACBRA1F/2E                        
*  MAPCODE - X'0003'                                                            
         LKMMI D,A#GRPL,O#SRVR28,AC#GRPLS   GROUP LIST LIST D/LOAD              
*  MAPCODE - X'0004'                                                            
         LKMMI U,A#PRAC,O#SRVR2A,(*,PRACCLIT)  ADD/DELETE PERSON AC UPL         
*  MAPCODE - X'0005'                                                            
         LKMMI UO,A#TLUPL,O#SRVR1E,(*,TLUPLLIT) Time Line Upload                
*  MAPCODE - X'0006'                                                            
         LKMMI D,A#ETYL,O#SRVR26,AC#ETYPS,RUNNER=B EXPENDIT.D/LOAD              
*  MAPCODE - X'0007'                                                            
         LKMMI D,A#WCOL,O#SRVR2D,AC#WCLST,RUNNER=B WC LIST D/LOAD               
*  MAPCODE - X'0008'                                                            
         LKMMI UO,A#ESTES,O#SRVR10,(*,ESTESLIT) Est.Electr.Sign.Upload          
*  MAPCODE - X'0009'                                                            
         LKMMI D,A#CURR,O#SRVR26,AC#CURRC,RUNNER=B CURRENCY LIST D/LOAD         
*  MAPCODE - X'000A'                                                            
         LKMMI UO,A#WRKUP,O#SRVR2B,(*,WORKUPLD)  WORKER FILE UPLOAD             
*  MAPCODE - X'000B'                                                            
         LKMMI D,A#OFFL,O#SRVR26,AC#OLIST   OFFICE LIST D/LOAD                  
*  MAPCODE - X'000C'                                                            
         LKMMI UO,A#OESI,O#SRVR2F,(*,ORDESLIT) Ord.Electr.Sign.Upload           
*  MAPCODE - X'000D'                                                            
         LKMMI D,A#ACCL,O#SRVR11,AC#LSTOA,RUNNER=B AC LIST D/LOAD               
*  MAPCODE - X'000E'                                                            
         LKMMI D,A#SRCH,O#SRVR11,AC#ACNSR,RUNNER=B   AC SEARCH D/LOAD           
*  MAPCODE - X'000F'                                                            
         LKMMI D,A#XDFA,O#SRVR1F,AC#XDATA   APPL. XTRA DATA FLDS D/LOAD         
*  MAPCODE - X'0010'      - ORDER EXTRA DATA DOWNLOAD - ACBRA1F                 
*  MAPCODE - X'0011'      - ORDER TEXT/ITEM/ARTICLE DOWNLOAD - ACBRA1F          
*  MAPCODE - X'0012'                                                            
         LKMMI D,A#ITML,O#SRVR1D,(*,ITEMLLIT),RUNNER=B ITEM LIST DOWN           
*  MAPCODE - X'0013'                                                            
         LKMMI U,A#ALCK,O#SRVR21,(*,ACLKULOD)    Account lock upload            
*  MAPCODE - X'0014'                                                            
         LKMMI D,A#APPL,O#SRVR2D,AC#APRVR,RUNNER=B APPROVER LIST D/LOAD         
*  MAPCODE - X'0015'                                                            
         LKMMI D,A#OAUD,O#SRVR1F,AC#ACTY    ORDER ACTIVITY D/LOAD               
*  MAPCODE - X'0016'                                                            
         LKMMI D,A#TINI,O#SRVR27,(*,TMINILIT),RUNNER=B TIME INI D/LOAD          
*  MAPCODE - X'0017'                                                            
         LKMMI UO,A#CNTUP,O#SRVR19,(*,CNTCRDLD)  CONTRA CREDITOR UPLOAD         
*  MAPCODE - X'0018'                                                            
         LKMMI D,A#OVTM,O#SRVR26,(*,TMOVRLIT) TIMESHEET OVERDUE                 
*  MAPCODE - X'0019'                                                            
         LKMMI D,A#CALD,O#SRVR27,(*,CALDWLIT)    Calendar download              
*  MAPCODE - X'001A'                                                            
         LKMMI D,A#NARR,O#SRVR26,AC#NRTV    (NEW) NARRATIVE D/LOAD              
*  MAPCODE - X'001B'                                                            
         LKMMI D,A#TIML,O#SRVR12,(*,TMLSTLIT),RUNNER=B TIME LIST                
*  MAPCODE - X'001C'                                                            
         LKMMI D,A#TSTL,O#SRVR12,(*,TMSRCLIT),RUNNER=B  TIME SEARCH             
*  MAPCODE - X'001D'                                                            
         LKMMI D,A#TSUM,O#SRVR12,AC#SMY,RUNNER=B  TIME SUMMARIES                
*  MAPCODE - X'001E'                                                            
         LKMMI D,A#TSAT,O#SRVR12,AC#TSAUD,RUNNER=B T/S AUDIT TRAIL              
*  MAPCODE - X'001F'                                                            
         LKMMI D,A#UFDL,O#SRVR2D,AC#USER,RUNNER=B  USER FIELD DOWNLOAD          
*  MAPCODE - X'0020'                                                            
         LKMMI D,A#ADTL,O#SRVR11,AC#ACDTL,RUNNER=B  ACCOUNT DETAILS             
*  MAPCODE - X'0021'                                                            
         LKMMI D,A#CLID,O#SRVR26,AC#EXRIN   CLAIM INITIAL DOWNLOAD              
*  MAPCODE - X'0022'                                                            
         LKMMI D,A#CLLD,O#SRVR15,AC#EXRCL,RUNNER=B CLAIM LIST DOWNLOAD          
*  MAPCODE - X'0023'                                                            
         LKMMI D,A#CMCD,O#SRVR15,(*,CLDRTLIT),RUNNER=B MILEAGE CALC             
*  MAPCODE - X'0024'                                                            
         LKMMI D,A#CLSD,O#SRVR15,AC#EXRCS,RUNNER=B CLAIM SRCH                   
*  MAPCODE - X'0025'                                                            
         LKMMI D,A#MOAD,O#SRVR26,AC#MOA          MOA DOWNLOAD                   
*  MAPCODE - X'0026'                      - AVAILBALE                           
*  MAPCODE - X'0027'                      - AVAILBALE                           
*  MAPCODE - X'0028'                                                            
         LKMMI D,A#CLAD,O#SRVR15,AC#CHNGS,RUNNER=B CLAIM AUDIT                  
*  MAPCODE - X'0029'                      - AVAILBALE                           
*  MAPCODE - X'002A'                      - AVAILBALE                           
*  MAPCODE - X'002B'                                                            
         LKMMI D,A#FNVD,O#SRVR26,AC#FNAME   FILTER NAMES/VALUES D/LOAD          
*  MAPCODE - X'002C'                                                            
         LKMMI D,A#JLSD,O#SRVR2D,AC#JOBLI,RUNNER=B JOB LST AND SEARCH           
*  MAPCODE - X'002D'                      - AVAILBALE                           
*  MAPCODE - X'002E'                                                            
         LKMMI D,A#TEAP,O#SRVR2D,AC#TEAPP           EMAIL DOWNLOAD              
*  MAPCODE - X'002F'                                                            
         LKMMI D,A#MEDC,O#SRVR26,AC#MEDC    MEDIA CODE DOWNLOAD                 
*  MAPCODE - X'0030'                                                            
         LKMMI D,A#EIFA,O#SRVR17,AC#ERIFA,RUNNER=B  EST INI FOR ADD             
*  MAPCODE - X'0031'                                                            
         LKMMI U,A#EMUP,O#SRVR18,AC#ERMUP   EST MAIN UPLOAD                     
*  MAPCODE - X'0032'                                                            
         LKMMI D,A#ELIS,O#SRVR17,AC#ERLIS,RUNNER=B  EST LIST                    
*  MAPCODE - X'0033'                                                            
         LKMMI D,A#EDIS,O#SRVR17,AC#ERDIS,RUNNER=B   EST DISPLAY                
*  MAPCODE - X'0034'                                                            
         LKMMI D,A#ESCH,O#SRVR26,AC#ERSCH   EST SCHEME DOWNLOAD                 
*  MAPCODE - X'0035'                                                            
         LKMMI D,A#EINI,O#SRVR26,(*,ESINILIT)  EST INIT CALL                    
*  MAPCODE - X'0036'                                                            
         LKMMI D,A#JINI,O#SRVR26,(*,JBINILIT),RUNNER=B JOB INIT CALL            
*  MAPCODE - X'0037'                                                            
         LKMMI U,A#AALL,O#SRVR16,(*,ADDACLIT)  ADD A/C TO LIMIT LIST            
*  MAPCODE - X'0038'                                                            
         LKMMI D,A#ESRC,O#SRVR17,AC#ESTSR,RUNNER=B EST SEARCH DOWNLOAD          
*  MAPCODE - X'0039'                      - AVAILBALE                           
*  MAPCODE - X'003A'                                                            
         LKMMI D,A#VCDL,O#SRVR20,AC#VATRS   VAT CODE DOWNLOAD                   
*  MAPCODE - X'003B'                                                            
         LKMMI D,A#EAPP,O#SRVR17,AC#EAPVL,RUNNER=B EST APPROVERS D/LOAD         
*  MAPCODE - X'003C'                                                            
         LKMMI D,A#REFO,O#SRVR26,AC#FRMAT   REPORT FORMATS DOWNLOAD             
*  MAPCODE - X'003D'                      - AVAILBALE                           
*  MAPCODE - X'003E'                      - AVAILBALE                           
*  MAPCODE - X'003F'                      - AVAILBALE                           
*  MAPCODE - X'0040'                                                            
*&&UK*&& LKMMI D,A#IAIN,O#SRVR26,AC#IAINI,RUNNER=B INV APPR INITIAL D/L         
*  MAPCODE - X'0041'                                                            
         LKMMI D,A#FUMDL,O#SRVR1C,(*,FUPDLLIT) File Uploads download            
*  MAPCODE - X'0042'                                                            
         LKMMI D,A#ESAUD,O#SRVR1D,(*,ESAUILIT),RUNNER=B EST AUDIT DOWN          
*  MAPCODE - X'0043'                                                            
*&&UK*&& LKMMI D,A#IALI,O#SRVR22,AC#IALST,RUNNER=B INV APPR LIST D/L            
*  MAPCODE - X'0044'                                                            
*&&UK*&& LKMMI D,A#IASR,O#SRVR22,AC#IASRC,RUNNER=B INV APPR SEARCH D/L          
*  MAPCODE - X'0045'                                                            
*&&UK*&& LKMMI D,A#QRLL,O#SRVR20,AC#QV        QUICK REPORTS LIMLIST D/L         
*&&US*&& LKMMI D,A#QRLL,O#SRVR20,(*,QWKVWLIT) QUICK REPORTS LIMLIST D/L         
*  MAPCODE - X'0046'                                                            
         LKMMI D,A#GINI,O#SRVR26,AC#GEN     GENERAL INIT D/LOAD                 
*  MAPCODE - X'0047'                                                            
         LKMMI D,A#JAUD,O#SRVR2D,(*,JBAUDLIT) JOB AUDIT DL                      
*  MAPCODE - X'0048'                                                            
         LKMMI U,A#EROW,O#SRVR18,AC#ITMR    ESTIMATES ROW COPY UPLOAD           
*  MAPCODE - X'0049'                                                            
         LKMMI D,A#CSPS,O#SRVR26,AC#PSRCH,RUNNER=B  PERSON SEARCH D/L           
*  MAPCODE - X'004A'                                                            
         LKMMI D,A#JAPP,O#SRVR2D,(*,JBAPPLIT)    JOB APPROVER DL                
*  MAPCODE - X'004B'                                                            
         LKMMI U,A#OUPL,O#SRVR13,(*,ORUPLLIT)    NEW ORDER UPLOAD               
*  MAPCODE - X'004C'                                                            
         LKMMI D,A#RSTDL,O#SRVR24,(*,RUSTRLIT),RUNNER=B RES. STATUS REP         
*  MAPCODE - X'004D'                                                            
         LKMMI D,A#RSRDL,O#SRVR24,(*,RURESLIT),RUNNER=B RES SEARCH              
*  MAPCODE - X'004E'                      - AVAILBALE                           
*  MAPCODE - X'004F'                      - AVAILBALE                           
*  MAPCODE - X'0050'                      - AVAILBALE                           
*  MAPCODE - X'0051'                                                            
         LKMMI D,A#RMYDL,O#SRVR24,(*,RUMYTLIT),RUNNER=B TASK/SRCH               
*  MAPCODE - X'0052'                                                            
         LKMMI D,A#RDEDL,O#SRVR24,(*,RUDETLIT),RUNNER=B RESOURCES DET           
*  MAPCODE - X'0053'                                                            
         LKMMI D,A#RAUDL,O#SRVR24,(*,RUAUDLIT),RUNNER=B RES AUDIT               
*  MAPCODE - X'0054'                                                            
         LKMMI U,A#RWKUP,O#SRVR25,(*,RUWRKLIT)   RESOURCES WORK UPLOAD          
*  MAPCODE - X'0055'                                                            
         LKMMI D,A#TMRLDL,O#SRVR24,(*,RUROLLIT),RUNNER=B TEAM/ROLE              
*  MAPCODE - X'0056'      - ROLE DOWNLOAD - ACBRA24                             
*  MAPCODE - X'0057'                                                            
         LKMMI D,A#KSTMDL,O#SRVR24,(*,RUKSTLIT),RUNNER=B KEYST/TEMPL            
*  MAPCODE - X'0058'      - TEMPLATE - ACBRA24                                  
*  MAPCODE - X'0059'                                                            
         LKMMI U,A#OUTUP,O#SRVR25,(*,RUOTILIT)   OUT OF OFF UPLOAD ITM          
*  MAPCODE - X'005A'                                                            
         LKMMI D,A#OUTDL,O#SRVR24,(*,RUOTDLIT),RUNNER=B OUT OF OFFICE           
*  MAPCODE - X'005B'                                                            
         LKMMI D,A#CAMDL,O#SRVR26,(*,RUCAMLIT)   CAMPAIGN LIST                  
*  MAPCODE - X'005C'                                                            
         LKMMI D,A#OCDL,O#SRVR1F,(*,RUOCDLIT)    ORDER COMBINED D/LOAD          
*  MAPCODE - X'005D'      - ITEMS      - ACBRA1F                                
*  MAPCODE - X'005E'      - ITEMS TEXT - ACBRA1F                                
*  MAPCODE - X'005F'                                                            
         LKMMI U,A#OUTHD,O#SRVR25,(*,RUOTHLIT)   OUT OF OFF UPLOAD HDR          
*  MAPCODE - X'0060'                                                            
         LKMMI U,A#OUTTR,O#SRVR25,(*,RUOTTLIT)   OUT OF OFF UPLOAD TRL          
*  MAPCODE - X'0061'                                                            
         LKMMI D,A#TIMDIS,O#SRVR27,(*,TMDISLIT),RUNNER=B   T/S display          
*  MAPCODE - X'0062'      - TIMELINE INFORMATION - ACBRA27                      
*  MAPCODE - X'0063'                      - AVAILBALE                           
*  MAPCODE - X'0064'                                                            
         LKMMI UO,A#HISSAL,O#SRVR14,(*,HISLHLIT) His/Salary u/l-HDR             
*  MAPCODE - X'0065'                                                            
         LKMMI UO,R#HSLELE,O#SRVR14,(*,HISLELIT) His/Salary u/l-DTL             
*  MAPCODE - X'0066'                                                            
         LKMMI UO,R#HSLTRL,O#SRVR14,(*,HISLTLIT) His/Salary u/l-TRL             
*  MAPCODE - X'0067'                                                            
*&&UK*&& LKMMI D,A#IAUDL,O#SRVR22,(*,IAUDTRLD),RUNNER=B Inv audit d/l           
*  MAPCODE - X'0068'                                                            
         LKMMI D,A#HRRAT,O#SRVR26,(*,EHRRATES),RUNNER=Y Est. Hrly Rates         
*  MAPCODE - X'0069'                                                            
         LKMMI D,A#SMPW,O#SRVR20,(*,SMPWDLOD),RUNNER=Y Password rem             
*  MAPCODE - X'006A'                      - AVAILBALE                           
*  MAPCODE - X'006B'                                                            
*&&UK*&& LKMMI UO,A#IHDR,O#SRVR23,(*,IUPSTUPH)   Invoice posting header         
*  MAPCODE - X'006C'                                                            
*&&UK*&& LKMMI UO,A#IITM,O#SRVR23,(*,IUPSTUPI)   Invoice items                  
*  MAPCODE - X'006D'                                                            
*&&UK*&& LKMMI UO,A#IPOS,O#SRVR23,(*,IUPSTUPP)   Invoice posting items          
*  MAPCODE - X'006E'                                                            
*&&UK*&& LKMMI UO,A#IWC,O#SRVR23,(*,IUPSTUPW)    WC/Narr item                   
*  MAPCODE - X'006F'                                                            
*&&UK*&& LKMMI UO,A#ITRL,O#SRVR23,(*,IUPSTUPT)   Invoice posting trailr         
*  MAPCODE - X'0070'                                                            
         LKMMI D,A#XTLG,O#SRVR26,(*,XTINFLIT)    XT logon info                  
*                                                                               
* NEW ETYPE DOWNLOAD - USED ONLY IN INVOICE LOG                                 
*                                                                               
*  MAPCODE - X'0071'                                                            
         LKMMI D,A#ETYP,O#SRVR26,(*,ETYPELIT)    New expenditure type           
*  MAPCODE - X'0072'      - ETYPE EXPENSE ACCOUNT INFO  - ACBRA26               
*  MAPCODE - X'0073'      - ETYPE SUPPLIER ACCOUNT INFO - ACBRA26               
*  MAPCODE - X'0074'      - ETYPE WORK CODE INFO        - ACBRA26               
*  MAPCODE - X'0075'                                                            
         LKMMI UO,A#POHD,O#SRVR1A,(*,SALPHLIT) Salary posting u/l HDR           
*  MAPCODE - X'0076'                                                            
         LKMMI UO,A#PITM,O#SRVR1A,(*,SALPPLIT) Salary posting u/l PSTG          
*  MAPCODE - X'0077'                                                            
         LKMMI UO,A#PWKC,O#SRVR1A,(*,SALPILIT) Salary posting u/l ITEM          
*  MAPCODE - X'0078'                                                            
         LKMMI UO,A#POTL,O#SRVR1A,(*,SALPTLIT) Salary posting u/l TRL           
*  MAPCODE - X'0079'                      - AVAILBALE                           
*  MAPCODE - X'007A'                                                            
*        LKMMI U,A#ACCU,O#SRVR2B,(*,ACUPDLIT)    ACCOUNT UPLOAD                 
*  MAPCODE - X'007B'                                                            
*        LKMMI U,A#MEDU,O#SRVR2B,(*,ACUPDLIT)    MEDIA UPLOAD                   
*  MAPCODE - X'007C'                                                            
*        LKMMI U,A#ATRL,O#SRVR2B,(*,ACUPDLIT)    TRAILER UPLOAD                 
*  MAPCODE - X'007D'                      - AVAILBALE                           
*  MAPCODE - X'007E'                      - AVAILBALE                           
*  MAPCODE - X'007F'                      - AVAILBALE                           
*                                                                               
* FILE MAINTENANCE                                                              
*                                                                               
*  MAPCODE - X'0080'                                                            
         LKMMI D,A#ACTD,O#SRVR28,(*,ACDISLIT)    New account display            
*  MAPCODE - X'0081'                                                            
         LKMMI D,A#ACTS,O#SRVR28,(*,ACLISLIT),RUNNER=Y New Account list         
*  MAPCODE - X'0082'                                                            
         LKMMI U,A#ACTU,O#SRVR29,(*,ACUPDLIT)    Account upload                 
*  MAPCODE - X'0083'                      - AVAILBALE                           
*  MAPCODE - X'0084'                                                            
         LKMMI D,A#PSEC,O#SRVR28,(*,PSDISLIT)    Person sec display             
*  MAPCODE - X'0085'     - PERSON TIMESHEET DOWNLOAD    - ACBRA28               
*  MAPCODE - X'0086'     - PERSON CREDITOR DOWNLOAD     - ACBRA28               
*  MAPCODE - X'0087'     - PERSON ACCESS DOWNLOAD       - ACBRA28               
*  MAPCODE - X'0088'     - PERSON APPROVER DOWNLOAD     - ACBRA28               
*  MAPCODE - X'0089'     - PERSON CLIENT ROLES DOWNLOAD - ACBRA28               
*  MAPCODE - X'008A'                                                            
         LKMMI D,A#PLST,O#SRVR28,(*,PELSTLIT)    PERSON LIST                    
*  MAPCODE - X'008B'           -   PERSON OR GROUP AUDIT                        
*  MAPCODE - X'008C'     - LINE MANAGER APPROVER DOWNLOAD - ACBRA2D             
         LKMMI D,A#LINM,O#SRVR2D,(*,LMANRDLD),RUNNER=Y                          
*  MAPCODE - X'008D'                      - AVAILBALE                           
*  MAPCODE - X'008E'                      - AVAILBALE                           
*  MAPCODE - X'008F'                                                            
         LKMMI U,A#5PEM,O#SRVR21,(*,BACSTLIT)    BACS email sts upload          
*                                                                               
*  NEW CLAIMS REQUESTS                                                          
*                                                                               
*  MAPCODE - X'0090'                                                            
         LKMMI UO,A#XHDR,O#SRVR16,(*,XUHDRLIT)   Expense header upload          
*  MAPCODE - X'0091'                                                            
         LKMMI UO,A#XITM,O#SRVR16,(*,XUITMLIT)   Expense item upload            
*  MAPCODE - X'0092'                                                            
         LKMMI UO,A#XXDT,O#SRVR16,(*,XUXDTLIT)   Expense extra data upl         
*  MAPCODE - X'0093'                                                            
         LKMMI UO,A#XTRL,O#SRVR16,(*,XUTRLLIT)   Expense trailer upload         
*  MAPCODE - X'0094'                      - AVAILBALE                           
*  MAPCODE - X'0095'                                                            
         LKMMI D,A#XDIS,O#SRVR15,AC#EXRCD,RUNNER=B CLAIM DISPLAY                
*  MAPCODE - X'0096'      - DOWNLOAD EXPENSE ITEM       - ACBRA15               
*  MAPCODE - X'0097'      - DOWNLOAD EXPENSE ITEM XDATA - ACBRA15               
*  MAPCODE - X'0098'      - DOWNLOAD EXPENSE TRAILER    - ACBRA15               
*  MAPCODE - X'0099'                      - AVAILBALE                           
*                                                                               
* TIME UPLOAD                                                                   
*                                                                               
*  MAPCODE - X'009A'                                                            
         LKMMI UO,A#THDR,O#SRVR1B,(*,RUHDRLIT)   time header upload             
*  MAPCODE - X'009B'                                                            
         LKMMI UO,A#TTIM,O#SRVR1B,(*,RUTIMLIT)   time item upload               
*  MAPCODE - X'009C'                                                            
         LKMMI UO,A#TMAT,O#SRVR1B,(*,RUMATLIT)   time materials upload          
*  MAPCODE - X'009D'                                                            
         LKMMI UO,A#TDNR,O#SRVR1B,(*,RUMATLIT)   time day commts upload         
*  MAPCODE - X'009E'                      - AVAILBALE                           
*  MAPCODE - X'009F'                                                            
         LKMMI UO,A#TTRL,O#SRVR1B,(*,RUTRLLIT)   time trailer upload            
*                                                                               
* FILE MAINTENANCE                                                              
*                                                                               
*  MAPCODE - X'00A0'                                                            
         LKMMI U,A#PHDR,O#SRVR2A,(*,PUHDRLIT) PERSON HEADER - UPLOAD            
*  MAPCODE - X'00A1'                                                            
         LKMMI U,A#PCLI,O#SRVR2A,(*,PUCLLLIT) PERSON CLIENT ACCESS              
*  MAPCODE - X'00A2'                                                            
         LKMMI U,A#PMED,O#SRVR2A,(*,PUMELLIT) PERSON MEDIA ACCESS               
*  MAPCODE - X'00A3'                                                            
         LKMMI U,A#PEXP,O#SRVR2A,(*,PUEXLLIT) PERSON EXPT. TYPE ACCS            
*  MAPCODE - X'00A4'                                                            
         LKMMI U,A#PNCL,O#SRVR2A,(*,PUNCLLIT) PERSON NON CLIENT ACCESS          
*  MAPCODE - X'00A5'                                                            
         LKMMI U,A#PSTF,O#SRVR2A,(*,PU1RLLIT) PERSON STAFF ACCT ACCS            
*  MAPCODE - X'00A6'                                                            
         LKMMI U,A#PWC,O#SRVR2A,(*,PUWCLLIT)  PERSON WORK CODE ACCESS           
*  MAPCODE - X'00A7'                                                            
         LKMMI U,A#PREP,O#SRVR2A,(*,PURPLLIT) PERSON REPORT FORMAT ACCS         
*  MAPCODE - X'00A8'                                                            
         LKMMI U,A#PESCH,O#SRVR2A,(*,PUSCLLIT) PERSON SCHEME ACCESS             
*  MAPCODE - X'00A9'                                                            
         LKMMI U,A#PSUP,O#SRVR2A,(*,PUSULLIT)  PERSON SUPPLIER ACCESS           
*  MAPCODE - X'00AA'                                                            
         LKMMI U,A#PGRP,O#SRVR2A,(*,PUGLLLIT) PERSON GROUP LIST ACCESS          
*  MAPCODE - X'00AB'                      - AVAILBALE                           
*  MAPCODE - X'00AC'                      - AVAILBALE                           
*  MAPCODE - X'00AD'                      - AVAILBALE                           
*  MAPCODE - X'00AE'                      - AVAILBALE                           
*  MAPCODE - X'00AF'                      - AVAILBALE                           
*  MAPCODE - X'00BO'                                                            
         LKMMI U,A#PGLOAP,O#SRVR2A,(*,PUGLALIT) PERSON GLOBAL APPROVER          
*  MAPCODE - X'00B1'                                                            
         LKMMI U,A#PCLIAP,O#SRVR2A,(*,PUCLALIT) PERSON CLIENT APPROVER          
*  MAPCODE - X'00B2'                                                            
         LKMMI U,A#PNCLAP,O#SRVR2A,(*,PUNCALIT) PERSON NON CLIENT APPR          
*  MAPCODE - X'00B3'                                                            
         LKMMI U,A#PSTFAP,O#SRVR2A,(*,PU1RALIT) PERSON STAFF ACCTS APPR         
*  MAPCODE - X'00B4'                                                            
         LKMMI U,A#PI_OAP,O#SRVR2A,(*,PUIOALIT) PERSON INVICE/ORDS APPR         
*  MAPCODE - X'00B5'                                                            
         LKMMI U,A#PBUAP,O#SRVR2A,(*,PUBKALIT)  PERSON BACK UP APPROVER         
*  MAPCODE - X'00B6'                                                            
         LKMMI U,A#PROL,O#SRVR2A,(*,PUROLLIT)   PERSON ROLE UPLOAD              
*  MAPCODE - X'00B7'                      - AVAILBALE                           
*  MAPCODE - X'00B8'                      - AVAILBALE                           
*  MAPCODE - X'00B9'                      - AVAILBALE                           
*  MAPCODE - X'00BA'                      - AVAILBALE                           
*  MAPCODE - X'00BB'                      - AVAILBALE                           
*  MAPCODE - X'00BC'                      - AVAILBALE                           
*  MAPCODE - X'00BD'                      - AVAILBALE                           
*  MAPCODE - X'00BE'                      - AVAILBALE                           
*  MAPCODE - X'00BF'                                                            
         LKMMI U,A#PTRL,O#SRVR2A,(*,PUTRLLIT)   PERSON TRAILER                  
*  MAPCODE - X'00C0'          GROUP LIST UPLOAD MAPPING                         
*  MAPCODE - X'00C1'                      - AVAILBALE                           
*  MAPCODE - X'00C2'                                                            
         LKMMI D,A#STUL,O#SRVR26,(*,STUDLLIT),RUNNER=B STULST D/LOAD            
*  MAPCODE - X'00C3'                      - AVAILBALE                           
*  MAPCODE - X'00C4'                      - AVAILBALE                           
*  MAPCODE - X'00C5'                      - AVAILBALE                           
*  MAPCODE - X'00C6'                      - AVAILBALE                           
*  MAPCODE - X'00C7'                      - AVAILBALE                           
*  MAPCODE - X'00C8'                      - AVAILBALE                           
*  MAPCODE - X'00C9'                      - AVAILBALE                           
*  MAPCODE - X'00CA'                                                            
         LKMMI U,A#RATUPL,O#SRVR2C,(*,RATUPLIT)  TMS rates upload               
*  MAPCODE - X'00CB'                                                            
         LKMMI U,A#PERUPL,O#SRVR21,(*,PERUPLIT)  Acc Person upload              
*  MAPCODE - X'00CC'                      - AVAILBALE                           
*  MAPCODE - X'00CD'                      - AVAILBALE                           
*  MAPCODE - X'00CE'                      - AVAILBALE                           
*  MAPCODE - X'00CF'                      - AVAILBALE                           
*                                                                               
* TRANSACTION CALL FOR ALL ACCOUNTS                                             
*                                                                               
*  MAPCODE - X'00D0'                                                            
         LKMMI D,A#ACSD,O#SRVR30,(*,ACSUMLIT),RUNNER=Y account summ d/l         
*  MAPCODE - X'00D1'       -  ACCOUNTING DETAIL     - ACBRA30                   
*  MAPCODE - X'00D2'       -  ESTIMATES BY WORKCODE - ACBRA30                   
*  MAPCODE - X'00D3'       -  BILLING DETAIL        - ACBRA30                   
*  MAPCODE - X'00D4'       -  JOB DETAIL            - ACBRA30                   
*  MAPCODE - X'00D5'                                                            
         LKMMI D,A#ESCD,O#SRVR1D,(*,ESCDILIT),RUNNER=Y EST COMB DOWN            
*  MAPCODE - X'00D6'       -  ORDER LIST            - ACBRA30                   
*  MAPCODE - X'00D7'       -  ESTIMATE LIST         - ACBRA30                   
*  MAPCODE - X'00D8'                      - AVAILBALE                           
*  MAPCODE - X'00D9'                      - AVAILBALE                           
*  MAPCODE - X'00DA'                      - AVAILBALE                           
*  MAPCODE - X'00DB'                      - AVAILBALE                           
*  MAPCODE - X'00DC'                      - AVAILBALE                           
*  MAPCODE - X'00DD'                      - AVAILBALE                           
*  MAPCODE - X'00DE'                      - AVAILBALE                           
*  MAPCODE - X'00DF'                      - AVAILBALE                           
*                                                                               
* NEW INVOICE DISPLAY CALLS                                                     
*                                                                               
*  MAPCODE - X'00E0'                                                            
         LKMMI D,A#IDIS,O#SRVR22,(*,IDISDLIT),RUNNER=B INV display d/l          
*  MAPCODE - X'00E1'   - INVOICE DISPLAY ITEM RECORD        - ACBRA22           
*  MAPCODE - X'00E2'   - INVOICE DISPLAY WORKCODE/NARRATIVE - ACBRA22           
*  MAPCODE - X'00E3'   - INVOICE DISPLAY ORDER RECORD       - ACBRA22           
*  MAPCODE - X'00E4'   - INVOICE DISPLAY QUERY RECORD       - ACBRA22           
*  MAPCODE - X'00E5'                      - AVAILBALE                           
*                                                                               
* EXPENSE CLAIM MILEAGE RECORDS                                                 
*                                                                               
*  MAPCODE - X'00E6'                                                            
         LKMMI D,A#XMIL,O#SRVR15,(*,MILERLIT),RUNNER=B Mileage records          
*  MAPCODE - X'00E7'   - DOWNLOAD FUEL TYPE RECORD    - ACBRA15                 
*  MAPCODE - X'00E8'   - DOWNLOAD ENGINE SIZE RECORD  - ACBRA15                 
*  MAPCODE - X'00E9'   - DOWNLOAD DISTANCE RECORD     - ACBRA15                 
*  MAPCODE - X'00EA'   -                 -  AVAILBALE                           
*                                                                               
*  AURA ORDER LIST & SEARCH SERVER                                              
*                                                                               
*  MAPCODE - X'00EB'                                                            
         LKMMI D,A#AOLD,O#SRVR2E,(*,AOLSTLIT),RUNNER=B AuraOrderList            
*  MAPCODE - X'00EC'                                                            
         LKMMI D,A#ALKE,O#SRVR2E,(*,AOAPPLIT),RUNNER=B AuraOrderAppr            
*  MAPCODE - X'00ED'                                                            
         LKMMI D,A#OUCA,O#SRVR1F,AC#SHEST   Order Uncommitetd Amount            
*  MAPCODE - X'00EE'                                                            
         LKMMI D,A#PDFA,O#SRVR1D,(*,PDFGHLIT),RUNNER=B PDFGEN BILL              
*  MAPCODE - X'00EF'                      - AVAILBALE                           
         LKMMI E                                                                
*                                                                               
         MEND                                                                   
         EJECT                                                                  
                                                                                
* GENERATE TWO MASTER INDEX TABLES (BUT LP_ANDX WILL ONLY POINT TO ONE)         
*                                                                               
* THE FIRST IS THE "REGULAR" ONE                                                
*                                                                               
DDNDX    DS    0H                                                               
         BUILD_DDNDX                                                            
*                                                                               
* THIS IS THE SPECIAL ONE FOR A COUPLE OF AGENCIES                              
*                                                                               
DDNDX2   DS    0H                                                               
         BUILD_DDNDX FLAVOR=SPECIAL                                             
                                                                                
ESTESLIT DC    C'Estimate Electronic Signature Upload'                          
ORDESLIT DC    C'Order Electronic Signature Upload'                             
TLUPLLIT DC    C'Time Line Upload'                                              
TMINILIT DC    C'Time Initial Download'                                         
TMOVRLIT DC    C'Time Overdue Download'                                         
TMSRCLIT DC    C'Time Search Download'                                          
TMDISLIT DC    C'Time Display Download'                                         
CALDWLIT DC    C'Calendar Download'                                             
AOLSTLIT DC    C'Aura Order List'                                               
AOAPPLIT DC    C'Aura Orders Approver Lookup'                                   
AOSRCLIT DC    C'Aura Order Search'                                             
ACDISLIT DC    C'Account Display Download'                                      
ACLISLIT DC    C'Account list Download'                                         
ACUPDLIT DC    C'Account Upload'                                                
BACSTLIT DC    C'BACS Email Status Upload'                                      
PELSTLIT DC    C'Person list Download'                                          
PSDISLIT DC    C'Person Security Display Download'                              
PUHDRLIT DC    C'Person header Upload'                                          
PUCLLLIT DC    C'Person client access Upload'                                   
PUMELLIT DC    C'Person media access Upload'                                    
PUEXLLIT DC    C'Person expenditure type access Upload'                         
PUNCLLIT DC    C'Person non-client access Upload'                               
PU1RLLIT DC    C'Person staff access Upload'                                    
PUWCLLIT DC    C'Person workcode access Upload'                                 
PURPLLIT DC    C'Person report access Upload'                                   
PUSCLLIT DC    C'Person scheme access Upload'                                   
PUSULLIT DC    C'Person supplier access Upload'                                 
PUGLLLIT DC    C'Person group list access Upload'                               
PUGLALIT DC    C'Person global approver Upload'                                 
PUCLALIT DC    C'Person client approver Upload'                                 
PUNCALIT DC    C'Person non-client approver Upload'                             
PU1RALIT DC    C'Person staff approver Upload'                                  
PUIOALIT DC    C'Person invoice order approver Upload'                          
PUBKALIT DC    C'Person backup approver Upload'                                 
PUROLLIT DC    C'Person role Upload'                                            
PUTRLLIT DC    C'Person trailer Upload'                                         
RATUPLIT DC    C'TMS Rates Upload'                                              
PERUPLIT DC    C'Accounting Person Upload'                                      
XTINFLIT DC    C'XT login download'                                             
ETYPELIT DC    C'Expenditure type list download'                                
TMLSTLIT DC    C'Time List Download'                                            
CLDRTLIT DC    C'Claim distance rate calculation download'                      
MILERLIT DC    C'Claim mileage record download'                                 
ADDACLIT DC    C'Add account to LimList'                                        
IDBATDET DC    C'Invoices - Batch Details Download'                             
IUPSTUPH DC    C'Invoices Posting Upload - Header'                              
IUPSTUPI DC    C'Invoices Posting Upload - Invoice Item'                        
IUPSTUPP DC    C'Invoices Posting Upload - Posting Item'                        
IUPSTUPW DC    C'Invoices Posting Upload - WC/Narr Item'                        
IUPSTUPT DC    C'Invoices Posting Upload - Trailer'                             
IAUDTRLD DC    C'Invoices - Audit trail D/load'                                 
ESINILIT DC    C'Estimate Initial Download'                                     
ESCDILIT DC    C'Estimate Combined Download'                                    
ESAUILIT DC    C'Estimate Audit Download'                                       
ITEMLLIT DC    C'Item List Aura Download'                                       
SACCDLOD DC    C'Suspense A/C Download'                                         
JBBGDLIT DC    C'JobBag'                                                        
JBINILIT DC    C'Job Initial Download'                                          
JBAUDLIT DC    C'Job Audit Download'                                            
JBAPPLIT DC    C'Job Approver Download'                                         
ORUPLLIT DC    C'BrandOcean Order Upload'                                       
XUHDRLIT DC    C'Expense Upload - Header'                                       
XUITMLIT DC    C'Expense Upload - item line'                                    
XUXDTLIT DC    C'Expense Upload - extra data'                                   
XUTRLLIT DC    C'Expense Upload - trailer'                                      
RUHDRLIT DC    C'TimeSheet Upload - Header'                                     
RUTIMLIT DC    C'TimeSheet Upload - Time Line'                                  
RUMATLIT DC    C'TimeSheet Upload - Materials Line'                             
RUTRLLIT DC    C'TimeSheet Upload - Trailer'                                    
RUOCDLIT DC    C'Order Combined Download'                                       
RUSTRLIT DC    C'Status Report/Campaign Search Download'                        
RURESLIT DC    C'Resources Search Download'                                     
RUCAMLIT DC    C'Campaign List/Detail Download'                                 
RUMYTLIT DC    C'My Tasks/Task Search Download'                                 
RUDETLIT DC    C'Resources Detail Download'                                     
RUAUDLIT DC    C'Resources Audit Download'                                      
RUWRKLIT DC    C'Resources Work Upload'                                         
RUROLLIT DC    C'Team/Role Download'                                            
RUKSTLIT DC    C'Keystage/Template Download'                                    
RUOTHLIT DC    C'Out Of Office Upload - Header'                                 
RUOTILIT DC    C'Out Of Office Upload - Item'                                   
RUOTTLIT DC    C'Out Of Office Upload - Trailer'                                
RUOTDLIT DC    C'Out Of Office Download'                                        
SMPWDLOD DC    C'Send Mail for Password'                                        
ACLKULOD DC    C'Account lock/unlock upload'                                    
QWKVWLIT DC    C'Quick View'                                                    
ACSUMLIT DC    C'Accounting Summary Download'                                   
IDISDLIT DC    C'Invoices Display Download (new)'                               
HISLHLIT DC    C'History/Salary Upload - Header'                                
HISLELIT DC    C'History/Salary Upload - Element'                               
HISLTLIT DC    C'History/Salary Upload - Trailer'                               
FUPDLLIT DC    C'CSV File uploads available Download'                           
SALPHLIT DC    C'Salary posting Upload - Header'                                
SALPPLIT DC    C'Salary posting Upload - Posting'                               
SALPILIT DC    C'Salary posting Upload - WC Item'                               
SALPTLIT DC    C'Salary posting Upload - Trailer'                               
EHRRATES DC    C'Estimates hourly rates'                                        
PDFGHLIT DC    C'Pdfgen bill lookup'                                            
PRACCLIT DC    C'Add/delete person to account'                                  
STUDLLIT DC    C'Studio List Download'                                          
WORKUPLD DC    C'Worker file update upload'                                     
CNTCRDLD DC    C'Contra Creditor Ledger Transactions'                           
LMANRDLD DC    C'Line manager approver download'                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118ACBRA00   10/27/20'                                      
         END                                                                    
