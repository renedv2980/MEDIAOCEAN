*          DATA SET ACBRA01    AT LEVEL 111 AS OF 09/26/19                      
*PHASE T62401A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE WRKIO                                                                  
*INCLUDE ACGETALP                                                               
*INCLUDE ACJOBCOL                                                               
*INCLUDE BMONVAL                                                                
*INCLUDE VATICAN                                                                
*INCLUDE ACGETRTE                                                               
*INCLUDE RECUP                                                                  
*INCLUDE COVAIL                                                                 
*&&US                                                                           
*INCLUDE CATCALL                                                                
*INCLUDE CONVERT                                                                
*&&                                                                             
*&&UK                                                                           
*INCLUDE VATSUB                                                                 
*INCLUDE SRCHPASS                                                               
*&&                                                                             
ACBRA01  TITLE '- ACCOUNTING/BRA SERVER SUPPORT ROUTINES 1'                     
*                                                                               
* LEVEL CHANGE COMMENTS                                                         
* ---------------------                                                         
* UK LEVELS                                                                     
* ---------                                                                     
* TKLU 001 22APR05 LIVE DATE FOR BETA RELEASE OF EBUYER                         
* TKLU 002 27APR05 <LO01-4312> - DEF. EXP. TYPE AND APPR. NON EBUYER            
* TKLU 003 28APR05 COMMENT ENTRY: DELREC ROUTINE UNUSED                         
* TKLU 004 19AUG05 ETIME PREPARATIONS AND READ/ONLY CHECK                       
* TKLU 005 20SEP05 ENSURE ORDER TRX ADDED HAS 'GOOD' ACCOUNT CODES              
* TKLU 006 12OCT05 FURTHER ETIME MODULES + EMAIL ADDRESS FOR APPROVER           
*                  PTAORD TAKEN OUT AS IT REQUIRES ADDTRN PRESENT               
* TKLU 007 21OCT05 NEW MESSAGE EQUATES FOR UNIQUENESS                           
* TKLU 008 24OCT05 MIGRATE GAPLST FROM 01 TO 03 COMMON ROUTINES                 
* TKLU 011 03NOV05 PROFILES FROM 'EBY(ER)' TO 'P(URCHASE)O(RDERS)1'             
* TKLU 012 11NOV05 GENIE UKCR00003469 - MORE DETAILS IN GETACN                  
* TKLU 012 08DEC05 ADD BMONVAL TO ROUTINES                                      
* TKLU 013 12DEC05 ADD VATICAN TO ROUTINES AND ADJUST PERDTL CODE               
* NSHE 014 15DEC05 ADD FUTHER CORE RESIDENT PHASES                              
* TKLU 015 24JAN06 ADD GETUID ROUTINE (VICE VERSA OF GETUSR)                    
* TKLU 016 14FEB06 ADD QTSAR AND QPADDLE TO CORE RESIDENTS                      
* TKLU 017 23FEB06 <LO01-4757> ENABLE EXPENSE CLAIM INITIALISATION              
* TKLU 023 20MAR06 <DU01-4941> RETURN WCOELD IN GETWCD ROUTINE                  
* TKLU 024 11APR06 US VERSION IMPLEMENTATION/MERGER                             
* TKLU 025 20APR06 <LO01-5379> PROGRAM NUMBER FROM 22 TO 24                     
* TKLU 026 28APR06 <UKCR00006502> ADJUST EBUYER PROFILES                        
* TKLU 027 02MAY05 US VERSION CHANGES MERGED IN                                 
*                  ESTIMATE NUMBER ON ORDER TRANSACTIONS                        
* TKLU 028 22MAY05 GETUID BUG FIX (CTDSCD INSTEAD OF CTPASD)                    
* TKLU 029 26MAY05 MERGE IN LATEST US CHANGES (FROM JIM SHEA)                   
* TKLU 030 21JUN05 <LO01-4063> SET/GET OPTIONS TABLE MODULES                    
* TKLU 032 04SEP06 <DU01-5739> EXP. TYPE ON ORDER TRX (OADDTRX)                 
* NSHE 034 15SEP06 ADD CHANGES NEED FOR QA TESTING                              
* NSHE 035 09OCT06 <UKCR00009315> FIX BUG WITH TERMINATION DATES                
* TKLU 036 19OCT06 GETITM ROUTINE FOR ESTIMATES COPY AND COPY/ROW               
*          20OCT06 NEW FFTTESTN LAYOUT ON ORDER                                 
* TKLU 037 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* TKLU 038 31OCT06 <DU01-5917> REOPEN AN ORDER                                  
* TKLU 039 22NOV06 GETITM ADDITIONS AND APEMAN PREPARATIONS                     
* TKLU 040 08DEC06 SET MINOAMT AND NEW PROEBY13                                 
* TKLU 043 08JAN07 US MERGER                                                    
* NSHE 044 06FEB07 <LO01-6033> USE ALTERNATIVE NAMES                            
* TKLU     01MAR07 <DU01-6166> PID ON ORDER TRANSACTION                         
* TKLU 046 06MAR07 INCLUDE JFOS APEMAN FOR INVAPP TESTING                       
*          07MAR07 US MERGER (JIM SHEA)                                         
* TKLU 047 23MAR07 FC ADDITIONS TO GETITM ROUTINE                               
* TKLU 048 17APR07 ORDER C/A ROUTINE - READ FOR DELETES                         
* TKLU 049 26APR07 US MERGER                                                    
* NSHE 050 23MAY07 RESOURCES UPLOAD AND DOWNLOAD                                
* NSHE 051 20JUN07 BR12551L CHANGE PERSON DETAIL ROUTINE                        
* TKLU 052 11JUL07 ARTICLE/ITEM ON ORDER RELINK                                 
* TKLU 053 07AUG07 NEW ACGETALP + US MERGER                                     
* TKLU 054 01OCT07 <UKCR00014403> 'ALL ORDER ITEMS' CALL FOR =BRA               
*          08OCT07 <UKCR00014559> FFTTESTN ON ORDER TRX BUG FIX                 
* NSHE 055 04JAN08 MERGE US CHANGES                                             
* NSHE 058 01FEB08 FIX WRKINI PROBLEM                                           
* TKLU 059 18FEB08 <DU01-7333> ORDERS PROFILE SPLIT + PO3 PROFILES              
* TKLU 061 11MAR08 <DU01-7410> GETITM ADJUSTMENTS FOR ORDERS                    
* TKLU 062 12MAR08 <DU01-7398> BUG FIX                                          
* TKLU 063 11APR08 <UKCR00016983> SELF APPROVAL BUG FIX                         
* YNGX 064 14APR08 <BR12251D> RELINK TO PICK UP NEW VERSION OF TIMETRN          
* TKLU 065 23APR08 <UKCR00017120> GETITM PARAMETER & UK ESTIM BUG FIX           
* NSHE 066 18MAR08 <LO01-7186> ADD NEW ROUTINES TO SUPPORT TIME                 
* TKLU 066 19JUN08 <UKCR00017816> FIX LOGIC TO ENSURE TRSPASD ADDED             
*          20JUN08 PREPARE TO HANDLE SETCPY/CPYINI ERROR                        
* NSHE 067 24SEP08 ENSURE SECURITY ALPHA IS SAVED                               
* TKLU 068 15OCT08 <LO01-8207> 'INTERNAL APPROVALS' IMPLEMENTATION              
* TKLU 069 27NOV08 <DU01-7817> ENABLE MQIO                                      
* TKLU 070 13DEC08 <LO01-8306> NEW ACGETCRL FOR 150 CURRENCIES                  
* TKLU 071 13JAN09 <UKCR00018988> IO3 FOR GETITM (IO1 USED FOR TSAR)            
* NSHE 073 19JAN09 MERGE UK/US VERSIONS                                         
* TKLU 073 21JAN09 <DU01-8087> RETURN TELEPHONE EXTENSION IN GETPIN             
* SMAN 074 13MAR09 <LO01-8421> SAVE INTERNAL USE ONLY SETTING                   
* NSHE 074 19MAY09 ADD EDIT ROUTINES                                            
* SMAN 075 08JUL09 <BR25779L> COPY OAMELS CORRECTLY FOR ORDER TRX RECS          
* MPEN 076 06APR09 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT                  
* NSHE 077 19SEP09 CHANGES REQUIRED FOR EXPENSES                                
* NSHE 078 29OCT09 ADD NEW ROUTINE TO GET ROLE NAME                             
* NSHE 079 12NOV09 FIX ORDER PROFILE ERROR FOR US                               
* JFOS     13NOV09 ADD DMUNLK TO I/S COMMANDS                                   
* YNGX 080 04JAN10 BR15244D - BUG FIX CLOSING ORDER                             
* SMAN 081 10DEC09 <UKCR00026124> SMALL FIX AND IGNORE 2 CHAR OFFICES           
* NSHE 082 24FEB10 ADD INCOME ACCOUNT ROUTINE                                   
* NRAK 083 28JUL10 <BR34818L> BRANDOCEAN, MEET ORDOJOB                          
* TKLU 084 04JUN10 <PR000250> RELINKED FOR NEW GETALP                           
* NSHE 085 11AUG10 RELINKED FOR ACBRAWRKD CHANGES                               
* YNGX 086 05JAN10 <BR38679L> RELINKED FOR NEW ACGETRATE                        
* TKLU 087 08FEB11 <LO01-8459> RELINKED FOR NEW ACGETCRL                        
* NRAK 088 22FEB11 <PR000235> ADD SUPPORT FOR ARCHIVED DATA                     
* JFOS 090 15FEB11 <BR39740L> ACCPFP: EXTEND LAST NAME TO GPNTLST MAX           
* MPEN 091 23MAR11 <PR000716> INCLUDE PROMOTE                                   
* SMAN 092 22SEP11 <PR000031> ALTER CALL TO SECRET FOR SALARY UPLOAD            
* NRAK 093 16OCT11 <PR002202> ADD SYSTEM SWITCH/MEDIA IO                        
* NSHE 094 01NOV11 <BR45112L> SET COUNTRY CODE CORRECTLY                        
* MPEN 095 21NOV11 <PR002202> ADD TRAVAIL                                       
* NSHE 096 30MAR12 Extract full connection program details                      
* NSHE 097 10APR12 US merge                                                     
* NSHE 098 15MAY12 Amend IOEXEC routine to switch systems                       
* MPEN 099 25MAR13 <OT78922L> FIX BUG IN PROMOTE                                
* NSHE 101 28MAY14 <DSRD-2626> Ensure get office routine sets key               
* MPEN 102 02JUN14 <DSBO-675> Don't include ACGETCRL                            
* NSHE 103 24SEP14 <DSRD-4448> Create new edit rountine for HEX data            
* NSHE 104 03DEC14 <DSRD-5380> Make EDTHEX soft                                 
* JFOS 105 08OCT15 <PCA-1989> Support Limit Account Access                      
* NSHE     29DEC15 <DSRD-9807> If Aura don't error on flexible price            
* MPEN 106 01Jul16 MF changes for federated url                                 
* TKLU 107 12Oct16 <PCA02486> Pass User ID to BMONVAL                           
* TKLU 109 07May18 DSPCA-2844 Relink for PROMOTE (RNSPASD Adjustments)          
*                             which is now in COMFACS                           
* ABID 110 05JUN19 DSPCA-2941 RELINKED FOR ACVATICAN CHANGES                    
* ABID 111 18SEP19 SPEC-39198 FIX RUNNER FULIURE WHEN READING ARCHIVED          
*                             TRANSACTIONS                                      
* NSHE     26SEP19 DSRD-23871 SECRET CALL TO READ UPLOAD AND TIME               
*                                                                               
* US LEVELS                                                                     
* ---------                                                                     
* JSHA 005 28SEP07 ALL UK LEVELS UP TO 51                                       
* JSHA 006 29NOV07 US/UK MERGE FOR UK LEVELS FROM 52 TO 54                      
*                                                                               
ACBRA01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BO01**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     RA,ATWA             (OFFLINE ACMCS10 SETS FROM LP_ATWA)          
         USING TWAD,RA             RA=A(TWA)                                    
INI010   ST    RE,ROU1RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         XR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(WRKINI-ACBRA01),AL2(0)                                       
         DC    AL2(CPYINI-ACBRA01),AL2(0)                                       
         DC    AL2(IOEXEC-ACBRA01),AL2(IOEXWKL)                                 
         DC    AL2(NXTREC-ACBRA01),AL2(0)                                       
         DC    AL2(TSTNUM-ACBRA01),AL2(0)                                       
         DC    AL2(SETCPY-ACBRA01),AL2(0)                                       
         DC    AL2(ORDPRF-ACBRA01),AL2(0)                                       
         DC    AL2(SECCHK-ACBRA01),AL2(0)                                       
         DC    AL2(SETLDG-ACBRA01),AL2(0)                                       
         DC    AL2(GETWCD-ACBRA01),AL2(0)                                       
         DC    AL2(GETACN-ACBRA01),AL2(0)                                       
         DC    AL2(GETETN-ACBRA01),AL2(0)                                       
         DC    AL2(GETPIN-ACBRA01),AL2(0)                                       
         DC    AL2(GETUID-ACBRA01),AL2(0)                                       
         DC    AL2(GETUSR-ACBRA01),AL2(0)                                       
         DC    AL2(CONAMT-ACBRA01),AL2(0)                                       
         DC    AL2(GETPID-ACBRA01),AL2(0)                                       
         DC    AL2(SYSCHK-ACBRA01),AL2(0)                                       
         DC    AL2(ACCPID-ACBRA01),AL2(0)                                       
         DC    AL2(ACCPFP-ACBRA01),AL2(0)                                       
         DC    AL2(GETITM-ACBRA01),AL2(0)                                       
         DC    AL2(EDTPID-ACBRA01),AL2(0)                                       
         DC    AL2(EDTANM-ACBRA01),AL2(0)                                       
         DC    AL2(EDTCLI-ACBRA01),AL2(0)                                       
         DC    AL2(EDTCLN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTPRD-ACBRA01),AL2(0)                                       
         DC    AL2(EDTPRN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTJOB-ACBRA01),AL2(0)                                       
         DC    AL2(EDTJBN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTWCD-ACBRA01),AL2(0)                                       
         DC    AL2(EDTUSR-ACBRA01),AL2(0)                                       
         DC    AL2(EDTRLN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTCMN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTMED-ACBRA01),AL2(0)                                       
         DC    AL2(EDTMDN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTOFF-ACBRA01),AL2(0)                                       
         DC    AL2(EDTOFN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTDPT-ACBRA01),AL2(0)                                       
         DC    AL2(EDTDPN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTSUB-ACBRA01),AL2(0)                                       
         DC    AL2(EDTSUN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTPER-ACBRA01),AL2(0)                                       
         DC    AL2(EDTPEN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTMCN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTSCN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTGRN-ACBRA01),AL2(0)                                       
         DC    AL2(EDT2CD-ACBRA01),AL2(0)                                       
         DC    AL2(SEMAIL-ACBRA01),AL2(0)                                       
         DC    AL2(GETOFN-ACBRA01),AL2(0)                                       
         DC    AL2(GETROL-ACBRA01),AL2(0)                                       
         DC    AL2(EDTMIN-ACBRA01),AL2(0)                                       
         DC    AL2(EDTHEX-ACBRA01),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALISE WORKING STORAGE VARIABLES                                *         
***********************************************************************         
         DS    0H                                                               
WRKINI   J     *+12                                                             
         DC    CL8'*WRKINI*'                                                    
         LR    RB,RF                                                            
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
         USING WRKINI,RB                                                        
                                                                                
         LA    R0,SVALUES          MOVE LITERALS TO W/S                         
         LHI   R1,SVALUESL                                                      
         L     RE,ALVALUES                                                      
         A     RE,ROU1RELO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   GCSWSYSN,QSACC      DEFAULT IS ACC SYSTEM                        
         MVI   GCSWSYSC,QSACC                                                   
         MVI   GCSWSYSP,QSACC                                                   
                                                                                
         LA    R1,RELOLST          RELOCATE ADCONS                              
         LA    R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,ROU1RELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IOAREA1-WORKD    SET ADDRESSES OF I/O AREAS                   
         LA    RF,WORKD(RF)                                                     
         LA    R0,AIONM                                                         
         LA    R1,AIO1                                                          
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AHI   RF,IOLENQ                                                        
         AHI   R1,L'AIO1                                                        
         BCTR  R0,RE                                                            
                                                                                
         L     RF,=AL4(OFFAREA-WORKD)   SET ADDRESS OF OFFAL AREA               
         LA    RF,WORKD(RF)                                                     
         ST    RF,AOFFAREA                                                      
                                                                                
         L     RF,=AL4(GENAEXTN-WORKD)  SET ADDRESS OF GEN/XTNS AREA            
         LA    RF,WORKD(RF)                                                     
         ST    RF,AGENAXTN                                                      
                                                                                
         L     RF,=AL4(ELEAREA-WORKD)   SET ADDRESS OF ELEMENT AREA             
         LA    RF,WORKD(RF)                                                     
         ST    RF,AELEAREA                                                      
                                                                                
         L     RF,=AL4(WKBUFF-WORKD)    SET ADDRESS OF WORK BUFF AREA           
         LA    RF,WORKD(RF)                                                     
         ST    RF,AWKBUFF                                                       
                                                                                
         L     RF,=AL4(COLTAB-WORKD)    SET ADDRESS OF COLTAB AREA              
         LA    RF,WORKD(RF)                                                     
         ST    RF,ACOLTAB                                                       
                                                                                
         L     RF,=AL4(FREESTOR-WORKD)  SET ADDRESS OF FREESTOR AREA            
         LA    RF,WORKD(RF)                                                     
         ST    RF,AFREEST                                                       
                                                                                
         L     RF,=AL4(GOBLOCKA-WORKD)  SET ADDRESS OF GOBLOCK AREA             
         LA    RF,WORKD(RF)                                                     
         ST    RF,AGOBLOCK                                                      
                                                                                
         L     RF,=AL4(JOBLOCKA-WORKD)  SET ADDRESS OF JOBLOCK AREA             
         LA    RF,WORKD(RF)                                                     
         ST    RF,AJOBLOCK                                                      
                                                                                
         L     RF,=AL4(COLIST-WORKD)    SET ADDRESS OF COLIST AREA              
         LA    RF,WORKD(RF)                                                     
         ST    RF,ACOLIST                                                       
                                                                                
         L     RF,=AL4(COBLOCKA-WORKD)  SET ADDRESS OF COBLOCK AREA             
         LA    RF,WORKD(RF)                                                     
         ST    RF,ACOBLOCK                                                      
                                                                                
         L     RF,=AL4(GOBLOCKB-WORKD)  SET ADDRESS OF GOBLOCKB AREA            
         LA    RF,WORKD(RF)                                                     
         ST    RF,AGOBLOCB                                                      
                                                                                
         L     RF,=AL4(BLOCK-WORKD)     SET ADDRESS OF BLOCK AREA               
         LA    RF,WORKD(RF)                                                     
         ST    RF,ABLOCK                                                        
                                                                                
         L     RF,=AL4(GOXBLCKA-WORKD)  SET ADDRESS OF GOXBLOCK AREA            
         LA    RF,WORKD(RF)                                                     
         ST    RF,AGOXBLCK                                                      
                                                                                
         L     RF,=AL4(GOBBLCKA-WORKD)  SET ADDRESS OF GOBBLOCK AREA            
         LA    RF,WORKD(RF)                                                     
         ST    RF,AGOBBLCK                                                      
                                                                                
         L     R1,AFACTAB                                                       
         USING FACTABD,R1          EXTRACT FACILITES LIST ADCONS                
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         SR    RF,RF                                                            
         SR    R2,R2                                                            
         ICM   R2,3,FACTDOUT                                                    
         LA    R2,WORKD(R2)                                                     
         SR    R3,R3                                                            
         ICM   R3,3,FACTDIN                                                     
         IC    RF,FACTFLST                                                      
         L     RF,FACLISTS(RF)                                                  
         AR    R3,RF                                                            
*        LTR   RF,RF                                                            
*        BZ    *+10                                                             
         MVC   0(4,R2),0(R3)                                                    
         AHI   R1,FACTABL                                                       
         BCTR  R0,RE                                                            
                                                                                
         L     R2,ACORPHS          R2=A(CORE PHASE LIST)                        
         LA    R3,APHASES          R3=A(CORE PHASE ADDRESS LIST)                
         LA    R4,CORPHSN          R4=CORE PHASE COUNT                          
         SR    R0,R0                                                            
         ICM   R0,14,T00A                                                       
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
WRKINI02 ICM   R0,1,0(R2)          TEST PHASE                                   
         BZ    WRKINI04            NONE, SKIP TO THE NEXT ENTRY                 
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
WRKINI04 AHI   R2,1                BUMP TO THE NEXT ENTRY                       
         AHI   R3,L'APHASES                                                     
         BCT   R4,WRKINI02                                                      
         J     EXITY                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ALVALUES DC    A(LVALUES)                                                       
         EJECT                                                                  
***********************************************************************         
* INITIALISE COMPANY VALUES                                           *         
***********************************************************************         
         DS    0H                                                               
CPYINI   J     *+12                                                             
         DC    CL8'*CPYINI*'                                                    
         LR    RB,RF                                                            
         USING CPYINI,RB                                                        
         TM    GIND1,GIONLINE      TEST ONLINE                                  
         JZ    CPYINI02                                                         
         MVC   CUACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   CUUSER,TWAUSRID     USER-ID NUMBER                               
         MVC   CUAUTH,TWAAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,TWAAGY       AGENCY ALPHA-ID                              
         MVC   CUTERM,TWATRM       TERMINAL ID #                                
         J     CPYINI10                                                         
                                                                                
         USING LP_D,R1                                                          
CPYINI02 L     R1,ALP              SET OFFLINE VALUES                           
         MVC   CUACCS,LP_ACCS      LIMIT ACCESS                                 
         MVC   CUUSER,LP_USRID     USER-ID NUMBER                               
         MVC   CUAALF,LP_AGY       AGENCY ALPHA-ID                              
         MVC   CULANG,LP_LANG      LANGUAGE                                     
         MVC   CUAUTH,LP_AUTH      AUTHORIZATION CODE                           
         MVC   CUTSYS,LP_SENO                                                   
         L     RF,LP_ALPXD                                                      
         USING LP_XD,RF                                                         
         MVC   CUXINFO,LP_XPINF    EXTERNAL PROGRAM INFORMATION                 
         MVC   CUTSYM,LP_LUID                                                   
         DROP  RF                                                               
         L     RF,LP_ARUNP                                                      
         SR    RE,RE               A(RUNNER->SERVER FACILITIES)                 
         ICM   RE,7,RUNPARUN-RUNPARMD(RF)                                       
         ICM   RF,15,RMASTC-RUNFACSD(RE)                                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING MASTD,RF            RF=A(MASTER CONTROLLER AREA)                 
         MVC   CUCTRY,MCCTRY       COUNTRY                                      
         OC    CUCTRY,CUCTRY       DO WE HAVE A VALUE                           
         JNZ   CPYINI04            YES - OTHERWISE SET DEFAULT                  
*&&US*&& MVI   CUCTRY,CTRYUSA      FORCE USA                                    
*&&UK*&& MVI   CUCTRY,CTRYGBR      FORCE GREAT BRITAIN                          
                                                                                
CPYINI04 MVC   CUXCPY,MCIDAGYB     COMPANY                                      
         MVC   CUAUTL,MCUTL                                                     
         DROP  RF                                                               
         MVC   TWAACCS,CUACCS      LIMIT ACCESS INTO OFFLINE TWA                
         MVC   TWAUSRID,CUUSER     USER-ID NUMBER                               
         MVC   TWAAGY,CUAALF       AGENCY ALPHA-ID                              
         MVC   TWAAUTH,CUAUTH      AUTHORIZATION CODE                           
         L     RF,LP_ASECD                                                      
         USING SECD,RF                                                          
         MVC   CUSALF,SECOAGYS     SET SECURITY AGENCY ALPHA                    
*                                                                               
         LA    R1,IOKEY            READ ID RECORD TO BUILD SWCH TABLE           
         USING CTIREC,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         L     RF,AIO1                                                          
         AHI   RF,IODDWQ+L'IODA                                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IOKEY,AIO1,(RF)              
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R1,AIO1                                                          
         LA    R1,CTIDATA          LOOK FOR SYSTEM ELEMENTS                     
         USING CTSYSD,R1                                                        
         XR    RF,RF                                                            
         LA    RE,SWSTAB                                                        
         USING SYSSWTAB,RE                                                      
CPYINI06 CLI   CTSYSEL,0           END OF RECORD?                               
         JE    CPYINI18                                                         
         CLI   CTSYSEL,CTSYSELQ                                                 
         JNE   CPYINI08                                                         
*                                                                               
         MVC   SYSSWSYS,CTSYSSE                                                 
         MVC   SYSSWSOV,CTSYSNUM                                                
         MVC   SYSSWAGB,CTSYSAGB                                                
         MVC   SYSSWACS,CTSYSLMT                                                
         CLC   CTSYSSE,CUTSYS      FLAG CURRENT OPEN SYSTEM                     
         JNE   *+8                                                              
         OI    SYSSINDS,SYSSIOPN                                                
         LA    RE,SYSSWLEN(RE)                                                  
*                                                                               
CPYINI08 ICM   RF,1,CTSYSLEN                                                    
         LA    R1,0(RF,R1)                                                      
         J     CPYINI06                                                         
         DROP  R1,RE                                                            
*                                                                               
CPYINI10 CLI   TWAOFFC,C'*'        TEST DDS OFFICE CODE                         
         JNE   *+8                                                              
         MVI   CUSTAT,CUSDDS       SET DDS STATUS INDICATOR                     
         GOTOR VGETFACT,DMCB,0                                                  
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R1=A(SYSTEM DEFINITION BLOCK)                
         MVC   CUTSYM,FASYM                                                     
*&&UK*&& MVC   CUTSYS,FASYS                                                     
         MVC   CUPASS,FAPASSWD     SET CONNECTED PASSWORD                       
         MVC   CUTERM,FATWAS       SET CONNECTED USER ? SEE ABOVE               
         MVC   CTIME,FATIME        SYSTEM TIME (STANDARD FORMAT)                
*&&US*&& AP    CTIME,=P'60000'     CONVERT FROM DDS TIME TO REAL TIME           
                                                                                
         TM    FATFLAG,X'08'                                                    
         JZ    *+8                                                              
         OI    CUSTAT,CUSPER       SET PERSONAL PASSWORD                        
         MVC   AXLATES,FAXLATES    A(CASE TRANSLATION MATRIX)                   
                                                                                
         GOTOR VGETFACT,DMCB,(X'80',0),F#TCBD                                   
         L     R1,0(R1)                                                         
         USING F@TCBD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,F@BSWNUM       R0=N'ENTRIES IN TCBSWTAB                     
         JZ    CPYINI16                                                         
*                                                                               
         CLM   R0,1,=AL1(SYSSWMAX)                                              
         JNH   *+8                                                              
         LA    R0,SYSSWMAX                                                      
         LA    R1,F@BSWTAB                                                      
         USING F@BSWTAB,R1         R1=A(TCB SWITCH TABLE)                       
         LA    RE,SWSTAB                                                        
         USING SYSSWTAB,RE         RE=A(LOCAL SWITCH TABLE)                     
CPYINI14 MVC   SYSSWSYS,F@BSWSYS                                                
         MVC   SYSSWSOV,F@BSWSOV                                                
         MVC   SYSSWAGB,F@BSWAGB                                                
         MVC   SYSSWACS,F@BSWACS                                                
         MVC   SYSSWAC2,F@BSWAC2                                                
         LA    R1,F@BSWLEN(R1)     BUMP TO NEXT                                 
         LA    RE,SYSSWLEN(RE)                                                  
         JCT   R0,CPYINI14         DO FOR ALL ENTRIES                           
         DROP  R1,RE                                                            
*                                                                               
CPYINI16 LA    R0,TWAD             SET A(SECRET BLOCK)                          
         AHI   R0,SECBLK-TWAD                                                   
         ST    R0,ASECBLK                                                       
                                                                                
         MVI   WORK,ACCSYSQ                                                     
         MVI   WORK+1,BRATIMQ                                                   
         MVI   WORK+2,0            MARK END OF LIST                             
*&&UK                                                                           
         MVI   WORK+2,ACCSYSQ                                                   
         MVI   WORK+3,BRAUPLQ                                                   
         MVI   WORK+4,0            MARK END OF LIST                             
*&&                                                                             
         GOTOR VSECRET,DMCB,('SECPINIT+SECPOSP',(R0)),L'SECBLK,WORK             
         JNL   *+6                                                              
         DC    H'0'                CAN'T INITIALISE SECRET                      
         USING SECD,RF                                                          
         L     RF,ASECBLK          GET AUTHORISER PID                           
         MVC   CUSALF,SECOAGYS     SET SECURITY AGENCY ALPHA                    
*&&US*&& MVC   REQRPID,SECPID                                                   
         DROP  RF                                                               
         GOTOR VGETFACT,DMCB,(X'80',0),F#TXPINF                                 
         L     R1,0(R1)                                                         
         MVC   CUXPINFO,0(R1)      EXTERNAL PROGRAM INFO                        
         MVC   CUXINFO,0(R1)                                                    
*&&UK                                                                           
         LA    RF,TWAD             INITIALISE LIMACC BLOCK                      
         AHI   RF,LIMACBLK-TWAD                                                 
         ST    RF,ALACCBLK                                                      
         USING LIMACCD,RF                                                       
         XC    LABLOCK(LAINITLN),LABLOCK                                        
         MVI   LASYS,4                                                          
         MVC   LALIMACC,CUACCS                                                  
         DROP  RF                                                               
         GOTO1 VLIMACC,DMCB,(C'I',(RF)),ACOMFACS                                
         JE    CPYINI18                                                         
         DC    H'0'                                                             
*&&                                                                             
CPYINI18 XC    CCTPID,CCTPID                                                    
         USING COMFACSD,R1                                                      
         L     R1,ACOMFACS         A(COMFACS)                                   
         ICM   RE,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         JZ    CPYINI20                                                         
         USING XTRAINFD,RE                                                      
         OC    XIPID,XIPID                                                      
         JZ    CPYINI20                                                         
         MVC   CCTPID,XIPID                                                     
         DROP  R1,RE                                                            
                                                                                
CPYINI20 GOTOR (#SETCPY,ASETCPY)   GET COMPANY DETAILS                          
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAIND1,TWAIINIT    INITIALISED                                  
                                                                                
CPYINIX  J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* I/O EXECUTIVE                                                       *         
***********************************************************************         
         DS    0H                                                               
IOEXEC   J     *+12                                                             
         DC    CL8'*IOEXEC*'                                                    
         LR    RB,RF               I/O EXECUTIVE                                
         USING IOEXEC,RB                                                        
         USING IOEXWKD,RC          RC=A(LOCAL WORKING STORAGE)                  
         ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN SYSCOND            
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         MVI   IOFLAG,0                                                         
         N     R1,=AL4(IOLOCK)     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
                                                                                
         L     R1,IOCTRL                                                        
         N     R1,=AL4(IORDEL)     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
                                                                                
         L     R1,IOCTRL                                                        
         N     R1,=AL4(IOAREAS)    ESTABLISH I/O AREA ADDRESS                   
         BZ    IOEX02                                                           
         SRL   R1,4                R1=I/O AREA NUMBER                           
         BCTR  R1,0                                                             
         MHI   R1,IOLENQ                                                        
         A     R1,AIO1                                                          
         STCM  R1,15,IOADDR        SET ADDRESS OF I/O AREA                      
IOEX02   L     R1,IOCTRL                                                        
         N     R1,=AL4(IOFILES)          ESTABLISH FILE                         
         BNZ   IOEX04                                                           
         OC    IOFILE,IOFILE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         B     IOEX10                                                           
                                                                                
IOEX04   SRL   R1,8                R1=FILE NUMBER                               
         L     RE,ANFITAB                                                       
         USING NFITABD,RE                                                       
IOEX06   CLI   NFINUM,0                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,NFINUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,NFITABL(RE)                                                   
         B     IOEX06                                                           
*                                                                               
         MVC   IOFILV,NFINUM       EXTRACT FILE VALUES                          
         CLC   GCSWSYSC,IOFILSE    CHECK WE ARE IN THE CORRECT SYSTEM           
         BE    IOEX08                                                           
         CLC   IOFILNO,=AL1(IOCONFIL/256)                                       
         BE    IOEX08                                                           
         CLC   IOFILNO,=AL1(IOGENFIL/256)                                       
         BE    IOEX08                                                           
         CLC   IOFILNO,=AL1(IOGENDIR/256)                                       
         BE    IOEX08                                                           
         GOTOR IOSWITCH,IOFILOSE   SWITCH TO REQUIRED SYSTEM                    
         BE    IOEX08                                                           
         GOTOR IOSWITCH,GCSWSYSN   CAN`T SWITCH - SWITCH BACK TO NATIVE         
         J     EXITL               SWITCH SETS ERROR MESSAGE                    
                                                                                
IOEX08   L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX12                                                           
*                                                                               
IOEX10   OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
*                                                                               
         ICM   R0,15,IOADDR        SEE IF ADDRESS SET                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEX12   CLI   CMDFILT,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),CMDFILT                                                 
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),CMDFILT                                                 
         BNE   *+12                                                             
         LA    RE,CMDNTRY                                                       
         B     IOEX14                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CMDTABL                                                     
         AR    RE,RF                                                            
         B     IOEX12                                                           
*                                                                               
         USING CMDNTRY,RE          RE=A(COMMAND TABLE ENTRY)                    
IOEX14   CLI   CMDNTRY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUM         MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDNTRYL(RE)                                                  
         B     IOEX14                                                           
*                                                                               
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST ADDREC                                  
         BO    IOEX22                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX24                                                           
                                                                                
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOEX16                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         ICM   R1,15,IOADDR        R1=A(I/O AREA)                               
         BZ    *+14                                                             
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODAOVER                                            
         XC    IODAOVER,IODAOVER                                                
         B     IOEX22                                                           
                                                                                
IOEX16   ICM   R1,15,IOADDR        FIND THIS I/O AREA DA/WORK                   
         BZ    IOEX20                                                           
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         LA    R1,L'IODA(R1)                                                    
         OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
                                                                                
IOEX20   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX22                                                           
         TM    IOFILI,NFIIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,NFIIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN-OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         SR    R1,R1                                                            
         IC    R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    IOEX22              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
*                                                                               
IOEX22   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK             
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRFIL                                                   
         ICM   R1,15,IOADDR        PARK DA/WORK FOR THIS I/O AREA               
         BZ    IOEXX                                                            
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         B     IOEXX               EXIT TO CALLER                               
                                                                                
IOEX24   TM    IOFILI,NFIIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX28                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,NFIIID      TEST I/S FILE HAS D/A ATTACHED               
         BNZ   IOEX26                                                           
         TM    IOFILI,NFIIVL                                                    
         BZ    IOEX26                                                           
         ICM   R0,15,IOADDR        VL I/S READS INTO IOAREA ADDRESS             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
IOEX26   GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
         GOTOR VDATAMGR,IOCB                                                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
         TM    IOERR,IOERRS        TEST ANY ERRORS FOUND                        
         BZ    *+12                                                             
         TM    IOERR,IOEDEL        TEST DELETED RECORD FOUND                    
         BZ    IOEXX               NO - EXIT WITH ERROR                         
         TM    IOFILI2,NFIIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEXX               NO - EXIT                                    
         LLC   R1,IOFILKL                                                       
         LLC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOADDR        PARK DA FOR THIS I/O AREA                    
         BZ    IOEXX                                                            
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODA                                                
         B     IOEXX                                                            
                                                                                
IOEX28   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IOEXX2                                                           
         TM    IOINDS1,IO1XSWCH    TEST AUTO SWITCH BACK AFTER I/O              
         BO    IOEXX2                                                           
         GOTOR IOSWITCH,GCSWSYSP   SWITCH TO PREVIOUS SYSTEM                    
*                                                                               
IOEXX2   TM    IOERR,IOERRS                                                     
         JZ    EXITY               EXIT OK                                      
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         JNZ   EXITH               LOGICAL ERROR                                
         J     EXITL               PHYSICAL ERROR                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT I/O TRACE                                          *         
***********************************************************************         
                                                                                
IOTRCE   CLI   IOTRACE,C' '        TEST TRACE IS ACTIVE                         
         BNHR  RE                                                               
         CLI   IOTRACE,C'N'                                                     
         BER   RE                                                               
         STC   R1,IOTRIND                                                       
         ST    RE,IOSAVERE                                                      
         MVC   IOP,SPACES                                                       
         L     RF,ALP                                                           
         L     RF,LP_ARUNP-LP_D(RF)                                             
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   IOVPRNT,MCVPRINT-MASTD(RF)                                       
                                                                                
         L     RF,IOCB+0                                                        
         MVC   IOP(L'IOCMDNM),0(RF)                                             
         L     RF,IOCB+4                                                        
         MVC   IOP+L'IOCMDNM+1(L'IOFILNM),0(RF)                                 
         SR    R2,R2                                                            
         IC    R2,IOFILKL          R2=KEY LENGTH                                
         TM    IOTRIND,IOTRDIR     TEST DIRECTORY I/O                           
         BZ    IOTRCE04                                                         
         MVC   IOP+20(L'IOIKEYI),IOIKEYI                                        
         TM    IOTRIND,IOTRAFT                                                  
         BZ    IOTRCE02                                                         
         MVC   IOP+20(L'IOIKEYO),IOIKEYO                                        
         TM    IOFILI,FILIVL                                                    
         BNZ   IOTRCE02                                                         
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0                                                            
         AHI   R2,4                                                             
                                                                                
IOTRCE02 BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOKEY                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    IOP+25(0),TRTTAB                                                 
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
         MVC   IOP,SPACES                                                       
         GOTOR VHEXOUT,IOPARM,IOKEY,IOHEXWRK,1(R2),HEXSEP                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R1)                                                  
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
         B     IOTRCEX                                                          
                                                                                
IOTRCE04 TM    IOTRIND,IOTRFIL                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0               ADD ON CONTROL LENGTH                        
         AHI   R2,6                PLUS L'LENGTH AND LINK AREA                  
         L     R0,IOCB+8                                                        
         GOTOR VHEXOUT,IOPARM,(R0),IOWORK,4,HEXTOG                              
         L     R3,IOCB+12                                                       
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R3)                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    IOP+25(0),TRTTAB                                                 
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
         MVC   IOP,SPACES                                                       
         MVC   IOP(L'IODALIT),IODALIT                                           
         MVC   IOP+L'IODALIT(8),IOWORK                                          
         GOTOR VHEXOUT,IOPARM,IOCB+8,IOP+13,1,HEXTOG                            
         GOTOR VHEXOUT,IOPARM,(R3),IOHEXWRK,1(R2),HEXSEP                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R1)                                                  
         GOTOR IOVPRNT,IOPARM,IOP-1,PRTBL01                                     
                                                                                
IOTRCEX  L     RE,IOSAVERE                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO A SYSTEM                                       *         
*                                                                     *         
* NTRY - R1=A(LOGICAL SYSTEM NUMBER)                                  *         
* EXIT - CC=LOW   - USER NOT AUTHORISED FOR SYSTEM                    *         
*        CC=EQUAL - SWITCH SUCCESSFUL                                 *         
*        CC=HIGH  - SYSTEM NOT AVAILABLE (ONLINE ONLY)                *         
* NOTE - IF ERROR OCCURRS IOERR IS SET TO X'FF' WHICH WILL RETURN A   *         
*        CC OF HIGH FROM I/O ROUTINE WITH FVMSGNO SET TO FVFIOER. IT  *         
*        IS THE CALLER'S RESPONSIBILITY TO DEAL WITH THIS OTHERWISE   *         
*        A RANDOM DATAMGR ERROR WILL BE REPORTED.                     *         
***********************************************************************         
         SPACE 1                                                                
IOSWITCH CLC   GCSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BNE   IOSW02                                                           
         CLC   GCSWSE,IOFILSE      TEST CORRECT SE NUMBER                       
         BER   RE                                                               
*                                                                               
IOSW02   LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IOBYTE,0(R1)        SAVE SYSTEM NUMBER REQUIRED                  
*                                                                               
         OC    IOFILSE,IOFILSE     OVERRIDE SYSTEM PASSED FOR SWITCH?           
         BZ    IOSW04                                                           
         CLC   GCSWSYSP,0(R1)      SWITCHING BACK TO PREV SYSTEM?               
         BNE   *+12                                                             
         MVI   IOFILSE,0           SET NO OVERRIDE SYSTEM THEN                  
         B     IOSW04                                                           
*                                                                               
         MVC   IOBYTE,IOFILSE                                                   
         LA    R1,IOFILSE                                                       
         B     IOSW10                                                           
*                                                                               
IOSW04   LA    RE,SWSTAB                                                        
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
IOSW06   CLC   SYSSWSOV,IOBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   IOSW08                                                           
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         CLI   IOFILSE,0           SPECIFIC USER SE NUMBER SUPPLIED?            
         BE    IOSW10                                                           
         CLC   SYSSWSYS,IOFILSE    MATCH ON USER SE NUMBER                      
         BE    IOSW10                                                           
*                                                                               
IOSW08   LA    RE,SYSSWLEN(RE)     BUMP TO NEXT SWITCH TABLE ENTRY              
         BCT   RF,IOSW06                                                        
         MVI   IOBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSWX                                                            
*                                                                               
IOSW10   MVC   GCSWSE,IOFILSE      SAVE USER SUPPLIED SE NUMBER                 
         TM    GIND1,GIONLINE      TEST ONLINE                                  
         BNZ   IOSW12                                                           
         ICM   RF,15,CUAUTL        YES - MOVE SE NUMBER TO UTL                  
         JZ    IOSW12                                                           
         MVC   TSYS-UTLD(L'TSYS,RF),0(R1)                                       
         B     IOSW14                                                           
*                                                                               
IOSW12   MVC   DMCB(1),0(R1)     SWITCH TO A SYSTEM                             
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         XC    DMCB+4(4),DMCB+4                                                 
         GOTOR VSWITCH,DMCB                                                     
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW14                                                           
         MVI   IOBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSWX                                                            
*                                                                               
IOSW14   MVC   GCSWSYSP,GCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   GCSWSYSC,IOBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
         MVI   IOBYTE,1            SET CC=EQUAL FOR OK                          
*                                                                               
IOSWX    CLI   IOBYTE,1            SET CC FOR CALLER                            
         BE    *+8                                                              
         MVI   IOERR,X'FF'         SET ALL ERROR BITS ON                        
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 1                                                                
HEXSEP   DC    C'SEP'                                                           
HEXTOG   DC    C'TOG'                                                           
PRTBL01  DC    C'BL01'                                                          
IOIKEYI  DC    C'IKEY='                                                         
IOIKEYO  DC    C'OKEY='                                                         
IODALIT  DC    C'D/A='                                                          
                                                                                
TRTTAB   DS    0XL256                                                           
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
                                                                                
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
                                                                                
IOEXWKD  DSECT                                                                  
IODUB    DS    D                                                                
IOCB     DS    6F                                                               
IOPARM   DS    6F                                                               
IOSAVERE DS    A                                                                
IOVPRNT  DS    A                   V(PRINT)                                     
IOCTRL   DS    XL4                 I/O COMMAND WORD                             
IOBYTE   DS    X                   I/O BYTE                                     
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
*                                                                               
IOFILV   DS    0XL17               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILGLB EQU   14                  GLOBAL FILES ARE 14-15                       
IOFILOSE DS    XL1                 OVERLAY SYSTEM                               
IOFILSE  DS    XL1                 OVERRIDE SYSTEM                              
IOFILNM  DS    CL7                 COMMAND NAME                                 
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
*                                                                               
IOTRIND  DS    XL1                 TRACE INDICATORS                             
IOTRDIR  EQU   X'40'               I/O TO DIRECTORY                             
IOTRFIL  EQU   X'20'               I/O TO FILE                                  
IOTRBEF  EQU   X'01'               BEFORE I/O                                   
IOTRAFT  EQU   X'02'               AFTER I/O                                    
*                                                                               
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOSWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IOSWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IOSWSYSN DS    CL3                 SWITCH SYSTEM NAME                           
IOP      DS    CL132                                                            
IOHEXWRK DS    XL220                                                            
IOEXWKL  EQU   *-IOEXWKD                                                        
ACBRA01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD                                                     *         
*                                                                     *         
* NTRY   P1 B0   C'Y' - READ FOR THIS RECORD                          *         
*           B1-3 A(KEY TABLE)                                         *         
*        P2 B0   #(LP_BLK RECORD AREA)                                *         
*           B1-3 A(KEY SAVE AREA) OR 0                                *         
*        P3 B1-3 A(WORK SAVE AREA)                                    *         
*        P4 B0   #(KEY FILTER ROUTINE) OR 0                           *         
*           B1-3 A(ROUTINES) OR 0                                     *         
*        P5 B0   #(RECORD FILTER ROUTINE) OR 0                        *         
*           B1-3 A(ROUTINES) OR 0                                     *         
***********************************************************************         
         DS    0H                                                               
NXTREC   J     *+12                                                             
         DC    CL8'*NXTREC*'                                                    
         LR    RB,RF                                                            
         USING NXTREC,RB                                                        
         L     R0,0(R1)            R0=A(KEY TABLE)                              
         LM    R3,R6,4(R1)         R3,R4,R5,R6 (P2-5 SEE ABOVE)                 
         LA    R3,0(R3)                                                         
         MVC   BYTE1,8(R1)         R6=A(RECORD FILTER ROUTINE)                  
         L     R2,ALP                                                           
         USING LP_D,R2                                                          
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,IOADDR                                                        
         TM    BYTE1,$NXTRXAD      TEST INHIBIT SETTING LP_ADATA                
         BNZ   *+8                                                              
         ST    RE,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST                                                
         BNE   NREC02                                                           
         MVI   LP_RMODE,LP_RNEXT   RESET FIRST TIME CALL                        
         CLI   0(R1),C'N'                                                       
         BE    NREC08                                                           
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   0(L'IOKEY,R3),IOKEY                                              
         XC    IOKEY,IOKEY                                                      
*                                                                               
NREC02   GOTOR LP_ASETK,DMCB,(0,(R0)),IOKEY,(R4),('FF',ALP)                     
         BH    NREC06                                                           
         LHI   R1,IODIR                                                         
         TM    BYTE1,$NXTRCTF      TEST I/O TO CONTROL FILE                     
         BZ    *+8                                                              
         LHI   R1,IOCTL                                                         
         TM    BYTE1,$NXTRGEN      TEST I/O TO GENERAL FILES                    
         BZ    *+8                                                              
         LHI   R1,IOGND                                                         
         AHI   R1,IOHI                                                          
         TM    BYTE1,$NXTRDEL      TEST DELETED RECORDS REQUIRED                
         BZ    *+8                                                              
         A     R1,=AL4(IORDEL)                                                  
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BE    *+14                                                             
         CLI   IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',ALP)                     
         BNE   NREC02                                                           
                                                                                
         LTR   RF,R5               SET/TEST A(KEY FILTER ROUTINE)               
         BZ    *+12                                                             
         GOTOR GOFILT                                                           
         BNE   NREC02                                                           
                                                                                
         TM    BYTE1,$NXTRCTF+$NXTRXGR                                          
         JNZ   EXITY                                                            
         LHI   R1,IOMST                                                         
         TM    IOKEY+(TRNKSTAT-TRNRECD),TRNSARCH                                
         JZ    *+8                                                              
         LHI   R1,IOARC            ARCHIVE RECORD                               
         TM    BYTE1,$NXTRGEN      TEST I/O TO GENERAL FILE                     
         BZ    *+8                                                              
         LHI   R1,IOGNF                                                         
         A     R1,=AL4(IOGET+IORDEL)                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
                                                                                
         BE    NREC04                                                           
         TM    IOERR,IOEDEL        IGNORE DELETED RECORDS (SPILL)               
         BNZ   *+6                                                              
         DC    H'0'                DIE ON OTHER ERRORS                          
                                                                                
         LR    R1,R0               R1=A(KEY DRIVER TABLE)                       
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BCTR  RE,0                RE=L'RECORD KEY-1                            
         L     RF,IOADDR           RF=A(RECORD)                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),0(RF)      TEST KEY MATCHES (ACTIVE POINTER)            
         BNE   NREC02              NO - IGNORE UNDELETED PASSIVES               
         TM    BYTE1,$NXTRDEL      TEST DELETED RECORDS WANTED                  
         BZ    NREC02                                                           
*                                                                               
NREC04   LTR   RF,R6               SET/TEST A(RECORD FILTER ROUTINE)            
         JZ    EXITY                                                            
         L     R1,IOADDR           PASS A(RECORD) IN R1                         
         GOTOR GOFILT                                                           
         BNE   NREC02              DIDN'T PASS FILTERS - GET NEXT               
         J     EXITY                                                            
                                                                                
NREC06   LTR   R3,R3                                                            
         BZ    NREC08                                                           
         MVC   IOKEY,0(R3)                                                      
NREC08   MVI   LP_RMODE,LP_RLAST                                                
         J     EXITN                                                            
                                                                                
GOFILT   NTR1  LABEL=NO            CALL RECORD FILTER ROUTINE                   
         L     RE,ALP                                                           
         LM    R2,RB,LP_R2RB-LP_D(RE)                                           
         BASR  RE,RF                                                            
         J     EXIT                                                             
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
                                                                                
ACBRA01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST INPUT DATA IS CHARACTER NUMERIC                                *         
* P1 - A(INPUT DATA)                                                  *         
* P2 - L'DATA TO BE TESTED                                            *         
***********************************************************************         
         DS    0H                                                               
TSTNUM   J     *+12                                                             
         DC    CL8'*TSTNUM*'                                                    
         LR    RB,RF                                                            
         USING TSTNUM,RB                                                        
         LM    RE,RF,0(R1)         CHECK NUMERIC                                
TSTNUM02 CLI   0(RE),C'0'                                                       
         JL    EXITN                                                            
         CLI   0(RE),C'9'                                                       
         JH    EXITN                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,TSTNUM02                                                      
         J     EXITY                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ COMPANY RECORD, SAVE CPYEL TO W/S                              *         
***********************************************************************         
         DS    0H                                                               
SETCPY   J     *+12                                                             
         DC    CL8'*SETCPY*'                                                    
         LR    RB,RF                                                            
         USING SETCPY,RB                                                        
         LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUXCPY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                COMPANY NOT ON FILE                          
         L     R2,AIO1             COPY COMPANY RECORD TO IO8                   
         LHI   R3,IOLENQ                                                        
         LR    RF,R3                                                            
         L     RE,AIO8                                                          
         MVCL  RE,R2                                                            
         L     R2,AIO1                                                          
         LA    R2,CPYRFST          LOCATE CPYEL                                 
         USING CPYELD,R2                                                        
         XR    RF,RF                                                            
         MVI   BYTE4,0                                                          
         XC    SCPYEL,SCPYEL                                                    
         XC    SCPXEL,SCPXEL                                                    
*                                                                               
         MVI   FEDLEN,0                                                         
         LA    R0,FEDURL                                                        
         LHI   R1,L'FEDURL                                                      
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   REQALIAS,SPACES                                                  
         MVC   TEMP2,SPACES                                                     
         MVC   DEFEXPTY,SPACES                                                  
SETCPY02 CLI   CPYEL,0                                                          
         BE    SETCPY11                                                         
         CLI   CPYEL,CPYELQ                                                     
         BE    SETCPY06                                                         
         CLI   CPYEL,CPXELQ                                                     
         BE    SETCPY10                                                         
         CLI   CPYEL,FFTELQ                                                     
         BE    SETCPY08                                                         
SETCPY04 IC    RF,CPYLN                                                         
         AR    R2,RF                                                            
         B     SETCPY02                                                         
                                                                                
SETCPY06 MVI   BYTE4,CPYELQ                                                     
         IC    RF,CPYLN                                                         
         SHI   RF,1                                                             
         MVC   SCPYEL(0),CPYEL     COPY WHOLE ELEMENT TO W/S                    
         EX    RF,*-6                                                           
         MVC   PRODUL,CPYPROD                                                   
         MVI   OFFIND,PANDLQ                                                    
         TM    CPYSTAT5,CPYSOFPL                                                
         BNZ   SETCPY07                                                         
         MVI   OFFIND,FULLYQ                                                    
         TM    CPYSTAT1,CPYSOROE                                                
         BNZ   SETCPY07                                                         
         MVI   OFFIND,NONEQ                                                     
SETCPY07 MVC   AGYCURR,CPYCURR                                                  
         CLI   AGYCURR,X'40'                                                    
         BH    SETCPY04                                                         
*&&UK*&& MVC   AGYCURR,=C'GBP'     SET DEFAULT                                  
*&&US*&& MVC   AGYCURR,=C'USD'                                                  
         B     SETCPY04                                                         
                                                                                
         USING FFTELD,R2                                                        
SETCPY08 CLI   FFTTYPE,FFTTRALI    REQUISITION ALIAS?                           
         BNE   SETCPY09                                                         
         IC    RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   REQALIAS(0),FFTDATA                                              
         EX    RF,*-6                                                           
         B     SETCPY04                                                         
SETCPY09 CLI   FFTTYPE,FFTTEXTY    DEF EXP TYPE?                                
         BNE   SETCPY9A                                                         
         MVC   DEFEXPTY,FFTDATA                                                 
         B     SETCPY04                                                         
SETCPY9A CLI   FFTTYPE,FFTTTINR    INTERNAL REFERENCE NAME                      
         BNE   SETCPY9B                                                         
         IC    RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   TEMP2(0),FFTDATA                                                 
         EX    RF,*-6                                                           
         B     SETCPY04                                                         
                                                                                
SETCPY9B CLI   FFTTYPE,FFTTFURL    FEDERATED URL                                
         BNE   SETCPY04                                                         
         LLC   RE,FEDLEN                                                        
         LA    RE,FEDURL(RE)                                                    
         LLC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         MVC   0(0,RE),FFTDATA                                                  
         EX    RF,*-6                                                           
         B     SETCPY04                                                         
*                                                                               
         USING CPXELD,R2                                                        
SETCPY10 MVC   SCPXEL,CPXELD                                                    
         CLI   CPXLN,CPXLNQ                                                     
         BNH   SETCPY04                                                         
         DC    H'0'                                                             
                                                                                
SETCPY11 CLI   BYTE4,CPYELQ                                                     
         BE    SETCPY12                                                         
         DC    H'0'                                                             
                                                                                
         USING LDGRECD,R2                                                       
SETCPY12 CLI   CUXPINFO,XPMDBYRQ   HAVE WE CONNECTED AS MEDIA BUYER             
         BE    SETCPY24            YES                                          
         LA    R2,IOKEY            PRODUCTION LEDGER                            
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(2),PRODUL                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                PROD'N LEDGER NOT ON FILE                    
         L     R2,AIO1             COPY LEDGER RECORD TO IO7                    
         LHI   R3,IOLENQ                                                        
         LR    RF,R3                                                            
         L     RE,AIO7                                                          
         MVCL  RE,R2                                                            
         L     R2,AIO1                                                          
         GOTOR (#SECCHK,ASECCHK)   SECURITY CHECK                               
         BE    SETCPY13                                                         
         MVC   ROUERRV,=AL2(AE$SCLOK)                                           
         J     EXITN                                                            
SETCPY13 L     R2,AIO1                                                          
         LA    R2,LDGRFST          LOCATE LEVEL ELEMENT                         
         USING ACLELD,R2                                                        
         XR    R0,R0                                                            
         MVI   BYTE4,0                                                          
         MVI   PLSEC,0                                                          
SETCPY14 CLI   ACLEL,0                                                          
         BE    SETCPY22                                                         
         CLI   ACLEL,ACLELQ                                                     
         BE    SETCPY18                                                         
         CLI   ACLEL,LDGELQ                                                     
         BE    SETCPY20                                                         
         CLI   ACLEL,RSTELQ                                                     
         BE    SETCPY21                                                         
SETCPY16 IC    R0,ACLLN                                                         
         AR    R2,R0                                                            
         B     SETCPY14                                                         
SETCPY18 MVC   PCLILEN,ACLELLVA                                                 
         MVC   PPROLEN,ACLELLVB                                                 
         MVC   PJOBLEN,ACLELLVC                                                 
         OI    BYTE4,X'10'                                                      
         B     SETCPY16                                                         
         USING LDGELD,R2                                                        
SETCPY20 MVC   POFFPOS,LDGOPOS                                                  
         OI    BYTE4,X'20'                                                      
         B     SETCPY16                                                         
         USING RSTELD,R2                                                        
SETCPY21 MVC   PLSEC,RSTSECY+1                                                  
         B     SETCPY16                                                         
*                                                                               
SETCPY22 CLI   BYTE4,X'10'+X'20'                                                
         BE    SETCPY24                                                         
         DC    H'0'                                                             
                                                                                
SETCPY24 DS    0H                                                               
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         XC    OFFALD(OFFAWORK-OFFALD),OFFALD                                   
                                                                                
         MVC   OFFACOMF,ACOMFACS   INITIALIZE OFFAL BLOCK                       
         MVC   OFFAALPH,CUAALF                                                  
         MVC   OFFACPY,CUXCPY                                                   
         MVC   OFFACST1,SCPYEL+CPYSTAT1-CPYELD                                  
         MVC   OFFACST2,SCPYEL+CPYSTAT2-CPYELD                                  
         MVC   OFFACST3,SCPYEL+CPYSTAT3-CPYELD                                  
         MVC   OFFACST4,SCPYEL+CPYSTAT4-CPYELD                                  
         MVC   OFFACST5,SCPYEL+CPYSTAT5-CPYELD                                  
         MVC   OFFACST6,SCPYEL+CPYSTAT6-CPYELD                                  
         MVC   OFFACST7,SCPYEL+CPYSTAT7-CPYELD                                  
         MVC   OFFACST8,SCPYEL+CPYSTAT8-CPYELD                                  
*&&UK*&& MVC   OFFAXSTA,SCPXEL+CPXSTATA-CPXELD                                  
         MVI   OFFACTRL,OFFACCNV   NEW RECORDS DIR/MST                          
         MVC   OFFALIMA,CUACCS                                                  
         MVC   OFFAAUTH,CUAUTH                                                  
                                                                                
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI     SET TO INITIALIZE                            
                                                                                
         GOTO1 VOFFAL              INITIALIZE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING LDGRECD,R2                                                       
SETCPY60 LA    R2,IOKEY            1R LEDGER                                    
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(2),=C'1R'                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    SETCPY62                                                         
         CLI   GIND2,GI2ETIM       ETIME/EEXPENSE RUNNING?                      
         BE    *+12                                                             
         CLI   GIND2,GI2EEXP                                                    
         BNE   SETCPY78                                                         
         DC    H'0'                DIE FOR THESE AS MUST BE PRESENT             
SETCPY62 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    SETCPY64                                                         
         CLI   GIND2,GI2ETIM       ETIME/EEXPENSE RUNNING?                      
         BE    *+12                                                             
         CLI   GIND2,GI2EEXP                                                    
         BNE   SETCPY78                                                         
         DC    H'0'                1R LEDGER NOT ON FILE                        
SETCPY64 CLI   GIND2,GI2ETIM       ETIME/EEXPENSE RUNNING?                      
         BE    SETCPY70                                                         
         CLI   GIND2,GI2EEXP                                                    
         BNE   SETCPY72                                                         
SETCPY70 GOTOR (#SECCHK,ASECCHK)   SECURITY CHECK                               
         BE    SETCPY72                                                         
         MVC   ROUERRV,=AL2(AE$SCLOK)                                           
         J     EXITN                                                            
SETCPY72 L     R2,AIO1                                                          
         LA    R2,LDGRFST          LOCATE LEVEL ELEMENT                         
         USING ACLELD,R2                                                        
         XR    R0,R0                                                            
SETCPY74 CLI   ACLEL,0                                                          
         BE    SETCPY78                                                         
         CLI   ACLEL,ACLELQ                                                     
         BE    SETCPY76                                                         
         IC    R0,ACLLN                                                         
         AR    R2,R0                                                            
         B     SETCPY74                                                         
                                                                                
SETCPY76 MVC   ONERL1L,ACLELLVA                                                 
         MVC   ONERL2L,ACLELLVB                                                 
         MVC   ONERL3L,ACLELLVC                                                 
         MVC   ONERL4L,ACLELLVD                                                 
                                                                                
         USING OFFRECD,R2                                                       
SETCPY78 MVI   G#OFSTA2,0          READ OFFRECD                                 
*        TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
*        BZ    SETCPY80                                                         
         CLI   CUACCS,0                                                         
         BE    SETCPY80                                                         
         LA    R2,IOKEY                                                         
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,CUACCS+1                                                 
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         BZ    *+10                                                             
         MVC   OFFKOFF,CUACCS+2                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BNE   SETCPY80                                                         
         MVC   G#OFSTA2,OFFKSTA2                                                
                                                                                
SETCPY80 DS    0H                  ESTIMATE INT APPROVALS OVERRIDE?             
         TM    SCPXEL+CPXSTAT1-CPXELD,CPXSEIAO                                  
         BNZ   SETCPY82                                                         
         NI    G#OFSTA2,X'FF'-OFFSIAEQ                                          
         TM    SCPXEL+CPXSTAT1-CPXELD,CPXSEIAY                                  
         BZ    SETCPY82                                                         
         OI    G#OFSTA2,OFFSIAEQ                                                
                                                                                
SETCPY82 DS    0H                  INTERNAL USE ONLY OVERRIDE?                  
         TM    SCPXEL+CPXSTAT2-CPXELD,CPXIUSDO                                  
         BNZ   SETCPYX                                                          
         NI    G#OFSTA2,X'FF'-OFFIUSDF                                          
         TM    SCPXEL+CPXSTAT2-CPXELD,CPXIUSDY                                  
         BZ    SETCPYX                                                          
         OI    G#OFSTA2,OFFIUSDF                                                
                                                                                
SETCPYX  J     EXITY                                                            
                                                                                
         DROP  R1,R2,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ ORDER PROFILE RECORDS                                          *         
***********************************************************************         
         DS    0H                                                               
ORDPRF   J     *+12                                                             
         DC    CL8'*ORDPRF*'                                                    
         LR    RB,RF                                                            
         USING ORDPRF,RB                                                        
         USING ORDPRFD,R3                                                       
         L     R3,0(R1)                                                         
         XC    ORDPRFD(ORDPRFL),ORDPRFD                                         
         XC    ULEBYPRF,ULEBYPRF                                                
         XC    ELEMENT,ELEMENT     IF 1ST TIME READ EBUYER PROFILE              
         MVI   ELEMENT+00,C'A'-X'40'                                            
         MVC   ELEMENT+01(3),=C'PO1'                                            
         MVC   ELEMENT+12(2),CUAALF                                             
         GOTO1 VGETPROF,DMCB,ELEMENT,ULEBYPRF,VDATAMGR                          
                                                                                
         MVI   TEMP2+00,DEFEBY01   SET DEFAULT VALUES                           
         MVI   TEMP2+01,DEFEBY02                                                
         MVI   TEMP2+02,DEFEBY03                                                
         MVI   TEMP2+03,DEFEBY04                                                
         MVI   TEMP2+04,DEFEBY05                                                
         MVI   TEMP2+05,DEFEBY06                                                
         MVI   TEMP2+06,DEFEBY07                                                
         MVI   TEMP2+07,DEFEBY08                                                
         MVI   TEMP2+08,DEFEBY09                                                
         MVI   TEMP2+09,DEFEBY10                                                
         MVI   TEMP2+10,DEFEBY11                                                
         MVI   TEMP2+11,DEFEBY12                                                
         MVI   TEMP2+12,DEFEBY13                                                
         MVI   TEMP2+13,DEFEBY14                                                
         MVI   TEMP2+14,DEFEBY15                                                
         MVI   TEMP2+15,DEFEBY16                                                
                                                                                
         LA    RE,ULEBYPRF+1       SET DEFAULTS (#1 IS AMOUNT)                  
         LA    RF,TEMP2+1                                                       
         LA    R1,L'ULEBYPRF-1                                                  
                                                                                
ORDPRF02 CLI   0(RE),C' '                                                       
         BE    ORDPRF03                                                         
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
ORDPRF03 MVC   0(1,RE),0(RF)                                                    
                                                                                
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R1,ORDPRF02                                                      
                                                                                
         MVC   BYTE2,PROEBY13                                                   
         NI    BYTE2,X'0F'         MAKE MULTIPLIER BINARY NUMBER                
         ZAP   MINOAMT,=P'0'                                                    
*&&UK*&& CLI   PROEBY13,C'9'       9 -> IGNORE ALL VALUE                        
*&&US*&& CLI   BYTE2,9             9 -> IGNORE ALL VALUE - US HAS               
         BE    ORDPRF06                  A MIXTURE OF BINARY AND CHAR           
         XR    RE,RE                                                            
         IC    RE,PROEBY01                                                      
         MHI   RE,100              INTO CENTS/PENNIES                           
         CVD   RE,DUB                                                           
         ZAP   MINOAMT,DUB                                                      
         XR    RE,RE                                                            
         ICM   RE,1,BYTE2                                                       
         BZ    ORDPRF06                                                         
                                                                                
ORDPRF04 MP    MINOAMT,=P'10'                                                   
         BCT   RE,ORDPRF04                                                      
                                                                                
ORDPRF06 XC    ULPO2PRF,ULPO2PRF   PROFILES (2)                                 
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+00,C'A'-X'40'                                            
         MVC   ELEMENT+01(3),=C'PO2'                                            
         MVC   ELEMENT+12(2),CUAALF                                             
         GOTO1 VGETPROF,DMCB,ELEMENT,ULPO2PRF,VDATAMGR                          
                                                                                
         CLI   PROPO201,C' '       SET DEFAULT VALUES                           
         BH    *+8                                                              
         MVI   PROPO201,DEFPO201                                                
         CLI   PROPO202,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO202,DEFPO202                                                
         CLI   PROPO204,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO204,DEFPO204                                                
         CLI   PROPO205,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO205,DEFPO205                                                
         CLI   PROPO206,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO206,DEFPO206                                                
         CLI   PROPO208,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO208,DEFPO208                                                
         CLI   PROPO209,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO209,DEFPO209                                                
         CLI   PROPO210,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO210,DEFPO210                                                
         CLI   PROPO212,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO212,DEFPO212                                                
         CLI   PROPO213,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO213,DEFPO213                                                
         CLI   PROPO214,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO214,DEFPO214                                                
         CLI   PROPO216,C' '                                                    
         BH    *+8                                                              
         MVI   PROPO216,DEFPO216                                                
                                                                                
         MVI   RNSAIND,0           REQ. NO. AND SELF APPR. INDICATOR            
                                                                                
         OI    RNSAIND,RNSARYQ     REQUISITION NUMBER                           
         CLI   PROEBY09,YESQ       ALL TYPES?                                   
         BE    ORDPRF08                                                         
         CLI   PROEBY09,C'P'       PRODUCTION?                                  
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSARPQ                                               
         CLI   PROPO205,YESQ       EXPENSE?                                     
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSAREQ                                               
         CLI   PROPO209,YESQ       INTERNAL?                                    
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSARIQ                                               
         CLI   PROPO213,YESQ       ARTISTS?                                     
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSARAQ                                               
                                                                                
ORDPRF08 OI    RNSAIND,RNSASYQ     SELF APPROVAL?                               
         CLI   PROEBY08,YESQ       ALL TYPES?                                   
         BE    ORDPRF10                                                         
         CLI   PROEBY08,C'P'       PRODUCTION?                                  
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSASPQ                                               
         CLI   PROPO206,YESQ       EXPENSE?                                     
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSASEQ                                               
         CLI   PROPO210,YESQ       INTERNAL?                                    
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSASIQ                                               
         CLI   PROPO214,YESQ       ARTISTS?                                     
         BE    *+8                                                              
         NI    RNSAIND,FF-RNSASAQ                                               
                                                                                
ORDPRF10 ZAP   MINPAMT,MINOAMT     MINIMUM AMOUNTS                              
         ZAP   MINEAMT,MINOAMT                                                  
         ZAP   MINAAMT,MINOAMT                                                  
         ZAP   MINIAMT,MINOAMT                                                  
*&&UK*&& CLI   PROEBY13,C'9'       9 -> IGNORE ALL VALUE                        
*&&US*&& CLI   PROEBY13,9          9 -> IGNORE ALL VALUE                        
         BNE   ORDPRF18                                                         
                                                                                
         ZAP   MINPAMT,=P'0'                                                    
         CLI   PROPO203,0          PRODUCTION                                   
         BE    ORDPRF12                                                         
         MVC   BYTE2,PROPO204                                                   
         NI    BYTE2,X'0F'         MAKE MULTIPLIER BINARY NUMBER                
         XR    RE,RE                                                            
         IC    RE,PROPO203                                                      
         MHI   RE,100              INTO CENTS/PENNIES                           
         CVD   RE,DUB                                                           
         ZAP   MINPAMT,DUB                                                      
         XR    RE,RE                                                            
         ICM   RE,1,BYTE2                                                       
         BZ    ORDPRF12                                                         
         MP    MINPAMT,=P'10'                                                   
         BCT   RE,*-6                                                           
                                                                                
ORDPRF12 ZAP   MINEAMT,=P'0'                                                    
         CLI   PROPO207,0          EXPENSE   ### WIP                            
         BE    ORDPRF14                                                         
         MVC   BYTE2,PROPO208                                                   
         NI    BYTE2,X'0F'         MAKE MULTIPLIER BINARY NUMBER                
         XR    RE,RE                                                            
         IC    RE,PROPO207                                                      
         MHI   RE,100              INTO CENTS/PENNIES                           
         CVD   RE,DUB                                                           
         ZAP   MINEAMT,DUB                                                      
         XR    RE,RE                                                            
         ICM   RE,1,BYTE2                                                       
         BZ    ORDPRF14                                                         
         MP    MINEAMT,=P'10'                                                   
         BCT   RE,*-6                                                           
                                                                                
ORDPRF14 ZAP   MINIAMT,=P'0'                                                    
         CLI   PROPO211,0          EXPENSE   ### WIP                            
         BE    ORDPRF16                                                         
         MVC   BYTE2,PROPO212                                                   
         NI    BYTE2,X'0F'         MAKE MULTIPLIER BINARY NUMBER                
         XR    RE,RE                                                            
         IC    RE,PROPO211                                                      
         MHI   RE,100              INTO CENTS/PENNIES                           
         CVD   RE,DUB                                                           
         ZAP   MINIAMT,DUB                                                      
         XR    RE,RE                                                            
         ICM   RE,1,BYTE2                                                       
         BZ    ORDPRF16                                                         
         MP    MINIAMT,=P'10'                                                   
         BCT   RE,*-6                                                           
                                                                                
ORDPRF16 ZAP   MINAAMT,=P'0'                                                    
         CLI   PROPO215,0          EXPENSE   ### WIP                            
         BE    ORDPRF18                                                         
         MVC   BYTE2,PROPO216                                                   
         NI    BYTE2,X'0F'         MAKE MULTIPLIER BINARY NUMBER                
         XR    RE,RE                                                            
         IC    RE,PROPO215                                                      
         MHI   RE,100              INTO CENTS/PENNIES                           
         CVD   RE,DUB                                                           
         ZAP   MINAAMT,DUB                                                      
         XR    RE,RE                                                            
         ICM   RE,1,BYTE2                                                       
         BZ    ORDPRF18                                                         
         MP    MINAAMT,=P'10'                                                   
         BCT   RE,*-6                                                           
                                                                                
ORDPRF18 XC    ULPO3PRF,ULPO3PRF   PROFILES (3)                                 
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+00,C'A'-X'40'                                            
         MVC   ELEMENT+01(3),=C'PO3'                                            
         MVC   ELEMENT+12(2),CUAALF                                             
         GOTO1 VGETPROF,DMCB,ELEMENT,ULPO3PRF,VDATAMGR                          
                                                                                
         MVI   TEMP2+00,DEFPO301   SET DEFAULT VALUES                           
         MVI   TEMP2+01,DEFPO302                                                
         MVI   TEMP2+02,DEFPO303                                                
         MVI   TEMP2+03,DEFPO304                                                
         MVI   TEMP2+04,DEFPO305                                                
         MVI   TEMP2+05,DEFPO306                                                
         MVI   TEMP2+06,DEFPO307                                                
         MVI   TEMP2+07,DEFPO308                                                
         MVI   TEMP2+08,DEFPO309                                                
         MVI   TEMP2+09,DEFPO310                                                
         MVI   TEMP2+10,DEFPO311                                                
         MVI   TEMP2+11,DEFPO312                                                
         MVI   TEMP2+12,DEFPO313                                                
         MVI   TEMP2+13,DEFPO314                                                
         MVI   TEMP2+14,DEFPO315                                                
         MVI   TEMP2+15,DEFPO316                                                
                                                                                
         LA    RE,ULPO3PRF         SET DEFAULTS                                 
         LA    RF,TEMP2                                                         
         LA    R1,L'ULPO3PRF                                                    
                                                                                
ORDPRF20 CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R1,ORDPRF20                                                      
                                                                                
ORDPRF22 J     EXITY                                                            
         DROP  RB                                                               
                                                                                
***********************************************************************         
* READ LEDGER FOR STRUCTURE AND OFFICE POSITION                       *         
***********************************************************************         
         DS    0H                                                               
SETLDG   J     *+12                                                             
         DC    CL8'*SETLDG*'                                                    
         LR    RB,RF                                                            
         USING SETLDG,RB                                                        
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(2),LDGAUL                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#SECCHK,ASECCHK)   SECURITY CHECK                               
         JNE   EXITN                                                            
         L     R2,AIO1                                                          
         LA    R2,LDGRFST          LOCATE LEVEL ELEMENT                         
         USING ACLELD,R2                                                        
         XR    R0,R0                                                            
         MVI   BYTE4,0                                                          
SETLDG10 CLI   ACLEL,0                                                          
         BE    SETLDG40                                                         
         CLI   ACLEL,ACLELQ                                                     
         BE    SETLDG20                                                         
         CLI   ACLEL,LDGELQ                                                     
         BE    SETLDG25                                                         
         CLI   ACLEL,RSTELQ                                                     
         BE    SETLDG30                                                         
SETLDG15 IC    R0,ACLLN                                                         
         AR    R2,R0                                                            
         B     SETLDG10                                                         
SETLDG20 MVC   LDGAL1,ACLELLVA                                                  
         MVC   LDGAL2,ACLELLVB                                                  
         MVC   LDGAL3,ACLELLVC                                                  
         MVC   LDGAL4,ACLELLVD                                                  
         OI    BYTE4,X'10'                                                      
         B     SETLDG15                                                         
         USING LDGELD,R2                                                        
SETLDG25 MVC   LDGAOP,LDGOPOS                                                   
         OI    BYTE4,X'20'                                                      
         B     SETLDG15                                                         
         USING RSTELD,R2                                                        
SETLDG30 MVC   LDGASC,RSTSECY+1                                                 
         B     SETLDG15                                                         
*                                                                               
SETLDG40 CLI   BYTE4,X'10'+X'20'                                                
         JNE   EXITN                                                            
         GOTOR (#SECCHK,ASECCHK)   SECURITY CHECK                               
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET WORK CODE DESCRIPTION (DATA PASSED AND RETURNED IN TEMP2)       *         
***********************************************************************         
         DS    0H                                                               
GETWCD   J     *+12                                                             
         DC    CL8'*GETWCD*'                                                    
         LR    RB,RF                                                            
         USING GETWCD,RB                                                        
         USING WCORECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   WCOKEY,SPACES       BUILD KEY TO READ                            
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,TEMP2                                                    
         MVC   TEMP2(20),SPACES                                                 
         XC    TEMP2+20(40),TEMP2+20                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETWCDX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETWCDX                                                          
         L     R3,AIO3                                                          
         LA    R2,WCORFST                                                       
         USING WCOELD,R2                                                        
         XR    R0,R0                                                            
GETWCD2  CLI   WCOEL,WCOELQ                                                     
         JE    GETWCD4                                                          
         CLI   WCOEL,0                                                          
         JE    GETWCDX                                                          
         IC    R0,WCOLN                                                         
         AR    R2,R0                                                            
         J     GETWCD2                                                          
GETWCD4  MVC   TEMP2,WCODESC                                                    
         XR    R1,R1                                                            
         IC    R1,WCOLN                                                         
         SHI   R1,1                                                             
         MVC   TEMP2+20(0),WCOELD                                               
         EX    R1,*-6                                                           
GETWCDX  J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT NAME                                                    *         
* - ACCOUNT PASSSED IN TEMP2(14)                                      *         
* - NAME RETURNED IN TEMP2(36)                                        *         
* - CURRENCY RETURNED IN TEMP2+50                                     *         
***********************************************************************         
         DS    0H                                                               
GETACN   J     *+12                                                             
         DC    CL8'*GETACN*'                                                    
         LR    RB,RF                                                            
         USING GETACN,RB                                                        
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       BUILD KEY TO READ                            
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,TEMP2                                                    
         MVC   TEMP2,SPACES                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETACNX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETACNX                                                          
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
         XC    BYTE3,BYTE3                                                      
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
GETACN2  CLI   NAMEL,0                                                          
         BE    GETACN12                                                         
         TM    SCPYEL+CPYSTATC-CPYELD,CPYSMEDN                                  
         BZ    GETACN4                                                          
         CLI   NAMEL,SNMELQ                                                     
         BE    GETACN8                                                          
GETACN4  TM    SCPYEL+CPYSTATC-CPYELD,CPYSALTN   APPLIES TO SJ ONLY             
         BZ    GETACN6                                                          
         CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'SJ'                              
         BNE   GETACN6                                                          
         CLI   NAMEL,XNMELQ                                                     
         BE    GETACN10                                                         
GETACN6  IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETACN2                                                          
*                                                                               
         USING SNMELD,R3                                                        
GETACN8  XR    RE,RE                                                            
         MVI   BYTE3,1                                                          
         IC    RE,SNMLN                                                         
         SHI   RE,SNMLN1Q+1                                                     
         MVC   TEMP2(0),SNMNAME                                                 
         EX    RE,*-6                                                           
         B     GETACN6                                                          
*                                                                               
         USING XNMELD,R3                                                        
GETACN10 XR    RE,RE                                                            
         MVI   BYTE3,1                                                          
         IC    RE,XNMSUBL                                                       
         SHI   RE,1                                                             
         MVC   TEMP2(0),XNMSUBN                                                 
         EX    RE,*-6                                                           
         B     GETACN6                                                          
*                                                                               
GETACN12 LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
GETACN14 CLI   NAMEL,NAMELQ                                                     
         BE    GETACN18                                                         
         CLI   NAMEL,ASTELQ                                                     
         BE    GETACN20                                                         
         CLI   NAMEL,0                                                          
         BE    GETACNX                                                          
GETACN16 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETACN14                                                         
GETACN18 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         CLI   BYTE3,1                                                          
         BE    GETACN19                                                         
         MVC   TEMP2(0),NAMEREC                                                 
         EX    RE,*-6                                                           
         B     GETACN16                                                         
GETACN19 MVC   TEMP2+40(0),NAMEREC                                              
         EX    RE,*-6                                                           
         B     GETACN16                                                         
*                                                                               
         USING ASTELD,R3                                                        
GETACN20 MVC   TEMP2+36(3),ASTCUR                                               
         B     GETACN16                                                         
GETACNX  J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET EXPENDITURE TYPE NAME (DATA PASSED AND RETURNED IN TEMP2/WORK2) *         
***********************************************************************         
         DS    0H                                                               
GETETN   J     *+12                                                             
         DC    CL8'*GETETN*'                                                    
         LR    RB,RF                                                            
         USING GETETN,RB                                                        
         USING ETYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ETYKEY,ETYKEY       BUILD KEY TO READ                            
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,CUXCPY                                                   
         MVC   ETYKCODE,TEMP2                                                   
         MVC   TEMP2,SPACES                                                     
         MVC   WORK2,SPACES                                                     
         MVC   IOKEYSAV,IOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BNE   GETETNX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETETNX                                                          
         L     R2,AIO3                                                          
         LA    R3,ETYRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
GETETN2  CLI   NAMEL,NAMELQ                                                     
         JE    GETETN6                                                          
         CLI   NAMEL,XNMELQ                                                     
         JE    GETETN8                                                          
         CLI   NAMEL,0                                                          
         JE    GETETNX                                                          
GETETN4  IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     GETETN2                                                          
                                                                                
GETETN6  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         AHI   RE,-3                                                            
         MVC   TEMP2(0),NAMEREC                                                 
         EX    RE,*-6                                                           
         J     GETETN4                                                          
                                                                                
         USING XNMELD,R3                                                        
GETETN8  XR    RE,RE                                                            
         IC    RE,XNMSUBL                                                       
         AHI   RE,-1                                                            
         MVC   WORK2(0),XNMSUBN                                                 
         EX    RE,*-6                                                           
         J     GETETN4                                                          
                                                                                
GETETNX  J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PERSON ID FIRST+LAST NAME + TEL. EXT. (DATA PASSED AND RETURNED *         
* IN TEMP2) USES AIO1, ALSO RETURN EMAIL ADDRESS IN APPEMAIL          *         
***********************************************************************         
         DS    0H                                                               
GETPIN   J     *+12                                                             
         DC    CL8'*GETPIN*'                                                    
         LR    RB,RF                                                            
         USING GETPIN,RB                                                        
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY     BUILD KEY TO READ                            
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    SAPEAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         BNZ   *+10                                                             
         MVC   SAPEAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SAPEPID,TEMP2                                                    
         MVC   TEMP2(57),SPACES                                                 
         XC    TEMP2+50(2),TEMP2+50                                             
         MVC   APPEMAIL,SPACES                                                  
         MVC   WORK2,SPACES                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             MATCH KEY ON ALL BUT EFFECTIVE DATE          
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),IOKEYSAV                            
         BNE   GETPIN22                                                         
         LA    R3,SAPEDATA                                                      
         USING SANAMD,R3                                                        
         XR    R0,R0                                                            
GETPIN06 CLI   SANAMEL,SANAMELQ                                                 
         JE    GETPIN10                                                         
         CLI   SANAMEL,SAPWDELQ                                                 
         JE    GETPIN16                                                         
         CLI   SANAMEL,SAPERELQ                                                 
         JE    GETPIN17                                                         
         CLI   SANAMEL,SAPEEELQ                                                 
         JE    GETPIN18                                                         
         CLI   SANAMEL,0                                                        
         JE    GETPIN22            NO NAMES?                                    
GETPIN08 IC    R0,SANAMLN          L'ELEMENT                                    
         AR    R3,R0                                                            
         J     GETPIN06                                                         
                                                                                
GETPIN10 LA    R1,SANAMELN         L'NAME                                       
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         BZ    GETPIN12                                                         
         XR    RF,RF                                                            
         IC    RF,SANAMELN                                                      
         CHI   RF,16               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,16               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   TEMP2(0),SANAME                                                  
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
GETPIN12 TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT TOO                 
         BZ    GETPIN14                                                         
         IC    RF,SANAMELN                                                      
         CHI   RF,16               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,16               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   TEMP2+32(0),SANAME                                               
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
GETPIN14 TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         BZ    GETPIN08                                                         
         IC    RF,SANAMELN                                                      
         CHI   RF,58               TEST > MAX FULL LENGTH                       
         BNH   *+8                                                              
         LHI   RF,58                                                            
         SHI   RF,1                                                             
         MVC   WORK2(0),SANAME                                                  
         EX    RF,*-6                                                           
                                                                                
         CHI   RF,16-1             TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,16-1             SET IT IF GREATER                            
         MVC   TEMP2+16(0),SANAME                                               
         EX    RF,*-6                                                           
         J     GETPIN08                                                         
                                                                                
         USING SAPWDD,R3                                                        
GETPIN16 MVC   TEMP2+50(2),SAPWDNUM      PASS 2 CHAR HEX PID                    
         J     GETPIN08                                                         
                                                                                
         USING SAPERD,R3                                                        
GETPIN17 MVC   TEMP2+52(5),SAPEREXT      PASS TELEPHONE EXTENSION               
         J     GETPIN08                                                         
                                                                                
         USING SAPEED,R3                                                        
GETPIN18 XR    RF,RF               EXTRACT EMAIL ADRESS                         
         IC    RF,SAPEELN                                                       
         SHI   RF,3                                                             
         LTR   RF,RF                                                            
         JM    GETPIN08                                                         
         MVC   APPEMAIL(0),SAPEEID                                              
         EX    RF,*-6                                                           
         J     GETPIN08                                                         
                                                                                
*&&DO                                                                           
         USING SAPERD,R3                                                        
GETPIN20 OC    SAPERDTE,SAPERDTE   ARE THEY TERMINATED?                         
         JNZ   EXITN                                                            
*&&                                                                             
                                                                                
GETPIN22 CLC   TEMP2(2*16),SPACES  TEST ANY NAME FOUND                          
         JE    EXITN               RETURN CC NEQ                                
GETPINX  J     EXITY                                                            
                                                                                
         DROP  R1,R2,R3,RB                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
         DS    0H                                                               
SYSCHK   J     *+12                                                             
         DC    CL8'*SYSCHK*'                                                    
         LR    RB,RF                                                            
         USING SYSCHK,RB                                                        
         GOTO1 VGETFACT,DMCB,(X'80',DUB),F#SEIND                                
         TM    DUB,SEISETRO+SEIRONLY                                            
         BZ    SYSCHK2                                                          
         MVC   FULL2(2),=AL2(AE$FLRD)                                           
         TM    DUB,SEISETRO                                                     
         JNZ   EXITN               PLEASE TRY LATER                             
         MVC   FULL2(2),=AL2(AE$FLRD)                                           
         J     EXITN               CAN'T UPDATE                                 
SYSCHK2  GOTO1 (RF),(R1),(X'80',DUB),F#TSTATB                                   
         TM    DUB,TSTATROS+TSTATROM                                            
         JZ    EXITY                                                            
         MVC   FULL2(2),=AL2(AE$FLRD)                                           
         J     EXITN               CAN'T UPDATE                                 
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET 8 CHAR PID FROM 2 CHAR PID, USES AIO1, DATA PASSED IN TEMP2     *         
***********************************************************************         
         DS    0H                                                               
GETPID   J     *+12                                                             
         DC    CL8'*GETPID*'                                                    
         LR    RB,RF                                                            
         USING GETPID,RB                                                        
         USING SA0REC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    SA0KAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         BNZ   *+10                                                             
         MVC   SA0KAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SA0KNUM,TEMP2                                                    
         MVC   TEMP2,SPACES                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         JNE   EXITN                                                            
         L     R2,AIO1                                                          
         LA    R3,SA0DATA                                                       
         USING SAPALD,R3                                                        
         XR    R0,R0                                                            
GETPID2  CLI   SAPALEL,SAPALELQ                                                 
         JE    GETPID4                                                          
         CLI   SAPALEL,0                                                        
         JE    EXITN                                                            
         IC    R0,SAPALLN                                                       
         AR    R3,R0                                                            
         J     GETPID2                                                          
GETPID4  MVC   TEMP2(8),SAPALPID   PASS 8 CHAR PID                              
         J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET USER ID NAME/DESC FROM CODE (DATA PASSED AND RETURNED IN TEMP2) *         
***********************************************************************         
         DS    0H                                                               
GETUSR   J     *+12                                                             
         DC    CL8'*GETUSR*'                                                    
         LR    RB,RF                                                            
         USING GETUSR,RB                                                        
         USING CTIREC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TEMP2                                                    
         MVC   TEMP2(10),SPACES                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         BNE   GETUSR8                                                          
         L     R2,AIO1                                                          
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         XR    R0,R0                                                            
GETUSR2  CLI   CTDSCEL,CTDSCELQ                                                 
         BE    GETUSR4                                                          
         CLI   CTDSCEL,0                                                        
         BE    GETUSR8                                                          
         IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         B     GETUSR2                                                          
         SPACE 1                                                                
GETUSR4  MVC   TEMP2(10),CTDSC                                                  
         SPACE 1                                                                
GETUSR8  CLC   TEMP2(10),SPACES    TEST ANYTHING FOUND                          
         JE    EXITN               RETURN CC NEQ                                
GETUSRX  J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT A CHAR STRING INTO PL8 (IN: TEMP2, OUT: TEMP2+16)           *         
***********************************************************************         
         DS    0H                                                               
CONAMT   J     *+12                                                             
         DC    CL8'*CONAMT*'                                                    
         LR    RB,RF                                                            
         USING CONAMT,RB                                                        
         ZAP   TEMP2+16(8),=P'0'                                                
         CLC   TEMP2(16),SPACES                                                 
         JNH   EXITY                                                            
         LA    RE,TEMP2+15                                                      
         LA    RF,16                                                            
CONAMT2  CLI   0(RE),C'0'                                                       
         BNL   CONAMT4                                                          
         SHI   RE,1                                                             
         BCT   RF,CONAMT2                                                       
         DC    H'0'                                                             
CONAMT4  XR    R1,R1                                                            
         LA    RE,TEMP2                                                         
         CLI   TEMP2,C'-'                                                       
         BNE   CONAMT6                                                          
         LA    R1,1                                                             
         SHI   RF,1                                                             
         AHI   RE,1                                                             
CONAMT6  AHI   RF,X'6F'                                                         
         EX    RF,*+8                                                           
         B     CONAMT8                                                          
         PACK  TEMP2+16(0),0(0,RE)                                              
CONAMT8  LTR   R1,R1                                                            
         JZ    EXITY                                                            
         MP    TEMP2+16(8),=P'-1'                                               
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET USER ID CODE FROM NAME (DATA PASSED AND RETURNED IN TEMP2)      *         
***********************************************************************         
         DS    0H                                                               
GETUID   J     *+12                                                             
         DC    CL8'*GETUID*'                                                    
         LR    RB,RF                                                            
         USING GETUID,RB                                                        
         USING CTIREC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,TEMP2                                                     
         XC    TEMP2+10(2),TEMP+10                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO1'                               
         JNE   EXITN                                                            
         L     R2,AIO1                                                          
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         XR    R0,R0                                                            
GETUID2  CLI   CTDSCEL,CTDSCELQ                                                 
         BE    GETUID6                                                          
         CLI   CTDSCEL,0                                                        
         JE    EXITN                                                            
GETUID4  IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         B     GETUID2                                                          
         SPACE 1                                                                
GETUID6  CLI   CTDSCLEN,4                                                       
         BNE   GETUID4                                                          
         MVC   TEMP2+10(2),CTDSC                                                
         J     EXITY                                                            
         DROP  R2,R3,RB                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK SECURITY LEVEL AGAINST TWA                                    *         
***********************************************************************         
         DS    0H                                                               
SECCHK   J     *+12                                                             
         DC    CL8'*SECCHK*'                                                    
         LR    RB,RF                                                            
         USING SECCHK,RB                                                        
         CLI   TWAAUTH+1,0                                                      
         JE    EXITY                                                            
         L     R2,AIO1                                                          
         AHI   R2,ACCRFST-ACCRECD                                               
         USING RSTELD,R2                                                        
         XR    R0,R0                                                            
SECCHK02 CLI   RSTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RSTEL,RSTELQ                                                     
         BE    SECCHK04                                                         
         IC    R0,RSTLN                                                         
         AR    R2,R0                                                            
         B     SECCHK02                                                         
SECCHK04 CLC   RSTSECY+1(1),TWAAUTH+1                                           
         JNH   EXITY                                                            
         J     EXITN                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ACC PERSON CODE FORM PID, ALSO NAMES                            *         
***********************************************************************         
         DS    0H                                                               
ACCPID   J     *+12                                                             
         DC    CL8'*ACCPID '                                                    
         LR    RB,RF                                                            
         USING ACCPID,RB                                                        
                                                                                
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,TEMP2                                                    
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   CSVKEY1,IOKEY                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY1(PIDKPER-PIDRECD),IOKEY                                   
         BE    ACCPID2                                                          
         MVC   FULL2(2),=AL2(AE$NCPID)                                          
         J     EXITN                                                            
                                                                                
ACCPID2  MVC   TEMP2+10(8),PIDKPER                                              
                                                                                
         J     EXITY                                                            
                                                                                
         DROP  RB,R2                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PID FROM ACC PERSON                                             *         
***********************************************************************         
         DS    0H                                                               
ACCPFP   J     *+12                                                             
         DC    CL8'*ACCPFP '                                                    
         LR    RB,RF                                                            
         USING ACCPFP,RB                                                        
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TEMP2                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   EXITN                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
                                                                                
         L     R2,AIO3                                                          
         LA    R3,PERRFST          LOCATE ELEMENTS                              
         XC    TEMP2+8(2),TEMP2+8                                               
         MVC   TEMP2+10(70),SPACES                                              
         XR    R0,R0                                                            
         USING PIDELD,R3                                                        
                                                                                
ACCPFP1  CLI   PIDEL,0                                                          
         BE    ACCPFP9                                                          
         CLI   PIDEL,PIDELQ                                                     
         BE    ACCPFP3                                                          
         CLI   PIDEL,GPNELQ                                                     
         BE    ACCPFP4                                                          
                                                                                
ACCPFP2  IC    R0,PIDLN                                                         
         AR    R3,R0                                                            
         B     ACCPFP1                                                          
                                                                                
ACCPFP3  MVC   TEMP2+8(2),PIDNO                                                 
         B     ACCPFP2                                                          
                                                                                
         USING GPNELD,R3                                                        
ACCPFP4  LA    RE,TEMP2+10         FIRST NAME HERE                              
         CLI   GPNTYP,GPNTLST                                                   
         BNE   *+8                                                              
         LA    RE,TEMP2+30         LAST NAME HERE                               
         XR    R1,R1                                                            
         IC    R1,GPNLN                                                         
         SHI   R1,GPNLNQ+1                                                      
         LTR   R1,R1                                                            
         BM    ACCPFP2                                                          
         MVC   0(0,RE),GPNNME                                                   
         EX    R1,*-6                                                           
         B     ACCPFP2                                                          
                                                                                
ACCPFP9  OC    TEMP2+8(2),TEMP2+8                                               
         JZ    EXITN                                                            
                                                                                
         CLC   TEMP2+10(70),SPACES                                              
         JE    EXITN                                                            
                                                                                
         J     EXITY                                                            
                                                                                
         DROP  RB,R2,R3                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ITEM FOR ESTIMATE (ROW) COPY AND ORDER COPY                     *         
***********************************************************************         
         DS    0H                                                               
GETITM   J     *+12                                                             
         DC    CL8'*GETITM '                                                    
         LR    RB,RF                                                            
         USING GETITM,RB                                                        
         STC   R1,BYTE1                                                         
         USING PASRECD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,G#SGOLLA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   EXITL                                                            
         TM    PASKSTAT,ARTKLOCK                                                
         JNZ   EXITL                                                            
         TM    PASKSTAT,ARTKNOPQ                                                
         JNZ   EXITH                                                            
         XR    R1,R1                                                            
         ICM   R1,B'0011',CUXPNUM                                               
         CHI   R1,XPRODIKQ             If Aura ignore flexible price            
         JE    *+12                                                             
         TM    PASKSTAT,ARTKFLXQ                                                
         JNZ   EXITH                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   EXITL                                                            
                                                                                
         USING ARTRECD,R4                                                       
         L     R4,AIO3                                                          
                                                                                
         USING X_INFOD,R2                                                       
         USING X_LISTTABD,R3                                                    
         LA    R2,TEMP2                                                         
         L     R3,AELEAREA                                                      
         CLI   BYTE1,C'E'                                                       
         JE    *+8                                                              
         L     R3,AOFFAREA                                                      
         GOTO1 VDATCON,DMCB,(5,0),(1,X_INFODAT)                                 
*                                                                               
         XC    X_INFOSLL,X_INFOSLL                                              
         CLC   G#SGSUPP,SPACES                                                  
         JNH   GETITM0                                                          
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,G#SGSUPP                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   X_INFOSLL,LDGAL1    GET SUPPLIER LEDGER LENGTHS                  
*                                                                               
GETITM0  MVC   X_INFOART,ARTKART                                                
         MVC   X_INFOALP,CUAALF                                                 
         MVC   X_INFOCPY,CUXCPY                                                 
         MVC   X_INFOCUA,AGYCURR                                                
         MVC   X_INFOOFF,G#SGOOFF                                               
         MVC   X_INFOCLI,G#SGOCLI                                               
         MVC   X_INFOWRK,G#SGWRKC                                               
         MVC   X_INFOSUP,G#SGSUPP                                               
         CLI   CUCTRY,CTRYGER                                                   
         JE    GETITM2                                                          
         CLC   G#SGSUPP,SPACES                                                  
         JH    GETITM2                                                          
         CLI   BYTE1,C'E'                                                       
         JNE   GETITM2                                                          
         TM    ARTRSTA,ARTKTINT                                                 
         JNZ   GETITM2                                                          
         MVC   X_INFOSUP,ARTKSUP                                                
                                                                                
GETITM2  MVI   X_INFOSTT,X_INFOALQ                                              
         MVI   X_INFOTYP,X_INFOEST                                              
         CLI   BYTE1,C'E'                                                       
         JE    GETITM4                                                          
         MVI   X_INFOTYP,X_INFOORD                                              
         CLI   BYTE1,C'O'                                                       
         JE    GETITM4                                                          
         MVI   X_INFOTYP,X_INFOAOI                                              
                                                                                
GETITM4  MVI   X_INFOIOE,X_INFOINQ                                              
         TM    ARTRSTA,ARTKTINT                                                 
         BNZ   *+8                                                              
         MVI   X_INFOIOE,X_INFOEXQ                                              
         MVC   X_INFOCUR,AGYCURR                                                
         CLC   G#SGCURR,SPACES                                                  
         BNH   *+10                                                             
         MVC   X_INFOCUR,G#SGCURR                                               
         MVC   X_INFORAT,G#SGRATE                                               
         MVI   X_INFOLAN,C'N'                                                   
         MVC   X_INFOMAX,SPACES                                                 
         MVI   X_INFOFLG,0                                                      
         TM    SCPYEL+CPYSTAT6-CPYELD,CPYSFTXR                                  
         BZ    *+8                                                              
         OI    X_INFOFLG,ALLOWFTQ                                               
                                                                                
         GOTO1 VGETALP,DMCB,X_INFOD,X_LISTTABD,ACOMFACS                         
         CLI   0(R1),NOERRQ                                                     
         BE    GETITM6                                                          
         CLI   0(R1),PARAMQ                                                     
         JNE   EXITL                                                            
         DC    H'0'                PROGRAMMER'S FAULT                           
                                                                                
GETITM6  DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3,R4                                                         
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT PID CODE - PARM 1 IS PIN (BINARY)                              *         
***********************************************************************         
         DS    0H                                                               
EDTPID   J     *+12                                                             
         DC    CL8'*EDTPID*'                                                    
         LR    RB,RF                                                            
         USING EDTPID,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         MVC   TEMP2,SPACES                                                     
         OC    0(L'SA0KNUM,R2),0(R2)                                            
         JZ    EXITY                                                            
         MVC   0(L'SAPALPID,R4),=C'UNKNOWN '                                    
         MVC   TEMP2(L'SA0KNUM),0(R2)                                           
         GOTOR (#GETPID,AGETPID)                                                
         JNE   EDTPID02                                                         
         MVC   0(L'SAPALPID,R4),TEMP2                                           
EDTPID02 LHI   R3,L'SAPALPID                                                    
         STCM  R3,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT ACOUNT NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTANM   J     *+12                                                             
         DC    CL8'*EDTANM*'                                                    
         LR    RB,RF                                                            
         USING EDTANM,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR (#GETACN,AGETACN)                                                
         JNE   EXITY                                                            
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   R3,L'NAMEREC                                                     
         STCM  R3,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT CLIENT CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTCLI   J     *+12                                                             
         DC    CL8'*EDTCLI*'                                                    
         LR    RB,RF                                                            
         USING EDTCLI,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
***      CLC   0(L'ACTKULA,R2),SPACES                                           
         CLC   L'ACTKUNT+L'ACTKLDG(L'ACTKACT,R2),SPACES                         
         JNH   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         MVC   0(0,R4),L'ACTKUNT+L'ACTKLDG(R2)                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         STCM  RE,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT CLIENT NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTCLN   J     *+12                                                             
         DC    CL8'*EDTCLN*'                                                    
         LR    RB,RF                                                            
         USING EDTCLN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         AHI   RE,1                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT PRODUCT CODE - PARM 1 UNIT/LEDGER/ACCOUNT                      *         
***********************************************************************         
         DS    0H                                                               
EDTPRD   J     *+12                                                             
         DC    CL8'*EDTPRD*'                                                    
         LR    RB,RF                                                            
         USING EDTPRD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PCLILEN                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT PRODUCT NAME - PARM 1 UNIT/LEDGER/ACCOUNT                      *         
***********************************************************************         
         DS    0H                                                               
EDTPRN   J     *+12                                                             
         DC    CL8'*EDTPRN*'                                                    
         LR    RB,RF                                                            
         USING EDTPRN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
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
         CLC   0(0,RE),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         AHI   RE,1                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT JOB CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT                       *         
***********************************************************************         
         DS    0H                                                               
EDTJOB   J     *+12                                                             
         DC    CL8'*EDTJOB*'                                                    
         LR    RB,RF                                                            
         USING EDTJOB,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A JOB CODE                        
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT JOB NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                       *         
***********************************************************************         
         DS    0H                                                               
EDTJBN   J     *+12                                                             
         DC    CL8'*EDTJBN*'                                                    
         LR    RB,RF                                                            
         USING EDTJBN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
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
         CLC   0(0,RE),SPACES      DO WE HAVE A JOB CODE                        
         EX    RF,*-6                                                           
         JNH   EXITY               NO EXIT                                      
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT WORDCODE DESCIRPTION - PARM 1 IS WORDCODE                      *         
***********************************************************************         
         DS    0H                                                               
EDTWCD   J     *+12                                                             
         DC    CL8'*EDTWCD*'                                                    
         LR    RB,RF                                                            
         USING EDTWCD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'WCOKWRK,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2(L'WCOKWRK),0(R2)                                           
         GOTOR (#GETWCD,AGETWCD)                                                
         MVC   0(L'WCODESC,R4),TEMP2                                            
         LHI   RE,L'WCODESC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT USER ID - PARM 1 IS BINARY USER-ID                             *         
***********************************************************************         
         DS    0H                                                               
EDTUSR   J     *+12                                                             
         DC    CL8'*EDTUSR*'                                                    
         LR    RB,RF                                                            
         USING EDTUSR,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         OC    0(2,R2),0(R2)                                                    
         JZ    EDTUSR02                                                         
         MVC   TEMP2(2),0(R2)                                                   
         GOTOR (#GETUSR,AGETUSR)                                                
         JE    EDTUSR04                                                         
EDTUSR02 MVI   0(R4),C'<'        PASS <USER> IF NO NAME FOUND                   
         MVI   5(R4),C'>'                                                       
         XOUT  0(R2),1(R4),2                                                    
         LHI   R3,6                                                             
         STCM  R3,15,4(R5)                                                      
         J     EXITY                                                            
                                                                                
EDTUSR04 MVC   0(10,R4),TEMP2                                                   
         LHI   R3,10                                                            
         STCM  R3,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT ROLE NAME  - PARM 1 IS ROLE NUMBER (BINARY)                    *         
***********************************************************************         
         DS    0H                                                               
EDTRLN   J     *+12                                                             
         DC    CL8'*EDTRLN*'                                                    
         LR    RB,RF                                                            
         USING EDTRLN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         OC    0(L'ROLKNUM,R2),0(R2)                                            
         JZ    EXITY                                                            
         USING ROLRECD,R6                                                       
         LA    R6,IOKEY                                                         
         XC    ROLKEY,ROLKEY       BUILD KEY TO READ                            
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUXCPY                                                   
         MVC   ROLKNUM,0(R2)                                                    
         MVC   ROLKOFF,SPACES                                                   
         TM    SCPYEL+CPYSTATC-CPYELD,CPYSROFF                                  
         JZ    EDTRLN02                                                         
         MVC   ROLKOFF,SJOFFC                                                   
EDTRLN02 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
         LA    R6,ROLRFST                                                       
         USING NAMELD,R6                                                        
         XR    R0,R0                                                            
EDTRLN04 CLI   NAMEL,NAMELQ                                                     
         JE    EDTRLN06                                                         
         CLI   NAMEL,0                                                          
         JE    EXITY                                                            
         IC    R0,NAMLN                                                         
         AR    R6,R0                                                            
         J     EDTRLN04                                                         
*                                                                               
EDTRLN06 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   0(0,R4),NAMEREC                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT CAMPAIGN NAME - PARM 1 IS CAMPAIGN NUMBER (BINARY)             *         
***********************************************************************         
         DS    0H                                                               
EDTCMN   J     *+12                                                             
         DC    CL8'*EDTCMN*'                                                    
         LR    RB,RF                                                            
         USING EDTCMN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         OC    0(L'RWKKCCDE,R2),0(R2)                                           
         JZ    EXITY                                                            
         USING RWKRECD,R6                                                       
         LA    R6,IOKEY                                                         
         XC    RWKKEY,RWKKEY       BUILD KEY TO READ                            
         MVI   RWKKTYP,RWKKTYPQ                                                 
         MVI   RWKKSUB,RWKKSUBQ                                                 
         MVC   RWKKCPY,CUXCPY                                                   
         MVC   RWKKCCDE,0(R2)                                                   
         MVC   RWKKOFF,SPACES                                                   
         TM    SCPYEL+CPYSTATC-CPYELD,CPYSROFF                                  
         JZ    EDTCMN02                                                         
         MVC   RWKKOFF,SJOFFC                                                   
EDTCMN02 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
         LA    R6,RWKRFST                                                       
         USING NAMELD,R6                                                        
         XR    R0,R0                                                            
EDTCMN04 CLI   NAMEL,NAMELQ                                                     
         JE    EDTCMN06                                                         
         CLI   NAMEL,0                                                          
         JE    EXITY                                                            
         IC    R0,NAMLN                                                         
         AR    R6,R0                                                            
         J     EDTCMN04                                                         
*                                                                               
EDTCMN06 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   0(0,R4),NAMEREC                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT MEDIA CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT                     *         
***********************************************************************         
         DS    0H                                                               
EDTMED   J     *+12                                                             
         DC    CL8'*EDTMED*'                                                    
         LR    RB,RF                                                            
         USING EDTMED,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A MEDIA CODE                      
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(1,R4),0(R2)                                                    
         LHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT MEDIA NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                     *         
***********************************************************************         
         DS    0H                                                               
EDTMDN   J     *+12                                                             
         DC    CL8'*EDTMDN*'                                                    
         LR    RB,RF                                                            
         USING EDTMDN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A MEDIA CODE                      
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         USING PMDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
         LA    R6,PMDRFST                                                       
         USING PMDELD,R6                                                        
         XR    R0,R0                                                            
EDTMDN04 CLI   PMDEL,PMDELQ                                                     
         JE    EDTMDN06                                                         
         CLI   PMDEL,0                                                          
         JE    EXITY                                                            
         IC    R0,PMDLN                                                         
         AR    R6,R0                                                            
         J     EDTMDN04                                                         
*                                                                               
EDTMDN06 MVC   0(L'PMDDESC,R4),PMDDESC                                          
         LHI   RE,L'PMDDESC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT OFFICE CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTOFF   J     *+12                                                             
         DC    CL8'*EDTOFF*'                                                    
         LR    RB,RF                                                            
         USING EDTOFF,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         CLC   L'ACTKUNT+L'ACTKLDG(L'ACTKACT,R2),SPACES                         
         JNH   EXITY                                                            
                                                                                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         MVC   0(0,R4),L'ACTKUNT+L'ACTKLDG(R2)                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         STCM  RE,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT OFFICE NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTOFN   J     *+12                                                             
         DC    CL8'*EDTOFN*'                                                    
         LR    RB,RF                                                            
         USING EDTOFN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         CLC   L'ACTKUNT+L'ACTKLDG(L'ACTKACT,R2),SPACES                         
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         LLC   RE,ONERL1L                                                       
         AHI   RE,1                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT DEPARTMENT CODE - PARM 1 UNIT/LEDGER/ACCOUNT                   *         
***********************************************************************         
         DS    0H                                                               
EDTDPT   J     *+12                                                             
         DC    CL8'*EDTDPT*'                                                    
         LR    RB,RF                                                            
         USING EDTDPT,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         LLC   RE,ONERL1L                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A DEPARTMENT LEVEL                
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT DEPARTMENT NAME - PARM 1 UNIT/LEDGER/ACCOUNT                   *         
***********************************************************************         
         DS    0H                                                               
EDTDPN   J     *+12                                                             
         DC    CL8'*EDTDPN*'                                                    
         LR    RB,RF                                                            
         USING EDTDPN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         LLC   RE,ONERL1L                                                       
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         AHI   RE,L'ACTKUNT+L'ACTKLDG                                           
         SHI   RF,1                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A DEPARTMENT LEVEL                
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         LLC   RE,ONERL2L                                                       
         AHI   RE,1                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT SUB-DEPARTMENT CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT            *         
***********************************************************************         
         DS    0H                                                               
EDTSUB   J     *+12                                                             
         DC    CL8'*EDTSUB*'                                                    
         LR    RB,RF                                                            
         USING EDTSUB,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         LLC   RE,ONERL2L                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A SUB-DEPT LEVEL                  
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT SUB-DEPARTMENT NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT            *         
***********************************************************************         
         DS    0H                                                               
EDTSUN   J     *+12                                                             
         DC    CL8'*EDTSUN*'                                                    
         LR    RB,RF                                                            
         USING EDTSUN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         LLC   RE,ONERL2L                                                       
         LLC   RF,ONERL3L                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         AHI   RE,L'ACTKUNT+L'ACTKLDG                                           
         SHI   RF,1                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A SUB-DEPT LEVEL                  
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         LLC   RE,ONERL3L                                                       
         AHI   RE,1                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT PERSON CODE - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTPER   J     *+12                                                             
         DC    CL8'*EDTPER*'                                                    
         LR    RB,RF                                                            
         USING EDTPER,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         LLC   RE,ONERL3L                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         LLC   RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A PERSON CODE                     
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         MVC   0(0,R4),0(R2)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STCM  RF,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT PERSON NAME - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTPEN   J     *+12                                                             
         DC    CL8'*EDTPEN*'                                                    
         LR    RB,RF                                                            
         USING EDTPEN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         LLC   RE,ONERL3L                                                       
         LLC   RF,ONERL4L                                                       
         SR    RF,RE               RF=LENGTH OF PERSON CODE                     
         AHI   RE,1                RE=DISPLACEMENT TO PERSON CODE               
         AR    RE,R2                                                            
         SHI   RF,1                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A PERSON CODE                     
         EX    RF,*-6                                                           
         JNH   EXITY               NO EXIT                                      
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT MEDIA CODE NAME - PARM 1 IS MEDIA CODE                         *         
***********************************************************************         
         DS    0H                                                               
EDTMCN   J     *+12                                                             
         DC    CL8'*EDTMCN*'                                                    
         LR    RB,RF                                                            
         USING EDTMCN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLI   0(R2),C' '                                                       
         JNH   EXITY                                                            
                                                                                
         USING PMDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
         LA    R6,PMDRFST                                                       
         USING PMDELD,R6                                                        
         XR    R0,R0                                                            
EDTMCN04 CLI   PMDEL,PMDELQ                                                     
         JE    EDTMCN06                                                         
         CLI   PMDEL,0                                                          
         JE    EXITY                                                            
         IC    R0,PMDLN                                                         
         AR    R6,R0                                                            
         J     EDTMCN04                                                         
*                                                                               
EDTMCN06 MVC   0(L'PMDDESC,R4),PMDDESC                                          
         LHI   RE,L'PMDDESC                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT ESTIMATE SCHEME NAME - PARM 1 IS SCHEME CODE                   *         
***********************************************************************         
         DS    0H                                                               
EDTSCN   J     *+12                                                             
         DC    CL8'*EDTSCN*'                                                    
         LR    RB,RF                                                            
         USING EDTSCN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'SCHKCODE,R2),SPACES                                          
         JNH   EXITY                                                            
                                                                                
         USING SCHRECD,R6                                                       
         LA    R6,IOKEY                                                         
         XC    SCHKEY,SCHKEY                                                    
         MVI   SCHKTYP,SCHKTYPQ                                                 
         MVI   SCHKSUB,SCHKSUBQ                                                 
         MVC   SCHKCPY,CUXCPY                                                   
         MVC   SCHKUNT(L'PRODUL),PRODUL                                         
         MVC   SCHKCODE,0(R2)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
         LA    R6,SCHRFST                                                       
         USING SCHELD,R6                                                        
         XR    R0,R0                                                            
EDTSCN04 CLI   SCHEL,SCHELQ                                                     
         JE    EDTSCN06                                                         
         CLI   SCHEL,0                                                          
         JE    EXITY                                                            
         IC    R0,SCHLN                                                         
         AR    R6,R0                                                            
         J     EDTSCN04                                                         
*                                                                               
EDTSCN06 MVC   0(L'SCHNAME,R4),SCHNAME                                          
         LHI   RE,L'SCHNAME                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT GROUP NAME - PARM 1 IS GROUP CODE                              *         
***********************************************************************         
         DS    0H                                                               
EDTGRN   J     *+12                                                             
         DC    CL8'*EDTGRN*'                                                    
         LR    RB,RF                                                            
         USING EDTGRN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'GLSKGRP,R2),SPACES                                           
         JNH   EXITY                                                            
                                                                                
         USING GLSRECD,R6                                                       
         LA    R6,IOKEY                                                         
         XC    GLSKEY,GLSKEY                                                    
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUXCPY                                                   
         MVC   GLSKGRP,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     R6,AIO1                                                          
         LA    R6,GLSRFST                                                       
         USING NAMELD,R6                                                        
         XR    R0,R0                                                            
EDTGRN04 CLI   NAMEL,NAMELQ                                                     
         JE    EDTGRN06                                                         
         CLI   NAMEL,0                                                          
         JE    EXITY                                                            
         IC    R0,NAMLN                                                         
         AR    R6,R0                                                            
         J     EDTGRN04                                                         
*                                                                               
EDTGRN06 LLC   RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         MVC   0(0,R4),NAMEREC                                                  
         EX    RE,*-6                                                           
                                                                                
         AHI   RE,1                                                             
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT 2S COMPLIMENT COMPRESSED DATE                                  *         
***********************************************************************         
         DS    0H                                                               
EDT2CD   J     *+12                                                             
         DC    CL8'*EDT2CD*'                                                    
         LR    RB,RF                                                            
         USING EDT2CD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         XR    R1,R1                                                            
         ICM   R1,7,0(R2)                                                       
         LNR   R1,R1                                                            
         STCM  R1,7,0(R4)                                                       
         LA    RE,2                                                             
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
*        STCM  R1,7,TEMP2                                                       
         GOTO1 VDATCON,DMCB,(2,TEMP2),(20,0(R4))                                
         LA    RE,8                                                             
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK SETTING AS TO WHETHER TO SEND EMAILS                          *         
* R4=OFFICE CODE                                                      *         
* R5=APPLICATION                                                      *         
***********************************************************************         
         DS    0H                                                               
SEMAIL   J     *+12                READ COMPANY RECORD TO CHECK WHETHER         
         DC    CL8'*SEMAIL*'                                                    
         LR    RB,RF                                                            
         USING SEMAIL,RB                                                        
*                                                                               
         L     R4,0(R1)            OFFICE CODE                                  
         L     R5,4(R1)                                                         
         LA    R3,SCPXEL           CHECK THE APPLICATION                        
         USING CPXELD,R3                                                        
         CLI   0(R5),SE#TIME       TIMESHEETS?                                  
         BE    SEMAIL2                                                          
         CLI   0(R5),SE#ORDS       ORDERS?                                      
         BE    SEMAIL4                                                          
         CLI   0(R5),SE#ESTS       ESTIMATES?                                   
         BE    SEMAIL6                                                          
         CLI   0(R5),SE#EXPS       EXPENSES?                                    
         BE    SEMAIL8                                                          
         CLI   0(R5),SE#JOBS       JOBS                                         
         BE    SEMAIL10                                                         
         CLI   0(R5),SE#INVS       INVOICES?                                    
         BE    SEMAIL12                                                         
         DC    H'0'                PROBLEM WITH CALL                            
*                                                                               
SEMAIL2  TM    CPXSTAT4,CPXAETIO   TIMESHEETS                                   
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT4,CPXAETIM   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL4  TM    CPXSTAT4,CPXAEORO   ORDERS                                       
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT4,CPXAEORD   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL6  TM    CPXSTAT4,CPXAEESO   ESTIMATES                                    
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT4,CPXAEEST   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL8  TM    CPXSTAT5,CPXAEEXO   EXPENSES                                     
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT5,CPXAEEXP   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL10 TM    CPXSTAT5,CPXAEJOO   JOBS                                         
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT5,CPXAEJOB   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL12 TM    CPXSTAT5,CPXAEINO   INVOICES                                     
         JNZ   SEMAIL14            THEN READ AT OFFICE LEVEL                    
         TM    CPXSTAT5,CPXAEINV   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
         USING OFLPASD,IOKEY                                                    
SEMAIL14 CLC   0(2,R4),SPACES      ANY OFFICE PASSED?                           
         JNH   SEMAILN                                                          
         MVC   CSVKEY1,IOKEY       CHECK WHETHER OFFICE PASSED IS PART          
         XC    IOKEY,IOKEY         OF OFFICE LIST                               
         MVI   OFLPTYP,OFLPTYPQ                                                 
         MVI   OFLPSUB,OFLPSUBQ                                                 
         MVC   OFLPCPY,CUXCPY                                                   
         LA    RF,1                                                             
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2  2 CHAR OFFICE?                  
         BZ    *+8                                                              
         LA    RF,2                             THEN GET LENGTH RIGHT           
         MVC   OFLPOFF(0),0(R4)                                                 
         EX    RF,*-6                                                           
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   SEMAIL34                                                         
         J     SEMAIL16                                                         
*                                                                               
SEMAIL15 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
*                                                                               
SEMAIL16 CLC   CSVKEY2(OFLPOFL-OFLPASD),IOKEY                                   
         JNE   SEMAIL34                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         L     R3,OFFRFST-OFFRECD(R2)                                           
*                                                                               
SEMAIL18 CLI   CPXEL,0                                                          
         JNE   SEMAIL34                                                         
         CLI   CPXEL,CPXELQ       IS IT COMPANY EXTRA STATUS ELEMENT            
         JE    SEMAIL20                                                         
         IC    R3,CPXLN                                                         
         AR    R3,R0                                                            
         J     SEMAIL18                                                         
*                                                                               
SEMAIL20 CLI   0(R5),SE#TIME       TIMESHEETS?                                  
         BE    SEMAIL2                                                          
         CLI   0(R5),SE#ORDS       ORDERS?                                      
         BE    SEMAIL4                                                          
         CLI   0(R5),SE#ESTS       ESTIMATES?                                   
         BE    SEMAIL6                                                          
         CLI   0(R5),SE#EXPS       EXPENSES?                                    
         BE    SEMAIL8                                                          
         CLI   0(R5),SE#JOBS       JOBS                                         
         BE    SEMAIL10                                                         
         CLI   0(R5),SE#INVS       INVOICES?                                    
         BE    SEMAIL12                                                         
         DC    H'0'                PROBLEM WITH CALL                            
*                                                                               
SEMAIL22 TM    CPXSTAT4,CPXAETIO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
SEMAIL24 TM    CPXSTAT4,CPXAEORD   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
SEMAIL26 TM    CPXSTAT4,CPXAEESO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
SEMAIL28 TM    CPXSTAT5,CPXAEEXO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
SEMAIL30 TM    CPXSTAT5,CPXAEJOO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
SEMAIL32 TM    CPXSTAT5,CPXAEINO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAIL15            OFF=DO SEND EMAILS READ SEQ                  
*                                                                               
         USING OFFRECD,IOKEY                                                    
SEMAIL34 MVC   IOKEY,SPACES        READ OFFICE SETTING                          
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         LA    RF,1                                                             
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2  2 CHAR OFFICE?                  
         BZ    *+8                                                              
         LA    RF,2                             THEN GET LENGTH RIGHT           
         MVC   OFFKOFF(0),0(R4)   PASSED OFFICE                                 
         EX    RF,*-6                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   SEMAILN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,OFFRFST-OFFRECD(R2)                                           
         XR    R0,R0                                                            
*                                                                               
SEMAIL36 CLI   CPXEL,0                                                          
         JNE   SEMAILN                                                          
         CLI   CPXEL,CPXELQ       IS IT COMPANY EXTRA STATUS ELEMENT            
         JE    SEMAIL38                                                         
         IC    R3,CPXLN                                                         
         AR    R3,R0                                                            
         J     SEMAIL36                                                         
*                                                                               
SEMAIL38 CLI   0(R5),SE#TIME       TIMESHEETS?                                  
         BE    SEMAIL2                                                          
         CLI   0(R5),SE#ORDS       ORDERS?                                      
         BE    SEMAIL4                                                          
         CLI   0(R5),SE#ESTS       ESTIMATES?                                   
         BE    SEMAIL6                                                          
         CLI   0(R5),SE#EXPS       EXPENSES?                                    
         BE    SEMAIL8                                                          
         CLI   0(R5),SE#JOBS       JOBS                                         
         BE    SEMAIL10                                                         
         CLI   0(R5),SE#INVS       INVOICES?                                    
         BE    SEMAIL12                                                         
         DC    H'0'                PROBLEM WITH CALL                            
*                                                                               
SEMAIL40 TM    CPXSTAT4,CPXAETIO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL42 TM    CPXSTAT4,CPXAEORD   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL44 TM    CPXSTAT4,CPXAEESO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL46 TM    CPXSTAT5,CPXAEEXO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL48 TM    CPXSTAT5,CPXAEJOO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAIL50 TM    CPXSTAT5,CPXAEINO   DON'T SEND EMAIL FOR ORDERS                  
         JNZ   SEMAILN             ON=DON'T SEND EMAILS                         
         J     SEMAILY             OFF=DO SEND EMAILS                           
*                                                                               
SEMAILY  MVC   IOKEY,CSVKEY1       RESTORE KEY                                  
         J     EXITY                                                            
*                                                                               
SEMAILN  MVC   IOKEY,CSVKEY1       RESTORE KEY                                  
         J     EXITN                                                            
         DROP  R3,RB                                                            
***********************************************************************         
* GET OFFICE NAME                                                     *         
* - OFFICE PASSSED IN TEMP2(2)                                        *         
* - NAME RETURNED IN TEMP2(36)                                        *         
***********************************************************************         
         DS    0H                                                               
GETOFN   J     *+12                                                             
         DC    CL8'*GETOFN*'                                                    
         LR    RB,RF                                                            
         USING GETOFN,RB                                                        
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2 ARE WE 2 CHARACTER OFF           
         BNZ   GETOFN02            YES                                          
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       BUILD KEY TO READ                            
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),=C'2D'                              
         MVC   ACTKACT(L'TRNOFFC),TEMP2                                         
         MVC   TEMP2,SPACES                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETOFNX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETOFNX                                                          
         B     GETOFN04                                                         
         USING OFFRECD,R2                                                       
GETOFN02 LA    R2,IOKEY                                                         
         MVC   OFFKEY,SPACES       BUILD KEY TO READ                            
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,TEMP2                                                    
         MVC   TEMP2,SPACES                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETOFNX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETOFNX                                                          
GETOFN04 L     R2,AIO3                                                          
         LA    R3,OFFRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
GETOFN06 CLI   NAMEL,0                                                          
         BE    GETOFNX                                                          
         CLI   NAMEL,NAMELQ                                                     
         BE    GETOFN10                                                         
GETOFN08 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETOFN06                                                         
                                                                                
GETOFN10 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   TEMP2(0),NAMEREC                                                 
         EX    RE,*-6                                                           
                                                                                
GETOFNX  J     EXITY                                                            
                                                                                
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ROLE NAME FROM NAME                                             *         
* - NAME RETURNED IN TEMP2(36)                                        *         
***********************************************************************         
         DS    0H                                                               
GETROL   J     *+12                                                             
         DC    CL8'*GETROL*'                                                    
         LR    RB,RF                                                            
         USING GETROL,RB                                                        
         LM    R2,R3,0(R1)                                                      
         MVC   TEMP2,SPACES                                                     
         OC    0(L'ROLKNUM,R2),0(R2)                                            
         BZ    GETROLX                                                          
                                                                                
         USING ROLRECD,R6                                                       
         LA    R6,IOKEY                                                         
         XC    ROLKEY,ROLKEY       BUILD KEY TO READ                            
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUXCPY                                                   
         MVC   ROLKNUM,0(R2)                                                    
         MVC   ROLKOFF,SPACES                                                   
         TM    SCPYEL+CPYSTATC-CPYELD,CPYSROFF                                  
         JZ    *+10                                                             
         MVC   ROLKOFF,0(R3)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETROLX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BNE   GETROLX                                                          
GETROL04 L     R6,AIO3                                                          
         LA    R3,ROLRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
GETROL06 CLI   NAMEL,0                                                          
         BE    GETROLX                                                          
         CLI   NAMEL,NAMELQ                                                     
         BE    GETROL10                                                         
GETROL08 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     GETROL06                                                         
                                                                                
GETROL10 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   TEMP2(0),NAMEREC                                                 
         EX    RE,*-6                                                           
                                                                                
GETROLX  J     EXITY                                                            
                                                                                
         DROP  R6,R3,RB                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET MEDIA INCOME - PARM 1 IS UNIT/LEDGER/ACCOUNT                    *         
***********************************************************************         
         DS    0H                                                               
EDTMIN   J     *+12                                                             
         DC    CL8'*EDTMIN*'                                                    
         LR    RB,RF                                                            
         USING EDTMIN,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),PRODUL                                 
         JNE   EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         AR    R2,RE                                                            
         AHI   R2,L'ACTKUNT+L'ACTKLDG                                           
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         CLC   0(0,R2),SPACES      DO WE HAVE A MEDIA CODE                      
         EX    RF,*-6                                                           
         JNH   EXITY               NO                                           
         USING PMDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(R2)                                                    
         L     R6,AIO1                                                          
         CLC   IOKEY(PMDKEND),PMDKEY     HAVE WE ALREADY GOT RECORD             
         JE    EDTMIN02                  YES                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
         L     R6,AIO1                                                          
EDTMIN02 LA    R6,PMDRFST                                                       
         USING PMDELD,R6                                                        
         XR    R0,R0                                                            
EDTMIN04 CLI   PMDEL,PMDELQ                                                     
         JE    EDTMIN06                                                         
         CLI   PMDEL,0                                                          
         JE    EXITY                                                            
         IC    R0,PMDLN                                                         
         AR    R6,R0                                                            
         J     EDTMIN04                                                         
*                                                                               
EDTMIN06 CLI   PMDLN,PMDLN2Q                                                    
         JL    EDTMIN08                                                         
         MVC   0(L'ACTKULA,R4),PMDCOM2+1   SET ACCOUNT                          
                                                                                
EDTMIN08 CLC   0(L'ACTKULA,R4),SPACES                                           
         JH    EDTMIN10                                                         
         MVC   0(L'ACTKULA,R4),PMDCOMU1                                         
                                                                                
EDTMIN10 CLC   0(L'ACTKULA,R4),SPACES                                           
         JH    EDTMIN12                                                         
         MVC   ROUERRV,=AL2(AE$MIACC)                                           
         J     EXITN                                                            
                                                                                
EDTMIN12 LHI   RE,L'ACTKULA                                                     
         STCM  RE,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  R6,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT HEX CODE - PARM 1 IS HEX                                       *         
***********************************************************************         
         DS    0H                                                               
EDTHEX   J     *+12                                                             
         DC    CL8'*EDTHEX*'                                                    
         LR    RB,RF                                                            
         USING EDTHEX,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
         XC    4(4,R1),4(R1)                                                    
         MVC   TEMP2,SPACES                                                     
         SHI   R3,1                                                             
         OC    0(0,R2),0(R2)                                                    
         EX    R3,*-6                                                           
         JZ    EXITY                                                            
         AHI   R3,1                                                             
         AR    R3,R3                                                            
                                                                                
         GOTOR VHEXOUT,DMCB,0(R2),0(R4),(R3),HEXMIX                             
         STCM  R3,15,4(R5)                                                      
         J     EXITY                                                            
HEXMIX   DC    C'MIX'                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
                                                                                
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    SR    RE,RE               SET CC LOW                                   
         J     *+8                                                              
EXITH    LA    RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
                                                                                
EXITY    LA    RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
EXITYR1  CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R1)           PASS BACK R1                                 
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    V(WRKIO)                                                         
         DC    A(0)                                                             
         DC    V(ACGETALP)                                                      
         DC    A(0)                V(ACPTAORD)                                  
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(NFITAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    V(ACJOBCOL)                                                      
         DC    V(BMONVAL)                                                       
         DC    V(VATICAN)                                                       
         DC    V(ACGETRTE)                                                      
         DC    V(RECUP)                                                         
*&&UK*&& DC    V(VATSUB)                                                        
*&&US*&& DC    V(CONVERT)                                                       
         DC    V(COVAIL)                                                        
*&&US*&& DC    V(CATCALL)                                                       
*&&UK*&& DC    A(0)                was V(PROMOTE)                               
         DC    A(0)                                                             
*&&UK*&& DC    V(SRCHPASS)                                                      
*&&US*&& DC    10A(0)                                                           
*&&UK*&& DC    9A(0)                                                            
         DC    CL132' '                                                         
         DC    C'FD'                                                            
         DC    C'FE'                                                            
         DC    X'D9000A'                                                        
         DC    C'00000000'                                                      
         DC    CL60'><'                                                         
LVALUESL EQU   *-LVALUES                                                        
                                                                                
FACTAB   DS    0XL(FACTABL)        ** EXTRACTED FACILITIES ADDRESSES **         
         DC    AL1(FACTCOMQ),AL2(VADDAY-WORKD,CADDAY-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VBLDCUR-WORKD,CBLDCUR-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VCALLOV-WORKD,CCALLOV-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VCASHVAL-WORKD,CCASHVAL-COMFACSD)              
*&&UK*&& DC    AL1(FACTCOMQ),AL2(VCONVERT-WORKD,CCONVERT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VCUREDIT-WORKD,CCUREDIT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDATCON-WORKD,CDATCON-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATVAL-WORKD,CDATVAL-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATAMGR-WORKD,CDATAMGR-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDDLINK-WORKD,CDDLINK-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDICTATE-WORKD,CDICTATE-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VEUREKA-WORKD,CEUREKA-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGENERAL-WORKD,CGENERAL-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VGETMSG-WORKD,CGETMSG-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGETPROF-WORKD,CGETPROF-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VGETTXT-WORKD,CGETTXT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGLOBBER-WORKD,CGLOBBER-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VHELLO-WORKD,CHELLO-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VLIMACC-WORKD,CLIMACC-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VPARSNIP-WORKD,CPARSNIP-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VPERVAL-WORKD,CPERVAL-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VPERVERT-WORKD,CPERVERT-COMFACSD)              
*&&UK*&& DC    AL1(FACTCOMQ),AL2(VPROMOTE-WORKD,CPROMOTE-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VREPORT-WORKD,CREPORT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VREQTWA-WORKD,CREQTWA-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VSCANNER-WORKD,CSCANNER-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VSEARCH-WORKD,CSEARCH-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSECRET-WORKD,CSECRET-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSOFDAT-WORKD,CSOFDAT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VWSSVR-WORKD,CWSSVR-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VXSORT-WORKD,CXSORT-COMFACSD)                  
*&&UK*&& DC    AL1(FACTCOMQ),AL2(VTOBACCO-WORKD,CTOBACCO-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VJESMAIL-WORKD,CJESMAIL-COMFACSD)              
*&&UK*&& DC    AL1(FACTCOMQ),AL2(VMQIO-WORKD,CMQIO-COMFACSD)                    
         DC    AL1(FACTCOMQ),AL2(VGETRET-WORKD,CGETRET-COMFACSD)                
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTFLST DS    XL1                 FACILITIES LIST DISPLACEMENT                 
FACTCOMQ EQU   ACOMFACS-FACLISTS   COMFACS                                      
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
ACBRA01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QSPOOL)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QUNDAY)                                                      
         DC    AL1(QUNDRLIN)                                                    
         DC    AL1(QUNTIME)                                                     
*&&UK*&& DC    AL1(QGETEST)                                                     
         DC    AL1(QQSORT)                                                      
*&&UK*&& DC    AL1(QDEMPARS)                                                    
         DC    AL1(QSETLOCK)                                                    
         DC    AL1(QRFPIO)                                                      
*&&UK*&& DC    AL1(QCCRTABS)                                                    
         DC    AL1(QFALINK)                                                     
         DC    AL1(QGETIDS)                                                     
         DC    AL1(QGENIDS)                                                     
         DC    AL1(QDDLINK)                                                     
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QGETCAP)                                                     
         DC    AL1(QADDTRN)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QPADDLE)                                                     
         DC    AL1(QGENBAT)                                                     
         DC    AL1(QPRORATA)                                                    
*&&UK*&& DC    AL1(QTRAVAIL)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
NFITAB   DS    0X                  ** FILE DEFINITIONS **                       
*                                  ** ACCOUNT SYSTEM FILES **                   
         DC    AL1(IOACCDIR/256,QSACC,0),C'ACCDIR '                             
         DC    AL1(NFIIIS,NFIIID+NFIIARC)                                       
         DC    AL1(IOACCMST/256,42,08),AL2(54)                                  
         DC    AL1(NFIABOOL),XL1'04',AL1(IOACCARC/256)                          
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOACCMST/256,QSACC,0),C'ACCMST '                             
         DC    AL1(NFIIDA,NFIIDI+NFIIARC)                                       
         DC    AL1(IOACCDIR/256,42,56),AL2(2000)                                
         DC    AL1(NFIABOOL),XL1'04',AL1(IOACCARC/256)                          
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOACCARC/256,QSACC,0),C'ACCARC '                             
         DC    AL1(NFIIDA,NFIIDI+NFIIARC)                                       
         DC    AL1(IOACCDIR/256,42,56),AL2(2000)                                
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                  ** FEE SYSTEM FILES **                       
         DC    AL1(IOFEEDIR/256,QSFEE,0),C'FEEDIR '                             
         DC    AL1(NFIIIS,NFIIID)                                               
         DC    AL1(IOFEEFIL/256,32,04),AL2(32)                                  
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOFEEFIL/256,QSFEE,0),C'FEEFIL '                             
         DC    AL1(NFIIDA,NFIIDI)                                               
         DC    AL1(IOFEEDIR/256,32,42),AL2(2000)                                
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
*                                  ** MEDIA SYSTEM FILES **                     
         DC    AL1(IOMEDDIR/256,QSMED,0),C'MEDDIR '                             
         DC    AL1(NFIIIS,NFIIID)                                               
         DC    AL1(IOMEDFIL/256,20,08),AL2(32)                                  
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOMEDFIL/256,QSMED,0),C'MEDFIL '                             
         DC    AL1(NFIIDA,NFIIDI)                                               
         DC    AL1(IOMEDDIR/256,20,34),AL2(2000)                                
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOMEDZDR/256,QSMED,QSMEDZ),C'MEDDIR '                        
         DC    AL1(NFIIIS,NFIIID)                                               
         DC    AL1(IOMEDZFL/256,20,08),AL2(32)                                  
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOMEDZFL/256,QSMED,QSMEDZ),C'MEDFIL '                        
         DC    AL1(NFIIDA,NFIIDI)                                               
         DC    AL1(IOMEDZFL/256,20,34),AL2(2000)                                
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                  ** CONTROL SYSTEM FILES **                   
         DC    AL1(IOGENDIR/256,QSCON,QSCON),C'GENDIR '                         
         DC    AL1(NFIIIS,NFIIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOGENFIL/256,QSCON,QSCON),C'GENFIL '                         
         DC    AL1(NFIIDA,NFIIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DC    AL1(IOCONFIL/256,QSCON,QSCON),C'CTFILE '                         
         DC    AL1(NFIIVL+NFIIIS,0)                                             
         DC    AL1(0,25,01),AL2(2000)                                           
         DC    AL1(0),XL1'00',AL1(0)                                            
         DC    8XL1'00'                                                         
*                                                                               
         DS    AL1(0)                                                           
                                                                                
CMDTAB   DS    0X                  ** I/O COMMANDS **                           
                                                                                
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIIS,0),AL2(CMDISX-CMDIS)                                  
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
         DC    AL1(0)                                                           
CMDISX   EQU   *                                                                
                                                                                
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(NFIIDA,0),AL2(CMDDAX-CMDDA)                                  
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
         DC    C'ADFREC ',AL1(IOADFREC,CMDIDARQ,0)                              
         DC    AL1(0)                                                           
CMDDAX   EQU   *                                                                
                                                                                
CMDTABX  DC    AL1(0)                                                           
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111ACBRA01   09/26/19'                                      
         END                                                                    
