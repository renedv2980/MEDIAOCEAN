*          DATA SET ACCAP40    AT LEVEL 117 AS OF 11/05/20                      
*PHASE T61D40A                                                                  
*INCLUDE CADET                                                                  
***********************************************************************         
*  TITLE:        ACCAP40 -- TIMESHEET SINGLE PERIODS                  *         
*                        -- TAX (US ONLY)                             *         
*                        -- SAVE                                      *         
*                        -- VARIABLE LENGTH (MAX # ITEMS = 170)       *         
*                        -- TEMPO UPLOAD SCREEN                       *         
*                        -- PROFILE FOR DEFAULT SCREEN                *         
*                        -- DEFAULT TASK CODE                         *         
*                        -- SCROLL                                    *         
*                        -- FILTERS                                   *         
*                        -- TEMPO EXTRA STATUS ELEMENT                *         
*                                                                     *         
*  COMMENTS:     MAIN LOGIC FOR MAINTAINING SINGLE PERIOD TIMESHEETS  *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS ACCAP30,              *         
*                WHICH CALLS THIS.                                    *         
*                                                                     *         
*  CALLS:        ROUTINES ARE IN ACCAP31/32/33                        *         
*                                                                     *         
*  OUTPUTS:      UPDATED TIMESHEET RECS, POSTINGS                     *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- APPLICATION SAVED STORAGE                      *         
*                R5 -- THIRD BASE IN 1ST NMOD                         *         
*                R6 -- WORK (MAINLY KEY/AIO USE)                      *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SYSD - BASE SAVED STORAGE                      *         
*                R9 -- TIME GLOBAL WORKING STORAGE                    *         
*                RA -- GEND                                           *         
*                RB -- FIRST BASE                                     *         
*                RC -- ATWA                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
* HISTORY                                                             *         
* -------                                                             *         
*                                                                     *         
* NSHE 110      140300 PASS COMPANY STATUS BYTE 9 TO ACTIMETRND       *         
* EMOU 111      210800 PASS LIMIT ACCESS OFFICE TO MTHLOCK            *         
* JSHA 112-113  220601 SET UP OFFICE SECURITY                         *         
* JSHA 114      230601 FIX PHASE TO INCLUDE A EXTENSION               *         
* JSHA 115-117  220801 FIX HISTORY SECTION                            *         
* JSHA 122      110202 ?                                              *         
* TKLU 123      030902 DISALLOW WORKCODE '**' AS THIS IS ODERS ONLY   *         
* NSHE 124      150103 ALLOW ONLINE UPDATE OF COST BUCKETS            *         
* JSHA 125                                                            *         
* NSHE 126      180903 FIX OCAEL BUG - GET SECONDARY AND PRIMARY CURS *         
* NSHE 127      240903 FIX CLIENT POSITION BUG - ZERO DEFAULT TO ONE  *         
* NSHE 128 091003 FIX LOOKING UP HISTORY FOR MOA FOR ONLINE UPDATE    *         
* NSHE 129 130504 FIX DUPLICATE TEMPO LINES                           *         
* YNGX 130 070704 BUG FIX CHECKING HISTORY RECORD <LO0150294>         *         
* NSHE 132 120704 MERGE US AND UK VERSIONS                            *         
* NSHE 133 090804 CHANGES TO ALLOW DAILY TIME                         *         
* JFOS 134 080205 REMOVE CALENDAR YEAR CHECK ON TIME MOA              *         
* JFOS 135 100205 MAKE CALL TO CSTCHK US ONLY                         *         
* JFOS 136 020805 <LO01-4478>CADET CREATES ETIME TIMEL IF CPYSETIM SET*         
* JFOS 136 050406 <LO01-4478>REVISED CADET/USE COMCT PRFL NOT CPYSETIM*         
*                 SHOW ETIME TIMEPIDC IN XD (DDS-ONLY)                *         
* JFOS 136 150506 <LO01-4478> FURTHER AMENDMENTS                      *         
* JFOS 136 160606 <LO01-4478> SUPPORT FOR MATERIALS ETIME ELS         *         
* JFOS 137 250906 <UKCR7999> DISPLAY BRANDOCEAN T/SHEET STATUS        *         
* JFOS 138 151106 <UKCR7999> DISPLAY BRANDOCEAN T/S 'IN PROGRESS' STS *         
* JFOS 139 131206 <BR10272L> CHANGE CADET PARMS, FIX STATUS DISPLAY,  *         
*                            SET T/S STATUS FROM CADET RETURN IF BR'O *         
* NSHE 140 281206 <BR10534L> CHANGE STATUS TO REFLECT SCRIBE ONES               
* JFOS 141 100107 <BR10684L> FURTHER CHANGE TO BRANDOCEAN T/S STS DISP*         
* NSHE 142 030907 <Z4991> DON'T ALLOW TEMPO UPLOAD ONE ON BRANDOCEAN  *         
* NSHE 143 080708 <LO01-7842> PASS MORE INFO TO CADET TO BUILD AUDIT  *         
* MPEN 144 270709 <LO01-8967> RELINK FOR BIGGER COBLOCK               *         
* MPEN 153 18JUL17 <DSRD-15589> RELINK FOR LONGER NARRATIVE           *         
* SGAV 154 270717 <SPEC-14474> 0 HOUR T/S TO BE DISPLAYED IN COST/TIME*         
* JSHA 117 AUG21/20 <DSRD-27176> CHANGE NOT ALLOWED IF TIMEDNAR EXIST *         
***********************************************************************         
         TITLE 'T61D40 - TIMESHEETS - SINGLE PERIOD'                            
T61D40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (BCBUFLNQ),**1D40**,R7,R5,RR=R3                                  
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         ST    RC,ABUFF1                                                        
         AH    RC,=Y(BCBUFLNQ/2)                                                
         ST    RC,ABUFF2                                                        
*                                                                               
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         LA    R4,SYSSPARE                                                      
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
         ST    R3,RELO                                                          
         L     R1,=A(SCRNTAB)                                                   
         A     R1,RELO                                                          
         ST    R1,ASCRNTAB                                                      
         L     R1,=V(CADET)                                                     
         A     R1,RELO                                                          
         ST    R1,ACADET                                                        
*&&US                                                                           
         LA    R1,TMSUBLOK                                                      
         ST    R1,ATMSUBLK                                                      
         XC    TMSUBLOK,TMSUBLOK                                                
*&&                                                                             
         LA    R2,CONRECH                                                       
*&&US*&& TM    BCCPYST7,CPYSTMSY   TIME MANAGEMENT SYSTEM IN USE                
*&&US*&& BNO   ENATMS              NOT AUTHORIZED FOR TMS                       
         CLI   ACTEQU,ACTADD                                                    
         BNE   MAIN1                                                            
         TWAXC CONOPTH,CONOPTH     THE OPTIONS FIELD                            
         OI    CONOPTH+6,X'80'                                                  
         OI    CONOPTH+1,X'20'     PROTECTED                                    
         MVI   CONOPTH+5,0         ZERO OUT LENGTH                              
         B     MAIN2                                                            
MAIN1    NI    CONOPTH+1,X'FF'-X'20'  UNPROTECT                                 
*                                                                               
MAIN2    MVI   LAP#,0              INITIALIZE LAP COUNTER                       
         MVI   BCBYTE2,0           INITIALIZE DISPLAY FLAG                      
         MVI   IOOPT,C'Y'          TMS WILL HANDLE I/O                          
         CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   MAIN10                                                           
*                                                                               
MAIN3    CLI   PFKEY,PFRETURN                                                   
         BNE   MAIN5                                                            
         XC    LISTDIR(108),LISTDIR   ONLY CLEAR DIRECTORY AND NOT              
         MVI   PFKEY,0                OTHER ADDRESSES LISTDIR COVERS            
         B     ROUTE                  CAUSE ITS DEFINED WEIRD                   
*                                                                               
MAIN5    OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION                        
         CLI   PFKEY,PFNEXT                                                     
         BNE   MAIN10                                                           
*&&UK                                                                           
         TM    STATUS2,DAILYTIM    ARE WE RUNNING UNDER DAILY TIME              
         BZ    MAIN9                                                            
         LA    R2,LISTDTES         LIST OF STORED DATE                          
MAIN6    CLI   0(R2),X'FF'                                                      
         BNE   MAIN7                                                            
         LA    R2,L'TIMXTDDT(R2)                                                
         B     MAIN6                                                            
MAIN7    MVI   0(R2),X'FF'                                                      
         OC    L'TIMXTDDT(L'TIMXTDDT,R2),L'TIMXTDDT(R2) HAVE WE A DATE          
         BZ    MAIN9               NO                                           
         MVI   PFKEY,0                                                          
         OI    STATUS2,HASDATE                                                  
         MVI   MODE,DISPKEY        SET MODE TO DISPLAY KEY                      
         B     MAIN10                                                           
*&&                                                                             
MAIN9    OI    GENSTAT2,NEXTSEL    GET NEXT SELECTION                           
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
*&&UK*&& NI    STATUS2,X'FF'-HASDATE                                            
         MVI   PFKEY,0                                                          
         B     ROUTE                                                            
*                                                                               
MAIN10   MVC   AIO,AIO1            DEFAULT AIO                                  
         NI    TSARSTAT,X'FF'-(TSAREST+TSARSAVE)                                
         SR    R3,R3               PFKEY LINE                                   
         L     R2,=A(PFTABLE)                                                   
         A     R2,RELO                                                          
         GOTO1 INITIAL,BCDMCB,(X'40',(R2)),(R3) INITIALIZE THE PFKEYS           
         BAS   RE,SCRADDRS         GET SCREEN ADDRESSES                         
*                                                                               
         LA    R2,CONOPTH                                                       
*&&UK                                                                           
         MVC   BCTHISEL,THISLSEL                                                
         TM    STATUS2,HASDATE                                                  
         BZ    MAIN11                                                           
         MVC   BCTHISEL,SVLSEL                                                  
*&&                                                                             
MAIN11   GOTO1 AVALOPTS,(R2)       EXTRACT OPTIONS - BCOPTS SET                 
         BNE   ACCERRX                                                          
         TM    4(R2),X'20'                                                      
         BO    MAIN15                                                           
         MVC   SVOPT1,BCOPT1                                                    
         MVC   SVOPT2,BCOPT2                                                    
         TM    4(R2),X'80'         WERE THE OPTS CHANGED THIS TIME?             
         BNO   MAIN14                                                           
*&&UK*&& CLC   SVOPT3,BCOPT3       HAS ALL OPTION CHANGED                       
*&&UK*&& BNE   MAIN13                                                           
         CLI   ACTEQU,ACTDIS       ALWAYS REBUILD FOR ACTION DISPLAY            
         BE    MAIN13                                                           
         CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    IF CHANGE-REDISPLAY NOT REBUILD              
         BNE   MAIN13              MUST BE DISPLAY                              
         MVC   DISLINES(2),=X'0001'                                             
         B     MAIN14                                                           
MAIN13   BAS   RE,CLRNUMBS         CLEAR DISPLAYED LINES                        
*&&UK                                                                           
         CLI   ACTEQU,ACTCHA       IF ACTION CHANGE AND COME FROM               
         BNE   MAIN14              DISPLAY WITH OPTION ALL                      
         MVI   PFKEY,PFCLEAR       SET PFKEY TO CLEAR AND REDISPLAY             
*&&                                                                             
MAIN14   OI    4(R2),X'20'         VALIDATED OPTIONS                            
*&&UK*&& MVC   SVOPT3,BCOPT3                                                    
*                                                                               
MAIN15   LA    R2,CHASCRLH         SCROLL FIELD                                 
         TM    4(R2),X'20'                                                      
         BO    MAIN20                                                           
         GOTO1 ASCROLL,(R2)                                                     
         BNE   ACCERRX                                                          
         MVC   SVSCROLL,BCHALF     SCROLL AMOUNT                                
         TM    SVSCROLL,SCRLPAGE                                                
         BNO   *+10                                                             
         MVC   CHASCRL,AC@PAGE                                                  
         TM    SVSCROLL,SCRLHALF                                                
         BNO   *+10                                                             
         MVC   CHASCRL,AC@HALF                                                  
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
MAIN20   CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD (CALLS DISPREC)              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD (FROM LIST)                   
         BE    VR                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ACTION = DISPLAY KEY                                                *         
***********************************************************************         
         SPACE 1                                                                
DK       DS    0H                                                               
*&&UK*&& TM    STATUS2,HASDATE     HAVE WE ALREADY GOT DATE                     
*&&UK*&& BNZ   DK40                YES                                          
         USING TSSRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         CLI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         BNE   DK20                                                             
         CLI   TSSKSUB,TSSKSUBQ    X'11'                                        
         BNE   DK20                                                             
         SR    R1,R1                                                            
         ICM   R1,7,TSSKEND                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,YYMMDD                                                      
         MVC   BCOFFC,BCSPACES                                                  
         MVC   BCDEPT,BCSPACES                                                  
         MVC   BCSDPT,BCSPACES                                                  
         MVC   BCPERSON,BCSPACES                                                
         LA    RF,TSSKODS                                                       
         SR    R1,R1                                                            
         IC    R1,BCX1RLV1                                                      
         MVC   BCOFFC(0),0(RF)                                                  
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R1,BCX1RLV2                                                      
         MVC   BCDEPT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R1,BCX1RLV3                                                      
         MVC   BCSDPT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         MVC   BCPERSON,TSSKPER                                                 
         MVC   SVYYMMDD,YYMMDD     NO - FIND IT FROM RECORD                     
         GOTO1 AADJDATE,BCDMCB,0,YYMMDD                                         
         BNE   *+10                                                             
         MVC   YYMMDD,BCENDTE                                                   
         B     DK30                                                             
*                                                                               
         USING TIMRECD,R6                                                       
DK20     LA    R6,BIGKEY                                                        
         MVC   YYMMDD,TIMKPEDT     PERIOD ENDDATE                               
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   DKX                                                              
         MVC   SVTIMST1,TIMKSTAT   SAVE TIMESHEET STATUS                        
         MVC   SVYYMMDD,YYMMDD     NO - FIND IT FROM RECORD                     
         GOTO1 AADJDATE,BCDMCB,TIMKCULA,YYMMDD                                  
         BNE   *+10                                                             
         MVC   YYMMDD,BCENDTE                                                   
*                                                                               
DK30     MVC   BCYYMMDD,SVYYMMDD   PERIOD END DATE                              
         MVC   BCTIMST1,SVTIMST1   TIMESHEET STATUS                             
         LA    R2,CHACODEH         R2=A(FIELD FOR ERROR IF INVALID AC)          
         MVC   SV1RACT,BCSPACES                                                 
         MVC   SV1RCPY,CMPY                                                     
         MVC   SV1RUL,=C'1R'                                                    
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC                                                          
         LA    R3,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BCX1RLV1         EX LENGTH OF LEVEL A                         
         MVC   0(0,R3),BCOFFC                                                   
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV2         EX LENGTH OF LEVEL B                         
         MVC   0(0,R3),BCDEPT                                                   
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV3                                                      
         MVC   0(0,R3),BCSDPT                                                   
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV4         EX LENGTH OF LEVEL D                         
         MVC   0(0,R3),BCPERSON                                                 
         EX    R1,*-6                                                           
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC                                                          
*&&UK                                                                           
         TM    BCPRIND,BCPRIDTM    HAVE WE GOT DAILY TIME PERSON                
         BZ    DK50                NO                                           
         MVC   BCACCODE,SV1RACT                                                 
         GOTO1 AGETDTES,BCDMCB,SVYYMMDD,LISTDTES                                
         MVC   YYMMDD,LISTDTES                                                  
         B     DK50                                                             
*                                                                               
DK40     LA    R2,LISTDTES                                                      
DK42     CLI   0(R2),X'FF'                                                      
         BNE   DK45                                                             
         LA    R2,L'TIMXTDDT(R2)                                                
         B     DK42                                                             
DK45     MVC   YYMMDD,0(R2)                                                     
*&&                                                                             
DK50     LA    R2,CHAPERH                                                       
         MVC   BCWORK,BCSPACES                                                  
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(17,BCWORK)                             
         MVC   CHAPER,BCWORK                                                    
         OI    6(R2),X'80'                                                      
         MVI   5(R2),8                                                          
*&&UK*&& TM    STATUS2,HASDATE     HAVE WE ALREADY GOT PERSON                   
*&&UK*&& BNZ   VK02                YES                                          
*                                                                               
         LA    R2,CHACODEH                                                      
         MVC   CHACODE,BCPERSON                                                 
         OI    6(R2),X'80'                                                      
         MVC   5(1,R2),BC1RLEV4                                                 
DKX      DS    0H                                                               
         CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   *+10                                                             
         MVC   SVLSEL,THISLSEL     SAVE FIELD FOR LATER COMPARES                
         B     VK02                                                             
         EJECT                                                                  
***********************************************************************         
* ACTION = VALIDATE KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
VK       DS    0H                                                               
         MVI   STATUS2,0                                                        
VK02     GOTO1 =A(VKEY),BCDMCB,RR=RELO            VALIDATE KEY                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DR       DS    0H                                                               
         BRAS  RE,SCREENS          MANIPULATE SCREENS                           
         BAS   RE,SCRADDRS         GET SCREEN ADDRESSES                         
         GOTO1 =A(DISPLAY),BCDMCB,RR=RELO     DISPLAY ITEMS                     
         BNE   VR                  REVALIDATE-DISPLAYING INCOMPLETE             
         L     R2,ALINE1                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD -  REBUILD TSAR IF NECESSARY                        *         
***********************************************************************         
         SPACE 1                                                                
VR       DS    0H                                                               
*&&US                                                                           
         OC    SVVKBLCK(22),SVVKBLCK  ANY PERS INFO SAVED FROM VALKEY?          
         BZ    ERRCNCH                    NO - DONT CHANGE                      
         OC    SV1RACT(30),SV1RACT    ANY 1R OR INC INFO SAVED?                 
         BZ    ERRCNCH                    NO - DONT CHANGE                      
*&&                                                                             
*                                                                               
         MVC   BCTIMST1,SVTIMST1   TIMREC STATUS                                
         MVC   BCPRIND,STATUS2     INDICATOR FOR DAILY TIME                     
         CLI   BOTSCR,0                                                         
         BE    VR10                                                             
         CLC   SVYYMMDD,SAVPERDT   OR PERIOD                                    
         BNE   VR10                                                             
         CLC   SV1RPRSN,SAVPERSN   OR PERSON                                    
         BNE   VR10                                                             
         CLI   ACTEQU,ACTDIS       ALWAYS REBUILD FOR ACTION DISPLAY            
         BE    VR15                                                             
         CLI   ACTEQU,ACTSEL       REBUILD FOR 'SELECT' FROM LIST               
         BNE   *+14                                                             
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BNE   VR15                NO - JUST SELECT                             
         CLI   LAP#,0                                                           
         BNE   VR2                                                              
         GOTO1 ATSAR,BCDMCB,TSARES,1  ELSE, RESTORE TSAR RECS                   
*                                                                               
VR2      DS    0H                                                               
         TM    STATUS2,MCSTIME     TEST PERSON ON MCS TIME                      
         BZ    VR3                 NO - NO NEED TO CHECK STATUS/TDN             
*&&US*&& TM    SVFLAG5,BCFLUAT     TEST UNAPPROVED TIMESHEET                    
*&&US*&& BO    ERRUAT              YES - CHANGE NOT ALLOWED                     
         TM    SVFLAG5,BCFLTDN     TEST TIME DAY NARRATIVE EXIST                
         BO    ERRTDN              YES - CHANGE NOT ALLOWED                     
*                                                                               
VR3      OI    TSARSTAT,TSARSAVE   SAVE TSAR BUFF ON EXIT                       
         CLI   SVOPT1,C'Y'         SORT TSAR IN SJ SEQUENCE?                    
         BNE   VR5                                                              
         TM    SORTSTAT,SORTSJ     IF NOT CURRENTLY IN SJ SEQUENCE              
         BO    VR20                THEN SORT IN SJ SEQUENCE                     
         GOTO1 ASJSORT,BCDMCB,1                                                 
         MVI   SORTSTAT,SORTSJ                                                  
         B     VR20                                                             
VR5      TM    SORTSTAT,SORTTSN    IS IT CURRENTLY IN TS# SEQUENCE              
         BO    VR20                                                             
         GOTO1 ATSNSORT,BCDMCB,1   SORT TSAR IN TS# SEQUENCE                    
         MVI   SORTSTAT,SORTTSN                                                 
         B     VR20                                                             
*                                                                               
VR10     MVI   BOTSCR,0                                                         
         BAS   RE,CLRNUMBS         CLEAR LINE NUMBERS                           
         BAS   RE,CLRCNFM          CLEAR OUT CONFIRMATION FIELD                 
VR15     MVC   SAVPERDT,SVYYMMDD   SAVE PERIOD AND                              
         MVC   SAVPERSN,SV1RPRSN   PERSON AND                                   
         BRAS  RE,SCREENS          MANIPULATE SCREENS                           
         BAS   RE,SCRADDRS         GET SCREEN ADDRESSES                         
         BAS   RE,CLRCHNGS         GO CLEAR EVERTHING                           
         EJECT                                                                  
***********************************************************************         
* SEE IF RECORD ALREADY EXISTS                                        *         
***********************************************************************         
         SPACE 1                                                                
VR20     DS    0H                                                               
         TM    STATUS,NOREC        DOES A RECORD EXIST                          
         BNO   VR25                FOUND                                        
         CLI   ACTEQU,ACTADD       NO RECS, OK FOR ADD                          
         BE    VR30                                                             
         LA    R2,CHAPERH          FOR CHANGE AND DISP                          
         CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   VR22                                                             
         CLC   SVLSEL,AC@CHAU      IF CHANGE - PUT OUT MORE HELPFUL             
         BE    ERRONDTM            MESSAGE                                      
VR22     B     ERRRECNF            RECORD NOT FOUND                             
*                                                                               
VR25     CLI   ACTEQU,ACTADD       RECORD EXISTS ON ADD                         
         BNE   VR30                                                             
         LA    R2,ADDPERH                                                       
         B     ERRNOADD            RECORD ALREADY EXISTS                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY FIRST ?                                                     *         
***********************************************************************         
         SPACE 1                                                                
VR30     CLI   ACTEQU,ACTDIS       IF ACTION=DISPLAY - DISPLAY                  
         BE    DR                                                               
         CLI   ACTEQU,ACTSEL       DISPLAY FOR 'SELECT' FROM LIST               
         BNE   *+14                                                             
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BNE   DR                                                               
*                                                                               
         TM    STATUS2,MCSTIME     TEST MCS TIME PERSON                         
         BZ    VR31                                                             
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VR31                                                             
         CLI   BOTSCR,X'C8'        MAKE SURE WE ARE AT UPLOAD SCREEN            
         BE    ERRONBRO                                                         
VR31     CLI   PFKEY,PFCLEAR       PFKEY TO CLEAR CHANGES                       
         BNE   VR32                                                             
         BAS   RE,CLRNUMBS                                                      
         BAS   RE,CLRCHNGS                                                      
         TM    STATUS,NOREC        DOES A RECORD EXIST                          
         BNO   DR                  FOUND                                        
         CLI   ACTEQU,ACTADD       NO RECS, OK FOR ADD                          
         BE    DR                                                               
         LA    R2,CHAPERH          FOR CHANGE AND DISP                          
         B     ERRONDTM            RECORD NOT FOUND                             
*                                                                               
VR32     CLI   ACTEQU,ACTADD       IF NOT ACTION ADD                            
         BNE   VR35                                                             
         BAS   RE,HRSWRKD          MUST ENTER TOTAL HRS WORKED ON ADD           
         CLC   SCXNEXT,=H'1'       ARE THERE ANY RECS                           
         BNH   *+12                NOT YET, PROMPT FOR INPUT                    
VR35     TM    STATUS,DISPED       IF THERE ARE MAKE SURE DISPLAYED             
         BNO   DR                                                               
*                                                                               
         L     R2,ALINE1           FIRST DISPLAY LINE ON SCREEN                 
         MVC   SCXLINE#,DISLINES   LINE/SCXSUB # FIRST DISPLAYED                
         LA    R1,DISLINES                                                      
         LA    R1,4(R1)            POINT TO NEXT TO BE DISPLAYED                
         ST    R1,ADISLINE                                                      
*        BAS   RE,DUPTABLE         MAKE TABLE OF KEY DATA FOR DUPLICATE         
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT LINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VR100    DS    0H                                                               
         TM    SVASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BNO   *+14                                                             
         MVC   GERROR,=AL2(ACEACTLK)                                            
         B     ACCERRX                                                          
*                                                                               
         LA    RE,SVVRBLCK         CLEAR BOTTOM HALF OF SAVE BLOCK              
         LA    RF,SVVRBLKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   SVAMOUNT,BCPZERO                                                 
         ZAP   SVRATE,BCPZERO                                                   
         ZAP   SVHOURS,BCPZERO                                                  
*                                                                               
         USING SCRNTABD,R1                                                      
         L     R1,ASCRNTAB         FIND A(VALIDATION/DISPLAY TABLE)             
         CLC   BOTSCR,SCRNNUM                                                   
         BE    *+18                                                             
         LA    R1,SCRNTLNQ(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         ST    R1,ASCRNITM                                                      
         ICM   R3,15,SCRNTBL                                                    
         A     R3,ASCRNTAB                                                      
         ST    R3,AVDTAB           SAVE A(VALIDATION/DISPLAY TABLE)             
         DROP  R1                                                               
*                                                                               
         USING DISTABD,R3                                                       
         L     R3,AVDTAB           TABLE OF SCREEN INFO                         
         XC    FLDINPS,FLDINPS                                                  
         XC    ONSCREEN,ONSCREEN                                                
         NI    STATUS,X'FF'-NEWINPUT                                            
         TM    1(R2),X'20'         SKIP LINE IF 'TYPE' PROTECTED                
         BO    VR800                                                            
*                                                                               
VR110    OC    DTFLEN,DTFLEN       IS FIELD DISPLAYED ON THIS SCREEN            
         BZ    VR120                                                            
         LR    R1,R2               R2=BEGINNING OF LINE                         
         AH    R1,DTFDISP          DISPLACEMENT TO FIELD IN LINE                
         CLI   1(R1),X'FF'         SKIP NOP FIELDS                              
         BO    VR120                                                            
         OI    6(R1),X'80'         TRANSMIT                                     
         TM    1(R1),X'20'         SKIP PROTECTED FIELDS                        
         BO    VR120                                                            
         OC    DTINPFLD,DTINPFLD   SKIP IF FIELD NOT VALIDATED                  
         BZ    VR120                                                            
         MVC   BCHALF,DTINPFLD     SKIP DISPLAY ONLY FIELDS                     
         NC    BCHALF,=Y(FLDEFS-FLDAMNT-FLDNAME)                                
         BZ    VR120                                                            
         TM    4(R1),X'20'         HAS ANYTHING CHANGED                         
         BO    *+8                                                              
         OI    STATUS,NEWINPUT                                                  
         CLI   5(R1),0             ANYTHING IN FIELD                            
         BE    *+10                                                             
         OC    FLDINPS,DTINPFLD    SET INPUT BIT                                
VR120    LA    R3,DTLENQ(R3)       NEXT FIELD                                   
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BNE   VR110                                                            
*                                                                               
         MVI   LINESTAT,0          CLEAR LINE ENTRY                             
         MVC   FLDREQS,SCRREQS     SET SCREEN REQUIRED FEILDS                   
         OC    FLDREQS,=Y(FLDTYPE+FLDHRS+FLDCLI)                                
         NI    PCSTAT,X'FF'-PCSJ                                                
         OC    FLDINPS,FLDINPS                                                  
         BZ    VR800                                                            
         CLI   PFKEY,PFUNMARK                                                   
         BNE   *+8                                                              
         OI    STATUS,NEWINPUT                                                  
         TM    STATUS,NEWINPUT                                                  
         BNO   VR800                                                            
*                                                                               
         USING DISTABD,R3                                                       
VR140    L     R3,AVDTAB           A(VALIDATION TABLE)                          
         MVC   AIO,AIO2                                                         
VR150    TM    LINESTAT,LBDELETE   DELETED LINE                                 
         BZ    VR152                                                            
         TM    BCASTAT1,RSTSACIC   IS ACCOUNT CLOSED                            
         BO    EACCCLS                                                          
VR152    CLI   DTFDISP,X'FF'       END OF TABLE                                 
         BE    VR170                                                            
         LH    R1,DTFDISP          DISPLACEMENT TO FIELD IN LINE                
         AR    R1,R2               R2=A(BEGINNING OF LINE)                      
         ST    R1,AFIELDH          A(FIELD HEADER)                              
         ST    R2,AFLD1STH         A(1ST FIELD ON LINE)                         
         MVC   FIELDLEN,DTFLEN     FIELD LENGTH                                 
         OC    DTFLEN,DTFLEN       IS FIELD ON DISPLAYED ON THIS SCREEN         
         BZ    *+10                                                             
         OC    ONSCREEN,DTINPFLD                                                
*                                                                               
         TM    LINESTAT,LBSAVE     SAVED LINE                                   
         BNO   VR155                                                            
         OC    DTFLEN,DTFLEN       IS FIELD ON DISPLAYED ON THIS SCREEN         
         BZ    VR160                                                            
         OI    4(R1),X'20'                                                      
         OI    6(R1),X'80'                                                      
         B     VR160                                                            
*                                                                               
VR155    SR    RF,RF                                                            
         ICM   RF,3,DTVALRTE       VALIDATION ROUTINE EQUATE                    
         BZ    VR160                                                            
         LA    RE,VR160            SET RETURN ADDRESS                           
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALTYPE             VALIDATE TYPE                                
         B     VALHRS              VALIDATE HOURS                               
         B     VALCPJT             VALIDATE CLIENT/PRODUCT/JOB                  
         B     VALMOA              VALIDATE MOA                                 
         B     VALRATE             VALIDATE RATE                                
         B     VALINC              VALIDATE INCOME ACCOUNT                      
         B     VALNAR              VALIDATE NARRATIVE                           
*&&US*&& B     VALTAXF             VALIDATE TAX FIELD                           
*&&US*&& B     VALTAX              VALIDATE TAX INFORMATION                     
         B     VALTMP#             VALIDATE TEMPO LINE #                        
*                                                                               
VR160    LA    R3,DTLENQ(R3)       NEXT FIELD                                   
         B     VR150                                                            
         DROP  R3                                                               
*                                                                               
VR170    TM    LINESTAT,LBSAVE                                                  
         BNO   VR200                                                            
         BAS   RE,VALSAVE          BUILD SAVE BLOCK                             
         MVC   SVFLDBLK,BCELEM     SAVE BUILT 8B COMMENT ELEMENT                
         EJECT                                                                  
***********************************************************************         
* READ FOR TSAR REC BY LINE NUMBER                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING TSARRECD,R6                                                      
VR200    MVC   AIO,AIO1            SEE IF TSAR RECORD EXISTS                    
         GOTO1 AXAIO,0                                                          
         L     R6,AIO                                                           
         MVC   TRKLINE,SCXLINE     LINE NUMBER                                  
         MVC   TRKLNSUB,SCXSUB     SUB LINE NUMBER                              
         GOTO1 ATSAR,BCDMCB,TSARDH,1    READ HIGH FOR LINE NUMBER               
         TM    BCTSERRS,TSEEOF                                                  
         BO    VR230                                                            
*                                                                               
         CLC   SCXLINE#,TRKLINE    IF LINE # DIFFERENT THEN ADD                 
         BNE   VR230                                                            
         TM    LINESTAT,LB1N       1N OR SJ                                     
         BNO   VR210                                                            
         CLC   TRKACC,SV1NULA                                                   
         BNE   VR215                                                            
         B     VR220                                                            
*                                                                               
VR210    CLC   TRKACC,SVSJULA      SJ ACCOUNT                                   
         BNE   *+14                                                             
         CLC   TRKTSK,SVTASK       TASK CODE                                    
         BE    VR220                                                            
VR215    BRAS  RE,EXTRACT          EXTRACT INFO BEFORE DELETING RECRD           
         GOTO1 ATSAR,BCDMCB,TSADEL,1                                            
         B     VR230                                                            
*                                                                               
*              DELETE TSAR RECORD IF HOURS=0                                    
*                                                                               
VR220    TM    LINESTAT,LBDELETE   DELETE LINE                                  
         BNO   VR240                                                            
         TM    BCASTAT1,RSTSACIC   IS ACCOUNT CLOSED                            
         BO    EACCCLS                                                          
         GOTO1 ATSAR,BCDMCB,TSADEL,1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*        BAS   RE,DELDUP           DELETE ENTRY IN DUPLICATE TABLE TOO          
         B     VR800                                                            
*                                                                               
*              ADD NEW TSAR RECORD                                              
*                                                                               
VR230    TM    LINESTAT,LBDELETE                                                
         BO    VR800                                                            
         OI    LINESTAT,LBADDREC   FLAG TO ADD                                  
         OC    SCXLINE,SCXLINE     NO NUMBER IN DISPLAY TABLE                   
         BNZ   VR250                                                            
         MVC   SCXLINE,SCXNEXT     THEN USE NEXT AVAILABLE LINE NUMBER          
         LH    R1,SCXNEXT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,SCXNEXT                                                       
         SR    R1,R1                                                            
         ICM   R1,3,SVTSNUM                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,SVTSNUM                                                     
         MVC   SVTS#NXT,SVTSNUM    GET NEXT TS#                                 
         B     VR250                                                            
*                                                                               
VR240    BRAS  RE,EXTRACT          EXTRACT INFO FROM OLD TSAR RECORD            
*                                                                               
VR250    GOTO1 AXAIO,0             CLEAR AIO                                    
         MVC   TRKLINE,SCXLINE     LINE NUMBER                                  
         MVC   TRKLNSUB,SCXSUB     SUB LINE NUMBER                              
         TM    LINESTAT,LBSAVE                                                  
         BNO   VR260                                                            
         MVI   TRKSTAT,TRKSSAVE                                                 
         TM    LINESTAT,LBSAVE#                                                 
         BNO   *+8                                                              
         OI    TRKSTAT,TRKSSAV#                                                 
         MVC   TRKTSNUM,SVTS#NXT                                                
         XC    RECLEN,RECLEN                                                    
         SR    RF,RF                                                            
         IC    RF,SVFLDBLK+1                                                    
         LA    RF,TRLEN1Q(RF)      LENGTH OF REC BEFORE CLUSTER                 
         STH   RF,RECLEN                                                        
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   TRDATA(0),SVFLDBLK                                               
         EX    RF,*-6                                                           
         MVC   TRDATA+(TIMFLINE-TIMELD)(2),SVTS#NXT                             
         B     VR700                                                            
*                                                                               
VR260    MVC   TRKTSK,SVTASK       TASK CODE                                    
         OC    TRKTSK,BCSPACES                                                  
         MVC   TRKPEDT,PRDENDTE    PERIOD DATE                                  
         MVC   TRKACC,SVSJULA      SJ ACCOUNT                                   
         MVC   TRKCNTRA,SV1CULA    1C ACCOUNT                                   
         MVC   TRKOFF,SVCLIOFF     CLIENT OFFICE                                
         TM    LINESTAT,LB1N       1N OR SJ                                     
         BNO   *+22                                                             
         MVC   TRKACC,SV1NULA                                                   
         MVC   TRKCNTRA,SV1NULA                                                 
         MVC   TRKOFF,SV1ROFFC     1R OFFICE FOR 1N CONTRA ACCNT                
         MVC   TRKTSNUM,SVTS#NXT                                                
         MVC   TRKSTAT,SVKSTAT                                                  
         MVC   TRKERROR,SVERROR    ERROR STATUS BYTE                            
         XC    RECLEN,RECLEN                                                    
         LA    R0,TRLEN1Q          LENGTH OF REC BEFORE CLUSTER                 
         STH   R0,RECLEN                                                        
*                                                                               
*              CREATE X'8B' INPUT DETAIL ELEMENT                                
*                                                                               
         USING TIMELD,R3                                                        
         LA    R3,TRDATA           ALWAYS HAVE X'8B' INPUT DETAIL ELEM          
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMEINP                                                  
         MVC   TIMACC,SVSJULA      SJ ACCOUNT                                   
         MVC   TIMOFF,SVCLIOFF     CLIENT OFFICE                                
         MVC   TIMMED,SVMED                                                     
         TM    LINESTAT,LB1N                                                    
         BNO   *+16                                                             
         MVC   TIMACC,SV1NULA      1N ACCOUNT                                   
         MVC   TIMOFF,SV1ROFFC     1R OFFICE CODE                               
         MVC   TIMTSK,SVTASK                                                    
         OC    TIMTSK,BCSPACES                                                  
         MVC   TIMTTYP,SVTYPE                                                   
         MVC   TIMIND,SVIND                                                     
*&&UK                                                                           
         TM    STATUS2,DAILYTIM    ARE WE USING DAILY TIME                      
         BZ    VR270                                                            
         OI    TIMIND,TIMIDALY     SET INDICATOR                                
*&&                                                                             
VR270    MVC   TIMMOA,SVMOA                                                     
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+16                                                             
         CLI   BOTSCR,X'C8'        MAKE SURE WE ARE AT UPLOAD SCREEN            
         BNE   *+8                                                              
         OI    TIMSTAT,TIMTEMPO    ITEM CREATED THROUGH TEMPO                   
*&&US                                                                           
         TM    PCSTAT,PCSJ         PROJECT CONTROL                              
         BNO   *+8                                                              
         OI    TIMSTAT,TIMSPCJ                                                  
         TM    PCSTAT,PC1R                                                      
         BNO   *+8                                                              
         OI    TIMSTAT,TIMSPCC                                                  
         TM    SVTAXSTA,SVTAXNO    NO TAX FOR THIS ITEM                         
         BNO   *+8                                                              
         OI    TIMSTAT,TIMNOTAX                                                 
         TM    SVTAXSTA,SVTAXREQ                                                
         BNO   *+8                                                              
         OI    TIMSTAT,TIMRQTAX                                                 
*&&                                                                             
         MVC   TIMLINE#,SVTS#NXT   TIMESHEET #                                  
         MVC   TIMADAT,BCTODAY3                                                 
         ZAP   TIMHRS,SVHOURS      HOURS                                        
         LA    R1,TIMILN1Q         ADD TO RECORD LENGTH                         
*                                                                               
*              ADD BILLABLE INFO                                                
*                                                                               
         CLI   SVTYPE,TIMTCB       B AND R TIME HAVE RATE                       
         BE    *+12                                                             
         CLI   SVTYPE,TIMTCR                                                    
         BNE   VR280                                                            
         MVC   TIMRATE,SVRATE      RATE                                         
         MVC   TIMRBSTA,SVRATIND   RATE INDICATOR                               
         MVC   TIMREFF,SVRATEFF    RATE EFFECTIVE DATE                          
         CLC   SVSIULA,BCSPACES                                                 
         BNH   *+16                                                             
         MVC   TIMINC,SVSIULA      INCOME ACCOUNT                               
         OC    TIMINC,BCSPACES                                                  
         ZAP   TIMAMNT,SVAMOUNT    AMOUNT                                       
*&&UK*&& ZAP   TIMCRATE,SVCRATE    COST RATE                                    
*&&UK*&& MVC   TIMCREFF,SVCRATEF   COST RATE EFFECTIVE DATE                     
*&&UK*&& ZAP   TIMERTE,SVERATE     EURO CHARGE RATE                             
*&&UK*&& ZAP   TIMECRTE,SVECRATE   EURO COST RATE                               
*&&UK*&& LA    R1,TIMB2LNQ         ADD TO RECORD LENGTH(ADDL RATE INFO)         
*&&US*&& LA    R1,TIMILN2Q         ADD TO RECORD LENGTH                         
*                                                                               
VR280    STC   R1,TIMLN                                                         
         LH    R0,RECLEN                                                        
         AR    R0,R1                                                            
         STH   R0,RECLEN                                                        
         EJECT                                                                  
***********************************************************************         
* CREATE X'8B' TIMESHEET TAX ELEMENT                                  *         
***********************************************************************         
         SPACE 1                                                                
VR500    DS    0H                                                               
*&&US                                                                           
         CLI   SVTYPE,TIMTCB       ONLY HAVE TAX ON BILLABLE ITEMS              
         BNE   VR600                                                            
         TM    SVTAXSTA,SVTAXNO    NO TAX FOR THIS ITEM                         
         BO    VR600                                                            
         CLC   SVTX(SVTXLNQ),BCSPACES                                           
         BNH   VR600                                                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMETAX                                                  
         MVC   TIMTWC,SVTXWC       TAX WORKCODE                                 
         MVC   TIMTLOC,SVTXLOC     TAX LOCALITY                                 
         ZAP   TIMTBAS,SVTXBAS     TAX BASIS                                    
         TM    SVTAXSTA,SVTAXOVR                                                
         BNO   *+8                                                              
         OI    TIMTSTA,TIMTOVR     TAX INFORMATION WAS OVERRIDDEN               
         MVC   TIMTMINI,SVTXMINI   # MINI ELEMENTS                              
         SR    R1,R1                                                            
         ICM   R1,1,TIMTMINI                                                    
         BZ    VR525                                                            
         MH    R1,=Y(TIMTMINQ)                                                  
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                NO TAX DATA                                  
         MVC   TIMTMINS(0),SVTXBLCK    COPY OVER MINI ELEMENTS                  
         EX    R1,*-6                                                           
*                                                                               
         LA    R1,1(R1)            FIX LENGTH FROM EXMVC                        
VR525    AH    R1,=Y(TIMTHDQ+1)    ADD IN LENGTH OF HEADER                      
         STC   R1,TIMLN                                                         
         LH    RF,RECLEN                                                        
         AR    RF,R1                                                            
         STH   RF,RECLEN                                                        
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CREATE X'8B' TIMESHEET NARRATIVE ELEMENT                            *         
***********************************************************************         
         SPACE 1                                                                
VR600    CLI   SVNARRLN,0                                                       
         BE    VR650                                                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMENAR                                                  
         SR    R1,R1                                                            
         IC    R1,SVNARRLN                                                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   TIMNARR(0),SVNARR                                                
         EX    R1,*-6                                                           
*                                                                               
         LA    R0,TIMHLNQ          HEADER+1 FOR EX                              
         SR    R1,R1                                                            
         IC    R1,SVNARRLN                                                      
         AR    R1,R0                                                            
         STC   R1,TIMLN                                                         
*                                                                               
         LH    R0,RECLEN           ADD TO RECORD LENGTH                         
         AR    R0,R1                                                            
         STH   R0,RECLEN                                                        
         EJECT                                                                  
***********************************************************************         
* CREATE X'8B' TEMPO EXTRA STATUS ELEMENT                             *         
***********************************************************************         
         SPACE 1                                                                
VR650    DS    0H                                                               
         TM    STATUS2,MCSTIME     TEST PERSON ON MCS TIME                      
         BNZ   VR670               YES - THEN NO TEMPO TIMELS                   
*        TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
*        BNO   VR700                                                            
*        OC    SVTMPLN#,SVTMPLN#                                                
*        BZ    VR700                                                            
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMXTLNQ                                                   
         MVI   TIMETYP,TIMEXTRA                                                 
         XC    TIMXTLN#,TIMXTLN#   AS DEFAULT-CLEAR TEMPO LINE #                
         OC    SVTMPLN#,SVTMPLN#   ONLY SET LINE NUMBER IF THERE                
         BZ    *+10                REALLY IS ONE - PER EMAR/PCAS-4/01           
         MVC   TIMXTLN#,SVTMPLN#   TEMPO LINE #                                 
         MVC   TIMXTPDT,PRDENDTE   PERIOD END DATE                              
*                                                                               
         OC    SV1RENDT,SV1RENDT   DO WE HAVE A LOC END DATE                    
         BZ    VR655                                                            
         CLC   PRDENDTE,SV1RENDT   COMPARE PER END DATE W/ LOC END DATE         
         BL    *+10                TAKE WHICHEVER IS LOWER                      
         MVC   TIMXTPDT,SV1RENDT   LOCATION END DATE                            
*                                                                               
VR655    DS    0H                                                               
*&&UK                                                                           
         TM    STATUS2,DAILYTIM    ARE WE USING DAILY TIME                      
         BZ    VR660                                                            
         MVC   TIMXTDDT,SVYYMMDD   SAVE DATE OF THIS TIME                       
*&&                                                                             
*                                                                               
VR660    LH    R1,RECLEN           ADD TO RECORD LENGTH                         
         LA    R1,TIMXTLNQ(R1)                                                  
         STH   R1,RECLEN                                                        
         EJECT                                                                  
***********************************************************************         
* CREATE X'8B' TIMESHEET ETIME ELEMENT                                *         
***********************************************************************         
         SPACE 1                                                                
VR670    TM    STATUS2,MCSTIME     TEST ETIME IN USE                            
         BZ    VR700               NO                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
*                                                                               
         LA    R0,BCFLDS                                                        
         LHI   R1,CADETDQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CADETD,BCFLDS       SET UP PARM BLOCK                            
         MVC   CCOMFACS,ACOMFACS                                                
         MVC   CADASEC,ASECBLK                                                  
         MVC   CADAIOA,AIOBC       A(IO AREA TO USE)                            
         ST    R3,CADAELM                                                       
         MVC   CADSJLVS,BCSJLNQS   SJ CUMULATIVE LEVEL LENGTHS                  
         MVC   CAD1RLVS,BC1RLNQS   1R CUMULATIVE LEVEL LENGTHS                  
         MVC   CAD1RACT,SV1RCODE   1R ACCOUNT CODE                              
         MVC   CADTMULA,SV1NULA    1N U/L/ACCOUNT CODE                          
         TM    LINESTAT,LB1N       TEST CLIENT TIME                             
         BNZ   *+10                NO                                           
         MVC   CADTMULA,SVSJULA    SJ U/L/ACCOUNT CODE                          
         MVC   CADTASK,SVTASK      WORKCODE                                     
         MVC   CADNARRL,SVNARRLN   NARRATIVE LENGTH                             
         MVC   CADNARR,SVNARR      NARRATIVE                                    
         MVC   CADCLIOF,SVCLIOFF   CLIENT OFFICE                                
         MVC   CADCPYCD,CMPY                                                    
         OC    CADPIDN,SVPIDNO     1R PID                                       
         BNZ   *+6                                                              
         DC    H'0'                PERSON MUST HAVE A PID TO USE ETIME          
         ZAP   CADTSHRS,SVHOURS    HOURS                                        
         MVC   CADTTYP,SVTYPE      TYPE OF TIME                                 
         MVC   CADTSPSD,PRDSTDTE   PERIOD START DATE                            
         MVC   CADTSPED,PRDENDTE   PERIOD END DATE                              
         MVC   CADPERNO,PRDNUM     PERIOD NUMBER                                
         MVC   CADLOCED,SV1RENDT   LOCATION END DATE OR 00'S                    
         MVC   CADMOA,SVMOA        MONTH OF ACTIVITY                            
         MVC   CADTODAY,BCTODAY3                                                
         MVC   CADCUID,TWAORIG     CONNECTED USER-ID                            
         CLI   ACTEQU,ACTADD       TEST ACTION IS ADD                           
         BNE   *+12                                                             
         MVI   CADXCIND,CADXCADD   TELL CADET                                   
         B     VR672                                                            
         TM    TRKSTAT,TRKSUPDT    TEST EXISTING LINE BEING CHANGED             
         BZ    VR672                                                            
         MVI   CADXCIND,CADXCCHA   TELL CADET                                   
         MVC   CADHIROW,SVMCSRNO   PASS ROW (LINE) NUMBER                       
VR672    GOTO1 ACADET,CADETD                                                    
         MVC   BCTIMST1,CADTSSTS   CADET RETURNS TAUREC T/S STATUS              
*                                                                               
         MVC   TRKSTAT2,TIMEPST1   SET MCS APPROVAL STATUS                      
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         LH    R0,RECLEN           ADD TO RECORD LENGTH                         
         AR    R0,R1                                                            
         STH   R0,RECLEN                                                        
         EJECT                                                                  
***********************************************************************         
* UPDATE TSAR REC                                                     *         
***********************************************************************         
         SPACE 1                                                                
VR700    DS    0H                  UPDATE TSAR REC                              
         L     R6,AIO1                                                          
         USING TSARRECD,R6                                                      
         MVC   TRLEN,RECLEN        NEW LENGTH                                   
*        BAS   RE,CHECKDUP         CHECK IF ENTRY IS A DUPLICATE                
*        BNE   EDUPEN                                                           
*                                                                               
         TM    LINESTAT,LBADDREC   *** ADD TSAR RECORD ***                      
         BNO   VR750                                                            
         BAS   RE,CHKTSAR          CHECK TSAR RECORD BEFORE ADDING              
         BNE   VR800               IF NOT EQUAL - DONT ADD IT                   
VR725    GOTO1 ATSAR,BCDMCB,TSAADD,1                                            
         OI    TSARSTAT,TSARSAVE                                                
         CLI   BCTSERRS,0                                                       
         BE    VR800                                                            
         TM    BCTSERRS,X'80'                                                   
         BO    EMAX#               TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
*                                                                               
VR750    GOTO1 ATSAR,BCDMCB,TSAWRT,1    *** WRITE TSAR BUFFER ***               
         OI    TSARSTAT,TSARSAVE                                                
         CLI   BCTSERRS,0                                                       
         BE    VR800                                                            
         DC    H'0'                BCTSERRS=X'10'=RECORD NOT FOUND              
         EJECT                                                                  
***********************************************************************         
* CHECK TSAR RECORD BEFORE ADDING TO BUFFER                           *         
*       TRYING TO ELIMINATE SAVED RECORDS THAT ONLY CONTAIN TYPE FLDS *         
*       R6=A(TSAR RECORD)                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TSARRECD,R6                                                      
CHKTSAR  NTR1                                                                   
         USING TIMELD,R3                                                        
         LA    R3,TRDATA           START OF 8B - TIME ELEMENT                   
         CLI   TIMETYP,TIMEFLD     ONLY CONCERNED WITH COMMENTED ELM            
         BNE   ROUTE                                                            
         CLI   TIMLN,15            IF ELEMENT IS GREATER THAN 15 BYTES          
         BH    ROUTE                 LET IT GET ADDED TO BUFFER                 
         B     ROUTH                 IF NOT-SKIP IT                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT SCREEN LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
VR800    DS    0H                  NEXT LINE                                    
         USING SCRNTABD,R1                                                      
         L     R1,ASCRNITM                                                      
         SR    RF,RF                                                            
         ICM   RF,3,SCRNLEN        LENGTH OF LINE                               
         AR    R2,RF                                                            
         L     R1,ALSTLINE         LAST LINE?                                   
         CR    R2,R1                                                            
         BH    VR900                                                            
         DROP  R1                                                               
*                                                                               
         L     R1,ADISLINE         NEXT LINE NUMBER IN TABLE                    
         MVC   SCXLINE#,0(R1)                                                   
         LA    R1,4(R1)                                                         
         ST    R1,ADISLINE                                                      
         B     VR100                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORDS AND MAKE POSTINGS ON UPDATE                          *         
***********************************************************************         
         SPACE 1                                                                
VR900    CLI   PFKEY,PFSAVE                                                     
         BE    VR905                                                            
         LA    R2,ADDCUPDH         CONFIRMATION NAME - ADD SCREEN               
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPDH         CONFIRMATION NAME - CHANGE SCREEN            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   PFKEY,PFUPDATE                                                   
         BNE   VRX                                                              
VR905    BRAS  RE,TOTALS           DO TOTALS MATCH ON ADD                       
         GOTO1 ACHKINC                       INCOMPLETE TSAR RECS               
         BL    EMAX#                                                            
         BH    VRX                 GO COMPLETE BEFORE UPDATING                  
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    *+12                DONT SHOW CONFIRM UPDATE FIELD               
         BAS   RE,CONFIRM          EVEN IF PROFILE IS SET CONFIRM=Y             
         BNE   VRX                 NOT CONFIRMED                                
*        CLI   PFKEY,PFUPDATE                                                   
*        BNE   VRX                                                              
*                                                                               
         MVC   BCACKCPY,CMPY       REBUILD TSAR FROM ACTUAL RECS                
         MVC   BCACKUL,=C'1R'                                                   
         MVC   BCACKACT,SV1RCODE                                                
         MVC   BCYYMMDD,PRDENDTE                                                
         MVC   BCPIDNO,SVPIDNO                                                  
         MVC   BCINPDTE,SVYYMMDD                                                
*                                                                               
         CLI   PFKEY,PFSAVE                                                     
         BNE   VR910                                                            
         GOTO1 ABLDTSAR,BCDMCB,SVOPT1,SVOMOA,2    BULD 2ND BUFFER               
         GOTO1 AMRKTSAR            MARK TSAR ITEMS AS SAVED                     
         OI    STATUS2,TRNCMPAR                                                 
         BRAS  RE,CALLTTRN                                                      
         LA    RF,ERRUPDTE                                                      
         BH    *+8                                                              
         LA    RF,VR920                                                         
         NI    STATUS2,X'FF'-TRNCMPAR                                           
         BR    RF                                                               
*                                                                               
VR910    DS    0H                                                               
*&&US                                                                           
         TM    BCCPYSTA,CPYTMSFY   ALLOW TIME OUTSIDE CUR CALENDAR?             
         BO    VR915                                                            
         GOTO1 ACSTCHK             CHECK COSTING ACCTS FOR HOURS                
         BNE   ECSTCK                                                           
*&&                                                                             
VR915    GOTO1 ANEGCHK             CHECK FOR JOBS GOING NEGATIVE                
         BNE   ENEGCK                                                           
         GOTO1 ABLDTSAR,BCDMCB,SVOPT1,SVOMOA,2    BULD 2ND BUFFER               
*                                                                               
VR920    GOTO1 AUPDTSN,BCDMCB,SVRVSN#,SVTSNUM                                   
         BE    *+12                                                             
         CLI   ACTEQU,ACTADD       DONT LOCK OUT USER ON ACTION=ADD             
         BNE   EFATAL                                                           
         GOTO1 ADSAVE              DELETE SAVED RECORDS                         
         GOTO1 ASAVTSAR            SAVE ANY COMMENTED RECORDS                   
*                                                                               
         USING TIMELD,R3                                                        
         LA    R3,BCELEM                                                        
         XC    BCELEM,BCELEM                                                    
         MVI   TIMEL,TIMELQ        X'8B' TEMPO X-REF ELEMENT                    
         MVI   TIMLN,TIMXLNQ                                                    
         MVI   TIMETYP,TIMEXREF                                                 
         MVC   TIMXPED#,PRDNUM     PERIOD NUMBER                                
         MVC   TIMXPEDT,PRDENDTE   PERIOD END DATE                              
*                                                                               
         OC    SV1RENDT,SV1RENDT   DO WE HAVE A LOC END DATE                    
         BZ    VR925                                                            
         CLC   PRDENDTE,SV1RENDT   COMPARE PER END DATE W/ LOC END DATE         
         BL    *+10                TAKE WHICHEVER IS LOWER                      
         MVC   TIMXPEDT,SV1RENDT   LOCATION END DATE                            
*                                                                               
VR925    MVC   TIMXOFFC,SV1ROFFC   OFFICE                                       
         MVC   TIMXDEPT(L'SV1RDPT),SV1RDPT    DEPARTMENT                        
         OC    TIMXDEPT,BCSPACES                                                
         MVC   TIMXSDPT(L'SV1RSDPT),SV1RSDPT  SUBDEPARTMENT                     
         OC    TIMXSDPT,BCSPACES                                                
         MVC   TIMXPSDT,PRDSTDTE   PERIOD START DATE                            
         CLC   PRDSTDTE,SV1RSTDT                                                
         BH    *+10                                                             
         MVC   TIMXPSDT,SV1RSTDT   LOCATION START DATE                          
*                                                                               
         GOTO1 ATEMPO,(R3)         MAINTAIN TEMPO X-REF RECORD                  
         DROP  R3                                                               
*                                                                               
         CLI   PFKEY,PFSAVE                                                     
         BE    VR930                                                            
         GOTO1 AUPDTSAR,0                                                       
*        GOTO1 APOSTBLD            BUILD TIMETRN BUFFER                         
         BRAS  RE,CALLTTRN         CALL TIMETRN TO ADD POSTINGS                 
         BAS   RE,CLRCNFM          CLEAR CONFIRMATION FIELDS                    
*&&US                                                                           
         GOTO1 ATMSUPD,BCDMCB,(X'80',SV1RACT),ACOMFACS,ATMSUBLK,       X        
               SV1ROFFC                                                         
*&&                                                                             
VR930    BAS   RE,CLRNUMBS         CLEAR LINE NUMBERS                           
         BAS   RE,CLRCHNGS         REBUILD BLOCK AND DISPLAY                    
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
*                                                                               
VRX      B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TYPE OF TIME                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTYPE  NTR1                                                                   
         L     R2,AFIELDH                                                       
         OC    8(L'LC5TYPE,R2),BCSPACES                                         
*                                                                               
         CLI   PFKEY,PFUNMARK                                                   
         BNE   VTYPE5                                                           
         CLC   8(L'LC5TYPE,R2),BCSPACES                                         
         BE    EINVTYPE                                                         
*                                                                               
         USING TIMETABD,R3                                                      
VTYPE5   L     R3,=A(TIMETAB)                                                   
         A     R3,RELO                                                          
VTYPE10  CLC   TTFIELD,8(R2)                                                    
         BE    VTYPE20                                                          
         LA    R3,TTLENQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   VTYPE10                                                          
*                                                                               
         CLI   8(R2),C'*'          COMMENT OUT THIS LINE                        
         BNE   *+12                                                             
         OI    LINESTAT,LBSAVE                                                  
         B     VTYPE30                                                          
         CLI   8(R2),C'?'          COMMENT OUT THIS LINE W/ERROR                
         BNE   *+12                                                             
         OI    LINESTAT,LBSAVE+LBSAVE#                                          
         B     VTYPE30                                                          
*                                                                               
         CLC   8(L'LC5TYPE,R2),=C'DE'                                           
         BNE   EINVTYPE                                                         
         OI    LINESTAT,LBDELETE   DELETE LINE                                  
         B     VTYPE30                                                          
*                                                                               
VTYPE20  MVC   8(L'TTDISP,R2),TTDISP                                            
         MVC   SVTYPECH,TTTYPECH   CHARACTER EQUIVALENT (B/N/R)                 
         MVC   SVTYPE,TTTYPE       TYPE EQUIV                                   
         OC    SVIND,TTIND         TIME INDICATOR (ADJ)                         
         OC    FLDREQS,=Y(FLDHRS+FLDCLI)                                        
         CLI   SVTYPE,TIMTCB       REQS FOR B-TIME                              
         BNE   *+10                                                             
         OC    FLDREQS,=Y(FLDHRS+FLDCLI+FLDPRD+FLDJOB+FLDTSK)                   
VTYPE30  DS    0H                                                               
*                                                                               
* CHECK ANY TYPE FILTERS?                                                       
*                                                                               
         OC    BCFLTTYP,BCFLTTYP                                                
         BZ    VTYPEX                                                           
         MVI   BCIFLDH+5,L'SVTYPE                                               
         MVC   BCIFLD,BCSPACES                                                  
         MVC   BCIFLD(L'SVTYPE),SVTYPE                                          
         GOTO1 AFILTER,BCDMCB,(X'80',BCIFLDH),BCFLTTYP                          
         BNE   ERRNOMAT                                                         
*                                                                               
VTYPEX   OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALHRS   NTR1                                                                   
         L     R2,AFIELDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         GOTO1 AVALHRS,(R2)                                                     
         BNE   ACCERRX                                                          
         ZAP   SVHOURS,BCHOURS                                                  
         MVI   5(R2),L'LC5HRS                                                   
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VALHRS10                                                         
         CURED SVHOURS,(L'LC5HRS,8(R2)),2,MINUS=YES,DECPNT=FORCE                
         B     VALHRS20                                                         
*                                                                               
VALHRS10 CURED SVHOURS,(L'LC5HRS,8(R2)),2,MINUS=YES                             
VALHRS20 CP    SVHOURS,BCPZERO                                                  
         BNE   VALHRS30                                                         
         TM    GFACTST6,X'40'      ERROR OUT IF SCRIPT UPLOAD RUNNING           
         BNO   *+12                                                             
         CLI   BOTSCR,X'C8'        DON'T ERROR IF TEMPO                         
         BNE   ERRINVHR            INVALID HOURS                                
         OI    LINESTAT,LBDELETE   DELETE LINE                                  
VALHRS30 TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVTXRFSH    REFRESH TAX                                  
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT/PRODUCT/JOB/TASK                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCPJT  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVI   BCFLAG1,0           PARAMETER TO VALCPJ ROUTINE                  
         L     R2,AFIELDH                                                       
         MVI   BCIFMIN,1           CLIENT ALWAYS REQUIRED                       
*                                                                               
         MVC   BCHALF,FLDINPS                                                   
         NC    BCHALF,=AL2(FLDPRD+FLDJOB+FLDTSK)  INPUT=SJ                      
         BNZ   CLIVAL                                                           
         CLI   SVTYPE,TIMTCB       IF B OR R TIME = SJ ACCOUNT                  
         BE    CLIVAL                                                           
         CLI   SVTYPE,TIMTCR                                                    
         BE    CLIVAL                                                           
         SPACE 3                                                                
***********************************************************************         
* VALIDATE NON CLIENT 1N ACCOUNT                                      *         
***********************************************************************         
         SPACE 1                                                                
NCLIVAL  DS    0H                                                               
         MVI   BCIFMAX,7                                                        
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX                                                          
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1N'   VALIDATE 1N ACCOUNT                          
         MVC   ACTKACT,BCIFLD                                                   
         GOTO1 AGETACT,0                                                        
         BE    NCLIV10                                                          
         CLC   5(1,R2),BCSJLEV1    MAXIMUM LENGTH FOR SJ CLIENT LEVEL           
         BH    EINVACC             INVALID 1N ACCOUNT                           
         B     CLIVAL                                                           
*                                                                               
NCLIV10  TM    BCINDS,BCFLABAL     ACCOUNT VALID FOR POSTING?                   
         BNO   EACCPOST                                                         
         TM    BCASTAT1,RSTSACIL   ACCOUNT LOCKED?                              
         BO    EACCLOCK                                                         
         MVC   SV1NACT,BCACCODE    1N ACCOUNT CODE                              
         MVC   SV1NNAME,BCACNAME   1N ACCOUNT NAME                              
         MVI   SVTYPE,TIMTNC       INDICATE NON-CLIENT TIME                     
         OI    LINESTAT,LB1N                                                    
         NC    FLDREQS,=AL2(FLDEFS-FLDPRD-FLDJOB-FLDTSK-FLDRATE-FLDINC)         
         B     CLIVALX                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SJ ACCOUNT - CLIENT                                        *         
***********************************************************************         
         SPACE 1                                                                
CLIVAL   DS    0H                                                               
         MVC   SVMED,BCSPACES                                                   
         MVC   BCIFMAX,BCSJLEV1    MAXIMUM LENGTH FOR SJ CLIENT LEVEL           
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX                                                          
*                                                                               
         OI    BCFLAG1,BCFL1CLI    VALIDATE CLIENT                              
         GOTO1 AVALCPJ,(R2)                                                     
         BNE   ACCERRX                                                          
*                                                                               
* CHECK ANY CLIENT FILTERS?                                                     
*                                                                               
         CLC   BCFLTCLI,BCSPACES                                                
         BNH   CLIVAL20                                                         
         GOTO1 AFILTER,BCDMCB,(X'80',(R2)),BCFLTCLI                             
         BNE   ERRNOMAT                                                         
*                                                                               
CLIVAL20 MVC   SVSJACT,BCACCODE                                                 
         MVC   SVTASK,BCSPACES                                                  
*                                                                               
*&&UK                                                                           
         CLI   SVTYPE,TIMTCN       IF NON-BILLABLE TIME                         
         BE    *+12                IGNORE SJ LOCK STATUS                        
*&&                                                                             
         TM    BCASTAT1,RSTSACIL   ACCOUNT LOCKED                               
         BO    EACCLOCK                                                         
         CLC   BCPRCOST,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SV1CACT,BCPRCOST    COSTING ACCOUNT                              
         CLC   BCPROFFC,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SVCLIOFF,BCPROFFC   OFFICE CODE (GENERAL ACCOUNTING)             
*                                                                               
*&&US                                                                           
         TM    BCCPYST3,CPYSPC1C+CPYSPCSJ IS CMPY ON PROJECT CONTROL            
         BZ    CLIVAL30                                                         
         TM    BCASTAT3,RSTSPRTS                                                
         BNO   CLIVAL30                                                         
         OI    PCSTAT,PCSJ                                                      
         OC    FLDREQS,=Y(FLDPRD+FLDJOB+FLDTSK)                                 
*&&                                                                             
*                                                                               
* CHECK FOR OFFICE SECURITY AT CLIENT LEVEL IF PROD IS NOT REQ'D                
* AND NOT ENETERED.  IF PROD IS ENTERED AND/OR REQ'D CHECK OFFICE               
* SECURITY AT PROD LEVEL.                                                       
*                                                                               
CLIVAL30 MVC   BCHALF,FLDREQS      IF PROD IS REQUIRED CHECK OFF SEC            
         NC    BCHALF,=Y(FLDPRD)      AT PROD LEVEL                             
         BNZ   CLIVALX                                                          
*                                                                               
         MVC   BCHALF,FLDINPS      IF PROD WAS ENTERED CHECK OFF SEC            
         NC    BCHALF,=Y(FLDPRD)      AT PROD LEVEL                             
         BNZ   CLIVALX                                                          
*                                                                               
         MVC   BCHALF,FLDINPS      IF JOB WAS ENTERED-PROD REQ'D                
         NC    BCHALF,=Y(FLDJOB)      SO CHECK OFF SEC AT PROD LEVEL            
         BNZ   CLIVALX                                                          
*                                                                               
         GOTO1 TSTSEC,SVCLIOFF     TEST OFFICE SECURITY AT CLI LEV              
*                                                                               
CLIVALX  TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVSRFRSH+SVTXRFSH   REFRESH RATE/INCOME/TAX              
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE SJ ACCOUNT - PRODUCT                                       *         
***********************************************************************         
         SPACE 1                                                                
PROVAL   DS    0H                                                               
         SR    R1,R1               BUMP TO PRODUCT FIELD                        
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         ST    R2,APRDH            SAVE A(PRODUCT FIELD)                        
*                                                                               
         MVC   BCHALF,FLDINPS      PRODUCT REQUIRED IF JOB                      
         NC    BCHALF,=Y(FLDJOB)    WAS INPUT ON THIS LINE                      
         BZ    *+10                                                             
         OC    FLDREQS,=Y(FLDPRD)                                               
*                                                                               
         MVI   BCIFMIN,0           ASSUME PRODUCT NOT REQUIRED                  
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDPRD)                                                
         BZ    *+8                                                              
         MVI   BCIFMIN,1           PRODUCT REQUIRED                             
         MVC   BCIFMAX,BCSJLEV2    MAX LEN FOR SJ PRODUCT LEVEL                 
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX             ERROR - INVALID PRODUCT                      
         BL    PROX                NOTHING ENTERED/NOTHING REQ'D                
*                                                                               
* CHECK ANY PRODUCT FILTERS?                                                    
*                                                                               
         CLC   BCFLTPRO,BCSPACES                                                
         BNH   PROVAL20                                                         
         GOTO1 AFILTER,BCDMCB,(X'80',(R2)),BCFLTPRO                             
         BNE   ERRNOMAT                                                         
*                                                                               
PROVAL20 LA    RF,SVSJCODE         MOVE PRODUCT INTO SJ SAVE AREA               
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ1                                                      
         AR    RF,R1                                                            
         IC    R1,BCSJLEV2         LENGTH OF LEVEL B                            
         AHI   R1,-1                                                            
         BM    ERRINV                                                           
         MVC   0(0,RF),BCIFLD                                                   
         EX    R1,*-6                                                           
*                                                                               
         OI    BCFLAG1,BCFL1PRD    VALIDATE PRODUCT                             
         GOTO1 AVALCPJ,(R2)                                                     
         BNE   ACCERRX                                                          
*&&UK                                                                           
         CLI   SVTYPE,TIMTCN       IF NON-BILLABLE TIME                         
         BE    *+12                IGNORE SJ LOCK STATUS                        
*&&                                                                             
         TM    BCASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BO    EACCLOCK                                                         
*                                                                               
         CLC   BCPRCOST,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SV1CACT,BCPRCOST    COSTING ACCOUNT                              
         CLC   BCPROFFC,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SVCLIOFF,BCPROFFC   OFFICE CODE (GENERAL ACCOUNTING)             
*                                                                               
         GOTO1 TSTSEC,SVCLIOFF     TEST OFFICE SECURITY AT PRD LEV              
*                                                                               
*&&US                                                                           
         TM    BCCPYST3,CPYSPC1C+CPYSPCSJ IS CMPY ON PROJECT CONTROL            
         BZ    PROX                                                             
         TM    BCASTAT3,RSTSPRTS                                                
         BNO   PROX                                                             
         OI    PCSTAT,PCSJ                                                      
         OC    FLDREQS,=Y(FLDJOB+FLDTSK)                                        
*&&                                                                             
*                                                                               
PROX     TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVSRFRSH+SVTXRFSH   REFRESH RATE/INCOME/TAX              
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE SJ ACCOUNT - JOB                                           *         
***********************************************************************         
         SPACE 1                                                                
JOBVAL   DS    0H                                                               
         SR    R1,R1               BUMP TO JOB FIELD                            
         IC    R1,0(R2)            NEXT FIELD IS JOB                            
         AR    R2,R1                                                            
         ST    R2,AJOBH                                                         
*                                                                               
         MVI   BCIFMIN,0           NOT REQUIRED                                 
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDJOB)                                                
         BZ    *+8                                                              
         MVI   BCIFMIN,1                                                        
         MVC   BCIFMAX,BCSJLEV3    MAX LEN FOR SJ JOB                           
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX             ERROR                                        
         BL    JOBVX               NOTHING ENTERED/NOTHING REQ'D                
*                                                                               
* CHECK ANY JOB FILTERS?                                                        
*                                                                               
         CLC   BCFLTJOB,BCSPACES                                                
         BNH   JOBVAL20                                                         
         GOTO1 AFILTER,BCDMCB,(X'80',(R2)),BCFLTJOB                             
         BNE   ERRNOMAT                                                         
*                                                                               
JOBVAL20 LA    RF,SVSJCODE         MOVE JOB INTO SJ SAVE AREA                   
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ2                                                      
         AR    RF,R1                                                            
         IC    R1,BCSJLEV3         LENGTH OF LEVEL C                            
         AHI   R1,-1                                                            
         BM    ERRINV                                                           
         MVC   0(0,RF),BCIFLD                                                   
         EX    R1,*-6                                                           
         MVC   SVMED,BCIFLD                                                     
*                                                                               
         OI    BCFLAG1,BCFL1JOB    VALIDATE JOB                                 
         GOTO1 AVALCPJ,(R2)                                                     
         BNE   ACCERRX                                                          
*&&UK                                                                           
         CLI   SVTYPE,TIMTCN       IF NON-BILLABLE TIME                         
         BE    JOBVAL30            IGNORE SJ LOCK STATUS                        
*&&                                                                             
         TM    BCASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BO    EACCLOCK                                                         
         TM    BCASTAT1,RSTSACIC   IS ACCOUNT CLOSED                            
         BO    EACCCLS                                                          
*                                                                               
JOBVAL30 CLC   BCPRCOST,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SV1CACT,BCPRCOST    COSTING ACCOUNT                              
         CLC   BCPROFFC,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SVCLIOFF,BCPROFFC   OFFICE CODE                                  
*&&UK                                                                           
         GOTO1 TSTSEC,SVCLIOFF     TEST OFFICE SECURITY AT JOB LEV              
*&&                                                                             
*&&US                                                                           
         TM    BCINDS,BCFLEADJ     JOB QUALIFIES FOR RATE ADJUST                
         BNO   *+8                                                              
         OI    LINESTAT,LBRATADJ   RATE IS ELEGIBLE FOR ADJUSTMENT              
         TM    BCINDS,BCFLXJOB     JOB IS AN X-JOB                              
         BNO   *+12                                                             
         CLI   SVTYPECH,C'B'       ERROR FOR B-TIME TO X-JOBS                   
         BE    EBXJOB                                                           
*&&                                                                             
         OC    FLDREQS,=Y(FLDTSK)  TASK REQUIRED WITH JOB                       
*                                                                               
JOBVX    TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVSRFRSH+SVTXRFSH   REFRESH RATE/INCOME/TAX              
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE TASK                                                       *         
***********************************************************************         
         SPACE 1                                                                
TASKVAL  DS    0H                                                               
         SR    R1,R1               BUMP TO TASK FIELD                           
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
TASKV10  MVI   BCIFMIN,0           NOT REQUIRED                                 
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDTSK)                                                
         BZ    *+8                                                              
         MVI   BCIFMIN,1                                                        
         MVI   BCIFMAX,2           MAX LEN FOR SJ TASK                          
         GOTO1 AFVAL,(R2)                                                       
         BL    TASKX               NOTHING ENTERED/NOTHING REQ'D                
         BE    TASKV20                                                          
         CLC   SVDFTASK,BCSPACES                                                
         BNH   ACCERRX                                                          
         MVC   8(L'SVDFTASK,R2),SVDFTASK                                        
         MVI   5(R2),L'SVDFTASK                                                 
         OI    6(R2),X'80'                                                      
         B     TASKV10                                                          
*                                                                               
TASKV20  CLC   BCIFLD(2),=C'99'    DO NOT ALLOW W/C 99                          
         BE    EINVTSK                                                          
         MVC   SVTASK,BCIFLD                                                    
*                                                                               
* CHECK ANY TASK FILTERS?                                                       
*                                                                               
         CLC   BCFLTTSK,BCSPACES                                                
         BNH   TASKV30                                                          
         GOTO1 AFILTER,BCDMCB,(X'80',(R2)),BCFLTTSK                             
         BNE   ERRNOMAT                                                         
*                                                                               
TASKV30  DS    0H                                                               
*&&US                                                                           
         CLI   SVTASK,C' '         MAKE SURE NEITHER POSITION                   
         BE    EINVTSK               IN THE WORK CODE IS A SPACE                
         CLI   SVTASK+1,C' '                                                    
         BE    EINVTSK                                                          
*                                                                               
         TM    PCSTAT,PCSJ+PC1R    IF USING PROJECT CONTROL                     
         BZ    TASKV40                                                          
         USING WCORECD,R6          *** VALIDATE 1J WORKCODE ***                 
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CMPY                                                     
         MVC   WCOKUNT(2),=C'1J'                                                
         MVC   WCOKWRK,SVTASK                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EINVTSK                                                          
*&&                                                                             
*                                                                               
TASKV40  DS    0H                  *** VALIDATE SJ WORKCODE ***                 
         USING WCORECD,R6          X'0A' WORK-CODE RECORDS                      
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CMPY                                                     
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,SVTASK                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EINVTSK                                                          
*&&UK                                                                           
         CLC   SVTASK,=CL2'**'     DISALLOW WORKCODE '**'                       
         BE    EINVTSK                                                          
         TM    BCINDS,BCFLTIME     MUST BE A TIME TYPE WORKCODE                 
         BNO   EINVTSK             IN THE UK AND GERMANY                        
*&&                                                                             
         TM    BCINDS,BCFLEADJ     TASK QUALIFIES FOR RATE ADJUST               
         BNO   *+8                                                              
         OI    LINESTAT,LBRATADJ   RATE IS ELEGIBLE FOR ADJUSTMENT              
*                                                                               
TASKX    TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVSRFRSH+SVTXRFSH   REFRESH RATE/INCOME/TAX              
         OI    4(R2),X'20'                                                      
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COSTING ACCOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
COSTVAL  DS    0H                                                               
         TM    LINESTAT,LB1N       POSTING TO A 1N NON CLIENT ACCT              
         BO    EXIT                                                             
*&&UK                                                                           
         TM    BCCPYST1,CPYSOROE   IS OFFICE REQUIRED? UK'S OPTIONAL            
         BO    *+20                                                             
         MVC   SVSJROFF,SVCLIOFF   SAVE CLIENT OFFICE FOR RATE LOOKUP           
         MVC   SVCLIOFF,BCSPACES   CLEAR CLIENT OFFICE FIELD                    
         B     *+14                                                             
*&&                                                                             
         OC    SVCLIOFF,SVCLIOFF                                                
         BZ    ERRINV              AE$ANFAN ERROR - NO CLIENT OFFICE            
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,SV1CACT                                                 
         OC    SVCCNTER,BCSPACES                                                
         CLI   SVCPOSN,0           IS THERE AN OVERRIDE POSITION                
         BNH   COSTV10                                                          
         LA    R3,ACTKEY+2                                                      
         ZIC   R1,SVCPOSN                                                       
         AR    R3,R1                                                            
         B     *+8                                                              
COSTV10  LA    R3,ACTKEY+7         NO OVERRIDE POSN DESIGNATED                  
         LA    R0,3                MAX THREE CHARS CAN BE OVERWRITTEN           
         LA    R1,SVCCNTER                                                      
COSTV20  CLI   SVCCNTER,C' '       CANNOT OVERRIDE WITH A SPACE                 
         BE    *+10                                                             
         MVC   0(1,R3),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,COSTV20                                                       
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 AGETACT,0                                                        
         BNE   EINVACC             INVALID COSTING ACCOUNT                      
*                                                                               
* CHECK ANY COSTING ACC FILTERS?                                                
*                                                                               
         CLC   BCFLTCA,BCSPACES                                                 
         BNH   COSTV25                                                          
         MVI   BCIFLDH+5,L'ACTKULA                                              
         MVC   BCIFLD,BCSPACES                                                  
         MVC   BCIFLD(L'ACTKULA),ACTKULA                                        
         GOTO1 AFILTER,BCDMCB,(X'80',BCIFLDH),BCFLTCA                           
         BNE   ERRNOMAT                                                         
*                                                                               
COSTV25  TM    BCASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BO    EACCLOCK                                                         
         TM    BCINDS,BCFLABAL     ACCOUNT VALID FOR POSTING                    
         BNO   EACCPOST                                                         
         MVC   SV1CACT,BCACCODE    SAVE COST ACCOUNT                            
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         LA    R3,L'BC1RLNQS       # TIMES TO LOOP THRU 1C                      
         LA    R2,BC1CLNQ4                                                      
*                                                                               
COSTV30  SR    R1,R1                                                            
         ICM   R1,1,0(R2)                                                       
         BZ    COSTV40                                                          
         MVC   ACTKULA,BCSPACES                                                 
         LA    R1,1(R1)            +2 FOR U/L - 1 FOR EX                        
         MVC   ACTKULA(0),SV1CULA                                               
         EX    R1,*-6                                                           
         GOTO1 AGETACT,0                                                        
         BNE   COSTV40                                                          
*                                                                               
         OC    BCASTAT5,BCASTAT5   IF NO STATUS GO TO NEXT HIGHER LEVEL         
         BZ    COSTV40                                                          
         TM    BCASTAT5,RSTSPROD+RSTSPRJB                                       
         BZ    COSTV40                                                          
         MVI   BCIFMIN,1           REQUIRED                                     
         MVC   BCIFMAX,BCSJLEV2    MAX LEN FOR SJ PROD                          
         L     R2,APRDH            PROD                                         
         GOTO1 AFVAL,(R2)                                                       
         BE    *+8                                                              
         B     ACCERRX                                                          
*                                                                               
         TM    BCASTAT5,RSTSPRJB                                                
         BZ    COSTV40                                                          
         MVI   BCIFMIN,1           REQUIRED                                     
         MVC   BCIFMAX,BCSJLEV3    MAX LEN FOR SJ JOB                           
         L     R2,AJOBH            JOB                                          
         GOTO1 AFVAL,(R2)                                                       
         BE    COSTX                                                            
         B     ACCERRX                                                          
*                                                                               
COSTV40  BCTR  R2,0                NEXT HIGHER LEVEL OF 1C                      
         BCT   R3,COSTV30                                                       
*                                                                               
COSTX    DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CALL OPTION/MAINT VALIDATION                                        *         
***********************************************************************         
         SPACE 1                                                                
OPTMNT   DS    0H                                                               
         L     R0,AGOBLOCK                                                      
         LH    R1,=Y(GOBLOCKX-GOBLOCKD)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AGOBLOCK                                                      
         AH    R0,=Y(GOBLOCKX-GOBLOCKD)                                         
         ST    R0,AGOXBLK          EXTENSION BLOCK                              
         LH    R1,=Y(GOXBLKX-GOXBLOCK)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOAEXT,AGOXBLK      EXTENSION BLOCK                              
         MVC   GOSELCUL,CMPY                                                    
         MVC   GOSELCUL+1(2),=C'SJ'                                             
*                                                                               
         MVC   GOSELCLI,BCSPACES                                                
         LA    RF,SVSJCODE         SJ CLIENT CODE                               
         SR    R1,R1                                                            
         IC    R1,BCXSJLV1                                                      
         MVC   GOSELCLI(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ PRODUCT CODE                              
         IC    R1,BCSJLEV2                                                      
         AHI   R1,-1                                                            
         BM    OPT100                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   OPT100                                                           
         MVC   GOSELPRO,BCSPACES                                                
         MVC   GOSELPRO(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ JOB CODE                                  
         IC    R1,BCSJLEV3                                                      
         AHI   R1,-1                                                            
         BM    OPT100                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   OPT100                                                           
         MVC   GOSELJOB,BCSPACES                                                
         MVC   GOSELJOB(0),0(RF)                                                
         EX    R1,*-6                                                           
         MVC   GOSELWC,SVTASK                                                   
*                                                                               
OPT100   DS    0H                                                               
         GOTO1 VGETOPT,BCDMCB,(R3)                                              
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLK                                                       
*                                                                               
*&&US                                                                           
OPT200   DS    0H                                                               
         CLI   GOFPT,C' '          ANY PROFILE ENTERED                          
         BNH   OPT300                                                           
         CLI   GOFPT,C'A'          PROD REQUIRED FOR ALL TYPES TIME             
         BE    OPT250                                                           
         CLC   GOFPT,SVTYPECH      REQUIRED FOR THIS TYPE?                      
         BNE   OPT300                                                           
*                                                                               
OPT250   SR    R1,R1                                                            
         IC    R1,BCSJLNQ1                                                      
         LA    RF,SVSJCODE(R1)                                                  
         IC    R1,BCSJLEV2                                                      
         AHI   R1,-1                                                            
         BM    OPT300                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BH    OPT300                                                           
         L     R2,APRDH                                                         
         B     ERRRQPRD            ERROR - REQUIRES PRODUCT                     
*                                                                               
OPT300   DS    0H                                                               
         CLI   GOFJT,C' '          ANY PROFILE ENTERED                          
         BNH   OPT400                                                           
         CLI   GOFJT,C'A'          JOB REQUIRED FOR ALL TYPES TIME              
         BE    OPT350                                                           
         CLC   GOFJT,SVTYPECH      REQUIRED FOR THIS TYPE?                      
         BNE   OPT400                                                           
*                                                                               
OPT350   SR    R1,R1                                                            
         IC    R1,BCSJLNQ2                                                      
         LA    RF,SVSJCODE(R1)                                                  
         IC    R1,BCSJLEV3                                                      
         AHI   R1,-1                                                            
         BM    OPT400                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BH    OPT400                                                           
         L     R2,AJOBH                                                         
         B     ERRRQJOB            JOB REQUIRED                                 
*                                                                               
OPT400   DS    0H                                                               
         CLI   GOTAX,C'Y'          TAX REQUIRED                                 
         BNE   *+8                                                              
         OI    SVTAXSTA,SVTAXREQ                                                
*&&                                                                             
*                                                                               
OPTX     DS    0H                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET CURRENCY CODE FROM GETOPT                                       *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   DS    0H                                                               
*&&UK                                                                           
         NI    BCFLAG5,X'FF'-BCFLSEC      START WITH SWITCH OFF                 
         XC    CURCD,CURCD                CLEAR OUT CURRENCY CODE               
         TM    BCCPYST7,CPYSSCNV          AGENCY ON DUAL CURRENCY?              
         BZ    GETCX                                                            
*                                                                               
         LA    RF,SVSJCODE         SJ CLIENT CODE                               
         SR    R1,R1               MAKE SURE WE ARE JOB LEVEL                   
         IC    R1,BCSJLEV3                                                      
         AR    RF,R1                                                            
         SH    R1,=H'1'                                                         
         BM    GETCX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETCX               EXIT IF NOT AT JOB LEVEL                     
*                                                                               
         USING GOBLOCKD,R3         R2=A(GETOPT BLOCK)                           
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOAEXT,AGOXBLK      SET A(BLOCK EXTENSION)                       
         MVC   GOCTRY,CTRY                                                      
         MVC   GOSELCUL(1),CMPY                                                 
         MVC   GOSELCUL+1(2),=C'SJ'                                             
*                                                                               
         MVC   GOSELCLI,BCSPACES                                                
         LA    RF,SVSJCODE         SJ CLIENT CODE                               
         SR    R1,R1                                                            
         IC    R1,BCXSJLV1                                                      
         MVC   GOSELCLI(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ PRODUCT CODE                              
         IC    R1,BCSJLEV2                                                      
         AHI   R1,-1                                                            
         BM    GETC10                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETC10                                                           
         MVC   GOSELPRO,BCSPACES                                                
         MVC   GOSELPRO(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ JOB CODE                                  
         ICM   R1,1,BCSJLEV3                                                    
         AHI   R1,-1                                                            
         BM    GETC10                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETC10                                                           
         MVC   GOSELJOB,BCSPACES                                                
         MVC   GOSELJOB(0),0(RF)                                                
         EX    R1,*-6                                                           
         MVC   GOSELWC,SVTASK                                                   
*                                                                               
         LA    RF,EXTBLK           NEW BILLING EXTENSION                        
         ST    RF,GOABEXT                                                       
*                                                                               
GETC10   DS    0H                                                               
         GOTO1 VGETOPT,BCDMCB,(R3)                                              
         USING GOBLOCKD,R3         R2=A(GETOPT BLOCK)                           
         L     R3,AGOBLOCK                                                      
*                                                                               
GETC20   LA    R1,EXTBLK           NEW BILLING EXTENSION                        
         USING GOBBLCKD,R1                                                      
         CLC   GOBILCUR,BCCPYSEC   2ND CURRENCY BILLED                          
         BNE   *+8                                                              
         OI    BCFLAG5,BCFLSEC     SET 2ND CURRENCY INDICATOR ON                
*                                                                               
         MVC   CURCD1,BCCPYSEC     BUILD CASHVAL CURRENCY CODES                 
         MVC   CURCD2,BCCPYCUR                                                  
         MVC   CURCD3,BCCPYSEC                                                  
         DROP  R1                                                               
*&&                                                                             
GETCX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROJECT CONTROL ACCOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
PRJVAL   DS    0H                                                               
         TM    BCCPYST3,CPYSPC1C+CPYSPCSJ    CMPY ON PROJECT CONTROL            
         BZ    PRJX                                                             
         MVC   BCHALF,FLDINPS                                                   
         NC    BCHALF,=AL2(FLDJOB) WAS JOB INPUT                                
         BZ    PRJX                                                             
         TM    PCSTAT,PCSJ+PC1R    WAS PC=Y ON AT ANY LEVEL OF 1R OR SJ         
         BZ    PRJX                                                             
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1J'                                                
         MVC   ACTKACT,SVSJCODE                                                 
         TM    PCSTAT,PCSJ                                                      
         BO    PRJ10                                                            
         LA    R3,SV1CCODE                                                      
         SR    R1,R1                                                            
         ICM   R1,1,SVCLIPOS                                                    
         BZ    PRJ05                                                            
         SHI   R1,1                                                             
         AR    R3,R1                                                            
PRJ05    MVC   ACTKACT(6),0(R3)                                                 
*                                                                               
PRJ10    GOTO1 AGETACT,0                                                        
         BNE   EINVACC             INV '1J' ACCOUNT                             
         MVC   SVPCACT,BCACCODE    SAVE PC ACCOUNT                              
*                                                                               
PRJX     DS    0H                                                               
         DROP  R6                                                               
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOA FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALMOA   NTR1                                                                   
         L     R2,AFIELDH                                                       
         CLI   5(R2),0                                                          
         BNE   VMOA10                                                           
         MVC   YYMMDD(2),SVOMOA    FIRST OPEN MOA                               
         MVI   YYMMDD+2,X'01'                                                   
         B     VMOA20                                                           
*                                                                               
VMOA10   MVC   BCWORK,BCSPACES     VALIDATE INPUT FROM SCREEN                   
         GOTO1 DATVAL,BCDMCB,(2,8(R2)),BCWORK                                   
         ICM   RF,15,BCDMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,BCDMCB,(0,BCWORK),(1,YYMMDD)                              
         CLC   YYMMDD(2),SVOMOA                                                 
         BL    EOPNMT              ERROR IF < OPEN MOA                          
*                                                                               
VMOA20   MVC   BCWORK,BCSPACES                                                  
         CLC   YYMMDD(2),PRDMON                                                 
         BL    EOPNMT                                                           
         GOTO1 AMTHLOCK,BCDMCB,YYMMDD,YYMMDD,TWAACCS                            
         BNE   ACCERRX                                                          
*                                                                               
* CHECK ANY MOA FILTERS?                                                        
*                                                                               
         MVC   SVMOA,YYMMDD                                                     
         OC    BCFLTMOA,BCFLTMOA                                                
         BZ    VMOA30                                                           
         MVI   BCIFLDH+5,L'SVMOA                                                
         MVC   BCIFLD,BCSPACES                                                  
         MVC   BCIFLD(L'SVMOA),SVMOA                                            
         GOTO1 AFILTER,BCDMCB,(X'A0',BCIFLDH),BCFLTMOA                          
         BNE   ERRNOMAT                                                         
*                                                                               
VMOA30   MVC   BCWORK,BCSPACES                                                  
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,BCWORK)                              
         MVC   8(6,R2),BCWORK                                                   
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         VALIDATED MOA                                
*                                                                               
*        CLC   YYMMDD(2),CALSTDTE                                               
*        BL    ERROCAL             ERROR IF DATE < CALENDAR START DTE           
*        CLC   YYMMDD(2),CALENDTE                                               
*        BH    ERROCAL             ERROR IF DATE > CALENDAR END DTE             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATE AND CALCULATE AMOUNT                                  *         
***********************************************************************         
         SPACE 1                                                                
VALRATE  NTR1                                                                   
         USING CAPRECD,R6                                                       
         LA    R6,BIGKEY           READ COST ALLOCATION PROFILE RECORD          
         MVC   BIGKEY,BCSPACES                                                  
         MVI   CAPKTYP,CAPKTYPQ    X'3E'                                        
         MVI   CAPKSUB,CAPKSUBQ    X'09'                                        
         MVC   CAPKCPY,CMPY        COMPANY CODE                                 
         MVI   CAPKMTHD,C'1'       METHOD ONE                                   
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),SYSDIR,BIGKEY,BIGKEY,0              
         B     VRATE04                                                          
*                                                                               
VRATE02  LA    R6,BIGKEY                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ '),SYSDIR,BIGKEY,BIGKEY,0              
VRATE04  CLC   CAPKEY(CAPKMIN-L'CAPKMTHD),KEYSAVE                               
         BE    VRATE06             SAME COMPANY SO LOOK FOR METHOD              
         B     VRATE20                                                          
*                                                                               
VRATE06  LA    R3,CAPKDA                                                        
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC ',SYSFIL,(R3),(R6),DMWORK                 
         CLI   DMCB+8,0            ANY DATAMANAGER ERRORS                       
         BE    *+6                 NO                                           
         DC    H'0'                YES - DIE                                    
         LA    R2,CAPRFST          POINT R4 TO FIRST ELEMENT OF RECORD          
         XR    R0,R0                                                            
         USING OPDELD,R2                                                        
VRATE08  CLI   OPDEL,OPDELQ        FIND OPTION DATA ELEMENTS                    
         BE    VRATE12                                                          
         CLI   OPDEL,0             EOR                                          
         BE    VRATE02             GET NEXT RECORD                              
VRATE10  IC    R0,OPDLN            INCREMENT TO NEXT ELEMENT                    
         AR    R2,R0                                                            
         B     VRATE08             BRANCH BACK TO ELEMENT READ                  
*                                                                               
VRATE12  CLI   OPDNUM,X'1C'        UPDATE ONLINE CA TYPE BUCKETS                
         BNE   VRATE10                                                          
         CLI   OPDDATA,C'Y'        YES                                          
         BNE   VRATE02             GET NEXT SEQUENTIAL RECORD                   
         OI    STATUS2,ONLNUPDT                                                 
*                                                                               
VRATE20  XC    BCFLAG2,BCFLAG2     INITIALIZE FLAG                              
         L     R2,AFIELDH                                                       
         CLI   SVTYPE,TIMTCB       B-TIME?                                      
         BE    VRATE22                                                          
         CLI   SVTYPE,TIMTCR       R-TIME?                                      
         BE    VRATE22                                                          
         TM    STATUS2,ONLNUPDT    ARE WE DOING ON-LINE CA UPDATE               
         BZ    VRATEX              NO                                           
         CLI   SVTYPE,TIMTCN       N-TIME FOR CLIENT?                           
         BNE   VRATEX              MUST CHECK COST RATE EXISTS                  
VRATE22  ZAP   SVCRATE,BCPZERO     COST RATE                                    
         XC    SVCRATEF,SVCRATEF   COST RATE EFFECTIVE DATE                     
*&&UK                                                                           
         ZAP   SVERATE,BCPZERO     EURO CHARGE RATE                             
         ZAP   SVECRATE,BCPZERO    EURO COST RATE                               
*                                                                               
         USING RATED,R3            *** COST RATE LOOKUP ***                     
         LA    R3,BLOCK                                                         
         XC    BLOCK(RATEDQ),BLOCK                                              
         MVC   RATCTRY,CTRY                                                     
         MVC   RAT1RACT,SV1RCODE                                                
         MVC   RAT1ROFF,SV1ROFFC                                                
         MVC   RAT1RDPT,SV1RDPT                                                 
         MVC   RAT1RSUB,SV1RSDPT                                                
         MVC   RAT1RPER,SV1RPRSN                                                
         MVC   RATSJCPJ,SVSJCODE                                                
         MVC   RATSJOFF,SVCLIOFF                                                
         CLC   SVCLIOFF,BCSPACES                                                
         BH    *+10                                                             
         MVC   RATSJOFF,SVSJROFF   SJ CLIENT OFFICE                             
         MVC   RATSJTSK,SVTASK                                                  
         MVC   RATCRDTE,PRDENDTE   PERIOD ENDING DATE                           
*                                                                               
         OC    SV1RENDT,SV1RENDT   DO WE HAVE A LOC END DATE                    
         BZ    *+20                                                             
         CLC   PRDENDTE,SV1RENDT   COMPARE PER END DATE W/ LOC END DATE         
         BL    *+10                TAKE WHICHEVER IS LOWER                      
         MVC   RATCRDTE,SV1RENDT   LOCATION END DATE                            
*                                                                               
*&&UK                                                                           
         MVC   RATCSTAT,BCCPYST7   COMPANY STATUS 7                             
         MVC   RATCSTA4,BCCPYST4   COMPANY STATUS 4                             
         MVC   RATCFLG,BCFLAG5     CURRENCY FLAG                                
         MVC   RATCURS,CURCD       CASHVAL CURRENCY CODES                       
*&&                                                                             
         OI    RATSTAT,RATSCOST                                                 
         MVC   RDATAMGR,DATAMGR                                                 
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RCASHVAL,CASHVAL                                                 
         MVC   RTOBACCO,ATOBACCO                                                
         MVC   RAT1CMPY,CMPY                                                    
         GOTO1 AGETRTE,BLOCK                                                    
         TM    RATSTAT2,RATEFNDC   WAS A COST RATE FOUND                        
         BO    VRATE24                                                          
         L     R2,AFLD1STH         POINT TO FIRST FIELD ON LINE                 
         CLC   SVMOA,PRDENDTE                                                   
         BNE   ENOCRATE                                                         
         TM    STATUS2,ONLNUPDT                                                 
         BO    ERRRQHST                                                         
         B     ENOCRATE                                                         
                                                                                
VRATE24  CLI   SVTYPE,TIMTCN       N-TIME FOR CLIENT?                           
         BE    VRATE2A             MUST CHECK COST RATE EXISTS                  
         ZAP   SVCRATE,RATEAMTC    COST RATE                                    
         MVC   SVCRATEF,RATEEFFC   COST RATE EFFECTIVE DATE                     
         ZAP   SVECRATE,RAT2AMTC   EURO RATE FOR COST RATE                      
*                                                                               
VRATE2A  TM    STATUS2,ONLNUPDT                                                 
         BZ    VRATE2B                                                          
         CLC   SVMOA,PRDENDTE                                                   
         BE    VRATE2B                                                          
                                                                                
         LA    R3,BLOCK                                                         
         XC    BLOCK(RATEDQ),BLOCK                                              
         MVC   RATCTRY,CTRY                                                     
         MVC   RAT1RACT,SV1RCODE                                                
         MVC   RAT1ROFF,SV1ROFFC                                                
         MVC   RAT1RDPT,SV1RDPT                                                 
         MVC   RAT1RSUB,SV1RSDPT                                                
         MVC   RAT1RPER,SV1RPRSN                                                
         MVC   RATSJCPJ,SVSJCODE                                                
         MVC   RATSJOFF,SVCLIOFF                                                
         CLC   SVCLIOFF,BCSPACES                                                
         BH    *+10                                                             
         MVC   RATSJOFF,SVSJROFF   SJ CLIENT OFFICE                             
         MVC   RATSJTSK,SVTASK                                                  
         MVC   RATCRDTE(L'SVMOA),SVMOA                                          
         MVI   RATCRDTE+L'SVMOA,0                                               
         GOTO1 DATCON,DMCB,(1,RATCRDTE),(0,BCWORK)                              
         GOTO1 ADDAY,DMCB,(C'Y',BCWORK),(X'80',BCWORK+6),0                      
         GOTO1 DATCON,DMCB,(0,BCWORK+6),(1,RATCRDTE)                            
*                                                                               
*&&UK                                                                           
         MVC   RATCSTAT,BCCPYST7   COMPANY STATUS 7                             
         MVC   RATCSTA4,BCCPYST4   COMPANY STATUS 4                             
         MVC   RATCFLG,BCFLAG5     CURRENCY FLAG                                
         MVC   RATCURS,CURCD       CASHVAL CURRENCY CODES                       
*&&                                                                             
         OI    RATSTAT,RATSCOST    SET ONLY WANT COST RATE                      
         MVC   RDATAMGR,DATAMGR                                                 
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RCASHVAL,CASHVAL                                                 
         MVC   RTOBACCO,ATOBACCO                                                
         MVC   RAT1CMPY,CMPY                                                    
         GOTO1 AGETRTE,BLOCK                                                    
         TM    RATSTAT2,RATEFNDC   WAS A COST RATE FOUND                        
         BO    VRATE2B                                                          
         L     R2,AFLD1STH         POINT TO FIRST FIELD ON LINE                 
         B     ERRRQHST                                                         
VRATE2B  CLI   SVTYPE,TIMTCN       N-TIME FOR CLIENT?                           
         BE    VRATEX              MUST CHECK COST RATE EXISTS                  
*&&                                                                             
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    VRATE28                                                          
         CLI   8(R2),C'*'          ONLY REFRESH FOR DEFAULT RATE                
         BE    *+12                                                             
         CLI   8(R2),C'A'          AND DEFAULT ADJUSTED RATE                    
         BNE   VRATE26                                                          
         TM    SVSTAT1,SVSRFRSH    LOOK UP DEFAULT RATE?                        
         BNO   VRATE26                                                          
         TWAXC 0(R2),0(R2)                                                      
         B     VRATE28                                                          
*                                                                               
VRATE26  SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=Y(L'LC5RATEH+1)                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),BCSPACES                                                 
         BH    VRATE40                                                          
*                                                                               
*              NOTHING ENTERED - GET FROM RATE RECORD                           
*                                                                               
         USING RATED,R3                                                         
VRATE28  XC    BLOCK(RATEDQ),BLOCK                                              
         LA    R3,BLOCK                                                         
         MVC   RATCTRY,CTRY                                                     
         MVC   RAT1RACT,SV1RCODE                                                
         MVC   RAT1ROFF,SV1ROFFC                                                
         MVC   RAT1RDPT,SV1RDPT                                                 
         MVC   RAT1RSUB,SV1RSDPT                                                
         MVC   RAT1RPER,SV1RPRSN                                                
         MVC   RATSJCPJ,SVSJCODE                                                
         MVC   RATSJOFF,SVCLIOFF                                                
         CLC   SVCLIOFF,BCSPACES                                                
         BH    *+10                                                             
         MVC   RATSJOFF,SVSJROFF   SJ CLIENT OFFICE                             
         MVC   RATSJTSK,SVTASK                                                  
         MVC   RATCRDTE,PRDENDTE   PERIOD ENDING DATE                           
*&&UK                                                                           
         MVC   RATCSTAT,BCCPYST7   COMPANY STATUS                               
         MVC   RATCSTA4,BCCPYST4   COMPANY STATUS 4                             
         MVC   RATCFLG,BCFLAG5     CURRENCY FLAG                                
         MVC   RATCURS,CURCD       CASHVAL CURRENCY CODES                       
*&&                                                                             
         TM    LINESTAT,LBRATADJ   IS RATE ELIGIBLE FOR ADJUSTMENT              
         BNO   *+8                                                              
         OI    RATSTAT,RATEADJ                                                  
         MVC   RDATAMGR,DATAMGR                                                 
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RCASHVAL,CASHVAL                                                 
         MVC   RTOBACCO,ATOBACCO                                                
         MVC   RAT1CMPY,CMPY                                                    
         GOTO1 AGETRTE,BLOCK                                                    
         TM    RATSTAT2,RATEFND    WAS A RATE FOUND                             
         BO    VRATE30                                                          
         TM    GFACTST6,X'40'      CHECK IF RUNNING UNDER SCRIPT                
         BNO   *+12                FOR TEMPO PUT OUT MISSING RATE MSG           
         CLI   BOTSCR,X'C8'        ARE WE AT TEMPO UPLOAD SCREEN?               
         BE    ERRMSRAT            IF WE ARE PUT OUT MISSING RATE MSG           
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BNZ   ERRMISS             YES, PROMPT FOR RATE                         
         OC    SVERROR,=Y(TRKERATM)     MISSING RATE                            
         B     VRATE80                                                          
*                                                                               
VRATE30  DS    0H                                                               
*&&UK*&& ZAP   SVERATE,RAT2AMNT    EURO RATE FOR CHARGE RATE                    
         TM    RATSTAT2,RATEWADJ   WAS RATE ADJUSTED                            
         BNO   *+8                                                              
         OI    SVRATIND,TIMRBADJ   RATE WAS ADJUSTED                            
         ZAP   SVRATE,RATEAMNT                                                  
         MVC   SVRATEFF,RATEEFFD   RATE EFFECTIVE DATE                          
         MVI   BCFLAG2,X'FF'       DEFAULT RATE IN USE                          
         B     VRATE80                                                          
         DROP  R3                                                               
*                                                                               
*              VALIDATE ENTERED RATE                                            
*                                                                               
VRATE40  DS    0H                  GET RATE FROM SCREEN                         
         MVI   BCIFMAX,L'LC5RATE                                                
         GOTO1 AFVAL,(R2)                                                       
         LA    R6,L'BCIFLD                                                      
         LA    RF,BCIFLD+L'BCIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R6,*-10                                                          
*                                                                               
         LA    R3,8(R2)            R3 = A(INPUT FIELD)                          
         CLI   8(R2),C'A'          ADJUSTMENT RATE                              
         BNE   *+12                                                             
         OI    SVRATIND,TIMRBADJ   RATE WAS ADJUSTED                            
         B     *+12                                                             
         CLI   8(R2),C'*'          DEFAULT RATE                                 
         BNE   VRATE50                                                          
         MVI   BCFLAG2,X'FF'                                                    
         LA    R3,1(R3)            BUMP PAST ASTERISK                           
         BCTR  R6,0                                                             
*                                                                               
VRATE50  DS    0H                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VRATE60                                                          
         GOTO1 CASHVAL,BCDMCB,(X'82',(R3)),(X'01',(R6))                         
         B     VRATE70                                                          
*                                                                               
VRATE60  GOTO1 CASHVAL,BCDMCB,(X'82',(R3)),(R6)                                 
VRATE70  CLI   BCDMCB,X'FF'                                                     
         BE    ERRINV              INVALID RATE                                 
         ZAP   SVRATE(4),BCDMCB+8(4)                                            
         CP    SVRATE(4),BCPZERO                                                
         BL    ERRINV              INVALID RATE                                 
         CP    SVRATE(4),=P'999999'                                             
         BH    ERRINV              INVALID RATE                                 
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV   AGENCY ON DUAL CURRENCY?                     
         BZ    VRATE80                                                          
         ZAP   DUB,SVRATE          CALCULATE RATE IN PRIMARY CURRENCY           
         BZ    VRATE80                                                          
         GOTO1 CASHVAL,DMCB,(X'80',DUB),(X'28',0),CURCD2                        
         LA    RE,SVERATE                                                       
         LA    RF,SVRATE                                                        
         ZAP   0(L'SVRATE,RF),4(8,R1)                                           
         ZAP   0(L'SVRATE,RE),12(8,R1) RATE IN OTHER CURRENCY                   
*&&                                                                             
VRATE80  DS    0H                  CALCULATE AMOUNT                             
         ZAP   DUB,SVRATE                                                       
         MP    DUB,SVHOURS                                                      
         SRP   DUB,64-2,5                                                       
         ZAP   SVAMOUNT,DUB                                                     
*                                                                               
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    VRATE110                                                         
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'*'                                                      
         TM    SVRATIND,TIMRBADJ   RATE WAS ADJUSTED                            
         BNO   *+8                                                              
         MVI   BCWORK,C'A'                                                      
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VRATE90                                                          
         CURED SVRATE,(8,BCWORK+1),2,ALIGN=LEFT,DECPNT=FORCE                    
         B     VRATE100                                                         
*                                                                               
VRATE90  CURED SVRATE,(8,BCWORK+1),2,ALIGN=LEFT                                 
VRATE100 LA    R1,BCWORK                                                        
         CLI   BCFLAG2,X'FF'                                                    
         BE    *+12                                                             
         OI    SVRATIND,TIMRORAT   RATE WAS OVERRIDDEN                          
         LA    R1,1(R1)                                                         
         MVC   8(L'LC5RATE,R2),0(R1)                                            
         MVI   5(R2),L'LC5RATE                                                  
*                                                                               
VRATE110 OC    SVRATEFF,SVRATEFF                                                
         BNZ   *+10                                                             
         MVC   SVRATEFF,BCTODAY3   DEFAULT TO TODAY                             
*                                                                               
VRATEX   DS    0H                                                               
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    *+8                                                              
         OI    4(R2),X'20'         VALIDATED RATE                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE INCOME ACCOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
VALINC   NTR1                                                                   
         L     R2,AFIELDH                                                       
         XC    BCFLAG2,BCFLAG2                                                  
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    VINC20                                                           
         CLI   8(R2),C'*'          DEFAULT INCOME ACCOUNT?                      
         BNE   VINC10                                                           
         TM    SVSTAT1,SVSRFRSH    REFRESH DEFAULT INCOME ACCOUNT?              
         BNO   VINC10                                                           
         TWAXC 0(R2),0(R2)                                                      
         B     VINC20                                                           
*                                                                               
VINC10   SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=Y(L'LC5INC+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),BCSPACES                                                 
         BH    VINC40                                                           
*                                                                               
*              GET DEFAULT INCOME ACCOUNT - NOTHING ENTERED                     
*                                                                               
VINC20   CLI   SVTYPE,TIMTCB       BILLABLE                                     
         BNE   VINCX                                                            
*&&UK                                                                           
         CLI   SVDLK,COOPTNQ       NO LOOKUP REQUESTED                          
         BNE   *+14                                                             
         OC    FIELDLEN,FIELDLEN   FIELD MUST BE ON SCREEN FOR THIS             
         BZ    ERRINC              DLK OPTION - IF IT ISN'T ERROR OUT           
*&&                                                                             
         OC    SVSPINC,SVSPINC     FIRST TRY SPECIAL ACCNT                      
         BZ    *+14                FROM 1R LEDGER                               
         MVC   SVSIACT,SVSPINC                                                  
         B     VINC30                                                           
*                                                                               
         GOTO1 AGETINC,BCDMCB,SVSJACT,SVTASK                                    
         BE    *+24                FOUND INCOME ACCOUNT                         
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BNZ   ERRMISS             YES, PROMPT FOR ACCOUNT                      
         OC    SVERROR,=Y(TRKEINCM)     MISSING INCOME ACCOUNT                  
         B     VINCX                                                            
         MVC   SVSIACT,BCINCOME                                                 
*                                                                               
VINC30   OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    *+14                NO, THEN DON'T DISPLAY                       
         MVI   8(R2),C'*'                                                       
         MVC   9(L'SVSIULA,R2),SVSIULA                                          
         MVI   BCFLAG2,X'FF'       DEFAULT INCOME ACCOUNT IN USE                
         B     VINC50                                                           
*                                                                               
*              VALIDATE ENTERED INCOME ACCOUNT                                  
*                                                                               
VINC40   CLI   SVTYPE,TIMTCB                                                    
         BNE   ERRINV              ONLY VALID FOR BILLABLE TIME                 
*&&UK                                                                           
         CLI   SVDLK,COOPTOQ       ACCT LOOKUP ONLY - NOT ENTERED               
         BE    ERROVR                                                           
*&&                                                                             
         LA    R3,8(R2)                                                         
         CLI   0(R3),C'*'                                                       
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         MVI   BCFLAG2,X'FF'                                                    
         MVC   SVSICPY,CMPY                                                     
         MVC   SVSIULA,0(R3)                                                    
         OC    SVSIULA,BCSPACES                                                 
*                                                                               
VINC50   DS    0H                  VALIDATE INCOME ACCOUNT                      
*&&UK                                                                           
         OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    *+12                NO, SKIP COMPARE                             
         CLI   8(R2),C'*'                                                       
         BNE   VINC60                                                           
         CLI   SVBTL,COSUSP        SUSPENSE ACCOUNT OVERRIDE?                   
         BNE   *+10                                                             
         MVC   SVSIUL,=C'SK'       OVERRIDE WITH SUSPENSE ACCOUNT               
*                                                                               
         CLI   SVBTL,COINCOME      INCOME ACCOUNT OVERRIDE?                     
         BNE   *+10                                                             
         MVC   SVSIUL,=C'SI'       OVERRIDE WITH INCOME ACCOUNT                 
*&&                                                                             
VINC60   XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,SVSIACT                                                 
         GOTO1 AGETACT,0                                                        
         BNE   VINC70              INVALID INCOME ACCOUNT                       
*&&UK                                                                           
         CLI   CTRY,CTRYGER        GERMANY CANT POST TO SI - ONLY SK            
         BNE   *+14                                                             
         CLC   SVSIUL,=C'SK'                                                    
         BNE   VINC70                                                           
*&&                                                                             
         TM    BCASTAT1,RSTSACIC+RSTSACIL                                       
         BNZ   VINC70                                                           
         TM    BCINDS,BCFLABAL                                                  
         BO    VINC80                                                           
*                                                                               
VINC70   OC    FIELDLEN,FIELDLEN                                                
         BNZ   EINVACC                                                          
         OC    SVERROR,=Y(TRKEINCI)   INVALID INCOME ACCOUNT                    
         B     VINCX                                                            
*                                                                               
VINC80   OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    *+8                                                              
         OI    6(R2),X'80'                                                      
         CLI   BCFLAG2,X'FF'                                                    
         BE    *+8                                                              
         OI    SVRATIND,TIMROINC   INCOME ACCOUNT OVERRIDDEN                    
*                                                                               
         CLC   SVSIUL,=C'SK'       NO ANALYSIS ACCT FOR INCOME SUSPENSE         
         BE    VINCX                                                            
         CLC   BCACOST,BCSPACES                                                 
         BH    *+14                                                             
         CLC   BCSPANAL,BCSPACES                                                
         BNH   ESIANLM             MISSING ANALYSIS ACCOUNT                     
*                                                                               
         XC    BIGKEY,BIGKEY       *** VALIDATE 12 ACCOUNT ***                  
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'12'                                                
         MVC   ACTKACT(1),BCACOST                                               
         CLC   BCSPANAL,BCSPACES                                                
         BNH   *+10                                                             
         MVC   ACTKCULA,BCSPANAL                                                
         GOTO1 AGETACT,0                                                        
         BNE   VINC90              INVALID ANALYSIS ACCOUNT                     
         TM    BCASTAT1,RSTSACIC+RSTSACIL                                       
         BNZ   VINC90                                                           
         TM    BCINDS,BCFLABAL     ACCOUNT VALID FOR POSTING                    
         BO    VINCX                                                            
*                                                                               
VINC90   OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BNZ   ESIANLI                                                          
         OC    SVERROR,=Y(TRKEANLI)   MARK TSAR INVALID ANALYSIS ACCT           
*                                                                               
VINCX    OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    *+8                 NO                                           
         OI    4(R2),X'20'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE NARRATIVE                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALNAR   NTR1                                                                   
         L     R2,AFIELDH                                                       
         OC    FIELDLEN,FIELDLEN   IS FIELD DISPLAYED ON THIS SCREEN            
         BZ    EXIT                                                             
         TM    1(R2),X'20'         SKIP IF NARRATIVE PROTECTED                  
         BO    VNARX                                                            
         CLI   5(R2),0             WAS NARRATIVE INPUT                          
         BE    VNARX                                                            
         MVI   SVNARRLN,0                                                       
         MVC   SVNARR,BCSPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         STC   R1,SVNARRLN                                                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   SVNARR(0),8(R2)                                                  
         EX    R1,*-6                                                           
         OC    FLDINPS,=AL2(FLDNARR)  NARRATIVE WAS INPUT                       
*                                                                               
VNARX    OI    4(R2),X'20'         VALIDATED BIT                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
VALTAXF  NTR1                                                                   
         L     R2,AFIELDH                                                       
*                                                                               
*        CLI   TWAOFFC,C'*'        ONLY TAX FOR DDS TERMINALS                   
*        BE    VTAXF2              FOR NOW                                      
         OC    FIELDLEN,FIELDLEN                                                
         BZ    *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD                                
         MVI   SVTAXSTA,SVTAXNO               NO TAX                            
         B     VTAXFX                                                           
*                                                                               
VTAXF2   OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    VTAXFX                                                           
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    SVSTAT1,SVTXRFSH                                                 
         CLI   8(R2),C'*'          DEFAULT TAX STATUS DIPLAYED?                 
         BNE   VTAXF5                                                           
         TM    SVSTAT1,SVTXRFSH    NEED TO REFRESH DEFAULT TAX STATUS?          
         BNO   VTAXF5                                                           
         TWAXC 0(R2),0(R2)                                                      
         B     VTAXF10                                                          
*                                                                               
VTAXF5   SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=Y(L'LC6TAXFH+1)                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),BCSPACES                                                 
         BH    VTAXF30                                                          
*                                                                               
*              NOTHING ENTERED - DISPLAY DEFAULT                                
*                                                                               
VTAXF10  CLI   SVTYPE,TIMTCB       ONLY B-TIME ITEMS CAN HAVE TAX               
         BNE   VTAXFX                                                           
         TM    SVTAXSTA,SVTAXREQ   TAX REQUIRED/ELIGIBLE                        
         BNO   *+10                                                             
         MVC   8(L'LC6TAXF,R2),=C'*Y'                                           
         B     VTAXFX                                                           
*                                                                               
*              VALIDATE ENTERED DATA                                            
*                                                                               
VTAXF30  CLI   SVTYPE,TIMTCB       ONLY B-TIME ITEMS CAN HAVE TAX               
         BNE   ERRINV                                                           
         CLC   8(L'LC6TAXF,R2),=C'*Y'                                           
         BNE   *+12                                                             
         MVI   SVTAXSTA,SVTAXREQ              TAX REQUIRED                      
         B     VTAXFX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   SVTAXSTA,SVTAXREQ+SVTAXOVR     REQUIRED & OVERRIDDEN             
         B     VTAXFX                                                           
         CLI   8(R2),C'N'                                                       
         BNE   ERRINV                                                           
         MVI   SVTAXSTA,SVTAXNO               NO TAX                            
         B     VTAXFX                                                           
*                                                                               
VTAXFX   OC    FIELDLEN,FIELDLEN                                                
         BZ    *+12                                                             
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    4(R2),X'20'         VALIDATED BIT                                
         B     ROUTE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX ITEM                                                   *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
VALTAX   NTR1                                                                   
         L     R2,AFIELDH                                                       
         TM    SVTAXSTA,SVTAXNO    NO TAX FOR THIS ITEM                         
         BO    VTAXX                                                            
         TM    SVTAXSTA,SVTAXREQ   TAX REQUIRED/ELIGIBLE                        
         BNO   VTAXX                                                            
         CLI   SVTYPE,TIMTCB       TAX INFORMATION ONLY FOR B TIME              
         BNE   VTAXX                                                            
         OC    FIELDLEN,FIELDLEN                                                
         BZ    *+10                                                             
         OC    FLDREQS,=Y(FLDBASIS+FLDLOCAL+FLDTAXWC)                           
*                                                                               
*              GET DEFAULT TAX INFORMATION                                      
*                                                                               
         GOTO1 AGETTAX,BCDMCB,SVSJACT,SVTASK                                    
         MVC   SVDTLOC,BCTAXLOC    DEFAULT LOCALITY                             
         MVC   SVDTWC,BCTAXWC      DEFAULT TAX WORKCODE                         
         ZAP   SVTXBAS,SVAMOUNT    TAXABLE AMOUNT                               
         MVC   SVTXLOC,SVDTLOC     DEFAULT LOCALITY                             
         MVC   SVTXWC,SVDTWC       DEFAULT WORKCODE                             
         OC    FIELDLEN,FIELDLEN                                                
         BNZ   VTAX10                                                           
         CLC   SVTXLOC,BCSPACES                                                 
         BNH   *+14                                                             
         CLC   SVTXWC,BCSPACES                                                  
         BH    *+14                                                             
         OC    SVERROR,=Y(TRKETAX)                                              
         B     VTAX120                                                          
         GOTO1 AVALLOC,DMCB,SVTXLOC,PRDENDTE,SVTXBAS                            
         BE    *+14                INVALID LOCALITY                             
         OC    SVERROR,=Y(TRKETAX)                                              
         B     VTAX120                                                          
         USING TXLOCALD,R3                                                      
         LA    R3,BCELEM                                                        
         SR    R1,R1                                                            
         IC    R1,TXMINI           # MINI ENTRIES                               
         MH    R1,=Y(TXLLNQ)                                                    
         MVC   SVTXMINI(0),TXMINI  COPY ENTIRE TAX BLOCK                        
         EX    R1,*-6                                                           
         GOTO1 AVALTXWC,SVTXWC     VALIDATE TAX WORKCODE                        
         BNE   ACCERRX             INVALID TAX WORKCODE                         
         B     VTAX120                                                          
*                                                                               
*              VALIDATE TAX BASIS                                               
*                                                                               
VTAX10   L     R2,AFIELDH                                                       
         ST    R2,ATAXBAS          A(TAX BASIS FIELD)                           
         CLI   SVTYPE,TIMTCB                                                    
         BNE   ERRINV              ONLY VALID FOR BILLABLE TIME                 
*                                                                               
         TM    SVTAXSTA,SVTAXOVR   OVERRIDDEN                                   
         BO    VTAX20                                                           
         TM    SVSTAT1,SVTXRFSH    NEED TO REFRESH DEFAULT TAX STATUS?          
         BNO   VTAX20                                                           
         TWAXC 0(R2),0(R2)                                                      
         MVI   5(R2),0                                                          
*                                                                               
VTAX20   CLI   5(R2),0                                                          
         BNE   VTAX40                                                           
         MVC   BCWORK,BCSPACES                                                  
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VTAX30                                                           
         CURED SVAMOUNT,(L'LC6BAS,BCWORK),2,FLOAT=-,DECPNT=FORCE                
         B     VTAX31                                                           
*                                                                               
VTAX30   CURED SVAMOUNT,(L'LC6BAS,BCWORK),2,FLOAT=-                             
VTAX31   CLC   BCWORK,BCSPACES                                                  
         BE    VTAX40                                                           
         MVC   8(L'LC6BAS,R2),BCWORK                                            
         MVI   5(R2),L'LC6BAS                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
VTAX40   MVI   BCIFMIN,0           ASSUME NOT REQUIRED                          
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDBASIS)                                              
         BZ    *+8                                                              
         MVI   BCIFMIN,1                                                        
         MVI   BCIFMAX,L'LC6BAS                                                 
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX             ERROR - INVALID BASIS AMOUNT                 
         BL    VTAX60              NOTHING ENTERED/NOTHING REQ'D                
*                                                                               
         LA    R0,L'BCIFLD                                                      
         LA    RF,BCIFLD+L'BCIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VTAX50                                                           
         GOTO1 CASHVAL,BCDMCB,(X'82',8(R2)),(X'01',(R0))                        
         B     VTAX51                                                           
*                                                                               
VTAX50   GOTO1 CASHVAL,BCDMCB,(X'82',8(R2)),(R0)                                
VTAX51   CLI   BCDMCB,X'FF'                                                     
         BE    ERRINV              INVALID AMOUNT                               
         ZAP   SVTXBAS,BCDMCB+8(4) BASIS AMOUNT                                 
         CP    SVTXBAS,BCPZERO                                                  
         BL    ERRINV              INVALID AMOUNT                               
         CP    SVTXBAS,=P'999999'                                               
         BH    ERRINV              INVALID AMOUNT                               
*                                                                               
*              VALIDATE TAX LOCALITY FIELD                                      
*                                                                               
VTAX60   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         ST    R2,ATAXLOC          STORE A(LOCALITY FIELD)                      
*                                                                               
         TM    SVTAXSTA,SVTAXOVR   OVERRIDDEN                                   
         BO    VTAX70                                                           
         TM    SVSTAT1,SVTXRFSH    NEED TO REFRESH DEFAULT TAX STATUS?          
         BNO   VTAX70                                                           
         TWAXC 0(R2),0(R2)                                                      
         MVI   5(R2),0                                                          
*                                                                               
VTAX70   CLI   5(R2),0                                                          
         BNE   VTAX80                                                           
         CLC   SVDTLOC,BCSPACES                                                 
         BNH   VTAX80                                                           
         MVC   8(L'LC6LOC,R2),SVDTLOC                                           
         MVI   5(R2),L'LC6LOC                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
VTAX80   MVI   BCIFMIN,0           ASSUME LOCALITY NOT REQUIRED                 
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDLOCAL)                                              
         BZ    *+8                                                              
         MVI   BCIFMIN,1                                                        
         MVI   BCIFMAX,L'LC6LOC                                                 
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX             ERROR - INVALID LOCALITY                     
         BL    VTAX90              NOTHING ENTERED/NOTHING REQ'D                
         MVC   SVTXLOC,BCIFLD                                                   
         OC    SVTXLOC,BCSPACES                                                 
         GOTO1 AVALLOC,DMCB,SVTXLOC,PRDENDTE,SVTXBAS                            
         BNE   ERRINV              INVALID LOCALITY                             
         USING TXLOCALD,R3                                                      
         LA    R3,BCELEM                                                        
         SR    R1,R1                                                            
         IC    R1,TXMINI           # MINI ENTRIES                               
         MH    R1,=Y(TXLLNQ)                                                    
         MVC   SVTXMINI(0),TXMINI  COPY ENTIRE TAX BLOCK                        
         EX    R1,*-6                                                           
*                                                                               
*              VALIDATE WORK CODE                                               
*                                                                               
VTAX90   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         ST    R2,ATAXWC           STORE A(TAX WORKCODE)                        
*                                                                               
         TM    SVTAXSTA,SVTAXOVR   OVERRIDDEN                                   
         BO    VTAX100                                                          
         TM    SVSTAT1,SVTXRFSH    NEED TO REFRESH DEFAULT TAX STATUS?          
         BNO   VTAX100                                                          
         TWAXC 0(R2),0(R2)                                                      
         MVI   5(R2),0                                                          
*                                                                               
VTAX100  CLI   5(R2),0                                                          
         BNE   VTAX110                                                          
         CLC   SVDTWC,BCSPACES                                                  
         BNH   VTAX110                                                          
         MVC   8(L'LC6WRK,R2),SVDTWC                                            
         MVI   5(R2),L'LC6WRK                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
VTAX110  MVI   BCIFMIN,0           ASSUME WORK CODE NOT REQUIRED                
         MVC   BCHALF,FLDREQS                                                   
         NC    BCHALF,=Y(FLDTAXWC)                                              
         BZ    *+8                                                              
         MVI   BCIFMIN,1                                                        
         MVI   BCIFMAX,L'LC6WRK                                                 
         GOTO1 AFVAL,(R2)                                                       
         BH    ACCERRX             ERROR - INVALID LOCALITY                     
         BL    VTAX120             NOTHING ENTERED/NOTHING REQ'D                
         MVC   SVTXWC,BCIFLD                                                    
         OC    SVTXWC,BCSPACES                                                  
         GOTO1 AVALTXWC,SVTXWC     VALIDATE TAX WORKCODE                        
         BNE   ACCERRX             INVALID TAX WORKCODE                         
*                                                                               
VTAX120  OC    FIELDLEN,FIELDLEN   IS FIELD ON SCREEN                           
         BZ    VTAXX                                                            
         L     R2,ATAXBAS                                                       
         OI    4(R2),X'20'                                                      
         L     R2,ATAXLOC                                                       
         OI    4(R2),X'20'                                                      
         L     R2,ATAXWC                                                        
         OI    4(R2),X'20'                                                      
VTAXX    DS    0H                                                               
         B     ROUTE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TEMPO LINE #                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTMP#  NTR1                      GET # FROM SCREEN                            
         L     R2,AFIELDH                                                       
         XC    SVTMPLN#,SVTMPLN#                                                
         MVI   BCIFMIN,0                                                        
         TM    GFACTST6,X'40'      TEMPO UPLOAD # REQUIRED IF RUNNING           
         BNO   *+16                UNDER A SCRIPT                               
         CLI   BOTSCR,X'C8'        MAKE SURE WE ARE AT UPLOAD SCREEN            
         BNE   *+8                 DON'T SHOW CONFIRM FIELD                     
         MVI   BCIFMIN,1                                                        
         MVI   BCIFMAX,L'LC8TLN#                                                
         GOTO1 AFVAL,(R2)                                                       
         BH    ERRINV              ERROR - INPUT REQUIRED                       
         BL    VALTMPX             NOTHING ENTERED/NOTHING REQ'D                
         TM    BCIFLDH+4,X'08'     NUMERIC INPUT                                
         BZ    ERRINV                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,BCDMCB,(0,BCIFLDH),(0,BLOCK)                             
         CLI   BCDMCB+4,1                                                       
         BNE   ERRINV                                                           
         ICM   R1,15,BLOCK+4                                                    
         C     R1,=F'65000'                                                     
         BH    ERRINV                                                           
         STCM  R1,3,SVTMPLN#       SAVE TEMPO INTERNAL KEY NUMBER               
         CLC   SVLSTTMP,SVTMPLN#   IS IT THE SAME NUMBER AS PREVIOUS?           
         BE    EDUPTLN#                                                         
         MVC   SVLSTTMP,SVTMPLN#   SAVE OFF TEMPO LINE # FOR NEXT CHK           
         MVC   BCWORK,BCSPACES                                                  
         LR    R3,R1                                                            
         CURED (R3),(8,BCWORK),0,ALIGN=LEFT                                     
         MVC   8(L'LC8TLN#,R2),BCWORK                                           
         MVI   5(R2),L'LC8TLN#                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
VALTMPX  OI    4(R2),X'20'         VALIDATED                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD SAVE BLOCK                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALSAVE  NTR1                                                                   
         USING DISTABD,R3                                                       
         L     R3,AVDTAB           A(VALIDATION/DISPLAY TABLE)                  
         USING TIMELD,R6                                                        
         LA    R6,BCELEM                                                        
         XC    BCELEM,BCELEM                                                    
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMFITEM-TIMELD                                            
         MVI   TIMETYP,TIMEFLD                                                  
*                                                                               
VSAVE100 CLI   DTFDISP,X'FF'       END OF TABLE                                 
         BE    VSAVEX                                                           
         LH    R1,DTFDISP                                                       
         AR    R1,R2               R2=A(START OF LINE)                          
         ST    R1,AFIELDH          A(FIELD HEADER)                              
         OC    DTFLEN,DTFLEN       FIELD ON SCREEN                              
         BZ    VSAVE200                                                         
         SR    RF,RF                                                            
         ICM   RF,3,DTFLEN                                                      
         BZ    VSAVE200                                                         
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R1),BCSPACES                                                 
         BNH   VSAVE200                                                         
         MVC   TIMFNUM,DTINPFLD    FIELD #                                      
*                                                                               
         LA    RE,8(R1)            RE=START OF FIELD                            
         AR    RE,RF               POINT TO LAST CHAR IN FIELD                  
         CLI   0(RE),C' '          SKIP NON SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         MVC   TIMFIELD(0),8(R1)                                                
         EX    RF,*-6                                                           
         LA    RF,1(RF)            ADJUST LENGTH FROM EX MVC                    
         LA    RF,TIMFITMQ(RF)     BUMP LENGTH OF MINI HEADER                   
         STC   RF,TIMFLEN          FIELD LENGTH                                 
         AR    R6,RF               BUMP LENGTH OF MINI DATA                     
         SR    RE,RE                                                            
         IC    RE,BCELEM+(TIMFMINI-TIMELD)   BUMP MINI ELEM COUNT               
         LA    RE,1(RE)                                                         
         STC   RE,BCELEM+(TIMFMINI-TIMELD)                                      
         IC    RE,BCELEM+(TIMLN-TIMELD)      INCREMENT LENGTH                   
         AR    RE,RF                                                            
         STC   RE,BCELEM+(TIMLN-TIMELD)                                         
*                                                                               
VSAVE200 LA    R3,DTLENQ(R3)                                                    
         B     VSAVE100                                                         
*                                                                               
VSAVEX   B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR LINE NUMBERS -  USUALLY WHEN REBUILDING TSAR RECS             *         
***********************************************************************         
         SPACE 1                                                                
CLRNUMBS NTR1                                                                   
         XC    DISLINES,DISLINES   CLEAR DISPLAYED LINES                        
         MVC   SCXLINE,=H'1'       START WITH LINE# = 1                         
         XC    SCXSUB,SCXSUB               SUBLINE# = 0                         
*                                                                               
         MVC   SCXSTLIN,=H'1'                                                   
         XC    SCXSTSUB,SCXSTSUB                                                
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+10                                                             
         MVC   SCXNEXT,=H'1'                                                    
         MVI   STATUS,0                                                         
         NI    STATUS2,X'FF'-(TRNCMPAR+ONLNUPDT+TRNBUILD)                       
         B     EXIT                                                             
         SPACE 3                                                                
***********************************************************************         
* PF TO CLEAR SCREEN AND RESET EVERYTHING ON ADD- ERASE CHANGES       *         
***********************************************************************         
         SPACE 1                                                                
CLRCHNGS NTR1                                                                   
         L     R2,ALINE1           CLEAR BOTTOM OF SCREEN                       
         MVI   5(R2),0             TO ENSURE A PROMT FOR INPUT                  
         L     R3,ALSTFLD                                                       
         GOTO1 AXFIELD,BCDMCB,(R2),(R3)                                         
*                                                                               
         MVC   BCACCODE,SV1RACT    BUILD TSAR RECORDS FROM ACTUALS              
         GOTO1 AGETTSN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVTSNUM,BCTSNUM     TIME SHEET #                                 
         MVC   SVRVSN#,BCTSNUM     REVISION #                                   
*                                                                               
         MVC   BCPERIOD,PRDNUM     PERIOD NUMBER                                
         MVC   BCYYMMDD,PRDENDTE   PERIOD END DATE                              
         MVC   BCINPDTE,SVYYMMDD                                                
         MVI   SORTSTAT,SORTTSN                                                 
         CLI   BCPROF1,C'Y'                                                     
         BE    *+12                                                             
         CLI   SVOPT1,C'Y'                                                      
         BNE   *+8                                                              
         MVI   SORTSTAT,SORTSJ                                                  
         GOTO1 ABLDTSAR,BCDMCB,SVOPT1,SVOMOA,1                                  
         BE    *+8                                                              
         OI    STATUS,NOREC                                                     
         MVC   SVFLAG5,BCFLAG5     SAVED FOR NEXT TIME THRU                     
         MVC   SCXNEXT,BCLINE#     NEXT LINE NUMBER TO USE ON ADD               
         OI    TSARSTAT,TSARSAVE   SAVE TSAR BUFF ON EXIT                       
*                                                                               
*&&US*&& GOTO1 ADRAFT                                                           
         TM    STATUS2,MCSTIME     TEST PERSON ON MCS TIME                      
         BZ    EXIT                NO - NO NEED TO CHECK STATUS/TDN             
         CLI   ACTEQU,ACTCHA       CHECK IF THEY ARE ALLOWED TO CHNG            
         BE    CLRCH10                                                          
         CLI   ACTEQU,ACTSEL       CHECK IF 'SELECT' FROM LIST                  
         BNE   EXIT                                                             
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BNE   EXIT                NO - JUST SELECT                             
CLRCH10  DS    0H                                                               
*&&US*&& TM    SVFLAG5,BCFLUAT     TEST UNAPPROVED TIMESHEET                    
*&&US*&& BO    ERRUAT              YES - CHANGE NOT ALLOWED                     
         TM    SVFLAG5,BCFLTDN     TEST TIME DAY NARRATIVE EXIST                
         BO    ERRTDN              YES - CHANGE NOT ALLOWED                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS WORKED - REQUIRED FOR ADD                            *         
***********************************************************************         
         SPACE 1                                                                
HRSWRKD  NTR1                                                                   
         ZAP   HRSWRK,BCPZERO      HOURS WORKED                                 
         MVC   ADDHINP,BCSPACES    CLEAR HOURS INPUT                            
         OI    ADDHINPH+6,X'80'                                                 
         MVC   ADDHDIF,BCSPACES    CLEAR DIFFERENCE                             
         OI    ADDHDIFH+6,X'80'                                                 
*                                                                               
         LA    R2,ADDHWRKH                                                      
         CLI   5(R2),0             ANY INPUT                                    
         BE    ERRMISS                                                          
         GOTO1 AVALHRS,ADDHWRKH                                                 
         BNE   ACCERRX             INVALID HOURS                                
*                                                                               
         ZAP   HRSWRK,BCHOURS                                                   
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   HRSW10                                                           
         CURED (P4,HRSWRK),(7,ADDHWRK),2,ZERO=NOBLANK,FLOAT=-,DECPNT=FOX        
               RCE                                                              
         B     HRSW20                                                           
*                                                                               
HRSW10   CURED (P4,HRSWRK),(7,ADDHWRK),2,ZERO=NOBLANK,FLOAT=-                   
HRSW20   OI    ADDHWRKH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONFIRMATION ON UPDATE                                              *         
***********************************************************************         
         SPACE 1                                                                
CONFIRM  NTR1                                                                   
         LA    R2,ADDCUPDH         CONFIRMATION FIELD - ADD SCREEN              
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPDH         CONFIRMATION FIELD - CHANGE SCREEN           
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   PFKEY,PFUPDATE      ONLY CHECK IF CONFIRM FIELD BLANK            
         BNE   ROUTE                                                            
         TM    SVCNFRM,SVCNFRMY    USING CONFIRMATION ON UPDATE                 
         BNO   ROUTE                                                            
*                                                                               
CONF10   LA    R2,ADDCUPNH         CONFIRMATION NAME - ADD SCREEN               
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPNH         CONFIRMATION NAME - CHANGE SCREEN            
*                                                                               
         NI    1(R2),X'FF'-X'0C'   NORMAL INTENSITY                             
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ADDCUPDH         CONFIRMATION FIELD - ADD SCREEN              
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPDH         CONFIRMATION FIELD - CHANGE SCREEN           
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    ERRY2UPD                                                         
         MVI   PFKEY,0                                                          
         CLI   8(R2),COOPTYQ                                                    
         BE    *+12                                                             
         BAS   RE,CLRCNFM                                                       
         B     ROUTH                                                            
*                                                                               
         MVI   PFKEY,PFUPDATE                                                   
         BAS   RE,CLRCNFM                                                       
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF KEY DATA TO CHECK FOR DUPLICATES                     *         
***********************************************************************         
*        SPACE 1                                                                
*UPTABLE NTR1                                                                   
*        L     R6,AIO                                                           
*        USING TSARRECD,R6                                                      
*        XC    DUPNUM,DUPNUM                                                    
*                                                                               
*        L     RE,ADISPBLK         CLEAR TABLE                                  
*        LH    RF,=H'5000'                                                      
*        SR    R0,R0                                                            
*        SR    R1,R1                                                            
*        MVCL  RE,R0                                                            
*                                                                               
*        GOTO1 AXAIO,0                                                          
*        LA    R3,TSARDH           BUILD TABLE FROM TSAR RECORDS                
*UP10    GOTO1 ATSAR,(R3)                                                       
*        TM    BCTSERRS,TSEEOF                                                  
*        BO    DUPX                                                             
*        LA    R3,TSANXT                                                        
*                                                                               
*        BAS   RE,BUILDUP          BUILD TABLE ENTRY IN WORK FROM TSAR          
*        BH    DUP10                                                            
*        LH    R4,DUPNUM                                                        
*        GOTO1 ABINSRCH,BCDMCB,(1,WORK),ADISPBLK,(R4),DUPDLEN,        X         
*              (3,L'DKEY),300                                                   
*        MVC   DUPNUM,BCDMCB+10                                                 
*        B     DUP10                                                            
*                                                                               
*UPX     B     EXIT                                                             
*        DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILDS BINSEARCH KEY FOR DUPLICATE TABLE                            *         
***********************************************************************         
*        SPACE 1                                                                
*UILDUP  NTR1                                                                   
*        USING TSARRECD,R6                                                      
*        L     R6,AIO                                                           
*                                                                               
*        USING DUPD,R2                                                          
*        LA    R2,WORK                                                          
*        XC    WORK,WORK                                                        
*        MVC   DLINENUM,TRKLINE                                                 
*        MVC   DLINESUB,TRKLNSUB                                                
*                                                                               
*        USING TIMELD,R3                                                        
*        LA    R3,TRDATA                                                        
*        CLI   TIMEL,TIMELQ        X'8B' ELEM                                   
*        BNE   ROUTH                                                            
*        CLI   TIMETYP,TIMEINP     INPUT DETAILS                                
*        BNE   ROUTH                                                            
*        MVC   DKINFO,TIMINP                                                    
*        CLI   TIMLN,TIMILN2Q      ANY BILLABLE DETAILS                         
*        BL    *+10                                                             
*        MVC   DKBINFO,TIMBID                                                   
*        B     ROUTE                                                            
*        DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ADD NEW ENTRY TO TABLE OR FLAG AS DUPLICATE                         *         
***********************************************************************         
*        SPACE 1                                                                
*HECKDUP NTR1                                                                   
*        BAS   RE,BUILDUP          BUILD ENTRY IN WORK FROM TSAR REC            
*        BH    ROUTH                                                            
*        LH    R4,DUPNUM                                                        
*        GOTO1 ABINSRCH,BCDMCB,(1,WORK),ADISPBLK,(R4),DUPDLEN,        X         
*              (3,L'DKEY),300                                                   
*        MVC   DUPNUM,BCDMCB+10                                                 
*        CLI   BCDMCB,X'01'                                                     
*        BE    ROUTE                                                            
*        L     R1,BCDMCB                                                        
*        CLC   0(3,R1),WORK        SAME LINE NUMBER IS OK                       
*        BE    ROUTE                                                            
*        B     ROUTH               RECORD FOUND = DUPLICATE                     
         EJECT                                                                  
***********************************************************************         
* DELETE ENTRY FROM TABLE - USER DELETED ITEM                         *         
***********************************************************************         
*        SPACE 1                                                                
*ELDUP   NTR1                                                                   
*        BAS   RE,BUILDUP          BUILD ENTRY IN WORK FROM TSAR REC            
*        BH    ROUTH                                                            
*        LH    R4,DUPNUM                                                        
*        GOTO1 ABINSRCH,BCDMCB,(X'80',WORK),ADISPBLK,(R4),DUPDLEN,    X         
*              (3,L'DKEY),300                                                   
*        MVC   DUPNUM,BCDMCB+10                                                 
*        B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SCREEN ADDRESSES                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRADDRS NTR1                                                                   
         XC    ALINE1,ALINE1       A(FIRST LINE)                                
         XC    ALSTLINE,ALSTLINE   A(LAST LINE)                                 
         XC    ALSTFLD,ALSTFLD     A(LAST FIELD)                                
         CLI   BOTSCR,0            FIRST TIME IN=NO BOTTOM SCREEN YET           
         BE    SCRADDX                                                          
         LH    R1,DBOTSCR          DISP TO BEGINNING OF BOTTOM SCREEN           
         AR    R1,RA               ADD TO ATWA (RA)                             
*                                                                               
         USING SCRNTABD,R3                                                      
         L     R3,ASCRNTAB         GET SOME SCREEN INFO                         
SCRADD10 CLC   BOTSCR,SCRNNUM      MATCH ON SCREEN                              
         BNE   SCRADD20                                                         
         MVC   SCRLINES,SCRNLINE   NUMBER OF LINES ON SCREEN                    
         LR    R2,R1               R1=A(BOTTOM SCREEN START)                    
         AH    R2,SCRNLIN1         DISP TO LINE 1                               
         ST    R2,ALINE1                                                        
         LR    R2,R1                                                            
         AH    R2,SCRNLSTL         DISP TO LAST LINE                            
         ST    R2,ALSTLINE                                                      
         LR    R2,R1                                                            
         AH    R2,SCRNLSTF         DISP TO LAST FIELD                           
         ST    R2,ALSTFLD                                                       
         B     SCRADDX                                                          
*                                                                               
SCRADD20 LA    R3,SCRNTLNQ(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   SCRADD10                                                         
*                                                                               
SCRADDX  DS    0H                                                               
*&&UK*&& GOTO1 AXMIT,BCDMCB,CONHEADH,0       TRANSMIT ENTIRE SCREEN             
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR CONFIRMATION FIELDS                                           *         
***********************************************************************         
         SPACE 1                                                                
CLRCNFM  NTR1                                                                   
         LA    R2,ADDCUPNH         CONFIRM NAME HEADER - ADD SCREEN             
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPNH         CONFIRM NAME HEADER - CHANGE SCREEN          
         OI    1(R2),X'0C'         LOW INTENSITY                                
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ADDCUPDH         CONFIRM FIELD - ADD SCREEN                   
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         LA    R2,CHACUPDH         CONFIRM FIELD - CHANGE SCREEN                
         MVI   5(R2),0             ZERO LENGTH                                  
         MVI   8(R2),C' '          CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
EXIT     XIT1                                                                   
*                                                                               
ERRUPDTE LHI   RF,ACEUPDTE         MUST UPDATE                                  
         L     R2,ALINE1                                                        
         B     ACCERRM                                                          
*&&US                                                                           
ERRRQPRD LHI   RF,ACERQPRD         OPT/MAINT REQUIRES PRODUCT                   
         B     ACCERRM                                                          
ERRRQJOB LHI   RF,ACERQJOB         OPT/MAINT REQUIRES JOB                       
         B     ACCERRM                                                          
ENATMS   LHI   RF,ACENATMS                                                      
         B     ACCERRM                                                          
EBXJOB   LHI   RF,ACEBXJOB         CANNOT ADD B TIME TO XJOB                    
         B     ACCERRM                                                          
ERRCNCH  LHI   RF,ACCANTCH         CANT CHANGE KEY FIELDS                       
         B     ACCERRM                                                          
ERRUS49  LHI   RF,ACEUSINP         USE INPUT PROGRAM TYPE 49                    
         B     ACCERRM                                                          
ECSTCK   LHI   RF,ACECSTCK         HRS ON COSTING ACCOUNT                       
         L     R2,ALINE1                                                        
         MVI   GLTXT,15                                                         
         LA    R1,BCWORK                                                        
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         B     ACCERRM                                                          
*&&                                                                             
ERRRQHST LHI   RF,ACENTUPD         REQUIRES HISTORY RECORD                      
         B     ACCERRM                                                          
ERRNOADD LHI   RF,ACERECEX         RECORD ALREADY EXISTS                        
         B     ACCERRM                                                          
ERRRECNF LHI   RF,ACERECNF         RECORD NOT FOUND                             
         B     ACCERRM                                                          
ERRONDTM LHI   RF,ACEONDTM         RECORD NOT FOUND - ON DT                     
         B     ACCERRM                                                          
ERRMISS  LHI   RF,ACEMISS          MISSING INPUT FIELD                          
         B     ACCERRM                                                          
ERRMSRAT LHI   RF,ACEMSRAT         MISSING SALES/BILLING RATE                   
         B     ACCERRM                                                          
ERRUAT   LHI   RF,ACEUAT           CANT CHANGE UNAPP TIMESHEET                  
         B     ACCERRM                                                          
ERRTDN   LHI   RF,ACETDN           CANT CHANGE TS WITH DAY NARRATIVE            
         B     ACCERRM                                                          
ERRINV   LHI   RF,ACEINV           INVALID INPUT FIELD                          
         B     ACCERRM                                                          
ERRINVHR LHI   RF,ACEIVHRS         INVALID HOURS                                
         B     ACCERRM                                                          
ERROVR   LHI   RF,ACEOVRRD         NO OVERRIDE ALLOWED                          
         B     ACCERRM                                                          
ERRINC   LHI   RF,ACEINCAC         INCOME ACCT MUST BE ENTERED                  
         B     ACCERRM                                                          
ERRNOMAT LHI   RF,ACENOOPT         ENTRY DOES NOT MATCH OPTION                  
         B     ACCERRM                                                          
EACCCLS  LHI   RF,ACEACTCL         ACCOUNT IS CLOSED                            
         B     ACCERRM                                                          
EACCLOCK LHI   RF,ACEACTLK         ACCOUNT IS LOCKED                            
         B     ACCERRM                                                          
EDUPTLN# LHI   RF,ACEDUPLN         DUPLICATE TEMPO LINE                         
         B     ACCERRM                                                          
EACCPOST LHI   RF,ACEINACP         INVALID ACCOUNT FOR POSTING                  
         B     ACCERRM                                                          
EINVTSK  LHI   RF,ACETASK                                                       
         B     ACCERRM                                                          
EDUPEN   LHI   RF,ACEDUPEN                                                      
         B     ACCERRM                                                          
EINVTYPE LHI   RF,ACEIVTYP                                                      
         B     ACCERRM                                                          
ENOCRATE LHI   RF,ACENCRAT                                                      
         B     ACCERRM                                                          
ERRONBRO LHI   RF,ACENUPBO         CAN'T UPLOAD ON BRANDOCEAN                   
         B     ACCERRM                                                          
ESIANLM  LHI   RF,ACESIANL         MISSING ANALYSIS ACCOUNT                     
         B     ACCERRM                                                          
ESIANLI  LHI   RF,ACEIANAL         INVALID ANALYSIS ACCOUNT                     
         B     ACCERRM                                                          
EOPNMT   LHI   RF,ACEOPNMT                                                      
         B     ACCERRM                                                          
EFATAL   LHI   RF,ACEFATAL         FATAL ERROR                                  
         B     ACCERRM                                                          
EMAX#    LHI   RF,ACEMAX#                                                       
         L     R2,ALINE1                                                        
         B     ACCERRM                                                          
ENEGCK   LHI   RF,ACENEGCK         NEGATIVE HRS ON JOB                          
         L     R2,ALINE1                                                        
         MVI   GLTXT,15                                                         
         LA    R1,BCWORK                                                        
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         B     ACCERRM                                                          
EINVACC  LHI   RF,ACEACCT          INVALID ACCOUNT                              
         MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         B     ACCERRM                                                          
ERRY2UPD MVC   GERROR,=AL2(ACIY2CFM) ENTER Y TO CONFIRM UPDATE                  
         B     ACCGERRX                                                         
*        UNREFERENCED ERRORS                                                    
*RRNODEL LHI   RF,ACENODEL         CANT DELETE-T/S HAS PROT LNS                 
*        B     ACCERRM                                                          
*COSTG   LHI   RF,ACECOSTG         MISSING COSTING GROUP                        
*        B     ACCERRM                                                          
*INVCLI  LHI   RF,ACECLI                                                        
*        B     ACCERRM                                                          
*INVPROD LHI   RF,ACEPROD                                                       
*        B     ACCERRM                                                          
*INVJOB  LHI   RF,ACEJOB                                                        
*        B     ACCERRM                                                          
*TOOLONG LHI   RF,ACELLONG                                                      
*        B     ACCERRM                                                          
*MISHIGH LHI   RF,ACEHIGH                                                       
*        B     ACCERRM                                                          
*INVYR   LHI   RF,ACEINVYR                                                      
*        B     ACCERRM                                                          
*INVDATE LHI   RF,ACEIVDTE                                                      
*        B     ACCERRM                                                          
*INVHRS  LHI   RF,ACEIVHRS                                                      
*        B     ACCERRM                                                          
*ACTHRS  LHI   RF,ACEACTHR                                                      
*        B     ACCERRM                                                          
*QRTHRS  LHI   RF,ACEQTRHR                                                      
*        B     ACCERRM                                                          
*INVPER  LHI   RF,ACEIVPER                                                      
*        B     ACCERRM                                                          
*INVLOC  LHI   RF,ACEIVLOC                                                      
*        B     ACCERRM                                                          
*PERIOD  LHI   RF,ACEPERDF                                                      
*        B     ACCERRM                                                          
*INVAMT  LHI   RF,ACEAMNT                                                       
*        B     ACCERRM                                                          
*NOCAL   LHI   RF,ACENOCAL                                                      
*        B     ACCERRM                                                          
*HRNOMAT LHI   RF,ACEIVHRS                                                      
*        B     ACCERRM                                                          
*ENTDATE LHI   RF,ACEENTDT                                                      
*        B     ACCERRM                                                          
*FUTMT   LHI   RF,ACEFUTMT                                                      
*        B     ACCERRM                                                          
*TIMER   LHI   RF,ACETIMER         TIMESHEET CONTAINS ERRORS                    
*        B     ACCERRM                                                          
*RROCAL  LHI   RF,ACEOCAL          DATE OUTSIDE PERMITTED CAL                   
*        B     ACCERRM                                                          
*                                                                               
ACCERRM  STCM  RF,3,GERROR                                                      
         B     ACCERRX                                                          
*                                  GENERAL ERROR EXIT                           
ACCERRX  NI    4(R2),X'FF'-X'20'   FIELD IS INVALID                             
         MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         GOTO1 MYERR                                                            
*                                                                               
ACCGERRX MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* GETEL - NO LONGER USED                                              *         
***********************************************************************         
         SPACE 1                                                                
***      GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
PFTABLE  DS    0C                                                               
*                                                                               
         DC    AL1(PPF01X-*,1,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF01X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF02X-*,2,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF03X-*,3,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF03X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF04X-*,4,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF04X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF05X-*,5,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF05X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF06X-*,6,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF06X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF07X-*,7,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF07X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF08X-*,8,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF08X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF09X-*,9,0,0,PFTRETRN,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PPF09X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF10X-*,10,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF10X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF11X-*,11,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF11X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF12X-*,12,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF13X-*,13,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF14X-*,14,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF14X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF15X-*,15,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF15X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF16X-*,16,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF16X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF19X-*,19,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF19X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TYPE OF TIME TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
TIMETAB  DS    0C                                                               
         DC    C'  ',C'N',C'N ',AL1(TIMTCN,0)                                   
         DC    C'N ',C'N',C'N ',AL1(TIMTCN,0)                                   
         DC    C'R ',C'R',C'R ',AL1(TIMTCR,0)                                   
         DC    C'B ',C'B',C'B ',AL1(TIMTCB,0)                                   
         DC    X'FF'                                                            
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY NMOD                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
VKEY     NMOD1 0,**VKEY**                                                       
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
*        L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         LA    R4,SYSSPARE                                                      
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
*                                                                               
         TM    TRANSTAT,RACHANG    RECORD/ACTION CHANGE                         
         BZ    *+8                                                              
         MVI   BOTSCR,0                                                         
*                                                                               
         TM    TRANSTAT,RCHANG     RECORD CHANGE                                
         BZ    *+10                                                             
         XC    SVYYMMDD,SVYYMMDD   CLEAR PERIOD EACH REC CHANGE                 
*                                                                               
         LA    RE,SVVKBLCK                                                      
         LA    RF,SVVKBLKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    BCFLTS,BCFLTS                                                    
*                                                                               
         CLI   ACTEQU,ACTADD       REVALIDATE OPTIONS FIELD EXCEPT              
         BE    *+8                 FOR ACTION = ADD                             
         NI    CONOPTH+4,X'FF'-X'20'                                            
*                                                                               
         CLI   BOTSCR,0                                                         
         BNE   VKPC                                                             
         BAS   RE,CLRCNFM                                                       
         EJECT                                                                  
***********************************************************************         
* PROJECT CONTROL INITIALIZATION                                      *         
***********************************************************************         
         SPACE 1                                                                
VKPC     DS    0H                                                               
*&&US                                                                           
         TM    BCCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    VKPCX                                                            
         GOTO1 AGETPCP                                                          
         MVC   SVCLIPOS,BCCLIPOS GET CLIENT POSITION                            
*                                                                               
VKPCX    DS    0H                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VKPRSN   DS    0H                                                               
         LA    R2,CHAPERH          PERIOD IS FIRST FIELD ON SCREEN              
         CLI   5(R2),0             BUT CAN'T VALIDATE WITHOUT PERSON            
         BH    VKPRSN10                                                         
         OC    SVYYMMDD,SVYYMMDD   DO WE HAVE A DATE SAVED AROUND?              
         BNZ   VKPRSN10                                                         
         MVC   GERROR,=Y(ACIPLSE)  PLEASE INPUT REQUIRED FIELDS                 
         B     VKGERRX                                                          
*                                                                               
VKPRSN10 LA    R2,CHACODEH                                                      
         MVI   BCIFMIN,1           MINIMUN LENGTH - PERSON REQUIRED             
         MVC   BCIFMAX,BC1RLEV4    MAXIMUM LENGTH                               
         GOTO1 AFVAL,CHACODEH                                                   
         BH    VKAERRX                                                          
         GOTO1 AVALPRSN,CHACODEH   VALIDATE PERSON                              
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIVPER)    INVALID PERSON CODE                     
         B     VKAERRX                                                          
*                                                                               
         MVC   SVPIDNO,BCPIDNO     SAVE PERSON ID #                             
         MVC   SV1RPRSN,BCIFLD     SAVE PERSON CODE                             
         MVC   CHANAME,BCWORK      DISPLAY PERSON NAME                          
*                                                                               
*&&US                                                                           
         MVI   BCBYTE1,NAMEFLDQ            CHECK ACCESS TO VIEW NAME            
         BRAS  RE,FLDSEC                   SECURITY TO VIEW NAME                
         BNL   *+8                                                              
         OI    CHANAMEH+1,X'0C'              LOW INTENSITY                      
*&&                                                                             
         OI    CHANAMEH+6,X'80'                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VKPERD   DS    0H                                                               
         LA    R2,CHAPERH          ANYTHING IN PERIOD FIELD                     
         CLI   5(R2),0                                                          
         BNE   VKPERD10                                                         
         LA    R3,SVYYMMDD                                                      
         MVI   BYTE,1                                                           
         OC    SVYYMMDD,SVYYMMDD   DO WE HAVE A SAVED DATE AROUND               
         BNZ   *+14                                                             
         MVC   GERROR,=Y(ACIPLSE) PLEASE INPUT REQUIRED FIELDS                  
         B     VKGERRX                                                          
VKPERD05 GOTO1 DATCON,BCDMCB,(BYTE,(R3)),(17,8(R2))                             
*&&US*&& MVI   5(R2),8             SET APPROPRIATE LENGTH                       
*&&UK*&& MVI   5(R2),7             SET APPROPRIATE LENGTH                       
VKPERD10 XC    BCFLAG4,BCFLAG4                                                  
*                                                                               
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK+L'PVALOUTB                                              
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,BCDMCB,(R2),(2,BLOCK+L'PVALOUTB),C',=-='                 
         CLI   BCDMCB+4,1                                                       
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         CLI   5(R2),3             L'INPUT>3 MEANS DATE  EX/'051594'            
         BH    *+12                                                             
         TM    SC1STVAL,SCNUMQ     DID THEY ENTER A PERIOD NUMBER               
         BO    VKPERD20                                                         
*                                                                               
*              USER ENTERED A DATE                                              
*                                                                               
         OI    BCFLAG4,BCFL4DTE    FLAG THAT USER ENTERED A DATE                
         USING PERVALD,R6                                                       
         LA    R6,BLOCK                                                         
         MVC   BYTE,LANGCODE                                                    
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+8                 THEN FORCE DATE VALIDATION TO                
         MVI   BYTE,LANGEUS        USA FORMAT (EX MM/DD/YY)                     
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,BCDMCB,(SC1STLEN,SC1STFLD),(BYTE,BLOCK)                   
         CLI   BCDMCB+4,PVRCMISS                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         CLI   BCDMCB+4,PVRCINV1                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         MVC   YYMMDD,PVALPSTA                                                  
         BAS   RE,GETLOC           GET OFFICE FOR THIS DATE                     
         BE    *+8                 NO LOCATION FOR THIS PERIOD                  
         OI    BCFLAG4,BCFL4LOC    FLAG-USER ENTERED DATE OUT OF LOC            
*        BE    *+14                NO LOCATION FOR THIS PERIOD                  
*        MVC   GERROR,=AL2(ACEINV)                                              
*        B     VKAERRX                                                          
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPYMD,YYMMDD                                                   
         OI    CALSTAT,CALYMDQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
         B     VKPERD30                                                         
*                                                                               
*              USER ENTERED A PERIOD NUMBER                                     
*                                                                               
VKPERD20 OI    BCFLAG4,BCFL4PER    FLAG THAT USER ENTERED A PERIOD#             
         MVC   YYMMDD,BCTODAY3                                                  
         BAS   RE,GETLOC           GET LOCATION FOR TODAY                       
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPNUM,SC1STNUM+3                                               
         OI    CALSTAT,CALNUMQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
*                                                                               
*              CALL GETCAL TO GET PERIOD INFO                                   
*                                                                               
VKPERD30 MVC   AIO,AIO2                                                         
         GOTO1 AGETCAL,WORK2                                                    
         BNE   VKAERRX                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
* FINAL CHECK TO MAKE SURE OFFICE IS CORRECT FOR THAT DATE                      
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FINAL CHECK TO ENSURE OFF IS CORRECT         
         MVC   CALSTDTE,CALCSTRT   CALENDAR START DATE                          
         MVC   CALENDTE,CALCEND    CALENDAR END DATE                            
         MVC   PRDSTDTE,CALRSTRT   PERIOD START DATE                            
         MVC   PRDENDTE,CALREND                                                 
         MVC   PRDNUM,CALRNUM                                                   
         MVC   PRDMON,CALRMTH                                                   
         MVC   SVYYMMDD,YYMMDD                                                  
         TM    BCFLAG4,BCFL4DTE    USER ENTERED A DATE?                         
         BO    VKPERD40                                                         
         MVC   YYMMDD,PRDENDTE                                                  
         MVC   SVYYMMDD,PRDENDTE                                                
         BAS   RE,GETLOC           DO OFFICE AND DATE MATCH                     
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACELOCDT)                                            
         B     VKAERRX             MUST ENTER A DATE                            
*                                                                               
VKPERD40 MVC   YYMMDD,SVYYMMDD                                                  
*        GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(2,SVPERDTE)   PERIOD END DTE         
         BAS   RE,GETLOC           REREAD TO GET ACTUAL OFFC/DPT/SUBD           
         DROP  R1,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE TMS START DATE                                             *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
VKDTE    OC    BCCPYTMS,BCCPYTMS   ANY DATE SAVED?                              
         BZ    VKDTEX              NO - EXIT                                    
         GOTO1 DATCON,DMCB,(1,PRDSTDTE),(2,WORK)                                
         CLC   BCCPYTMS,WORK       IF TMS START DATE IS LOWER THAN              
         BH    ERRUS49             PERIOD START DTE EXIT W/ERROR                
VKDTEX   DS    0H                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY MULTIPLE LOCATIONS                                          *         
***********************************************************************         
         SPACE 1                                                                
VKLOC    MVC   CHALOCS,BCSPACES                                                 
         GOTO1 ALOCLIST,BCDMCB,AIO,PRDSTDTE,PRDENDTE,SVYYMMDD                   
         BH    VKLOCX                                                           
         MVC   CHALOCS,BCWORK                                                   
         OI    CHALOCSH+1,X'08'    HIGH INTENSITY                               
         OI    CHALOCSH+6,X'80'                                                 
         TM    BCFLAG4,BCFL4PER                                                 
         BNO   *+14                                                             
         MVC   GERROR,=AL2(ACEENTDT)                                            
         B     VKAERRX             MUST ENTER A DATE                            
         CLI   BCFLAG3,BCFL3OK     WAS DATE ENTERED A LOCATION ENDDATE          
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     VKAERRX             INVALID DATE                                 
*                                                                               
VKLOCX   DS    0H                                                               
         OI    CHALOCSH+6,X'80'                                                 
         TM    BCFLAG4,BCFL4DTE                                                 
         BO    *+10                                                             
         MVC   SVYYMMDD,PRDENDTE   IF PERIOD # THEN USE PERIOD ENDDATE          
         EJECT                                                                  
***********************************************************************         
* GET NEXT OPEN MONTH                                                 *         
***********************************************************************         
         SPACE 1                                                                
         MVC   SVFMOA,=X'FFFF'     = NO FUTURE LIMIT                            
         CLC   PRDMON,BCTODAY3     IS TIME IN FUTURE                            
         BNH   *+10                                                             
         MVC   SVFMOA,PRDMON       FUTURE LIMIT = PERIOD MONTH                  
         GOTO1 AMTHLOCK,BCDMCB,PRDMON,SVFMOA,TWAACCS                            
         BE    VKMTH10                                                          
         CLI   ACTEQU,ACTDIS       FOR ACTION DISPLAY SKIP MONTH CHECK          
         BE    VKMTH10                                                          
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   SVLSEL,AC@CHAU      SAME FOR DISPLAYING FROM LIST                
         BNE   VKMTH10                                                          
         B     ACCERRX                                                          
*                                                                               
VKMTH10  MVC   SVOMOA,BCYYMMDD     OPEN MONTH OF ACTIVITY                       
         CLC   PRDMON,SVOMOA                                                    
         BNH   *+10                                                             
         MVC   SVOMOA,PRDMON                                                    
         MVC   BCYYMMDD(2),SVOMOA                                               
         MVI   BCYYMMDD+2,X'01'                                                 
         MVC   BCWORK,BCSPACES                                                  
         GOTO1 DATCON,BCDMCB,(1,BCYYMMDD),(9,BCWORK)                            
*&&US*&& MVC   CHAMOA,BCWORK                                                    
*&&UK*&& MVC   CHAMOA+1(5),BCWORK                                               
         OI    CHAMOAH+6,X'80'                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R OFFICE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
VKOFFC   DS    0H                                                               
         MVC   AIO,AIO2                                                         
         XC    SCRREQS,SCRREQS     REQUIRED FIELDS FOR ENTIRE SCREEN            
         MVI   PCSTAT,0            PROJECT CONTROL SATUS                        
*                                                                               
         MVI   BCPRIND,0           CLEAR INDICATORS                             
         MVC   BCYYMMDD,SVYYMMDD                                                
         MVC   SV1RACT,BCSPACES                                                 
         MVC   SV1RCPY,CMPY                                                     
         MVC   SV1RUL,=C'1R'                                                    
         BAS   RE,VAL1R            VALIDATE OFFICE CODE                         
         LA    R3,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BCX1RLV1         EX LENGTH OF LEVEL A                         
         MVC   0(0,R3),SV1ROFFC                                                 
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R            VALIDATE OFFICE CODE                         
*                                                                               
         LA    R2,CHAOFFH          DISPLAY OFFICE                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDOFFH                                                       
         MVC   8(L'CHAOFF,R2),SV1ROFFC                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CHAOFNMH         DISPLAY OFFICE NAME                          
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDOFNMH                                                      
         MVC   8(L'CHAOFNM,R2),BCACNAME                                         
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R DEPARTMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
VKDEPT   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV2         EX LENGTH OF LEVEL B                         
         MVC   0(0,R3),SV1RDPT                                                  
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
*                                                                               
         LA    R2,CHADEPTH         DISPLAY DEPARTMENT                           
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDDEPTH                                                      
         MVC   8(L'SV1RDPT,R2),SV1RDPT                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CHADPNMH         DISPLAY DEPARTMENT NAME                      
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDDPNMH                                                      
         MVC   8(L'CHADPNM,R2),BCACNAME                                         
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBDEPARTMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
VKSUBD   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV3                                                      
         MVC   0(0,R3),SV1RSDPT                                                 
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
*                                                                               
         LA    R2,CHASDPTH         DISPLAY SUBDEPARTMENT                        
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDSDPTH                                                      
         MVC   8(L'SV1RSDPT,R2),SV1RSDPT                                        
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CHASDNMH         DISPLAY SUBDEPARTMENT NAME                   
         CLI   ACTEQU,ACTADD                                                    
         BNE   *+8                                                              
         LA    R2,ADDSDNMH                                                      
         MVC   8(L'CHASDNM,R2),BCACNAME                                         
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE ON 1R LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
VKPERS   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BCX1RLV4         EX LENGTH OF LEVEL D                         
         MVC   0(0,R3),SV1RPRSN                                                 
         EX    R1,*-6                                                           
         LA    R2,CHACODEH         ERROR AT PERSON CODE                         
         BAS   RE,VAL1R            VALIDATE 1R ACCOUNT AND GET INFO             
         TM    BCINDS,BCFLABAL     VALID FOR POSTING                            
         BO    *+14                                                             
         MVC   GERROR,=AL2(ACEINACP)     NOT VALID FOR POSTING                  
         B     VKIERRX                                                          
*                                                                               
         NI    STATUS2,X'FF'-MCSTIME                                            
         TM    BCPRIND,BCPRIMCT    TEST MCS TIME PERSON                         
         BZ    VKPERS00                                                         
         OI    STATUS2,MCSTIME     SET IN SAVED STORAGE                         
         CLI   ACTEQU,ACTADD       DO NOT ALLOW ADDING IF ON BRANDO             
         BNE   VKPERS00                                                         
         MVC   GERROR,=AL2(ACENUOBO)                                            
         B     VKAERRX             MUST ENTER A DATE                            
*                                                                               
VKPERS00 DS    0H                                                               
*&&UK                                                                           
         TM    BCPRIND,BCPRIDTM    HAVE WE GOT DAILY TIME PERSON                
         BNZ   VKPERS02                                                         
         NI    STATUS2,X'FF'-DAILYTIM  NO                                       
         B     VKDESC                                                           
VKPERS02 OI    STATUS2,BCPRIDTM    YES - SET SAVE INDICATOR                     
         TM    BCFLAG4,BCFL4DTE    FLAG THAT USER ENTERED A DATE                
         BNZ   VKDESC                                                           
         MVC   GERROR,=AL2(ACEPRDTE)                                            
         B     VKAERRX             MUST ENTER A DATE                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY PERIOD NUMBER - DATE - MONTH                                *         
***********************************************************************         
         SPACE 1                                                                
VKDESC   MVC   BCWORK,BCSPACES                                                  
         OI    CHAPDESH+6,X'80'                                                 
         LA    R3,BCWORK                                                        
         MVI   0(R3),C'#'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (B1,PRDNUM),(2,(R3)),0                                           
         OC    0(2,R3),=X'F0F0'    PAD WITH ZERO                                
         LA    R3,3(R3)                                                         
         GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(17,(R3))                             
         LA    R3,9(R3)                                                         
         MVC   YYMMDD,PRDMON                                                    
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,(R3))                                
         TM    STATUS2,DAILYTIM    ARE WE ON DAILY TIME                         
         BZ    VKDESC02            NO                                           
         LA    R3,9(R3)            YES - PUT OUT MESSAGE                        
         MVC   0(L'AC@DTMS,R3),AC@DTMS                                          
VKDESC02 GOTO1 SQUASHER,DMCB,BCWORK,L'BCWORK                                    
         MVC   CHAPDES,BCWORK                                                   
         EJECT                                                                  
***********************************************************************         
* GET PROFILES                                                        *         
***********************************************************************         
         SPACE 1                                                                
VKPROF   DS    0H                                                               
         L     R0,AIO                                                           
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCKD,R3                                                      
         L     R3,AIO                                                           
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),BCSPACES                                        
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,BCSPACES                                                 
         MVC   COKOFC,SV1ROFFC                                                  
         MVC   COKDPT(L'SV1RDPT),SV1RDPT                                        
         MVC   COKSDT(L'SV1RSDPT),SV1RSDPT                                      
         MVC   COKPER,SV1RPRSN                                                  
         GOTO1 VGETCAP,BCDMCB,COBLOCK                                           
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDFSCRN,CODTS      DEFAULT INPUT SCREEN                         
         CLI   CNFTMS,COOPTYQ                                                   
         BNE   *+8                                                              
         OI    SVCNFRM,SVCNFRMY    FORCE EXTRA CONFIRMATION ON UPDTE            
         MVI   SVDFBRTE,SVDFRSAL                                                
         MVI   SVDFRRTE,SVDFRSAL                                                
         MVC   SVDGRP,CODIR        SAVE OFF DEFAULT COSTING GROUP               
*&&UK                                                                           
         CLI   COBRTE,COOPTYQ      USE COST RATE NOT SALES (B-TIME)             
         BE    *+8                                                              
         MVI   SVDFBRTE,SVDFRCST                                                
         CLI   CORRTE,COOPTYQ      USE COST RATE NOT SALES (R-TIME)             
         BE    *+8                                                              
         MVI   SVDFRRTE,SVDFRCST                                                
*                                                                               
         MVC   SVBTL,COTBTL        SAVE BILLABLE TIME LEDGER                    
         MVC   SVDLK,COTDLK        SAVE DEFAULT INCOME ACCT                     
*&&                                                                             
         MVC   SVTUP,COTUP         SAVE UPDATE TIME WHEN STATUS=?               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE 14 ACCOUNT                                                 *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
VK14AC   DS    0H                                                               
         CLC   SVCSTGRP,BCSPACES                                                
         BH    VK14AC10                                                         
         CLC   SVDGRP,BCSPACES                                                  
         BH    VK14AC10                                                         
         MVC   GERROR,=AL2(ACESIANL)       MISSING ANALYSIS ACCOUNT             
         B     VKIERRX                                                          
*                                                                               
         USING ACTRECD,R6                                                       
VK14AC10 LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       *** VALIDATE 14 ACCOUNT ***                  
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'14'                                                
         MVC   ACTKACT(1),SVCSTGRP                                              
         CLC   SVCSTGRP,BCSPACES                                                
         BH    *+10                                                             
         MVC   ACTKACT(1),SVDGRP                                                
         GOTO1 AGETACT,0                                                        
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIANAL)     INVALID ANALYSIS ACCOUNT               
         B     VKIERRX                                                          
*&&                                                                             
*                                                                               
VKX      B     VKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* GET PERSON'S LOCATION WITH DATE IN YYMMDD                           *         
***********************************************************************         
         SPACE 1                                                                
GETLOC   NTR1                                                                   
         MVC   CHALOCS,BCSPACES    CLEAR FIELD AND TRANSMIT                     
         OI    CHALOCSH+6,X'80'                                                 
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
*                                                                               
         USING LOCELD,R6                                                        
GETL10   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    GETLX                                                            
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   GETL10                                                           
         CLC   YYMMDD,LOCSTART                                                  
         BL    GETL10                                                           
         OC    LOCEND,LOCEND       ANY END DATE                                 
         BZ    GETLOC30                                                         
         CLC   YYMMDD,LOCEND                                                    
         BNH   GETLOC30                                                         
*                                                                               
         CLC   YYMMDD,BCTODAY3     DONT CONTINUE IF TODAYS DATE                 
         BE    GETL10                                                           
         TM    BCFLAG4,BCFL4PER+BCFL4LOC  USER ENTERED PERD/LOC DT              
         BNZ   GETL20                                                           
         CLC   PRDENDTE,BCSPACES   DO WE HAVE A PERIOD END DATE                 
         BNH   GETL10                                                           
         CLC   YYMMDD,PRDENDTE     MAKE SURE WE ARE IN PERIOD                   
         BH    GETL10                                                           
*                                                                               
GETL20   CLC   PRDSTDTE,LOCEND     MAKE SURE PERIOD START DATE IS               
         BH    GETL10              W/IN LOCATION - ELSE LOOP                    
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'('                                                      
         MVC   BCWORK+1(L'LOCOFF),LOCOFF       OFFICE CODE                      
         MVC   BCWORK+4(L'LOCDEPT),LOCDEPT     DEPARTMENT CODE                  
         MVC   BCWORK+12(L'LOCSUB),LOCSUB      SUBDEPARTMENT CODE               
         MVI   BCWORK+19,C'='                                                   
         GOTO1 DATCON,BCDMCB,(1,LOCEND),(17,BCWORK+21)                          
         MVI   BCWORK+29,C')'                                                   
         GOTO1 SQUASHER,BCDMCB,BCWORK,L'BCWORK                                  
         SR    RF,RF                                                            
         ICM   RF,15,BCDMCB+4                                                   
         SH    RF,=H'1'                                                         
         BM    GETL10                                                           
         MVC   CHALOCS(0),BCWORK                                                
         EX    RF,*-6                                                           
         B     GETL10                                                           
*                                                                               
GETLOC30 MVC   SV1ROFFC,LOCOFF                                                  
         MVC   SV1RDPT,LOCDEPT                                                  
         MVC   SV1RSDPT,LOCSUB                                                  
         MVC   SV1RSTDT,LOCSTART                                                
         MVC   SV1RENDT,LOCEND                                                  
*                                                                               
* LOOP TO ADJUST START AND END DATES IF NECESSARY.                              
*         BCWORK FOR START WILL START OUT AS X'FFFFFF' AND BCWORK FOR           
*         THE END DATE WILL BE CLEAR TO ZEROES.                                 
*                                                                               
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
GETL40   MVC   BC1RSTDT,BCEFFS     SET THIS TO HIGHEST AS DEFAULT               
         XC    BC1RENDT,BC1RENDT   SET THIS TO LOWEST AS DEFAULT                
         XC    BCLOCEPK,BCLOCEPK   WORK AREA FOR LOC END/ST COMPARE             
GETL50   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    GETL80              CHECK IF WE SHOULD UPDATE DATES?             
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   GETL50                                                           
         OC    BCLOCEPK,BCLOCEPK                                                
         BZ    GETL60                                                           
         GOTO1 DATCON,BCDMCB,(1,BCLOCEPK),(0,BCLOCEEB)                          
         GOTO1 ADDAY,DMCB,BCLOCEEB,BCLOCSEB,1                                   
         GOTO1 DATCON,BCDMCB,(0,BCLOCSEB),(1,BCLOCSPK)                          
         CLC   BCLOCSPK,LOCSTART   CONTIGUOUS DATES?                            
         BNE   GETL70                                                           
GETL60   MVC   BCLOCEPK,LOCEND     UPDATE END DATE AND CONTINUE                 
         CLC   SV1ROFFC,LOCOFF     SAME LOCATION OFFICE AS BEFORE               
         BNE   GETL70                                                           
         CLC   SV1RDPT,LOCDEPT     SAME LOCATION DEPARTEMENT AS B4              
         BNE   GETL70                                                           
         CLC   SV1RSDPT,LOCSUB     SAME LOCATION SUB-DEPART  AS B4              
         BNE   GETL70                                                           
         CLC   BC1RSTDT,LOCSTART                                                
         BNH   *+10                                                             
         MVC   BC1RSTDT,LOCSTART                                                
         OC    LOCEND,LOCEND       ANY END DATE?                                
         BZ    *+14                                                             
         CLC   BC1RENDT,LOCEND                                                  
         BNL   *+10                                                             
         MVC   BC1RENDT,LOCEND                                                  
         B     GETL50                                                           
*                                                                               
GETL70   CLC   YYMMDD,BC1RSTDT           MAKE SURE WE ARE IN PERIOD             
         BL    GETL40                                                           
         OC    BC1RENDT,BC1RENDT         ANY END DATES?                         
         BZ    *+14                                                             
         CLC   YYMMDD,BC1RENDT                                                  
         BH    GETL40                                                           
*                                                                               
GETL80   DS    0H                                                               
         CLC   YYMMDD,BC1RSTDT           MAKE SURE WE ARE IN PERIOD             
         BL    ROUTE                                                            
         OC    BC1RENDT,BC1RENDT         ANY END DATES?                         
         BZ    *+14                                                             
         CLC   YYMMDD,BC1RENDT                                                  
         BH    ROUTE                                                            
         CLC   BC1RSTDT,SV1RSTDT         SHOULD WE UPDATE START?                
         BNL   *+10                                                             
         MVC   SV1RSTDT,BC1RSTDT                                                
         CLC   BC1RENDT,SV1RENDT                                                
         BNH   *+10                                                             
         MVC   SV1RENDT,BC1RENDT                                                
         B     ROUTE                                                            
*                                                                               
GETLX    DS    0H                                                               
         CLC   CHALOCS,BCSPACES                                                 
         BNH   GETLX10                                                          
         OI    CHALOCSH+1,X'08'    HIGH INTENSITY                               
         OI    CHALOCSH+6,X'80'                                                 
GETLX10  TM    BCFLAG4,BCFL4PER                                                 
         BNO   *+14                                                             
         MVC   GERROR,=AL2(ACEENTDT)                                            
         B     VKAERRX             MUST ENTER A DATE                            
         TM    BCFLAG4,BCFL4LOC    USER ENTERED A DATE OUT OF LOC               
         BNO   VKROUTH             MUST ENTER VALID DATE                        
         MVC   GERROR,=AL2(ACELOCDT)                                            
         B     VKAERRX                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R ACCOUNT - CALL GETACT WITH EACH LEVEL OF 1R             *         
***********************************************************************         
         SPACE 1                                                                
VAL1R    NTR1                                                                   
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEACCT)                                             
         B     VKIERRX                                                          
*                                                                               
         LA    RE,ACTKACT          RE=A(ACCOUNT)                                
         SR    RF,RF                                                            
         IC    RF,BC1RLNQ3         BUMP PAST 1ST 3 LEVELS                       
         AR    RE,RF                                                            
         SR    R1,R1                                                            
         IC    R1,BCX1RLV4         EX LENGTH OF LEVEL D                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),BCSPACES                                                 
         BNH   VAL1R10                                                          
         GOTO1 TSTSEC,0                                                         
*                                                                               
VAL1R10  MVC   SVASTAT1,BCASTAT1   SAVE STATUS FOR COMPARE IN VALREC            
         CLI   ACTEQU,ACTDIS                                                    
         BE    VAL1R20                                                          
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   SVLSEL,AC@CHAU      IF SELECT IS CHANGE TEST IF LOCKED           
         BNE   VAL1R20                                                          
         TM    BCASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BNO   VAL1R20                                                          
         MVC   GERROR,=AL2(ACEACTLK)                                            
         B     VKAERRX                                                          
*                                                                               
VAL1R20  OC    BCASTAT5,BCASTAT5   INPUT REQS                                   
         BZ    VAL1R30                                                          
         TM    BCASTAT5,RSTSPRJB   PRD/JOB/TSK REQ                              
         BZ    *+10                                                             
         OC    SCRREQS,=Y(FLDCLI+FLDPRD+FLDJOB+FLDTSK)                          
         TM    BCASTAT5,RSTSPROD   PRD REQ                                      
         BZ    *+10                                                             
         OC    SCRREQS,=Y(FLDCLI+FLDPRD)                                        
*                                                                               
VAL1R30  CLC   BCACOST,BCSPACES                                                 
         BNH   *+10                                                             
         MVC   SVCSTGRP,BCACOST    COSTING GROUP                                
*                                                                               
         CLC   BCSPINC,BCSPACES                                                 
         BNH   *+10                                                             
         MVC   SVSPINC,BCSPINC     SPECIAL INCOME ACCOUNT                       
*                                                                               
         CLC   BCACOSTP,BCSPACES                                                
         BNH   *+10                                                             
         MVC   SVCPOSN,BCACOSTP    COSTING REPLACE POSITION                     
*                                                                               
         CLC   BCACCTR,BCSPACES                                                 
         BNH   *+10                                                             
         MVC   SVCCNTER,BCACCTR    COSTING CENTER                               
*                                                                               
         CLC   BCDFTASK,BCSPACES   DEFAULT TASK CODE                            
         BNH   *+10                                                             
         MVC   SVDFTASK,BCDFTASK                                                
*                                                                               
*&&US                                                                           
         TM    BCCPYST3,CPYSPC1C+CPYSPCSJ IS CMPY ON PROJECT CONTROL            
         BZ    VAL1RX                                                           
         TM    BCASTAT3,RSTSPRTS                                                
         BNO   VAL1RX                                                           
         OI    PCSTAT,PC1R                                                      
         OC    SCRREQS,=Y(FLDCLI+FLDPRD+FLDJOB+FLDTSK)                          
*&&                                                                             
*                                                                               
VAL1RX   B     VKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALKEY EXIT POINTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
VKROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     VKROUTCC                                                         
VKROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     VKROUTCC                                                         
VKROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
VKROUTCC CLI   BCDUB,1                                                          
VKEXIT   XIT1                                                                   
*                                                                               
VKIERRX  MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
*                                                                               
VKAERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         B     *+12                                                             
VKGERRX  MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
*              DISPLAY RECORD NMOD                                    *         
*=====================================================================*         
         SPACE 1                                                                
         DS    0H                                                               
DISPLAY  NMOD1 0,**DREC**,R7                                                    
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
*        L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         LA    R4,SYSSPARE                                                      
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
         EJECT                                                                  
***********************************************************************         
* DISPLAY FROM TSAR RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
         MVC   AIO,AIO1            DEFAULT AIO IS AIO1                          
         L     R2,ALINE1           FIRST DISPLAY LINE                           
         NI    STATUS,X'FF'-(STINSERT+STINCOMP+STINCTAX)                        
*                                                                               
         USING SCRNTABD,R1         DISPLAY ROUTINES FOR THIS SCREEN             
         L     R1,ASCRNTAB                                                      
         CLC   BOTSCR,SCRNNUM                                                   
         BE    *+18                                                             
         LA    R1,SCRNTLNQ(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         ST    R1,ASCRNITM         A(SCREEN ITEM)                               
         ICM   R3,15,SCRNTBL       R3=A(DISPLAY ROUTINES)                       
         A     R3,ASCRNTAB                                                      
         ST    R3,AVDTAB                                                        
         DROP  R1                                                               
*                                                                               
         GOTO1 ACHKINC             CHECK FOR ANY INCOMPLETES                    
         BE    DISP10                                                           
         OI    STATUS,STINCOMP     INDICATE ERROR OTHER THAN TAX                
*&&US                                                                           
         NC    BCHALF,=AL2(TRKETAX)                                             
         BZ    *+8                                                              
         OI    STATUS,STINCTAX     INDICATE ERROR IS ON TAX INFO                
*&&                                                                             
*                                                                               
DISP10   L     R2,ALINE1           CLEAR BOTTOM PORTION OF SCREEN               
         L     R3,ALSTFLD                                                       
         GOTO1 AXFIELD,BCDMCB,(R2),(R3)                                         
*                                                                               
*&&US*&& BRAS  RE,CHKSEC                      CHECK SECURITY                    
*                                                                               
         MVC   SCXSTLIN,DISLINES   FIRST DISPLAY LINE                           
         MVC   SCXSTSUB,DISLINES+2                                              
         LA    R1,DISLINES                                                      
         ST    R1,ADISLINE                                                      
         XC    DISLINES,DISLINES                                                
         CLI   LAP#,0                                                           
         BNE   DISP90                                                           
*                                                                               
         CLI   PFKEY,PFUP          *** SCROLL UP ***                            
         BNE   DISP30                                                           
         SR    R0,R0                                                            
         ICM   R0,3,SCXSTLIN       START LINE NUMBER                            
         SR    R1,R1                                                            
         IC    R1,SCRLINES         NUMBER OF RECORDS ON SCREEN (PREV)           
         TM    SVSCROLL,SCRLPAGE   PAGE UP                                      
         BO    DISP20                                                           
         TM    SVSCROLL,SCRLHALF   HALF PAGE UP                                 
         BNO   *+12                                                             
         SRL   R1,1                DIVIDE BY 2                                  
         B     DISP20                                                           
         IC    R1,SVSCROLL+1       # LINES TO SCROLL BY                         
*                                                                               
DISP20   SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BH    *+8                                                              
         LA    R0,1                START FROM TOP                               
         STCM  R0,3,SCXSTLIN                                                    
         XC    SCXSUB,SCXSUB                                                    
         B     DISP90                                                           
*                                                                               
DISP30   CLI   PFKEY,PFDOWN        *** SCROLL DOWN ***                          
         BNE   DISP90                                                           
         TM    SVSCROLL,SCRLPAGE   PAGE DOWN                                    
         BNO   DISP50                                                           
         CLI   ACTEQU,ACTDIS       ARE WE DISPLAYING?                           
         BE    DISP40                                                           
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BNE   DISP40              NO - JUST SELECT                             
         MVC   SCXSTLIN,SCXLINE    FOR ADD/CHANGE ONLY                          
         MVC   SCXSTSUB,SCXSUB                                                  
         B     DISP80                                                           
*                                                                               
DISP40   SR    R0,R0               PAGE DOWN                                    
         ICM   R0,3,SCXSTLIN                                                    
         SR    R1,R1                                                            
         IC    R1,SCRLINES                                                      
         AR    R0,R1                                                            
         B     DISP70                                                           
*                                                                               
DISP50   SR    R0,R0               HALF PAGE DOWN                               
         ICM   R0,3,SCXSTLIN                                                    
         SR    R1,R1                                                            
         IC    R1,SCRLINES                                                      
         TM    SVSCROLL,SCRLHALF                                                
         BNO   DISP60                                                           
         SRL   R1,1                                                             
         AR    R0,R1                                                            
         B     DISP70                                                           
*                                                                               
DISP60   SR    R1,R1                                                            
         IC    R1,SVSCROLL+1       # LINES TO SCROLL BY                         
         AR    R0,R1                                                            
*                                                                               
DISP70   STCM  R0,3,SCXSTLIN                                                    
         CLC   BCFLTS,BCSPACES     ANY FILTERS?                                 
         BH    DISP75                                                           
         CLC   SCXSTLIN,SCXLINE    FOR FILTERING USE NEXT LINE                  
         BNL   *+10                                                             
         MVC   SCXSTLIN,SCXLINE                                                 
DISP75   TM    TSARSTAT,TSARREBD         SHOULD WE REBUILD?                     
         BNO   *+10                                                             
         XC    SCXSTLIN,SCXSTLIN                                                
         XC    SCXSTSUB,SCXSTSUB                                                
         NI    TSARSTAT,X'FF'-TSARREBD   TURN OFF REBUILD SWITCH?               
*                                                                               
         USING TSARRECD,R6                                                      
DISP80   GOTO1 AXAIO,0             CLEAR AIO                                    
         L     R6,AIO                                                           
         MVC   TRKLINE,SCXSTLIN                                                 
         MVC   TRKLNSUB,SCXSTSUB                                                
         GOTO1 ATSAR,BCDMCB,TSARDH,1        READ HIGH                           
         TM    BCTSERRS,TSEEOF                                                  
         BNO   DISP90                                                           
         MVC   SCXSTLIN,=H'1'                                                   
*                                                                               
         USING TSARRECD,R6                                                      
DISP90   MVC   SCXLINE,SCXSTLIN                                                 
         MVC   SCXSUB,SCXSTSUB                                                  
         GOTO1 AXAIO,0             CLEAR AIO                                    
         L     R6,AIO                                                           
         MVC   TRKLINE,SCXLINE     START WITH THIS LINE NUMBER                  
         MVC   TRKLNSUB,SCXSUB                                                  
         GOTO1 ATSAR,BCDMCB,TSARDH,1        READ HIGH                           
         B     DISP110                                                          
*                                                                               
DISP100  GOTO1 AXAIO,0             CLEAR AIO                                    
         GOTO1 ATSAR,BCDMCB,TSANXT,1        GET NEXT REC                        
DISP110  TM    BCTSERRS,TSEEOF                                                  
         BNO   DISP117                                                          
         TM    BCBYTE1,X'10'       WAS PREVIOUS LINE FILTERED OUT?              
         BNO   DISPX                                                            
         OI    TSARSTAT,TSARREBD   REBUILD TSAR NEXT TIME IN                    
         XC    DISLINES,DISLINES   CLEAR DISPLAYED LINES                        
         LA    R1,DISLINES                                                      
         ST    R1,ADISLINE                                                      
         B     DISPX                                                            
*                                                                               
DISP117  L     R6,AIO1                                                          
         MVI   LINESTAT,0                                                       
*        MVI   BCBYTE2,1           NEXT SUBLINE #                               
         OC    TRKERROR,TRKERROR   IS ITEM INCOMPLETE                           
         BZ    *+8                                                              
         OI    LINESTAT,LBINCOMP                                                
*                                                                               
         CLI   PFKEY,PFUNMARK                                                   
         BNE   DISP120                                                          
         NI    TRKSTAT,X'FF'-TRKSSAVE                                           
         GOTO1 ATSAR,BCDMCB,TSAWRT,1                                            
         EJECT                                                                  
***********************************************************************         
* SAVE ADDRESSES OF ELEMENTS FOR DISPLAY ROUTINES                     *         
***********************************************************************         
         SPACE 1                                                                
DISP120  XC    AINPELEM,AINPELEM   GET ADDRESSES OF ELEMENTS                    
         XC    ATAXELEM,ATAXELEM                                                
         XC    ANARELEM,ANARELEM                                                
         XC    AXSTELEM,AXSTELEM                                                
         XC    ASAVELEM,ASAVELEM                                                
         XC    ATIMELEM,ATIMELEM                                                
         XC    AMATELEM,AMATELEM                                                
         NI    STATUS,X'FF'-STPROTCT                                            
*                                                                               
         USING TIMELD,R3                                                        
         LA    R3,TRDATA                                                        
         CLI   0(R3),TIMELQ        X'8B' ELEM                                   
         BNE   DISPX                                                            
*                                                                               
         CLI   TIMETYP,TIMEFLD     COMMENTED FIELD ELEMENT                      
         BNE   DISP130                                                          
         ST    R3,ASAVELEM                                                      
         CLI   PFKEY,PFUNMARK                                                   
         BE    DISP190                                                          
         OI    LINESTAT,LBSAVE+LBSAVE#                                          
         B     DISP190                                                          
*                                                                               
DISP130  CLI   0(R3),TIMELQ        X'8B' ELEM                                   
         BNE   DISP190                                                          
         CLI   TIMETYP,TIMEINP     INPUT DETAILS                                
         BE    DISP150                                                          
*&&US*&& CLI   TIMETYP,TIMETAX     TAX ELEM                                     
*&&US*&& BE    DISP160                                                          
         CLI   TIMETYP,TIMENAR     NARRATIVE ELEM                               
         BE    DISP170                                                          
         CLI   TIMETYP,TIMEXTRA    EXTRA STATUS ELEMENT                         
         BE    DISP180                                                          
         CLI   TIMETYP,TIMETIME    ETIME ELEMENT                                
         BE    DISP184                                                          
         CLI   TIMETYP,TIMEITMS    ETIME MATERIALS ELEMENT                      
         BE    DISP186                                                          
DISP140  SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DISP130                                                          
*                                                                               
DISP150  ST    R3,AINPELEM         INPUT DETAIL ELEMENT                         
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    DISP140                                                          
*&&UK                                                                           
* TASK CODE '!!' WAS CREATED BECAUSE OF TMS CONVERSION PROBLEMS - 2/96          
         CLC   TIMTSK,=C'!!'       FOR UK TEST FOR DUMMY TASK                   
         BNE   *+8                                                              
         OI    TIMSTAT,TIMLOCK     LOCK ANY DUMMY ENTRY                         
*&&                                                                             
         CLC   TIMMOA,SVOMOA       SAME MOA AS OPEN MOA                         
         BNL   *+8                                                              
         OI    STATUS,STPROTCT     PROTECT ITEMS W/DIFFERENT MOA'S              
         TM    TIMSTAT,TIMLOCK+TIMTEMPO+TIMSMCS                                 
         BZ    *+8                 PROTECT ITEMS FROM TEMPO                     
         OI    STATUS,STPROTCT     PROTECT ITEMS THAT ARE LOCKED                
*&&NOP                                                                          
         TM    TIMSTAT,TIMLOCK     UNNOP THIS FOR TEMPO FIX                     
         BZ    *+8                                                              
         OI    STATUS,STPROTCT     PROTECT ITEMS THAT ARE LOCKED                
*&&                                                                             
         TM    TIMIND,TIMIWO+TIMIADJ                                            
         BZ    *+8                                                              
         OI    STATUS,STPROTCT     PROTECT ITEMS THAT ARE WRITEOFFS             
         CLI   SVOPT3,C'Y'         ARE WE SHOWING ALL DAYS FOR                  
         BNE   DISP152             NO                                           
         TM    STATUS2,DAILYTIM    ARE WE ENTERING DAILY TIME                   
         BZ    DISP152             NO - NO NEED TO PROTECT FOR                  
         OI    STATUS,STPROTCT     PROTECT ITEMS THAT ARE WRITEOFFS             
         B     DISP140                                                          
DISP152  TM    TRKSTAT,TRKSSAVE                                                 
         BNO   *+8                                                              
         NI    STATUS,X'FF'-STPROTCT                                            
         B     DISP140             AND PROTECT ADJUSTED ITEMS FROM CBI          
*                                                                               
*&&US                                                                           
DISP160  ST    R3,ATAXELEM         TAX DETAIL ELEMENT                           
         B     DISP140                                                          
*&&                                                                             
*                                                                               
DISP170  ST    R3,ANARELEM         NARRATIVE ELEMENT                            
         B     DISP140                                                          
*                                                                               
DISP180  ST    R3,AXSTELEM         EXTRA STATUS ELEMENT                         
         B     DISP140                                                          
*                                                                               
DISP184  ST    R3,ATIMELEM         ETIME ELEMENT                                
         B     DISP140                                                          
*                                                                               
DISP186  ST    R3,AMATELEM         ETIME MATERIALS ELEMENT                      
         B     DISP140                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CALL DISPLAY ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
DISP190  GOTO1 AFILTER,BCDMCB,AINPELEM,(R6)                                     
         BE    *+12                                                             
         OI    BCBYTE1,X'10'       SHOW THAT LINE WAS FILTERED OUT              
         B     DISP100                                                          
*                                                                               
         NI    BCBYTE1,X'FF'-X'10'                                              
         USING DISTABD,R3                                                       
         L     R3,AVDTAB           DISPLAY TABLE FOR THIS SCREEN                
DISP200  CLI   DTFDISP,X'FF'       END OF TABLE                                 
         BE    DISP240                                                          
         OC    DTFLEN,DTFLEN       IS FIELD ON SCREEN                           
         BZ    DISP230             NO                                           
*                                                                               
         LH    R1,DTFDISP          DISPLACEMENT TO FIELD IN LINE                
         AR    R1,R2               R2=BEGINNING OF LINE                         
         ST    R1,AFIELDH          A(FIELD HEADER)                              
         OI    6(R1),X'80'         XMIT                                         
         CLI   PFKEY,PFUNMARK                                                   
         BE    DISP210                                                          
         OC    ASAVELEM,ASAVELEM                                                
         BNZ   DISP210                                                          
         TM    LINESTAT,LBINCOMP   DON'T MARK VALIDATED IF INCOMPLETE           
         BO    *+8                                                              
         OI    4(R1),X'20'         FIELD VALIDATED                              
*                                                                               
DISP210  TM    STATUS,STPROTCT     PROTECT                                      
         BNO   *+8                                                              
         OI    1(R1),X'20'                                                      
         MVC   FIELDLEN,DTFLEN     FIELD LENGTH                                 
*                                                                               
         OC    ASAVELEM,ASAVELEM                                                
         BZ    DISP220                                                          
         ST    R3,BCADDR           CURRENT DISPLAY TABLE ENTRY                  
         BAS   RE,DISSAVE          DISPLAY COMMENTED LINE                       
         CLI   PFKEY,PFUNMARK                                                   
         BE    *+8                                                              
         OI    4(R1),X'20'         FIELD VALIDATED                              
         B     DISP230                                                          
*                                                                               
DISP220  SR    RF,RF                                                            
         ICM   RF,3,DTDISRTE       DISPLAY ROUTINE EQUATES                      
         BZ    DISP230                                                          
         LA    RE,DISP230          SET RETURN ADDRESS                           
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     DISTYPE             DISPLAY TYPE                                 
         B     DISHRS              DISPLAY HOURS                                
         B     DISCPJT             DISPLAY CLIENT/PRODUCT/JOB                   
         B     DISMOA              DISPLAY MOA                                  
         B     DISRATE             DISPLAY RATE                                 
         B     DISINC              DISPLAY INCOME ACCOUNT                       
         B     DISNAR              DISPLAY NARRATIVE                            
         B     DISNAME             DISPLAY NAME                                 
         B     DISAMT              DISPLAY AMOUNT                               
*&&US*&& B     DISTAXF             DISPLAY TAX FIELD                            
*&&US*&& B     DISTAX              DISPLAY TAX DATA                             
         B     DISTMP#             VALIDATE TEMPO LINE #                        
*                                                                               
DISP230  LA    R3,DTLENQ(R3)       BUMP TO NEXT FIELD                           
         B     DISP200                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT DISPLAY LINE                                           *         
***********************************************************************         
         SPACE 1                                                                
DISP240  DS    0H                  BUMP TO NEXT DISPLAY LINE                    
         USING SCRNTABD,R1                                                      
         L     R1,ASCRNITM                                                      
         SR    RF,RF                                                            
         ICM   RF,3,SCRNLEN        RF=LENGTH OF SCREEN LINE                     
         STCM  RF,3,WORK                                                        
         AR    R2,RF               R2=A(CURRENT SCREEN LINE)                    
         DROP  R1                                                               
*                                                                               
         USING TSARRECD,R6                                                      
         L     R6,AIO                                                           
         L     R1,ADISLINE         FILL IN LINE NUMBERS IN TABLE                
         MVC   0(2,R1),TRKLINE                                                  
         MVC   2(2,R1),TRKLNSUB                                                 
         LA    R1,4(R1)                                                         
         ST    R1,ADISLINE                                                      
         MVC   SCXLINE,TRKLINE     SAVE FOR SCROLLING UP & DOWN                 
         MVC   SCXSUB,TRKLNSUB                                                  
         L     R1,ALSTLINE         EXIT IF LAST LINE ON SCREEN                  
         CR    R2,R1                                                            
         BNH   DISP100                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY TOTALS / MESSAGES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISPX    DS    0H                                                               
         MVI   SVTMSYS,X'FF'       SET SYSTEM AS GENERAL                        
         CLC   CHALOCS(L'AC@SVNUM),AC@SVNUM      NOT SURE WHAT JS WAS           
         BNE   *+10                              TRYING TO DO HERE              
         MVC   CHALOCS,BCSPACES                                                 
         GOTO1 ACNTSAVE            COUNT # SAVED ITEMS                          
         BE    DISPX10                                                          
         MVC   CHALOCS,BCSPACES                                                 
         MVC   CHALOCS(L'AC@SVNUM),AC@SVNUM                                     
*                                                                               
         LA    RE,CHALOCS+L'CHALOCS-1      APPEND NUMBER TO END OF              
         CLI   0(RE),C' '                  LITERAL STRING FROM DICTNRY          
         BH    *+12                                                             
         BCT   RE,*-8                                                           
         B     DISPX10             SOMETHING MESSED UP IN DICTIONARY            
         SR    R3,R3                                                            
         ICM   R3,3,BCHALF                                                      
         CURED (R3),(3,2(RE)),0,ALIGN=LEFT                                      
         OI    CHALOCSH+1,X'08'    HIGH INTENSITY                               
DISPX10  OI    CHALOCSH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    STATUS,DISPED                                                    
         BRAS  RE,TOTALS           DISPLAY TOTALS                               
         CLI   ACTEQU,ACTADD       UNLESS ACTION ADD                            
         BE    DISPX20                                                          
         TM    STATUS2,MCSTIME     TEST MCS TIME PERSON                         
         BZ    DISPX20                                                          
         CLI   BCTIMST1,0          ZERO IS 'IN PROGRESS'                        
         BE    *+12                                                             
         TM    BCTIMST1,X'FF'-(TIMSDELT+X'04')  TEST ANY OTHER MCS STS          
         BZ    DISPX20                                                          
         MVC   CHATSTS,AC@TSSTS    'T/SHEET STATUS'                             
         OI    CHATSTSH+6,X'80'                                                 
         TM    BCTIMST1,TIMSFAPP                                                
         BZ    *+10                                                             
         MVC   CHATSTD,AC@FUAPR    'FULLY APPROVED'                             
         TM    BCTIMST1,TIMSSUBM                                                
         BZ    *+10                                                             
         MVC   CHATSTD,AC@SUBMT    'SUBMITTED'                                  
         TM    BCTIMST1,TIMSPAPP                                                
         BZ    *+10                                                             
         MVC   CHATSTD,AC@PTAPR    'PART APPROVED'                              
*        TM    BCTIMST1,TIMSAWAP                                                
*        BZ    *+10                                                             
*        MVC   CHATSTD,AC@AWLMA    'AWAITING LINE MANAGER APPROVAL'             
         TM    BCTIMST1,TIMSREJE                                                
         BZ    *+10                                                             
         MVC   CHATSTD,AC@REJCT    'REJECTED'                                   
*        TM    BCTIMST1,TIMSMAAP                                                
*        BZ    *+10                                                             
*        MVC   CHATSTD,AC@LIMAP    'LINE MANAGER APPROVED'                      
         CLI   BCTIMST1,0                                                       
         BNE   *+10                                                             
         MVC   CHATSTD,AC@INPRO    'IN PROGRESS'                                
         OI    CHATSTDH+6,X'80'                                                 
DISPX20  L     R2,ALINE1                                                        
*                                                                               
DISPX30  CLI   ACTEQU,ACTDIS       USE REGULAR MESSAGE ON DISPLAY               
         BNE   DISPX35                                                          
         TM    STATUS2,DAILYTIM    ARE WE ON DAILY TIME                         
         BZ    DRROUTE             NO - USE REGULAR MESSAGE                     
         MVI   SVTMSYS,X'00'       YES                                          
         MVC   GERROR,=Y(ACIDTDIS) RECORD DISPLAYED                             
         B     DRGERRX                                                          
*                                                                               
DISPX35  CLI   ACTEQU,ACTSEL       AND FOR 'SELECT' FROM LIST                   
         BNE   DISPX40                                                          
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BE    DISPX40                                                          
         MVC   GERROR,=Y(ACIRDIS)  RECORD DISPLAYED                             
         TM    STATUS2,DAILYTIM    ARE WE ON DAILY TIME                         
         BZ    DRGERRX             NO - USE REGULAR MESSAGE                     
         MVI   SVTMSYS,X'00'       YES                                          
         MVC   GERROR,=Y(ACIDTDIN) RECORD DISPLAYED                             
         B     DRGERRX                                                          
*                                                                               
DISPX40  TM    STATUS,STINCOMP     DISPLAYING AN INCOMPLETE                     
         BNO   DISPX60                                                          
         SR    R1,R1                                                            
         IC    R1,LAP#             INCREMENT LAP COUNT                          
         LA    R1,1(R1)                                                         
         STC   R1,LAP#                                                          
         CLI   LAP#,2              DONT GET CAUGHT IN ENDLESS LOOP              
         BNL   DREXIT                                                           
*                                                                               
*&&US                                                                           
         TM    STATUS,STINCTAX                                                  
         BNO   *+16                                                             
         CLI   BOTSCR,X'C6'                                                     
         BE    DRROUTH                                                          
         B     DISPX50                                                          
*&&                                                                             
         CLI   BOTSCR,X'C4'        FORCE MISSING INPUT ERROR ON                 
         BE    DRROUTH             BILLABLE SCREENS                             
         CLI   BOTSCR,X'C5'                                                     
         BE    DRROUTH                                                          
         CLI   BOTSCR,X'C9'                                                     
         BE    DRROUTH                                                          
*                                                                               
DISPX50  MVC   GERROR,=AL2(ACETIMER)                                            
         L     R2,ALINE1                                                        
         B     DRAERRX             TIMESHEET CONTAINS ERRORS                    
*                                                                               
DISPX60  CLI   PFKEY,PFUPDATE      JUST UPDATED RECORD                          
         BNE   DISPX70                                                          
         LA    R2,CHAPERH                                                       
         MVC   GERROR,=Y(ACIUPD)   RECORD WAS UPDATED - ENTER NEXT              
         CLI   ACTEQU,ACTADD       ACTION = ADD                                 
         BNE   *+10                                                             
         MVC   GERROR,=Y(ACIRADD)  RECORD WAS ADDED - ENTER NEXT                
         B     DRGERRX                                                          
*                                                                               
DISPX70  CLI   PFKEY,PFSAVE                                                     
         BNE   DISPX80                                                          
         L     R2,ALINE1                                                        
         MVC   GERROR,=Y(ACISAVE)  RECORD WAS SAVED - ENTER NEXT                
         B     DRGERRX                                                          
*                                                                               
DISPX80  CLI   PFKEY,PFUNMARK                                                   
         BNE   DISPX90                                                          
         SR    R1,R1                                                            
         IC    R1,LAP#             INCREMENT LAP COUNT                          
         LA    R1,1(R1)                                                         
         STC   R1,LAP#                                                          
         CLI   LAP#,2              DONT GET CAUGHT IN ENDLESS LOOP              
         BNH   DRROUTH                                                          
*                                                                               
DISPX90  L     R2,ALINE1                                                        
         MVC   GERROR,=Y(ACICHNG)  ENTER CHANGES                                
         TM    STATUS2,DAILYTIM    ARE WE RUNNING UDER DAILY TIME               
         BZ    DRGERRX             NO                                           
         MVC   GERROR,=Y(ACIDTCHG)                                              
         MVI   SVTMSYS,X'00'                                                    
         CLI   ACTEQU,ACTSEL       AND FOR 'SELECT' FROM LIST                   
         BNE   DRGERRX                                                          
         CLC   SVLSEL,AC@CHAU      WAS CHANGE SELECTED?                         
         BNE   DRGERRX                                                          
         MVC   GERROR,=Y(ACIDTCHN) YES - ADD PF11 FOR NEXT DAY                  
         B     DRGERRX                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY TYPE OF TIME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISTYPE  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM                                                      
         L     R2,AFIELDH                                                       
         MVI   5(R2),1             LENGTH OF INPUT                              
         LA    RF,BCWORK                                                        
         MVC   BCWORK,BCSPACES                                                  
         TM    TRKSTAT,TRKSSAVE                                                 
         BNO   *+16                                                             
         MVI   BCWORK,C'*'                                                      
         LA    RF,1(RF)                                                         
         MVI   5(R2),2                                                          
*                                                                               
DSTYPE5  MVI   0(RF),C'B'          B TIME                                       
         CLI   TIMTTYP,TIMTCB                                                   
         BE    DSTYPE10                                                         
         MVI   0(RF),C'R'          R TIME                                       
         CLI   TIMTTYP,TIMTCR                                                   
         BE    DSTYPE10                                                         
         MVI   0(RF),C'N'          N TIME                                       
*                                                                               
DSTYPE10 TM    TIMIND,TIMIADJ      ADJUSTED                                     
         BNO   *+12                                                             
         MVI   1(RF),C'A'                                                       
         MVI   5(R2),2                                                          
         TM    TIMIND,TIMIWO       WRITEOFF                                     
         BNO   *+12                                                             
         MVI   1(RF),C'W'                                                       
         MVI   5(R2),2                                                          
         TM    TIMSTAT,TIMTEMPO    UPLOADED ITEM                                
         BNO   *+12                                                             
         MVI   1(RF),C'T'                                                       
         MVI   5(R2),2                                                          
         TM    TIMSTAT,TIMSMCS     MCS ETIME ITEM                               
         BNO   *+12                                                             
         MVI   1(RF),C'M'                                                       
         MVI   5(R2),2                                                          
*                                                                               
         MVC   8(2,R2),BCWORK                                                   
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HOURS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         L     R2,AFIELDH                                                       
         MVI   5(R2),L'LC5HRS                                                   
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   DISHRS10                                                         
         CURED TIMHRS,(L'LC5HRS,8(R2)),2,MINUS=YES,DECPNT=FORCE                 
         B     DISHRS20                                                         
*                                                                               
DISHRS10 CURED TIMHRS,(L'LC5HRS,8(R2)),2,MINUS=YES                              
DISHRS20 OI    6(R2),X'80'                                                      
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CLIENT/PRODUCT/JOB/TASK                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCPJT  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVC   SVSJACT,BCSPACES                                                 
         MVC   SVSJCLNM,BCSPACES                                                
         MVC   SVSJPRNM,BCSPACES                                                
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         L     R2,AFIELDH                                                       
         CLC   TIMACC,BCSPACES     TEST ACCOUNT NOT SET                         
         BNE   DSCPJ02                                                          
         TM    TIMSTAT,TIMSMCS     TEST MCS ITEM                                
         BNZ   DSCPJ40             YES - ASSUME DRAFT (OK)                      
         DC    H'0'                NO - SOMETHING WRONG                         
DSCPJ02  CLC   TIMACC(2),=C'1N'                                                 
         BNE   DSCPJ10                                                          
         CLI   BOTSCR,X'C9'        ONLY SCREEN THAT DISPS NAMES                 
         BE    *+12                                                             
         CLI   BOTSCR,X'C2'        ONLY SCREEN THAT DISPS NAMES                 
         BNE   DSCPJ05                                                          
         GOTO1 GTLEVNM,BCDMCB,C'1N',TIMACC+2,0                                  
         MVC   SV1NNAME,WORK                                                    
*                                                                               
DSCPJ05  LH    R1,FIELDLEN         DISPLAY 1N ACCOUNT NAME                      
         STC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   8(0,R2),TIMACC+2                                                 
         EX    R1,*-6                                                           
         B     DSCPJX                                                           
*                                                                               
DSCPJ10  MVC   SVSJCPY,CMPY        *** DISPLAY CLIENT CODE ***                  
         MVC   SVSJUL,TIMACC                                                    
         SR    R1,R1                                                            
         IC    R1,BCSJLEV1                                                      
         STC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         MVC   SVSJCODE(0),TIMACC+2                                             
         EX    R1,*-6                                                           
         MVC   8(0,R2),TIMACC+2                                                 
         EX    R1,*-6                                                           
         OI    6(R2),X'80'         TRANSMIT CLIENT CODE                         
         CLI   BOTSCR,X'C9'        ONLY SCREEN THAT DISPS CLI/PRD NAMES         
         BE    *+12                                                             
         CLI   BOTSCR,X'C2'        ONLY SCREEN THAT DISPS CLI/PRD NAMES         
         BNE   DSCPJ20                                                          
         GOTO1 GTLEVNM,BCDMCB,C'SJ',SVSJCODE,0                                  
         MVC   SVSJCLNM,WORK       GETS CLIENT NAME                             
*                                                                               
DSCPJ20  SR    R1,R1               *** DISPLAY PRODUCT FIELD ***                
         IC    R1,0(R2)                                                         
         AR    R2,R1               R2 = A(PRODUCT FIELD)                        
*                                                                               
         MVC   SVSJCODE,BCSPACES                                                
         SR    R1,R1               ISOLATE SJ CLIENT/PRODUCT                    
         IC    R1,BCSJLNQ2                                                      
         SH    R1,=H'1'                                                         
         MVC   SVSJCODE(0),TIMACC+2                                             
         EX    R1,*-6                                                           
*                                                                               
         LA    RE,SVSJCODE          COPY PRODUCT CODE TO SCREEN FIELD           
         IC    R1,BCSJLNQ1                                                      
         AR    RE,R1                                                            
         IC    R1,BCXSJLV2                                                      
         MVC   8(0,R2),0(RE)                                                    
         EX    R1,*-6                                                           
         OI    6(R2),X'80'         TRANSMIT PRODUCT CODE                        
*                                                                               
         EX    R1,*+8              DONT SET FIELD LENGTH IF BLANK FIELD         
         B     *+10                                                             
         CLC   0(0,RE),BCSPACES                                                 
         BNH   DSCPJ30                                                          
         MVC   5(L'BCSJLEV2,R2),BCSJLEV2                                        
*                                                                               
         CLI   BOTSCR,X'C2'        ONLY SCREEN THAT DISPS CLI/PRD NAMES         
         BNE   DSCPJ30                                                          
         GOTO1 GTLEVNM,BCDMCB,C'SJ',SVSJCODE,0                                  
         MVC   SVSJPRNM,WORK       GETS PRODUCT NAME                            
*                                                                               
DSCPJ30  SR    R1,R1               *** DISPLAY JOB FIELD ***                    
         IC    R1,0(R2)                                                         
         AR    R2,R1               R2 = A(JOB FIELD)                            
*                                                                               
         MVC   SVSJCODE,BCSPACES                                                
         SR    R1,R1               ISOLATE SJ CLIENT/PRODUCT/JOB                
         IC    R1,BCSJLNQ3                                                      
         SH    R1,=H'1'                                                         
         MVC   SVSJCODE(0),TIMACC+2                                             
         EX    R1,*-6                                                           
*                                                                               
         LA    RE,SVSJCODE         COPY JOB CODE TO SCREEN FIELD                
         IC    R1,BCSJLNQ2                                                      
         AR    RE,R1                                                            
         IC    R1,BCXSJLV3                                                      
         MVC   8(0,R2),0(RE)                                                    
         EX    R1,*-6                                                           
         OI    6(R2),X'80'         TRANSMIT JOB CODE                            
*                                                                               
         EX    R1,*+8              DONT SET FIELD LENGTH IF BLANK FIELD         
         B     *+10                                                             
         CLC   0(0,RE),BCSPACES                                                 
         BNH   DSCPJ40                                                          
         MVC   5(L'BCSJLEV3,R2),BCSJLEV3                                        
*                                                                               
DSCPJ40  SR    R1,R1               *** DISPLAY TASK CODE ***                    
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   8(L'TIMTSK,R2),TIMTSK                                            
         OI    6(R2),X'80'                                                      
         CLC   TIMTSK,BCSPACES                                                  
         BNH   *+8                                                              
         MVI   5(R2),L'TIMTSK                                                   
*                                                                               
DSCPJX   MVC   AIO,AIO1                                                         
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY MOA                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISMOA   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         L     R2,AFIELDH                                                       
         MVC   YYMMDD(2),TIMMOA    DISPLAY MOA                                  
         MVI   YYMMDD+2,X'01'                                                   
         MVC   BCWORK,BCSPACES                                                  
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,BCWORK)                              
         LH    R1,FIELDLEN                                                      
         STC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   8(0,R2),BCWORK                                                   
         EX    R1,*-6                                                           
         TM    STATUS,STPROTCT     PROTECT                                      
         BNO   *+8                                                              
         OI    1(R2),X'20'                                                      
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRATE  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
*&&US                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
*&&                                                                             
*&&UK                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BE    *+12                                                             
         CLI   TIMLN,TIMB2LNQ      LENGTH FOR BILLABLE INPUT(W/ EURO)           
*&&                                                                             
         BNE   DRROUTE                                                          
         OC    TIMRATE,TIMRATE     TEST RATE IS SET (MAY BE DRAFT MCS)          
         BZ    DRROUTE                                                          
         L     R2,AFIELDH                                                       
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'*'                                                      
         TM    TIMRBSTA,TIMRBADJ   DEFAULT ADJUSTMENT RATE                      
         BNO   *+8                                                              
         MVI   BCWORK,C'A'                                                      
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   DISRAT10                                                         
         CURED TIMRATE,(L'LC5RATE,BCWORK+1),2,ALIGN=LEFT,DECPNT=FORCE           
         B     DISRAT20                                                         
*                                                                               
DISRAT10 CURED TIMRATE,(L'LC5RATE,BCWORK+1),2,ALIGN=LEFT                        
DISRAT20 LA    R1,BCWORK                                                        
         TM    TIMRBSTA,TIMRORAT   WAS RATE OVERRIDDEN                          
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   8(L'LC5RATE,R2),0(R1)                                            
         MVI   5(R2),L'LC5RATE                                                  
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY AMOUNT                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISAMT   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
*&&US                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
*&&                                                                             
*&&UK                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BE    *+12                                                             
         CLI   TIMLN,TIMB2LNQ      LENGTH FOR BILLABLE INPUT(W/ EURO)           
*&&                                                                             
         BNE   DRROUTE                                                          
         OC    TIMAMNT,TIMAMNT     TEST AMT IS SET (MAY BE DRAFT MCS)           
         BZ    DRROUTE                                                          
         L     R2,AFIELDH                                                       
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   DISAMT10                                                         
         CURED TIMAMNT,(L'LC5AMT,8(R2)),2,FLOAT=-,DECPNT=FORCE                  
         B     DISAMT20                                                         
*                                                                               
DISAMT10 CURED TIMAMNT,(L'LC5AMT,8(R2)),2,FLOAT=-                               
DISAMT20 TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    *+8                                                              
         OI    1(R2),X'20'         PROTECTED                                    
         MVI   5(R2),L'LC5AMT                                                   
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY INCOME ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
DISINC   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEMENT                        
*&&US                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
*&&                                                                             
*&&UK                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BE    *+12                                                             
         CLI   TIMLN,TIMB2LNQ      LENGTH FOR BILLABLE INPUT(W/ EURO)           
*&&                                                                             
         BNE   DRROUTE                                                          
         CLC   TIMINC,BCSPACES                                                  
         BNH   DRROUTE                                                          
*&&UK*&& CLI   TIMTTYP,TIMTCB                                                   
*&&UK*&& BNE   DRROUTE                                                          
         L     R2,AFIELDH                                                       
         LA    R1,8(R2)                                                         
         TM    TIMRBSTA,TIMROINC   INCOME ACCOUNT OVERRIDDEN                    
         BO    *+12                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'TIMINC,R1),TIMINC                                            
         MVI   5(R2),L'LC5INC                                                   
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY NARRATIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISNAR   NTR1                                                                   
         L     R2,AFIELDH          R2=A(DISPLAY FIELD HEADER)                   
*                                                                               
         USING TSARRECD,R6                                                      
         L     R6,AIO1                                                          
         SR    R3,R3                                                            
         ICM   R3,3,TRKERROR                                                    
         BZ    DISNAR30                                                         
         CLI   BOTSCR,X'C4'        BILLABLE W/COMMENTS                          
         BNE   DISNAR10                                                         
         MVC   BCHALF,TRKERROR                                                  
         NC    BCHALF,=Y(TRKETAX)                                               
         BNZ   DISNAR20                                                         
         DROP  R6                                                               
*                                                                               
DISNAR10 CLI   BOTSCR,X'C3'        NON BILLABLE W/COMMENTS                      
         BNE   DISNAR30                                                         
*                                                                               
DISNAR20 OI    1(R2),X'20'                                                      
         GOTO1 AGETERR,(R3)                                                     
         SR    RF,RF                                                            
         IC    RF,0(R2)            RF=LENGTH OF DISPLAY FIELD                   
         SH    RF,=H'9'                                                         
         BM    DISNAR30                                                         
         MVC   8(0,R2),BCWORK                                                   
         EX    RF,*-6                                                           
         OI    1(R2),X'08'         HIGH INTENSITY                               
         B     DRROUTE                                                          
*                                                                               
DISNAR30 CLI   SVOPT2,COOPTYQ      XD=Y                                         
         BNE   DISNAR80                                                         
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEMENT                        
         LA    R6,BCWORK                                                        
         MVC   BCWORK,BCSPACES                                                  
         OI    1(R2),X'20'                                                      
*&&US                                                                           
         TM    TIMSTAT,TIMDRPND    IS IT PENDING?                               
         BNO   DISNAR40                                                         
         MVC   BCFLDS,BCSPACES                                                  
         USING TSARRECD,RF                                                      
         L     RF,AIO1                                                          
         MVC   BCFLD1(24),=CL24'PENDING(BBBBBB-NNNDD/YY)'                       
         MVC   BCFLD1+8(6),TRKREF  REFERENCE NUMBER                             
         GOTO1 DATCON,BCDMCB,(1,TRKDTE),(17,BCFLD1+15)   BATCH DATE             
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
         DROP  RF                                                               
*&&                                                                             
DISNAR40 DS    0H                                                               
         MVC   BCFLDS,BCSPACES                                                  
         L     RF,AIO1                                                          
         USING TSARRECD,RF                                                      
         CLC   TRKCNTRA,BCSPACES                                                
         BH    *+14                                                             
         MVC   0(09,R6),=C'*NO C/A*,'                                           
         B     DISNAR50                                                         
         MVC   BCFLD1(L'TRKCNTRA),TRKCNTRA                                      
         DROP  RF                                                               
         GOTO1 AFORMAT                                                          
*                                                                               
         USING ACTRECD,R5                                                       
         LA    R5,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         SR    R1,R1                                                            
         IC    R1,BCFLDSLQ                                                      
         SH    R1,=H'2'            DON'T INCLUDE THE COMMA                      
         BNP   DREINVC                                                          
         MVC   ACTKULA(0),BCFLD1                                                
         EX    R1,*-6                                                           
*                                                                               
         MVC   BCFULL,AIO          SAVE OFF IO AREA                             
         MVC   AIO,AIO3                                                         
         GOTO1 AGETACT,0                                                        
         BNE   DREINVC                                                          
         DROP  R5                                                               
*                                                                               
         BAS   RE,DISEXM                                                        
                                                                                
         MVC   AIO,BCFULL          RESTORE IO ADDRESS                           
*                                                                               
DISNAR50 DS    0H                  *** COST RATE *** (UK ONLY)                  
*&&UK                                                                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BE    *+12                                                             
         CLI   TIMLN,TIMB2LNQ      LENGTH FOR BILLABLE INPUT(W/ EURO)           
         BNE   DISNAR60                                                         
         OC    TIMCRATE,TIMCRATE   TEST RATE SET                                
         BZ    DISNAR60            NO - MAY BE DRAFT MCS                        
         MVC   BCFLDS,BCSPACES                                                  
         MVC   BCFLD1(L'BC@CRATE),BC@CRATE                                      
         CURED TIMCRATE,(10,BCFLD2),2,ALIGN=LEFT                                
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
*&&                                                                             
DISNAR60 DS    0H                   *** OFFICE = XX ***                         
         MVC   BCFLDS,BCSPACES                                                  
         TM    BCCPYST1,CPYSOROE   IS OFFICE REQUIRED? UK'S OPTIONAL            
         BNO   DISNAR65            NO - DON'T PUT IN XD                         
         MVC   BCFLD1(L'BC@OFFC),BC@OFFC                                        
         MVC   BCFLD2(L'TIMOFF),TIMOFF                                          
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
*                                                                               
DISNAR65 CLI   TWAOFFC,C'*'        DDS TERMINALS ONLY:                          
         BNE   DISNAR70                                                         
         MVC   BCFLDS,BCSPACES                                                  
         MVC   BCFLD1(L'BC@ACTYD),BC@ACTYD                                      
         GOTO1 DATCON,BCDMCB,(1,TIMADAT),(X'20',BCFLD2)                         
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
*                                                                               
         ICM   RF,15,ATIMELEM      TEST ETIME TIME ELEMENT EXISTS               
         BZ    DISNAR68                                                         
DN       USING TIMELD,RF                                                        
*&&UK                                                                           
         TM    DN.TIMEPST1,TIMESAPR+TIMESREJ                                    
         BZ    DISNAR66                                                         
         MVC   0(4,R6),=C'APR,'    SHOW APPROVED/REJECTED IF SET                
         TM    DN.TIMEPST1,TIMESAPR                                             
         BNZ   *+10                                                             
         MVC   0(4,R6),=C'REJ,'                                                 
         LA    R6,4(R6)                                                         
*&&                                                                             
DISNAR66 OC    AMATELEM,AMATELEM   TEST ETIME MATERIALS EL(S)                   
         BZ    *+14                                                             
         MVC   0(4,R6),=C'MAT,'                                                 
         LA    R6,4(R6)                                                         
         MVC   BCFLDS,BCSPACES                                                  
         MVC   BCFLD1(3),=C'CAP'   CLIENT APPROVER PID                          
         MVC   BCHALF,DN.TIMEPIDC                                               
         DROP  DN                                                               
         GOTO1 HEXOUT,BCDMCB,BCHALF,BCFLD2,L'BCHALF                             
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
         SR    RE,RE                                                            
DISNAR68 MVC   BCFLDS,BCSPACES                                                  
         MVC   BCFLD1(L'BC@DA),BC@DA  DISK ADDRESS                              
         L     RF,AIO1                                                          
         MVC   BCFULL,TRKDA-TSARRECD(RF)                                        
         GOTO1 HEXOUT,BCDMCB,BCFULL,BCFLD2,L'BCFULL                             
         GOTO1 AFORMAT                                                          
         BAS   RE,DISEXM                                                        
         MVC   0(5,R6),=C'LINE='      T/S LINE NUMBER                           
         CURED (B2,TIMLINE#),(10,5(R6)),0,ALIGN=LEFT                            
*                                                                               
DISNAR70 DS    0H                                                               
         LA    RF,BCWORK+L'BCWORK-1                                             
         LA    R0,L'BCWORK                                                      
         CLI   0(RF),C','                                                       
         BE    *+16                                                             
         SH    RF,=H'1'                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
         MVI   0(RF),C' '           STRIP OFF LAST COMMA                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R2)            RF=LENGTH OF DISPLAY FIELD                   
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    RF,=H'8'                                                         
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   8(0,R2),BCWORK                                                   
         EX    RF,*-6                                                           
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
         B     DRROUTE                                                          
*                                                                               
         USING TIMELD,R3                                                        
DISNAR80 ICM   R3,15,ANARELEM      NARRATIVE DETAIL ELEMENT                     
         BZ    DRROUTE                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R2)            RF=LENGTH OF DISPLAY FIELD                   
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    RF,=H'8'                                                         
*                                                                               
         SR    R1,R1               R1=LENGTH ELEMENT DATA                       
         IC    R1,TIMLN                                                         
         SH    R1,=Y(TIMHLNQ)                                                   
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CR    R1,RF               IF L'NARRATIVE > L'DISPLAY FIELD             
         BL    *+6                                                              
         LR    R1,RF               THEN USE L'DISPLAY FIELD FOR EXMVC           
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   8(0,R2),TIMNARR                                                  
         EX    R1,*-6                                                           
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         B     DRROUTE                                                          
         DROP  R3                                                               
                                                                                
DISEXM   SR    RF,RF               MOVE NARRATIVE ITEM INTO DISP LINE           
         IC    RF,BCFLDSLQ                                                      
         AHI   RF,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R6),BCFLD1                                                   
         EX    RF,*-6                                                           
         LA    R6,1(RF,R6)         INCREMENT R6                                 
         BR    RE                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* INSERT COMMAS BETWEEN EACH XD ITEM                                  *         
***********************************************************************         
         SPACE 1                                                                
COMMA    NTR1                                                                   
         LA    R0,L'BCWORK                                                      
         LA    R6,BCWORK+L'BCWORK-1                                             
         CLI   0(R6),X'40'                                                      
         BH    *+12                                                             
         SH    R6,=H'1'                                                         
         BCT   R0,*-12                                                          
         MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* DISPLAY CLIENT/PRODUCT NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNAME  NTR1                                                                   
         L     R2,AFIELDH                                                       
         OI    1(R2),X'20'                                                      
         MVC   BLOCK(100),BCSPACES                                              
*                                                                               
         USING TSARRECD,R6                                                      
         L     R6,AIO1                                                          
         SR    R3,R3                                                            
         ICM   R3,3,TRKERROR                                                    
         BZ    DISNAM5                                                          
         GOTO1 AGETERR,(R3)                                                     
         MVC   BLOCK(100),BCWORK   ERROR RETURNED IN BCWORK                     
         OI    1(R2),X'08'         HIGH INTENSITY                               
         B     DISNAM10                                                         
*                                                                               
DISNAM5  MVC   BLOCK(L'SVSJCLNM),SVSJCLNM                                       
         CLC   SVSJPRNM,BCSPACES                                                
         BNH   DISNAM10                                                         
         MVI   BLOCK+L'SVSJCLNM+2,C'/'                                          
         MVC   BLOCK+L'SVSJCLNM+5(L'SVSJPRNM),SVSJPRNM                          
         GOTO1 SQUASHER,BCDMCB,BLOCK,100                                        
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
*                                                                               
DISNAM10 LH    R1,FIELDLEN                                                      
         STC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   8(0,R2),BLOCK                                                    
         EX    R1,*-6                                                           
         B     DRROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY TAX CONTROL FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
DISTAXF  NTR1                                                                   
         L     R2,AFIELDH                                                       
         OI    1(R2),X'20'         PROTECT FIELD                                
*                                                                               
         USING TIMELD,R3                                                        
DTAXF5   L     R3,AINPELEM                                                      
         TM    TIMSTAT,TIMNOTAX    TAX SUPPRESSED                               
         BNO   DTAXF10                                                          
         MVI   8(R2),C'N'                                                       
         MVI   5(R2),1                                                          
         B     DTAXF20                                                          
*                                                                               
DTAXF10  SR    R3,R3                                                            
         ICM   R3,15,ATAXELEM      TAX ELEMENT                                  
         BZ    DRROUTE                                                          
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(2),=C'*Y'                                                 
         LA    R1,BCWORK                                                        
         TM    TIMTSTA,TIMTOVR     WAS TAX INFO OVERRIDDEN                      
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   8(L'LC6TAXF,R2),0(R1)                                            
         MVI   5(R2),L'LC6TAXF                                                  
*                                                                               
DTAXF20  OI    6(R2),X'80'         TRANSMIT                                     
         B     DRROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TAX INFORMATION                                             *         
***********************************************************************         
         SPACE 1                                                                
DISTAX   NTR1                                                                   
         USING TIMELD,R3                                                        
         SR    R3,R3                                                            
         ICM   R3,15,ATAXELEM      TAX ELEMENT                                  
         BZ    DRROUTE                                                          
*                                                                               
         L     R2,AFIELDH          R2 = A(TAX BASIS)                            
         MVC   BCWORK,BCSPACES                                                  
         CURED TIMTBAS,(L'LC6BAS,BCWORK),2,FLOAT=-,ALIGN=LEFT                   
         MVC   8(L'LC6BAS,R2),BCWORK                                            
         MVI   5(R2),L'LC6BAS                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         SR    R1,R1               R2 = A(LOCALITY)                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   8(L'LC6LOC,R2),TIMTLOC                                           
         MVI   5(R2),L'LC6LOC                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         SR    R1,R1               R2 = A(TAX WORKCODE)                         
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   8(L'LC6WRK,R2),TIMTWC                                            
         MVI   5(R2),L'LC6WRK                                                   
         OI    6(R2),X'80'                                                      
         B     DRROUTE                                                          
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY TEMPO LINE # - TEMPO SCREEN ONLY                            *         
***********************************************************************         
         SPACE 1                                                                
DISTMP#  NTR1                                                                   
         USING TIMELD,R3                                                        
         SR    R3,R3                                                            
         ICM   R3,15,AXSTELEM      EXTRA STATUS ELEMENT                         
         BZ    DRROUTE                                                          
*                                                                               
         L     R2,AFIELDH          R2 = A(TAX BASIS)                            
         MVC   BCWORK,BCSPACES                                                  
         SR    R0,R0                                                            
         ICM   R0,3,TIMXTLN#                                                    
         CURED (R0),(L'LC8TLN#,BCWORK),0,ALIGN=LEFT                             
         MVC   8(L'LC8TLN#,R2),BCWORK                                           
         MVI   5(R2),L'LC8TLN#                                                  
         OI    6(R2),X'80'                                                      
         B     DRROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY SAVE LINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISSAVE  NTR1                                                                   
         USING TIMELD,R3                                                        
         SR    R3,R3                                                            
         ICM   R3,15,ASAVELEM      SAVED DATA ELEMENT                           
         BZ    DRROUTE                                                          
*                                                                               
         USING DISTABD,RF                                                       
         L     RF,BCADDR           A(VALIDATION/DISPLAY ENTRY)                  
         OC    DTFLEN,DTFLEN       FIELD ON SCREEN                              
         BZ    DRROUTE                                                          
         MVC   BCHALF2,DTFLEN      FIELD LENGTH                                 
         MVC   BCHALF,DTINPFLD     INPUT FIELD IDENTIFICATION #                 
         DROP  RF                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,TIMFMINI       # MINI ELEMENTS                              
         BZ    DRROUTE                                                          
DSAVE50  CLC   TIMFNUM,BCHALF      MATCH ON FIELD #                             
         BE    DSAVE100                                                         
         SR    RF,RF                                                            
         IC    RF,TIMFLEN                                                       
         AR    R3,RF               BUMP TO NEXT MINI ELEMENT                    
         BCT   R0,DSAVE50                                                       
         B     DRROUTE             NO MATCH                                     
*                                                                               
DSAVE100 L     R2,AFIELDH          R2 = A(DISPLAY FIELD)                        
         SR    RF,RF                                                            
         IC    RF,TIMFLEN                                                       
         SH    RF,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         CH    RF,BCHALF2                                                       
         BL    *+12                                                             
         ICM   RF,3,BCHALF2                                                     
         SH    RF,=H'1'                                                         
         LA    RE,TIMFIELD                                                      
*                                                                               
         CLI   PFKEY,PFUNMARK                                                   
         BNE   DSAVE200                                                         
         CLC   TIMFNUM,=AL2(FLDTYPE)                                            
         BNE   DSAVE200                                                         
         CLI   TIMFIELD,C'?'                                                    
         BE    *+12                                                             
         CLI   TIMFIELD,C'*'                                                    
         BNE   DSAVE200                                                         
         LA    RE,TIMFIELD+1                                                    
         SH    RF,=H'1'                                                         
         BM    DRROUTE                                                          
*                                                                               
DSAVE200 DS    0H                                                               
         MVC   8(0,R2),0(RE)                                                    
         EX    RF,*-6                                                           
         B     DRROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD EXIT POINTS                                          *         
***********************************************************************         
         SPACE 1                                                                
DRROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     DRROUTCC                                                         
DRROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     DRROUTCC                                                         
DRROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
DRROUTCC CLI   BCDUB,1                                                          
DREXIT   XIT1                                                                   
*                                                                               
DRIERRX  MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
*                                                                               
DRAERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         GOTO1 MYERR                                                            
*                                                                               
DRGERRX  GOTO1 ATSAR,BCDMCB,TSASAV,1      SAVE OFF TSAR RECORDS                 
         ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT+USMYOK                                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVI   GTMTYP,C'I'         MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
         CLI   SVTMSYS,X'00'                                                    
         BE    *+10                                                             
         MVC   GTMSYS,SVTMSYS                                                   
         B     DRROUTE                                                          
         DROP  RF                                                               
*                                                                               
DREINVC  MVC   GERROR,=AL2(ACEACCT)        INVALID ACCOUNT                      
         MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         NI    4(R2),X'FF'-X'20'   FIELD IS INVALID                             
         MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R3,R4,R8,RA,RC                                                   
         EJECT                                                                  
***********************************************************************         
* CHECK IF SECURITY IS SET                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
*&&US                                                                           
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
CHKSEC   NTR1  BASE=*,LABEL=*                                                   
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    CHKSX               YES-DONT CHECK SECURITY                      
         CLI   BOTSCR,X'C4'        FORCE MISSING INPUT ERROR ON                 
         JE    CHKS10              BILLABLE SCREENS                             
         CLI   BOTSCR,X'C5'                                                     
         JE    CHKS10                                                           
         CLI   BOTSCR,X'C9'                                                     
         JNE   CHKSX                                                            
*                                                                               
CHKS10   L     R5,ALINE1           R5=BEGINNING OF LINE                         
         MVI   BCFLAG5,0                                                        
         MVI   BCBYTE1,RTAMFLDQ                                                 
         BRAS  RE,FLDSEC           SECURITY TO VIEW RATE/AMNT                   
         JE    *+16                          NO SECURITY AT ALL                 
         MVI   BCFLAG5,BCFLRTAM    ASSUME RATE/AMT HAS FULL SECURITY            
         JL    *+8                   IF LOW - FULL SECURITY                     
         MVI   BCFLAG5,BCFLRARD    RATE/AMT HAS READ ONLY SECURITY              
*                                                                               
         MVI   BCBYTE1,INCFLDQ                                                  
         BRAS  RE,FLDSEC           SECURITY TO VIEW INCOME                      
         JE    CHKS15                        NO SECURITY AT ALL                 
         JH    *+12                                                             
         OI    BCFLAG5,BCFLINC     ASSUME INCOME HAS FULL SECURITY              
         J     CHKS15                IF LOW - FULL SECURITY                     
         NI    BCFLAG5,X'FF'-BCFLINC ELSE TURN OFF FULL SECURITY                
         OI    BCFLAG5,BCFLINRD      INCOME HAS READ ONLY SECURITY              
*                                                                               
CHKS15   CLI   BCFLAG5,0           ANY SECURITY ISSUES?                         
         JE    CHKSX                                                            
*                                                                               
         USING DISTABD,R3                                                       
CHKS20   L     R3,AVDTAB           DISPLAY TABLE FOR THIS SCREEN                
CHKS30   CLI   DTFDISP,X'FF'       END OF TABLE                                 
         JE    CHKS70                                                           
         OC    DTFLEN,DTFLEN       IS FIELD ON SCREEN                           
         JZ    CHKS60              NO                                           
*                                                                               
         LH    R1,DTFDISP          DISPLACEMENT TO FIELD IN LINE                
         AR    R1,R5               R5=BEGINNING OF LINE                         
         ST    R1,AFIELDH          A(FIELD HEADER)                              
*                                                                               
         MVC   BCHALF,DTINPFLD     SAVE OFF INP FLD FOR MANIPULATION            
         TM    BCFLAG5,BCFLRTAM    FULL SECURITY ON RATE/AMOUNT?                
         JO    *+12                                                             
         TM    BCFLAG5,BCFLRARD    READ ONLY SECURITY ON RATE/AMOUNT?           
         JNO   CHKS40                                                           
         NC    BCHALF,=AL2(FLDRATE)                                             
         JZ    CHKS40                                                           
         OI    1(R1),X'20'         PROTECT                                      
         TM    BCFLAG5,BCFLRTAM    IS THERE FULL SECURITY ON FIELD              
         JZ    *+8                 NO - JUST LEAVE PROTECTED                    
         OI    1(R1),X'0C'         LOW INTENSITY                                
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
CHKS40   MVC   BCHALF,DTINPFLD     SAVE OFF INP FLD FOR MANIPULATION            
         TM    BCFLAG5,BCFLRTAM    FULL SECURITY ON RATE/AMOUNT?                
         JO    *+12                                                             
         TM    BCFLAG5,BCFLRARD    READ ONLY SECURITY ON RATE/AMOUNT?           
         JNO   CHKS50                                                           
         NC    BCHALF,=AL2(FLDAMNT)                                             
         JZ    CHKS50                                                           
         OI    1(R1),X'20'         PROTECT                                      
         TM    BCFLAG5,BCFLRTAM    IS THERE FULL SECURITY ON FIELD              
         JZ    *+8                 NO - JUST LEAVE PROTECTED                    
         OI    1(R1),X'0C'         LOW INTENSITY                                
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
CHKS50   MVC   BCHALF,DTINPFLD     SAVE OFF INP FLD FOR MANIPULATION            
         TM    BCFLAG5,BCFLINC     FULL SECURITY ON INCOME ACCT?                
         JO    *+12                                                             
         TM    BCFLAG5,BCFLINRD    SECURITY ON INCOME ACCOUNT?                  
         JNO   CHKS60                                                           
         NC    BCHALF,=AL2(FLDINC)                                              
         JZ    CHKS60                                                           
         OI    1(R1),X'20'         PROTECT                                      
         TM    BCFLAG5,BCFLINC     IS THERE FULL SECURITY ON FIELD              
         JZ    *+8                 NO - JUST LEAVE PROTECTED                    
         OI    1(R1),X'0C'         LOW INTENSITY                                
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
CHKS60   LA    R3,DTLENQ(R3)       BUMP TO NEXT FIELD                           
         J     CHKS30                                                           
*                                                                               
         USING SCRNTABD,R1                                                      
CHKS70   L     R1,ASCRNITM                                                      
         SR    RF,RF                                                            
         ICM   RF,3,SCRNLEN        RF=LENGTH OF SCREEN LINE                     
         AR    R5,RF               R5=A(CURRENT SCREEN LINE)                    
         DROP  R1                                                               
*                                                                               
         L     R1,ALSTLINE         EXIT IF LAST LINE ON SCREEN                  
         CR    R5,R1                                                            
         JNH   CHKS20                                                           
*                                                                               
CHKSX    B     CKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R3,R4,R8,RA,RC                                                   
         EJECT                                                                  
**********************************************************************          
* CHECK FIELD SECURITY TO DISPLAYING RATE                            *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
FLDSEC   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R2,BCBYTE1                                                       
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    CKROUTE                                                          
         CLI   DMCB,SECPREAD                                                    
         BE    CKROUTH                                                          
         B     CKROUTL                                                          
         EJECT                                                                  
***********************************************************************         
* FIELD SECURITY EXIT POINTS                                          *         
***********************************************************************         
         SPACE 1                                                                
CKROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     CKROUTCC                                                         
CKROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     CKROUTCC                                                         
CKROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
CKROUTCC CLI   BCDUB,1                                                          
CKEXIT   XIT1                                                                   
         DROP  R4,R8,RA,RC                                                      
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* CALL TIMETRN TO MAKE POSTINGS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
CALLTTRN NTR1  BASE=*,LABEL=*                                                   
         USING TTRND,R1                                                         
         LA    R1,TTRNBLK          TIMETRN CONTROL BLOCK                        
         XC    TTRNBLK,TTRNBLK                                                  
*        MVI   TTACTION,TTBLDFIL   ACTION = BUILD ORIGINAL BUFFER               
*        TM    STATUS2,TRNBUILD     OR                                          
*        BO    *+8                 ACTION = MAKE POSTINGS FROM TSAR             
         MVI   TTACTION,TTPOST+TTBLDTMS                                         
         TM    STATUS2,TRNCMPAR                                                 
         BNO   *+8                                                              
         MVI   TTACTION,TTCMPARE+TTBLDTMS                                       
         MVC   TTDMGR,DATAMGR      A(DATAMGR)                                   
         MVC   TTCFACS,ACOMFACS    A(COMFACS)                                   
         MVC   TTRECUP,VRECUP      A(RECUP)                                     
         MVC   TTADDTRN,AADDTRN    A(ADDTRN)                                    
         MVC   TTSORT,AXSORT       A(XSORT)                                     
         MVC   TTBUFF1,ABUFF1      A(BUFFER#1)                                  
         MVC   TTBUFF2,ABUFF2      A(BUFFER#2)                                  
         MVC   TTATSAR,ATSAR       A(TSAR MAINTENANCE ROUTINE)                  
         MVC   TTAIO,AIO1          A(IO AREA FOR TSAR ROUTINE)                  
*&&US*&& MVC   TTTMSUPD,ATMSUPD    A(TMSUPD)                                    
*&&US*&& MVC   TTTMSUBK,ATMSUBLK   A(TMS UPDATE BLOCK)                          
         MVC   TTBUFFLN,=Y(BCBUFLNQ/2)                                          
         MVC   TTUSERID,TWAORIG    USER ID NUMBER                               
         MVC   TT1RLNQS,BC1RLNQS   1R LEDGER STRUCTURE                          
         MVC   TTACCODE,SV1RACT    1R ACCOUNT CODE                              
         MVC   TTSJLNQS,BCSJLNQS   SJ LEDGER STRUCTURE                          
         MVC   TTYYMMDD,PRDENDTE   PERIOD ENDING DATE                           
         TM    STATUS2,DAILYTIM    IF USING DAILY TIME PASS THE ACTUAL          
         BZ    *+14                DATE OF THE TIME INSTEAD                     
         MVC   TTYYMMDD,SVYYMMDD                                                
         OI    TTSTAT1,TTSTDALY    DAILY TIME                                   
         MVC   TTPREDTE,PRDENDTE   PERIOD ENDING DATE                           
         MVC   TTPERIOD,PRDNUM     PERIOD NUMBER                                
         MVC   TTTID,BCTID         TERMINAL ID                                  
         MVC   TTPIDNO,SVPIDNO     PERSONAL ID #                                
         MVC   TTSECPID,SECPID#    USER PID #                                   
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+16                                                             
         CLI   BOTSCR,X'C8'                                                     
         BNE   *+8                                                              
         OI    TTSTAT1,TTSTTMPO    UPLOAD THROUGH TEMPO                         
         MVC   TTCTRY,CTRY         COUNTRY CODE                                 
         MVC   TTCPYST1,BCCPYST1   COMPANY STATUS BYTE #1                       
         MVC   TTCPYST2,BCCPYST2   COMPANY STATUS BYTE #2                       
         MVC   TTCPYST3,BCCPYST3   COMPANY STATUS BYTE #3                       
         MVC   TTCPYST4,BCCPYST4   COMPANY STATUS BYTE #4                       
         MVC   TTCPYST5,BCCPYST5   COMPANY STATUS BYTE #5                       
         MVC   TTCPYST6,BCCPYST6   COMPANY STATUS BYTE #6                       
         MVC   TTCPYST7,BCCPYST7   COMPANY STATUS BYTE #7                       
         MVC   TTCPYST8,BCCPYST8   COMPANY STATUS BYTE #8                       
         MVC   TTCPYST9,BCCPYST9   COMPANY STATUS BYTE #9                       
         MVC   TTCPYSTA,BCCPYSTA   COMPANY STATUS BYTE #10                      
*&&US                                                                           
         MVC   TTCPYSTB,BCCPYSTB   COMPANY STATUS BYTE #11                      
         MVC   TTCPYGLM,BCCPYGLM   COMPANY GL MOA                               
*&&                                                                             
         MVI   TTPROF1,TTPR1SAL    DEFAULT TO USE SALES RATE                    
         TM    SVDFBRTE,SVDFRCST                                                
         BNO   *+8                                                              
         MVI   TTPROF1,TTPR1CST    USE COST RATE                                
         MVI   TTPROF2,TTPR1SAL    DEFAULT TO USE SALES RATE                    
         TM    SVDFRRTE,SVDFRCST                                                
         BNO   *+8                                                              
         MVI   TTPROF2,TTPR1CST    USE COST RATE                                
         TM    STATUS2,MCSTIME     *** TEST UPDATING MCS TIME ***               
         BZ    CALTT02                                                          
         OI    TTSTAT1,TTSTTIME    MCS TIME                                     
         MVC   TTTSSTAT,BCTIMST1   SET T/SHEET STATUS FROM SAVED TIMREC         
         MVC   TTTSPRST,BCTIMST1   ADJUSTMENTS DON'T ALTER T/SHEET STS          
*                                                                               
         MVC   TTPROF3,SVTUP       'UPDATE TIME WHEN T/S STS=X' PROF            
*                                                                               
CALTT02  DS    0H                                                               
*&&UK                                                                           
         TM    BCFLAG5,BCFLSEC     DO WE HAVE A 2ND CURRENCY                    
         BNO   *+10                                                             
         MVC   TTCURCD,CURCD       SAVE CURRENCY CODES                          
*&&                                                                             
*                                                                               
         GOTO1 ATIMETRN,BCDMCB,TTRNBLK                                          
         LA    R1,TTRNBLK                                                       
         TM    TTRETURN,TTRUPDTE   FORCE UPDATE                                 
         BO    CALLTH                                                           
         TM    TTRETURN,TTRBUFFR                                                
         BNO   CALLTE                                                           
         DC    H'0'                IF DIED HERE THEN BUFFER TOO SMALL           
*                                                                               
CALLTH   MVI   BCDUB,2             SET CC HIGH                                  
         B     CALLTCC                                                          
CALLTE   MVI   BCDUB,1             SET CC EQUAL                                 
CALLTCC  CLI   BCDUB,1                                                          
         XIT1                                                                   
         DROP  R1                  MUST MAKE BCBUFF1 & BCBUFF2 BIGGER           
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCREEN MANIPULATION NMOD                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
SCREENS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BCFLAG1,0                                                        
         MVI   SCRNSTAT,0                                                       
         CLC   TOPSCR,TWASCR       SAME SCREEN                                  
         BE    *+8                                                              
         OI    SCRNSTAT,SCRNCHNG   SCREEN HAS CHANGED                           
         MVC   TOPSCR,TWASCR                                                    
*                                                                               
         LA    R1,ADDLAST                                                       
         CLI   TOPSCR,X'C0'        TOPSCREEN - ACTION = ADD                     
         BE    *+8                                                              
         LA    R1,CHALAST          TOPSCREEN - ACTION = CHANGE                  
         XC    BCDMCB(12),BCDMCB                                                
         ST    R1,BCDMCB           WHERE TO LOAD BOTTOM SCREEN                  
         SR    R1,RA                                                            
         STH   R1,DBOTSCR          DISPLACEMENT TO BOTTOM SCREEN                
*                                                                               
         USING SCRNTABD,R3                                                      
         L     R3,ASCRNTAB                                                      
SCREEN5  CLC   PFKEY,SCRNPFK       GET SCREEN ASSOCIATED WITH PFKEY             
         BNE   SCREEN8                                                          
*&&US*&& CLI   SCRNNUM,X'C6'       ONLY CAN PF TO TAX SCREEN                    
*&&US*&& BE    *+12                IF ON A DDS TERMINAL                         
         CLI   SCRNNUM,X'C8'       ONLY CAN PF TO TEMPO UPLOAD SCREEN           
         BNE   SCREEN6             IF ON A DDS TERMINAL                         
         CLI   TWAOFFC,C'*'                                                     
         BNE   SCREEN8                                                          
SCREEN6  MVC   BOTSCR,SCRNNUM                                                   
         B     SCREEN10                                                         
*                                                                               
SCREEN8  LA    R3,SCRNTLNQ(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   SCREEN5                                                          
*                                                                               
         CLI   BOTSCR,0            FIRST TIME IN SET DEFAULT                    
         BNE   SCREEN10                                                         
         TM    GFACTST6,X'40'      RUNNING UNDER $SCRIPT UPLOAD                 
         BNO   *+12                                                             
         MVI   BOTSCR,X'C8'        UPLOAD SCREEN                                
         B     *+10                                                             
         MVC   BOTSCR,SVDFSCRN     LOAD DEFAULT SCREEN                          
         OI    SCRNSTAT,SCRNCHNG   SCREEN HAS CHANGED                           
         CLI   BOTSCR,0                                                         
         BNE   SCREEN30                                                         
         MVI   BOTSCR,X'C2'                                                     
         MVI   BCFLAG1,X'FF'                                                    
         B     SCREEN30                                                         
*                                                                               
SCREEN10 TM    SCRNSTAT,SCRNCHNG   OTHERWISE, HAS THE SCREEN CHANGED            
         BO    SCREEN30                                                         
SCREEN20 CLC   SVBOTSCR,BOTSCR     NO CHANGE SKIP CALLOV                        
         BE    SCREEN35                                                         
         OI    SCRNSTAT,SCRNCHNG                                                
*                                                                               
SCREEN30 MVC   BCDMCB(1),BOTSCR    LOAD SCREEN                                  
         GOTO1 CALLOV,BCDMCB                                                    
         CLI   BCDMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SCRNTABD,R3                                                      
SCREEN35 L     R3,ASCRNTAB                                                      
SCREEN40 CLC   BOTSCR,SCRNNUM                                                   
         BE    SCREEN45                                                         
         LA    R3,SCRNTLNQ(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   SCREEN40                                                         
         DC    H'0'                SCREEN NOT DEFINED CORRECTLY                 
*                                                                               
SCREEN45 SR    R2,R2               POINT R2 @ PFKEY LINE                        
         ICM   R2,3,SCRNPFKY                                                    
         AH    R2,DBOTSCR                                                       
         AR    R2,RA                                                            
         GOTO1 INITIAL,BCDMCB,(X'40',0),(R2) INITIALIZE PFKEY LINE              
         NI    STATUS,X'FF'-DISPED MAKE SURE DISPLAYED                          
*                                                                               
SCREEN55 MVC   SVBOTSCR,BOTSCR                                                  
*                                                                               
SCREENX  XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT INFORMATION FROM OLD TSAR RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TSARRECD,R6                                                      
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
EXTRACT  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1             R6=A(TSAR RECORD)                            
         MVC   SVTS#NXT,TRKTSNUM   SAVE OLD TIMESHEET #                         
         MVC   SVKSTAT,TRKSTAT     TSAR KEY STATUS                              
         NI    SVKSTAT,TRKSUPDT                                                 
*                                                                               
         USING TIMELD,R3                                                        
EXTR5    LA    R3,TRDATA           R3=A(START OF 8B ELEMENT CLUSTER)            
*                                                                               
EXTR10   CLI   0(R3),TIMELQ        X'8B' ELEMENT                                
         BNE   EXTR60                                                           
         CLI   TIMETYP,TIMEINP     *** INPUT DETAIL ELEMENT ***                 
         BNE   EXTR20                                                           
         TM    LINESTAT,LBSAVE                                                  
         BNO   EXTR15                                                           
         GOTO1 ACONVERT,(R3)                                                    
         MVC   TRDATA(L'BCELEM),BCELEM                                          
         B     EXTR5                                                            
*                                                                               
EXTR15   CLI   SVTYPE,TIMTCR       ONLY EXTRACT DATA FOR B/R TIME               
         BH    EXTR50                                                           
         CLI   TIMTTYP,TIMTCR                                                   
         BH    EXTR50                                                           
*&&US                                                                           
         MVC   BCHALF,ONSCREEN                                                  
         NC    BCHALF,=AL2(FLDTAXF)   TAX CONTROL FIELD ON SCREEN               
         BNZ   EXTR12                                                           
         TM    TIMSTAT,TIMNOTAX                                                 
         BNO   *+8                                                              
         OI    SVTAXSTA,SVTAXNO    NO TAX FOR THIS ITEM                         
         TM    TIMSTAT,TIMRQTAX                                                 
         BNO   *+8                                                              
         OI    SVTAXSTA,SVTAXREQ   ELIGIBLE FOR TAX                             
*&&                                                                             
*                                                                               
EXTR12   MVC   BCHALF,ONSCREEN                                                  
         NC    BCHALF,=AL2(FLDRATE)     WAS RATE ON SCREEN                      
         BNZ   EXTR16                                                           
         TM    TIMRBSTA,TIMRORAT   IF OLD ONE WAS A DEFAULT                     
         BZ    EXTR16              THEN REPLACE WITH NEW RATE                   
         MVC   SVRATE,TIMRATE      RATE                                         
         NI    SVRATIND,X'FF'-TIMRORAT-TIMRBADJ CLEAR RATE STATUS BITS          
         MVC   BYTE,TIMRBSTA                                                    
         NI    BYTE,TIMRORAT+TIMRBADJ                                           
         OC    SVRATIND,BYTE       SET RATE STATUS BITS                         
         MVC   SVRATEFF,TIMREFF    RATE EFFECTIVE DATE                          
         MVC   BCHALF,TRKERROR                                                  
         NC    BCHALF,=AL2(TRKERAT#)                                            
         NC    SVERROR,=AL2(TRKEFFS-TRKERAT#)                                   
         OC    SVERROR,BCHALF                                                   
*                                                                               
EXTR16   MVC   BCHALF,ONSCREEN                                                  
         NC    BCHALF,=AL2(FLDINC) WAS INCOME ACCOUNT ON SCREEN                 
         BNZ   EXTR18                                                           
         CLI   SVTYPE,TIMTCB       ONLY REFRESH INCOME ACC FOR BTIME            
         BNE   EXTR18                                                           
         TM    TIMRBSTA,TIMROINC   IF OLD ONE WAS A DEFAULT                     
         BZ    EXTR18              THEN REPLACE WITH NEW INCOME ACCT            
         OI    SVRATIND,TIMROINC                                                
         MVC   SVSIULA,TIMINC      INCOME ACCOUNT                               
         MVC   BCHALF,TRKERROR                                                  
         NC    BCHALF,=AL2(TRKEINC#+TRKEANL#)                                   
         NC    SVERROR,=AL2(TRKEFFS-TRKEINC#-TRKEANL#)                          
         OC    SVERROR,BCHALF                                                   
*                                                                               
EXTR18   ZAP   DUB,SVRATE                                                       
         MP    DUB,SVHOURS                                                      
         SRP   DUB,64-2,5                                                       
         ZAP   SVAMOUNT,DUB        RECALCULATE AMOUNT SINCE RATE                
         B     EXTR50              MIGHT HAVE CHANGED                           
*                                                                               
EXTR20   DS    0H                                                               
*&&US                                                                           
         CLI   TIMETYP,TIMETAX     *** TAX ELEMENT ***                          
         BNE   EXTR30                                                           
         MVC   BCHALF,ONSCREEN                                                  
         NC    BCHALF,=AL2(FLDBASIS+FLDLOCAL+FLDTAXWC)                          
         BNZ   EXTR50                                                           
         TM    SVTAXSTA,SVTAXNO    NO TAX FOR THIS ITEM                         
         BO    EXTR50                                                           
         TM    SVTAXSTA,SVTAXREQ   ELIGIBLE FOR TAX                             
         BNO   EXTR50                                                           
         TM    TIMTSTA,TIMTOVR     IF OLD ONE WAS A DEFAULT THEN                
         BZ    EXTR50              REPLACE WITH NEW TAX INFO                    
         OI    SVTAXSTA,SVTAXOVR                                                
         MVC   SVTXWC,TIMTWC       TAX WORKCODE                                 
         MVC   SVTXLOC,TIMTLOC     TAX LOCALITY                                 
         ZAP   SVTXBAS,TIMTBAS     TAX BASIS AMOUNT                             
         MVC   SVTXMINI,TIMTMINI   # MINI ELEMENTS                              
         SR    R1,R1                                                            
         ICM   R1,1,SVTXMINI                                                    
         BZ    EXTR25                                                           
         MH    R1,=Y(TIMTMINQ)                                                  
         SH    R1,=H'1'                                                         
         MVC   SVTXBLCK(0),TIMTMINS                                             
         EX    R1,*-6                                                           
EXTR25   MVC   BCHALF,TRKERROR                                                  
         NC    BCHALF,=AL2(TRKETAX)                                             
         NC    SVERROR,=AL2(TRKEFFS-TRKETAX)                                    
         OC    SVERROR,BCHALF                                                   
         B     EXTR50                                                           
*&&                                                                             
*                                                                               
EXTR30   CLI   TIMETYP,TIMENAR     *** NARRATIVE ELEMENT ***                    
         BNE   EXTR36                                                           
         MVC   BCHALF,TRKERROR                                                  
         NC    BCHALF,=Y(TRKETAX)  ALWAYS REFRESH NARRATIVE IF                  
         BNZ   EXTR32              ITEM HAS TAX ERRORS                          
         MVC   BCHALF,ONSCREEN                                                  
         NC    BCHALF,=AL2(FLDNARR)                                             
         BNZ   EXTR50                                                           
EXTR32   SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         SH    R1,=Y(TIMHLNQ+1)    LENGTH OF HEADER                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         CHI   R1,L'STCTCNAR-1                                                  
         BNH   *+8                                                              
         LHI   R1,L'STCTCNAR-1                                                  
         MVC   SVNARR(0),TIMNARR   SAVE NARRATIVE                               
         EX    R1,*-6                                                           
         LA    R1,1(R1)                                                         
         STC   R1,SVNARRLN         SAVE NARRATIVE LENGTH                        
         B     EXTR50                                                           
*                                                                               
EXTR36   CLI   TIMETYP,TIMEXTRA    *** EXTRA STATUS ELEMENT ***                 
         BNE   EXTR40                                                           
         CLI   BOTSCR,X'C8'                                                     
         BE    EXTR50                                                           
         MVC   SVTMPLN#,TIMXTLN#   TEMPO LINE NUMBER                            
         B     EXTR50                                                           
*                                                                               
EXTR40   CLI   TIMETYP,TIMEFLD     *** COMMENTED LINE ***                       
         BNE   EXTR48                                                           
         TM    LINESTAT,LBSAVE     DONT RESTORE DATA IF NOT COMMENTED           
         BNO   EXTR50                                                           
         SR    R0,R0                                                            
         ICM   R0,1,TIMFMINI       # MINI ELEMENTS                              
         BZ    EXTR60                                                           
*                                                                               
EXTR42   SR    RF,RF                                                            
         IC    RF,SVFLDBLK+(TIMFMINI-TIMELD)                                    
         LA    R1,SVFLDBLK+(TIMFITEM-TIMELD)                                    
EXTR43   CLC   1(2,R1),TIMFNUM                                                  
         BE    EXTR45                                                           
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   RF,EXTR43                                                        
*                                                                               
         LA    RE,SVFLDBLK         ADD NEW ITEM                                 
         SR    RF,RF                                                            
         IC    RF,SVFLDBLK+(TIMLN-TIMELD)                                       
         AR    RE,RF                                                            
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=H'1'                                                         
         MVC   0(0,RE),TIMFITEM                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         STC   RF,SVFLDBLK+(TIMLN-TIMELD)                                       
         IC    RF,SVFLDBLK+(TIMFMINI-TIMELD)                                    
         LA    RF,1(RF)                                                         
         STC   RF,SVFLDBLK+(TIMFMINI-TIMELD)                                    
EXTR45   SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         AR    R3,R1                                                            
         BCT   R0,EXTR42                                                        
         B     EXTR60                                                           
                                                                                
EXTR48   CLI   TIMETYP,TIMETIME    *** MCSTIME TIMEL ***                        
         BNE   EXTR50                                                           
         MVC   SVMCSRNO,TIMEIDNO   SAVE ROW NUMBER                              
         B     EXTR50                                                           
*                                                                               
EXTR50   SR    R1,R1               BUMP TO NEXT 8B ELEMENT                      
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     EXTR10                                                           
*                                                                               
EXTR60   LA    R3,TRDATA           VERIFY ALL SAVED ITEMS ARE MARKED            
EXTR70   CLI   0(R3),TIMELQ        X'8B' ELEMENT                                
         BNE   EXTRX                                                            
         CLI   TIMETYP,TIMEFLD     IS IT A COMMENTED FIELD?                     
         BNE   *+8                                                              
         OI    TRKSTAT,TRKSSAVE    MARK ITEM AS SAVED                           
*                                                                               
         SR    R1,R1               BUMP TO NEXT 8B ELEMENT                      
         IC    R1,TIMLN                                                         
         AR    R3,R1                                                            
         B     EXTR70                                                           
*                                                                               
EXTRX    XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALCULATE AND DISPLAY TOTAL HOURS NMOD                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         USING T61DFFD,RA          RA=A(TWA)                                    
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
TOTALS   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,TOTALHQ          CLEAR HOURS ACCUMULATORS                     
         LA    RF,TOTALH                                                        
         ZAP   0(L'TOTALH,RF),BCPZERO                                           
         LA    RF,L'TOTALH(RF)                                                  
         BCT   R0,*-10                                                          
         EJECT                                                                  
***********************************************************************         
* GET CURRENT AND PREVIOUS TOTALS FROM TSAR RECS                      *         
***********************************************************************         
         SPACE 1                                                                
         L     R6,AIO1                                                          
         USING TSARRECD,R6                                                      
         LA    R3,TRDATA                                                        
         USING TIMELD,R3                                                        
*                                                                               
         GOTO1 AXAIO,0                                                          
         GOTO1 ATSAR,BCDMCB,TSARDH,1        READ HIGH                           
         B     TOT10                                                            
*                                                                               
TOT10NX  GOTO1 ATSAR,BCDMCB,TSANXT,1        GET NEXT                            
TOT10    TM    BCTSERRS,TSEEOF                                                  
         BO    TOT50                                                            
         CLI   TIMETYP,TIMEFLD     COMMENTED LINE                               
         BE    TOT10NX                                                          
         TM    TRKSTAT,TRKSSAVE    DONT INCLUDE SAVED LINES                     
         BO    TOT10NX                                                          
         CLI   TIMTTYP,0           TEST TIME TYPE SET                           
         BE    TOT10NX             NO - IGNORE (MAY BE DRAFT MCS)               
         GOTO1 AFILTER,BCDMCB,(R3),(R6)                                         
         BNE   TOT10NX                                                          
*                                                                               
         USING HOURTABD,R2                                                      
         LA    R2,HOURTAB          HOUR TOTALS TABLE                            
         CLC   TIMTTYP,HRTYPE      MATCH ON TYPE OF TIME                        
         BE    TOT20                                                            
         LA    R2,HRLENQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   *-18                                                             
*&&US                                                                           
         CLI   TIMTTYP,TIMTEM      TIME-TYPE = EMPTY-TIMESHEET?                 
         BE    TOT10NX             YES: IGNORE                                  
*&&                                                                             
         DC    H'0'                                                             
*                                                                               
TOT20    CLC   TIMMOA,SVOMOA       CURRENT OR PREVIOUS ACTIVITY                 
         BL    TOT30                                                            
*                                                                               
         LA    R0,STORED           *** CURRENT ACTIVITY TOTALS ***              
         SR    R1,R1                                                            
         ZICM  R1,HRCURR1,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRCURR2,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRCURR3,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRCURR4,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         B     TOT10NX                                                          
*                                                                               
TOT30    LA    R0,STORED           *** PREVIOUS ACTIVITY TOTALS ***             
         SR    R1,R1                                                            
         ZICM  R1,HRPREV1,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRPREV2,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRPREV3,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HRPREV4,2                                                     
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         B     TOT10NX                                                          
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* GET OTHER TOTALS FROM TIME RECS (SAME WEEK-DIFF LOCATION)           *         
***********************************************************************         
         SPACE 1                                                                
         USING TSWRECD,R6                                                       
TOT50    XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CMPY                                                     
         MVC   TSWKPER,SV1RPRSN                                                 
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND                                                     
         GOTO1 HIGH                                                             
*                                                                               
TOT52    LA    R6,BIGKEY                                                        
         CLC   BIGKEY(TSWKODS-TSWKEY),KEYSAVE     SAME TO DATE                  
         BNE   TOT100                                                           
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSWKODS(0),SV1RCODE SAME LOCATION                                
         BNE   TOT60               LOOKING FOR OTHER LOCATIONS                  
TOT54    GOTO1 SEQ                                                              
         B     TOT52                                                            
*                                                                               
TOT60    GOTO1 GETREC                                                           
         USING TIMELD,R6                                                        
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
TOT62    CLI   0(R6),0                                                          
         BE    TOT54                                                            
         CLI   0(R6),TIMELQ        X'8B'                                        
         BNE   *+12                                                             
         CLI   TIMETYP,TIMEINP     GET INPUT DETAILS ELEMENT                    
         BE    TOT70                                                            
TOT65    SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     TOT62                                                            
*                                                                               
         USING HOURTABD,R2                                                      
TOT70    LA    R2,HOURTAB          HOUR TOTALS TABLE                            
         CLC   TIMTTYP,HRTYPE      MATCH ON TYPE OF TIME                        
         BE    TOT75                                                            
         LA    R2,HRLENQ(R2)                                                    
         CLI   0(R2),X'FF'         EOT                                          
         BNE   *-18                                                             
*&&US                                                                           
         CLI   TIMTTYP,TIMTEM      TIME-TYPE = EMPTY-TIMESHEET?                 
         BE    TOT54               YES: IGNORE                                  
*&&                                                                             
         DC    H'0'                                                             
*                                                                               
TOT75    LA    R0,STORED           UPDATE OTHER TOTALS                          
         SR    R1,R1                                                            
         ZICM  R1,HROTH1,2         THERE ARE 4 TOTALS TO UPDATE                 
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HROTH2,2                                                      
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HROTH3,2                                                      
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         ZICM  R1,HROTH4,2                                                      
         AR    R1,R0                                                            
         AP    0(4,R1),TIMHRS                                                   
         B     TOT65                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY TOTALS                                                      *         
***********************************************************************         
         SPACE 1                                                                
TOT100   DS    0H                  NOW DISPLAY TOTALS                           
         USING TOTTABD,R3                                                       
         LA    R3,TOTATAB          ADD TOTAL TABLE                              
         CLI   ACTEQU,ACTADD       DIFFERENT TOTALS ON ADD SCREEN               
         BE    *+8                                                              
         LA    R3,TOTCTAB          CHANGE TOTAL TABLE                           
*                                                                               
TOT110NX CLI   0(R3),X'FF'                                                      
         BE    TOT150                                                           
*                                                                               
         ZICM  R2,TOTFIELD,2       DISP TO FIELD HEADER                         
         LA    R1,CONTAGH          FROM CONTAGH                                 
         AR    R2,R1                                                            
         ZICM  R5,TOTHOURS,2       DISP TO HOURS TOTAL                          
         LA    R1,STORED           FROM STORED                                  
         AR    R5,R1                                                            
*                                                                               
         OI    6(R2),X'80'         X-MIT                                        
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   TOT130                                                           
         CURED (P4,0(R5)),(7,8(R2)),2,ZERO=NOBLANK,FLOAT=-,DECPNT=FORCE         
         B     TOT140                                                           
*                                                                               
TOT130   CURED (P4,0(R5)),(7,8(R2)),2,ZERO=NOBLANK,FLOAT=-                      
TOT140   LA    R3,TOTTBLNQ(R3)                                                  
         B     TOT110NX                                                         
*                                                                               
TOT150   CLI   ACTEQU,ACTADD       CHECK IF HOURS MATCH ON ADD                  
         BNE   TOTX                                                             
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+18                                                             
         CLI   BOTSCR,X'C8'        DONT CHANGE IF AT TEMPO UPL SCREEN           
         BE    *+10                                                             
         ZAP   HRSWRK,CURTOT       ALWAYS TOTAL FOR SCRIPT UPL                  
*                                                                               
         ZAP   HRSDIF,HRSWRK                                                    
         SP    HRSDIF,CURTOT                                                    
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   TOT160                                                           
         CURED (P4,HRSDIF),(7,ADDHDIF),2,ZERO=NOBLANK,DECPNT=FORCE              
         B     TOT170                                                           
*                                                                               
TOT160   CURED (P4,HRSDIF),(7,ADDHDIF),2,ZERO=NOBLANK                           
TOT170   OI    ADDHDIFH+6,X'80'                                                 
*                                                                               
         CP    HRSDIF,BCPZERO      DO HRS INPUT MATCH TOTAL ENTERED             
         BE    TOTX                                                             
         CLI   PFKEY,PFUPDATE      SKIP HOURS ERROR IF NOT UPDATE               
         BNE   TOTX                                                             
         LA    R2,ADDHWRKH         HOURS DON'T MATCH TOTAL ENTERED              
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         LA    RF,ADDHWRK                                                       
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LA    R1,ADDHWRK                                                       
         SR    RF,R1                                                            
         STC   RF,TIOBCURI         DISPLACEMENT INTO FIELD                      
         MVC   GERROR,=AL2(ACENOMAT)                                            
         B     TAERRX                                                           
*                                                                               
TOTX     B     TROUTE                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TOTAL EXIT POINTS                                                   *         
***********************************************************************         
         SPACE 1                                                                
TROUTL   MVI   BCDUB,0             SET CC LOW                                   
         B     TROUTCC                                                          
TROUTH   MVI   BCDUB,2             SET CC HIGH                                  
         B     TROUTCC                                                          
TROUTE   MVI   BCDUB,1             SET CC EQUAL                                 
TROUTCC  CLI   BCDUB,1                                                          
TEXIT    XIT1                                                                   
*                                                                               
TIERRX   MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
*                                                                               
TAERRX   MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         B     *+12                                                             
TGERRX   MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* TOTALS TABLES                                                       *         
***********************************************************************         
         SPACE 1                                                                
TOTCTAB  DS    0C                  TOTALS FOR CHANGE SCREEN                     
         DC    AL2(PBHOURS-STORED,CHABIL1H-CONTAGH)                             
         DC    AL2(PRHOURS-STORED,CHARTM1H-CONTAGH)                             
         DC    AL2(PNHOURS-STORED,CHANON1H-CONTAGH)                             
         DC    AL2(PRETOT-STORED,CHATOT1H-CONTAGH)                              
*                                                                               
         DC    AL2(CBHOURS-STORED,CHABIL2H-CONTAGH)                             
         DC    AL2(CRHOURS-STORED,CHARTM2H-CONTAGH)                             
         DC    AL2(CNHOURS-STORED,CHANON2H-CONTAGH)                             
         DC    AL2(CURTOT-STORED,CHATOT2H-CONTAGH)                              
*                                                                               
         DC    AL2(OBHOURS-STORED,CHABIL3H-CONTAGH)                             
         DC    AL2(ORHOURS-STORED,CHARTM3H-CONTAGH)                             
         DC    AL2(ONHOURS-STORED,CHANON3H-CONTAGH)                             
         DC    AL2(OTHTOT-STORED,CHATOT3H-CONTAGH)                              
*                                                                               
         DC    AL2(BILTOT-STORED,CHABIL4H-CONTAGH)                              
         DC    AL2(RTMTOT-STORED,CHARTM4H-CONTAGH)                              
         DC    AL2(NONTOT-STORED,CHANON4H-CONTAGH)                              
         DC    AL2(TOTHRS-STORED,CHATOT4H-CONTAGH)                              
         DC    X'FF'                                                            
*                                                                               
TOTATAB  DS    0C                  TOTALS FOR ADD SCREEN                        
         DC    AL2(CBHOURS-STORED,ADDBTIMH-CONTAGH)                             
         DC    AL2(CRHOURS-STORED,ADDRTIMH-CONTAGH)                             
         DC    AL2(CNHOURS-STORED,ADDNTIMH-CONTAGH)                             
         DC    AL2(CURTOT-STORED,ADDHINPH-CONTAGH)                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* HOURS TABLE                                                         *         
***********************************************************************         
         SPACE 1                                                                
HOURTAB  DS    0C                                                               
         DC    AL1(TIMTCB)                                                      
         DC    AL2(PBHOURS-STORED,PRETOT-STORED,BILTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(CBHOURS-STORED,CURTOT-STORED,BILTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(OBHOURS-STORED,OTHTOT-STORED,BILTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
*                                                                               
         DC    AL1(TIMTCR)                                                      
         DC    AL2(PRHOURS-STORED,PRETOT-STORED,RTMTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(CRHOURS-STORED,CURTOT-STORED,RTMTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(ORHOURS-STORED,OTHTOT-STORED,RTMTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
*                                                                               
         DC    AL1(TIMTCN)                                                      
         DC    AL2(PNHOURS-STORED,PRETOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(CNHOURS-STORED,CURTOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(ONHOURS-STORED,OTHTOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
*                                                                               
         DC    AL1(TIMTNC)                                                      
         DC    AL2(PNHOURS-STORED,PRETOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(CNHOURS-STORED,CURTOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    AL2(ONHOURS-STORED,OTHTOT-STORED,NONTOT-STORED)                  
         DC    AL2(TOTHRS-STORED)                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL DEFINITIONS                                                 *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TMS UPDATE BLOCK                                                    *         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* SCREEN TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
SCRNTAB  DC    AL1(X'C2',PF2,10,0),AL2(LC2LENQ),AL4(DISC2TAB-SCRNTAB)           
         DC    AL2(NOTLIN1H-NOTDSECH,NOTLSTH-NOTDSECH,NOTENDH-NOTDSECH)         
         DC    AL2(NOTPFKYH-NOTDSECH)                                           
*                                                                               
         DC    AL1(X'C3',PF3,10,0),AL2(LC3LENQ),AL4(DISC3TAB-SCRNTAB)           
         DC    AL2(COMLIN1H-COMDSECH,COMLSTH-COMDSECH,COMENDH-COMDSECH)         
         DC    AL2(COMPFKYH-COMDSECH)                                           
*                                                                               
         DC    AL1(X'C4',PF5,5,0),AL2(LC4LENQ),AL4(DISC4TAB-SCRNTAB)            
         DC    AL2(CABLIN1H-CABDSECH,CABLSTH-CABDSECH,CABENDH-CABDSECH)         
         DC    AL2(CABPFKYH-CABDSECH)                                           
*                                                                               
         DC    AL1(X'C5',PF4,10,0),AL2(LC5LENQ),AL4(DISC5TAB-SCRNTAB)           
         DC    AL2(BDTLIN1H-BDTDSECH,BDTLSTH-BDTDSECH,BDTENDH-BDTDSECH)         
         DC    AL2(BDTPFKYH-BDTDSECH)                                           
*                                                                               
*&&US                                                                           
         DC    AL1(X'C6',PF15,10,0),AL2(LC6LENQ),AL4(DISC6TAB-SCRNTAB)          
         DC    AL2(TAXLIN1H-TAXDSECH,TAXLSTH-TAXDSECH,TAXENDH-TAXDSECH)         
         DC    AL2(TAXPFKYH-TAXDSECH)                                           
*&&                                                                             
*                                                                               
         DC    AL1(X'C8',PF19,2,0),AL2(LC8LENQ),AL4(DISC8TAB-SCRNTAB)           
         DC    AL2(TEPLIN1H-TEPDSECH,TEPLSTH-TEPDSECH,TEPENDH-TEPDSECH)         
         DC    AL2(TEPPFKYH-TEPDSECH)                                           
*                                                                               
         DC    AL1(X'C9',PF1,5,0),AL2(LC9LENQ),AL4(DISC9TAB-SCRNTAB)            
         DC    AL2(DESLIN1H-DESDSECH,DESLSTH-DESDSECH,DESENDH-DESDSECH)         
         DC    AL2(DESPFKYH-DESDSECH)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES - DEFAULT SCREEN                          *         
***********************************************************************         
         SPACE 1                                                                
DISC2TAB DS    0C                                                               
         DC    AL2(LC2TYPEH-LC2)   DISPLACEMENT TO FIELD                        
         DC    AL2(L'LC2TYPE)      LENGTH OF FIELD                              
         DC    AL2(FLDTYPE)        INPUT BIT IF ENTERED                         
         DC    AL2(DQTYPE)         A(DISPLAY ROUTINE)                           
         DC    AL2(VQTYPE)         A(VALIDATION ROUTINE)                        
*                                                                               
         DC    AL2(LC2HRSH-LC2)                                                 
         DC    AL2(L'LC2HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC2CLIH-LC2)                                                 
         DC    AL2(L'LC2CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC2PRDH-LC2)                                                 
         DC    AL2(L'LC2PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC2JOBH-LC2)                                                 
         DC    AL2(L'LC2JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC2TSKH-LC2)                                                 
         DC    AL2(L'LC2TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC2MOAH-LC2)                                                 
         DC    AL2(L'LC2MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
         DC    AL2(LC2NAMEH-LC2)                                                
         DC    AL2(L'LC2NAME)                                                   
         DC    AL2(FLDNAME)                                                     
         DC    AL2(DQNAME)                                                      
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAXF)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAX)                                                       
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISC3TAB DS    0C                                                               
         DC    AL2(LC3TYPEH-LC3)                                                
         DC    AL2(L'LC3TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC3HRSH-LC3)                                                 
         DC    AL2(L'LC3HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC3CLIH-LC3)                                                 
         DC    AL2(L'LC3CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC3PRDH-LC3)                                                 
         DC    AL2(L'LC3PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC3JOBH-LC3)                                                 
         DC    AL2(L'LC3JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC3TSKH-LC3)                                                 
         DC    AL2(L'LC3TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC3MOAH-LC3)                                                 
         DC    AL2(L'LC3MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FLDRATE)                                                     
         DC    AL2(0)                                                           
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FLDINC)                                                      
         DC    AL2(0)                                                           
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(LC3COMH-LC3)                                                 
         DC    AL2(L'LC3COM)                                                    
         DC    AL2(FLDNARR)                                                     
         DC    AL2(DQNARR)                                                      
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAXF)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAX)                                                       
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISC4TAB DS    0C                                                               
         DC    AL2(LC4TYPEH-LC4)                                                
         DC    AL2(L'LC4TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC4HRSH-LC4)                                                 
         DC    AL2(L'LC4HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC4CLIH-LC4)                                                 
         DC    AL2(L'LC4CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC4PRDH-LC4)                                                 
         DC    AL2(L'LC4PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC4JOBH-LC4)                                                 
         DC    AL2(L'LC4JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC4TSKH-LC4)                                                 
         DC    AL2(L'LC4TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC4MOAH-LC4)                                                 
         DC    AL2(L'LC4MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
*&&US                                                                           
         DC    AL2(LC4TAXFH-LC4)                                                
         DC    AL2(L'LC4TAXF)                                                   
         DC    AL2(FLDTAXF)                                                     
         DC    AL2(DQTAXF)                                                      
         DC    AL2(VQTAXF)                                                      
*&&                                                                             
*                                                                               
         DC    AL2(LC4RATEH-LC4)                                                
         DC    AL2(L'LC4RATE)                                                   
         DC    AL2(FLDRATE)                                                     
         DC    AL2(DQRATE)                                                      
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(LC4AMTH-LC4)                                                 
         DC    AL2(L'LC4AMT)                                                    
         DC    AL2(FLDAMNT)                                                     
         DC    AL2(DQAMT)                                                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC4INCH-LC4)                                                 
         DC    AL2(L'LC4INC)                                                    
         DC    AL2(FLDINC)                                                      
         DC    AL2(DQINC)                                                       
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(LC4COMH-LC4)                                                 
         DC    AL2(L'LC4COM)                                                    
         DC    AL2(FLDNARR)                                                     
         DC    AL2(DQNARR)                                                      
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAX)                                                       
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISC5TAB DS    0C                                                               
         DC    AL2(LC5TYPEH-LC5)                                                
         DC    AL2(L'LC5TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC5HRSH-LC5)                                                 
         DC    AL2(L'LC5HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC5CLIH-LC5)                                                 
         DC    AL2(L'LC5CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC5PRDH-LC5)                                                 
         DC    AL2(L'LC5PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC5JOBH-LC5)                                                 
         DC    AL2(L'LC5JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC5TSKH-LC5)                                                 
         DC    AL2(L'LC5TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC5MOAH-LC5)                                                 
         DC    AL2(L'LC5MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
*&&US                                                                           
         DC    AL2(LC5TAXFH-LC5)                                                
         DC    AL2(L'LC5TAXF)                                                   
         DC    AL2(FLDTAXF)                                                     
         DC    AL2(DQTAXF)                                                      
         DC    AL2(VQTAXF)                                                      
*&&                                                                             
*                                                                               
         DC    AL2(LC5RATEH-LC5)                                                
         DC    AL2(L'LC5RATE)                                                   
         DC    AL2(FLDRATE)                                                     
         DC    AL2(DQRATE)                                                      
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(LC5AMTH-LC5)                                                 
         DC    AL2(L'LC5AMT)                                                    
         DC    AL2(FLDAMNT)                                                     
         DC    AL2(DQAMT)                                                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC5INCH-LC5)                                                 
         DC    AL2(L'LC5INC)                                                    
         DC    AL2(FLDINC)                                                      
         DC    AL2(DQINC)                                                       
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FLDNARR)                                                     
         DC    AL2(0)                                                           
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAX)                                                       
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES - TAX SCREEN                              *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
DISC6TAB DS    0C                                                               
         DC    AL2(LC6TYPEH-LC6)                                                
         DC    AL2(L'LC6TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC6HRSH-LC6)                                                 
         DC    AL2(L'LC6HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC6CLIH-LC6)                                                 
         DC    AL2(L'LC6CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC6PRDH-LC6)                                                 
         DC    AL2(L'LC6PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC6JOBH-LC6)                                                 
         DC    AL2(L'LC6JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC6TSKH-LC6)                                                 
         DC    AL2(L'LC6TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC6MOAH-LC6)                                                 
         DC    AL2(L'LC6MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
         DC    AL2(LC6TAXFH-LC6)                                                
         DC    AL2(L'LC6TAXF)                                                   
         DC    AL2(FLDTAXF)                                                     
         DC    AL2(DQTAXF)                                                      
         DC    AL2(VQTAXF)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQNARR)                                                      
*                                                                               
         DC    AL2(LC6BASH-LC6)                                                 
         DC    AL2(L'LC6BAS)                                                    
         DC    AL2(FLDBASIS)                                                    
         DC    AL2(DQTAX)                                                       
         DC    AL2(VQTAX)                                                       
*                                                                               
         DC    AL2(LC6LOCH-LC6)                                                 
         DC    AL2(L'LC6LOC)                                                    
         DC    AL2(FLDLOCAL)                                                    
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC6WRKH-LC6)                                                 
         DC    AL2(L'LC6WRK)                                                    
         DC    AL2(FLDTAXWC)                                                    
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    X'FF'                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES - TEMPO UPLOAD SCREEN                     *         
***********************************************************************         
         SPACE 1                                                                
DISC8TAB DS    0C                                                               
         DC    AL2(LC8TYPEH-LC8)                                                
         DC    AL2(L'LC8TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC8HRSH-LC8)                                                 
         DC    AL2(L'LC8HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC8CLIH-LC8)                                                 
         DC    AL2(L'LC8CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC8PRDH-LC8)                                                 
         DC    AL2(L'LC8PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC8JOBH-LC8)                                                 
         DC    AL2(L'LC8JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC8TSKH-LC8)                                                 
         DC    AL2(L'LC8TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC8MOAH-LC8)                                                 
         DC    AL2(L'LC8MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
*&&US                                                                           
         DC    AL2(LC8TAXFH-LC8)                                                
         DC    AL2(L'LC8TAXF)                                                   
         DC    AL2(FLDTAXF)                                                     
         DC    AL2(DQTAXF)                                                      
         DC    AL2(VQTAXF)                                                      
*&&                                                                             
*                                                                               
         DC    AL2(LC8RATEH-LC8)                                                
         DC    AL2(L'LC8RATE)                                                   
         DC    AL2(FLDRATE)                                                     
         DC    AL2(DQRATE)                                                      
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(LC8AMTH-LC8)                                                 
         DC    AL2(L'LC8AMT)                                                    
         DC    AL2(FLDAMNT)                                                     
         DC    AL2(DQAMT)                                                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC8INCH-LC8)                                                 
         DC    AL2(L'LC8INC)                                                    
         DC    AL2(FLDINC)                                                      
         DC    AL2(DQINC)                                                       
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(LC8TLN#H-LC8)                                                
         DC    AL2(L'LC8TLN#)                                                   
         DC    AL2(FLDNARR)                                                     
         DC    AL2(DQTMPO#)                                                     
         DC    AL2(VQTMPO#)                                                     
*                                                                               
         DC    AL2(LC8COMH-LC8)                                                 
         DC    AL2(L'LC8COM)                                                    
         DC    AL2(FLDNARR)                                                     
         DC    AL2(DQNARR)                                                      
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(LC8BASH-LC8)                                                 
         DC    AL2(L'LC8BAS)                                                    
         DC    AL2(FLDBASIS)                                                    
         DC    AL2(DQTAX)                                                       
         DC    AL2(VQTAX)                                                       
*                                                                               
         DC    AL2(LC8LOCH-LC8)                                                 
         DC    AL2(L'LC8LOC)                                                    
         DC    AL2(FLDLOCAL)                                                    
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC8WRKH-LC8)                                                 
         DC    AL2(L'LC8WRK)                                                    
         DC    AL2(FLDTAXWC)                                                    
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY/VALIDATION TABLES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISC9TAB DS    0C                                                               
         DC    AL2(LC9TYPEH-LC9)                                                
         DC    AL2(L'LC9TYPE)                                                   
         DC    AL2(FLDTYPE)                                                     
         DC    AL2(DQTYPE)                                                      
         DC    AL2(VQTYPE)                                                      
*                                                                               
         DC    AL2(LC9HRSH-LC9)                                                 
         DC    AL2(L'LC9HRS)                                                    
         DC    AL2(FLDHRS)                                                      
         DC    AL2(DQHRS)                                                       
         DC    AL2(VQHRS)                                                       
*                                                                               
         DC    AL2(LC9CLIH-LC9)                                                 
         DC    AL2(L'LC9CLI)                                                    
         DC    AL2(FLDCLI)                                                      
         DC    AL2(DQCPJ)                                                       
         DC    AL2(VQCPJ)                                                       
*                                                                               
         DC    AL2(LC9PRDH-LC9)                                                 
         DC    AL2(L'LC9PRD)                                                    
         DC    AL2(FLDPRD)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC9JOBH-LC9)                                                 
         DC    AL2(L'LC9JOB)                                                    
         DC    AL2(FLDJOB)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC9TSKH-LC9)                                                 
         DC    AL2(L'LC9TSK)                                                    
         DC    AL2(FLDTSK)                                                      
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC9MOAH-LC9)                                                 
         DC    AL2(L'LC9MOA)                                                    
         DC    AL2(FLDMOA)                                                      
         DC    AL2(DQMOA)                                                       
         DC    AL2(VQMOA)                                                       
*                                                                               
         DC    AL2(LC9RATEH-LC9)                                                
         DC    AL2(L'LC9RATE)                                                   
         DC    AL2(FLDRATE)                                                     
         DC    AL2(DQRATE)                                                      
         DC    AL2(VQRATE)                                                      
*                                                                               
         DC    AL2(LC9AMTH-LC9)                                                 
         DC    AL2(L'LC9AMT)                                                    
         DC    AL2(FLDAMNT)                                                     
         DC    AL2(DQAMT)                                                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LC9NAMEH-LC9)                                                
         DC    AL2(L'LC9NAME)                                                   
         DC    AL2(FLDNAME)                                                     
         DC    AL2(DQNAME)                                                      
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQINC)                                                       
*                                                                               
         DC    AL2(LC9COMH-LC9)                                                 
         DC    AL2(L'LC9COM)                                                    
         DC    AL2(FLDNARR)                                                     
         DC    AL2(DQNARR)                                                      
         DC    AL2(VQNARR)                                                      
*                                                                               
*&&US                                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAXF)                                                      
*                                                                               
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(VQTAX)                                                       
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
IFLDD    DSECT                                                                  
IFLDLN   DS    XL1                 LENGTH OF MINI ELEMENT                       
IFLDNUM  DS    XL2                 FIELD NUMBER                                 
IFLDDATA DS    0X                  FIELD DATA                                   
*                                                                               
TOTTABD  DSECT                     DISPLAYING TOTALS DSECT                      
TOTHOURS DS    XL2                 DISP TO HOURS BUCKET                         
TOTFIELD DS    XL2                 DISP TO FIELD TO DISPLAY                     
TOTTBLNQ EQU   *-TOTTABD                                                        
*                                                                               
HOURTABD DSECT                     HOURS TOTALLING                              
HRTYPE   DS    XL1                 TYPE OF TIME AS IN REC (TIMTTYP)             
HRPREV1  DS    XL2                 DISP TO PREVIOUS HOURS TOTALS                
HRPREV2  DS    XL2                                                              
HRPREV3  DS    XL2                                                              
HRPREV4  DS    XL2                                                              
HRCURR1  DS    XL2                 DISP TO CURRENT HOURS TOTALS                 
HRCURR2  DS    XL2                                                              
HRCURR3  DS    XL2                                                              
HRCURR4  DS    XL2                                                              
HROTH1   DS    XL2                 DISP TO OTHER HOURS TOTALS                   
HROTH2   DS    XL2                                                              
HROTH3   DS    XL2                                                              
HROTH4   DS    XL2                                                              
HRLENQ   EQU   *-HOURTABD                                                       
*                                                                               
TIMETABD DSECT                     TYPE OF TIME DSECT                           
TTFIELD  DS    CL2                 TIME AS APPEARS ON SCREEN                    
TTTYPECH DS    CL1                 TYPE IN CHAR FORMAT (B/N/R)                  
TTDISP   DS    CL2                 TYPE TO DISPLAY IN FIELD                     
TTTYPE   DS    XL1                 TYPE EQUATE FOR TIMTTYP                      
TTIND    DS    XL1                 ANY TIME INDICATOR (ADJ)                     
TTLENQ   EQU   *-TIMETABD                                                       
*                                                                               
SCRNTABD DSECT                     SCREEN TABLE DSECT                           
SCRNNUM  DS    XL1                 SCREEN NUMBER                                
SCRNPFK  DS    XL1                 PFKEY TO GET TO SCREEN                       
SCRNLINE DS    XL1                 NUMBER OF LINES ON SCREEN                    
         DS    XL1                 N/D                                          
SCRNLEN  DS    XL2                 LENGTH OF 1 LINE                             
SCRNTBL  DS    XL4                 A(ROUTINES FOR SCREEN)                       
SCRNLIN1 DS    H                   DISP TO FIRST LINE FROM WHERE LOADED         
SCRNLSTL DS    H                   DISP TO LAST LINE FROM WHERE LOADED          
SCRNLSTF DS    H                   DISP TO LAST FIELD FROM WHERE LOADED         
SCRNPFKY DS    H                   DISP TO PFKEY LINE                           
SCRNTLNQ EQU   *-SCRNTABD                                                       
*                                                                               
DISTABD  DSECT                     DISPLAY AND VALIDATION ROUTINES              
DTFDISP  DS    XL2                 DISP TO FIELD                                
DTFLEN   DS    XL2                 LENGTH OF FIELD                              
DTINPFLD DS    XL2                 INPUT BITS TO SET ON IF ENTERED              
DTDISRTE DS    XL2                 ROUTINE TO DISPLAY                           
DTVALRTE DS    XL2                 ROUTINE TO VALIDATE                          
DTLENQ   EQU   *-DISTABD                                                        
*                                                                               
DUPD     DSECT                     DUPLICATE KEY DSECT                          
DLINENUM DS    XL2                 LINE NUMBER OF DUP                           
DLINESUB DS    XL2                 SUB REFERENCE                                
DKEY     DS    0CL(TIMILNQ+TIMBLNQ)                                             
DKINFO   DS    CL(TIMILNQ)         INPUT KEY DATA                               
DKBINFO  DS    CL(TIMBLNQ)         BILLABLE KEY INFO                            
DUPDLEN  EQU   *-DUPD                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN LINE DSECTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
LC2      DSECT                     NON BILLABLE / NO COMMENTS                   
LC2TYPEH DS    CL8                                                              
LC2TYPE  DS    CL2                                                              
LC2HRSH  DS    CL8                                                              
LC2HRS   DS    CL7                                                              
LC2CLIH  DS    CL8                                                              
LC2CLI   DS    CL7                                                              
LC2PRDH  DS    CL8                                                              
LC2PRD   DS    CL4                                                              
LC2JOBH  DS    CL8                                                              
LC2JOB   DS    CL6                                                              
LC2TSKH  DS    CL8                                                              
LC2TSK   DS    CL2                                                              
LC2MOAH  DS    CL8                                                              
LC2MOA   DS    CL6                                                              
LC2NAMEH DS    CL8                                                              
LC2NAME  DS    CL35                                                             
LC2LENQ  EQU   *-LC2                                                            
*                                                                               
LC3      DSECT                     NON BILLABLE W/COMMENTS                      
LC3TYPEH DS    CL8                                                              
LC3TYPE  DS    CL2                                                              
LC3HRSH  DS    CL8                                                              
LC3HRS   DS    CL7                                                              
LC3CLIH  DS    CL8                                                              
LC3CLI   DS    CL7                                                              
LC3PRDH  DS    CL8                                                              
LC3PRD   DS    CL4                                                              
LC3JOBH  DS    CL8                                                              
LC3JOB   DS    CL6                                                              
LC3TSKH  DS    CL8                                                              
LC3TSK   DS    CL2                                                              
LC3MOAH  DS    CL8                                                              
LC3MOA   DS    CL6                                                              
LC3COMH  DS    CL8                                                              
LC3COM   DS    CL35                                                             
LC3LENQ  EQU   *-LC3                                                            
*                                                                               
LC4      DSECT                     BILLABLE W/COMMENTS                          
LC4TYPEH DS    CL8                                                              
LC4TYPE  DS    CL2                                                              
LC4HRSH  DS    CL8                                                              
LC4HRS   DS    CL7                                                              
LC4CLIH  DS    CL8                                                              
LC4CLI   DS    CL7                                                              
LC4PRDH  DS    CL8                                                              
LC4PRD   DS    CL4                                                              
LC4JOBH  DS    CL8                                                              
LC4JOB   DS    CL6                                                              
LC4TSKH  DS    CL8                                                              
LC4TSK   DS    CL2                                                              
LC4MOAH  DS    CL8                                                              
LC4MOA   DS    CL6                                                              
*&&US                                                                           
LC4TAXFH DS    CL8                                                              
LC4TAXF  DS    CL2                                                              
*&&                                                                             
LC4RATEH DS    CL8                                                              
LC4RATE  DS    CL8                                                              
LC4AMTH  DS    CL8                                                              
LC4AMT   DS    CL9                                                              
LC4INCH  DS    CL8                                                              
LC4INC   DS    CL15                                                             
LC4COMH  DS    CL8                                                              
LC4COM   DS    CL60                                                             
LC4LENQ  EQU   *-LC4                                                            
*                                                                               
LC5      DSECT                     BILLABLE / NO COMMENTS                       
LC5TYPEH DS    CL8                                                              
LC5TYPE  DS    CL2                                                              
LC5HRSH  DS    CL8                                                              
LC5HRS   DS    CL7                                                              
LC5CLIH  DS    CL8                                                              
LC5CLI   DS    CL7                                                              
LC5PRDH  DS    CL8                                                              
LC5PRD   DS    CL4                                                              
LC5JOBH  DS    CL8                                                              
LC5JOB   DS    CL6                                                              
LC5TSKH  DS    CL8                                                              
LC5TSK   DS    CL2                                                              
LC5MOAH  DS    CL8                                                              
LC5MOA   DS    CL6                                                              
*&&US                                                                           
LC5TAXFH DS    CL8                                                              
LC5TAXF  DS    CL2                                                              
*&&                                                                             
LC5RATEH DS    CL8                                                              
LC5RATE  DS    CL8                                                              
LC5AMTH  DS    CL8                                                              
LC5AMT   DS    CL9                                                              
LC5INCH  DS    CL8                                                              
LC5INC   DS    CL15                                                             
LC5LENQ  EQU   *-LC5                                                            
*                                                                               
*&&US                                                                           
LC6      DSECT                     TAX SCREEN                                   
LC6TYPEH DS    CL8                                                              
LC6TYPE  DS    CL2                                                              
LC6HRSH  DS    CL8                                                              
LC6HRS   DS    CL7                                                              
LC6CLIH  DS    CL8                                                              
LC6CLI   DS    CL7                                                              
LC6PRDH  DS    CL8                                                              
LC6PRD   DS    CL4                                                              
LC6JOBH  DS    CL8                                                              
LC6JOB   DS    CL6                                                              
LC6TSKH  DS    CL8                                                              
LC6TSK   DS    CL2                                                              
LC6MOAH  DS    CL8                                                              
LC6MOA   DS    CL6                                                              
LC6TAXFH DS    CL8                 TAX CONTROL FIELD                            
LC6TAXF  DS    CL2                                                              
LC6BASH  DS    CL8                 TAX BASIS - #1                               
LC6BAS   DS    CL9                                                              
LC6LOCH  DS    CL8                 LOCALITY  - #1                               
LC6LOC   DS    CL8                                                              
LC6WRKH  DS    CL8                 WORK CODE - #1                               
LC6WRK   DS    CL2                                                              
LC6TNMH  DS    CL8                 TAX NAME  - #1                               
LC6TNM   DS    CL12                                                             
LC6LENQ  EQU   *-LC6                                                            
*&&                                                                             
*                                                                               
LC8      DSECT                     TEMPO UPLOAD SCREEN                          
LC8TYPEH DS    CL8                                                              
LC8TYPE  DS    CL2                                                              
LC8HRSH  DS    CL8                                                              
LC8HRS   DS    CL7                                                              
LC8CLIH  DS    CL8                                                              
LC8CLI   DS    CL7                                                              
LC8PRDH  DS    CL8                                                              
LC8PRD   DS    CL4                                                              
LC8JOBH  DS    CL8                                                              
LC8JOB   DS    CL6                                                              
LC8TSKH  DS    CL8                                                              
LC8TSK   DS    CL2                                                              
LC8MOAH  DS    CL8                                                              
LC8MOA   DS    CL6                                                              
*&&US                                                                           
LC8TAXFH DS    CL8                 TAX CONTROL FIELD                            
LC8TAXF  DS    CL2                                                              
*&&                                                                             
LC8RATEH DS    CL8                                                              
LC8RATE  DS    CL8                                                              
LC8AMTH  DS    CL8                                                              
LC8AMT   DS    CL9                                                              
LC8INCH  DS    CL8                                                              
LC8INC   DS    CL15                                                             
LC8TLN#H DS    CL8                                                              
LC8TLN#  DS    CL5                                                              
LC8COMH  DS    CL8                                                              
LC8COM   DS    CL60                                                             
*&&US                                                                           
LC8BASH  DS    CL8                 TAX BASIS - #1                               
LC8BAS   DS    CL9                                                              
LC8LOCH  DS    CL8                 LOCALITY  - #1                               
LC8LOC   DS    CL8                                                              
LC8WRKH  DS    CL8                 WORK CODE - #1                               
LC8WRK   DS    CL2                                                              
*&&                                                                             
LC8LENQ  EQU   *-LC8                                                            
*                                                                               
LC9      DSECT                     BILLABLE W/DESCRIPTION                       
LC9TYPEH DS    CL8                                                              
LC9TYPE  DS    CL2                                                              
LC9HRSH  DS    CL8                                                              
LC9HRS   DS    CL7                                                              
LC9CLIH  DS    CL8                                                              
LC9CLI   DS    CL7                                                              
LC9PRDH  DS    CL8                                                              
LC9PRD   DS    CL4                                                              
LC9JOBH  DS    CL8                                                              
LC9JOB   DS    CL6                                                              
LC9TSKH  DS    CL8                                                              
LC9TSK   DS    CL2                                                              
LC9MOAH  DS    CL8                                                              
LC9MOA   DS    CL6                                                              
LC9RATEH DS    CL8                                                              
LC9RATE  DS    CL8                                                              
LC9AMTH  DS    CL8                                                              
LC9AMT   DS    CL9                                                              
LC9NAMEH DS    CL8                                                              
LC9NAME  DS    CL18                                                             
LC9COMH  DS    CL8                                                              
LC9COM   DS    CL60                                                             
LC9LENQ  EQU   *-LC9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE # EQUATES                                                   *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
*                                  *** US FIELD VALIDATION ***                  
VQTYPE   EQU   1                   VALIDATE TYPE                                
VQHRS    EQU   2                   VALIDATE HOURS                               
VQCPJ    EQU   3                   VALIDATE CLIENT/PRODUCT/JOB                  
VQMOA    EQU   4                   VALIDATE MOA                                 
VQRATE   EQU   5                   VALIDATE RATE                                
VQINC    EQU   6                   VALIDATE INCOME ACCOUNT                      
VQNARR   EQU   7                   VALIDATE NARRATIVE                           
VQTAXF   EQU   8                   VALIDATE TAX FIELD                           
VQTAX    EQU   9                   VALIDATE TAX DATA                            
VQTMPO#  EQU   10                  VALIDATE TEMPO LINE #                        
*&&                                                                             
*&&UK                                                                           
*                                  *** UK FIELD VALIDATION ***                  
VQTYPE   EQU   1                   VALIDATE TYPE                                
VQHRS    EQU   2                   VALIDATE HOURS                               
VQCPJ    EQU   3                   VALIDATE CLIENT/PRODUCT/JOB                  
VQMOA    EQU   4                   VALIDATE MOA                                 
VQRATE   EQU   5                   VALIDATE RATE                                
VQINC    EQU   6                   VALIDATE INCOME ACCOUNT                      
VQNARR   EQU   7                   VALIDATE NARRATIVE                           
VQTMPO#  EQU   8                   VALIDATE TEMPO LINE #                        
*&&                                                                             
*                                                                               
*&&US                                                                           
*                                  *** US FIELD DISPLAY ***                     
DQTYPE   EQU   1                   DISPLAY TYPE                                 
DQHRS    EQU   2                   DISPLAY HOURS                                
DQCPJ    EQU   3                   DISPLAY CLIENT/PRODUCT/JOB                   
DQMOA    EQU   4                   DISPLAY MOA                                  
DQRATE   EQU   5                   DISPLAY RATE                                 
DQINC    EQU   6                   DISPLAY INCOME ACCOUNT                       
DQNARR   EQU   7                   DISPLAY NARRATIVE                            
DQNAME   EQU   8                   DISPLAY NAME                                 
DQAMT    EQU   9                   DISPLAY AMOUNT                               
DQTAXF   EQU   10                  DISPLAY TAX FIELD                            
DQTAX    EQU   11                  DISPLAY TAX DATA                             
DQTMPO#  EQU   12                  DISPLAY TEMPO LINE #                         
*&&                                                                             
*&&UK                                                                           
*                                  *** UK FIELD DISPLAY ***                     
DQTYPE   EQU   1                   DISPLAY TYPE                                 
DQHRS    EQU   2                   DISPLAY HOURS                                
DQCPJ    EQU   3                   DISPLAY CLIENT/PRODUCT/JOB                   
DQMOA    EQU   4                   DISPLAY MOA                                  
DQRATE   EQU   5                   DISPLAY RATE                                 
DQINC    EQU   6                   DISPLAY INCOME ACCOUNT                       
DQNARR   EQU   7                   DISPLAY NARRATIVE                            
DQNAME   EQU   8                   DISPLAY NAME                                 
DQAMT    EQU   9                   DISPLAY AMOUNT                               
DQTMPO#  EQU   10                  DISPLAY TEMPO LINE #                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACCADETD                                                       
*&&UK                                                                           
TIMELD   DSECT                                                                  
         ORG   TIMELD+TIMILN2Q                                                  
TIMERTE  DS    PL4                 EURO CHARGE RATE                             
TIMECRTE DS    PL4                 EURO COST RATE                               
TIMB2LNQ EQU   *-TIMELD            LENGTH OF BILLABLE DATA W/EURO INFO          
*&&                                                                             
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
GOBBLCKD DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
       ++INCLUDE ACTIMETRND                                                     
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SCREENS                                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAPC2D         BOTTOM SCREENS                                
       ++INCLUDE ACCAPC3D                                                       
       ++INCLUDE ACCAPC4D                                                       
       ++INCLUDE ACCAPC5D                                                       
*&&US                                                                           
       ++INCLUDE ACCAPC6D         TAX SCREEN                                    
*&&                                                                             
       ++INCLUDE ACCAPC8D                                                       
       ++INCLUDE ACCAPC9D                                                       
       ++INCLUDE ACCAPFFD          BASE SCREEN                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPC0D          TOP SCREENS                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPC1D                                                       
***********************************************************************         
* THE FOLLOWING ORGS ARE DIFFERENT BECAUSE THE RESOLVED LENGTH ADDED  *         
* TO CHALAST MUST BE THE LENGTH OF THE LARGEST SCREEN.  SINCE TAX IS  *         
* NOT SUPPORTED IN THE UK VERSION THE DISPLACEMENT IS CALCULATED FROM *         
* THE LENGTH OF THE BILLABLE DETAILS SCREEN.                          *         
***********************************************************************         
         ORG   CONHEADH+(3520-STARTWKQ-8)                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION STORAGE                                                 *         
*                                                                     *         
*  COMMENTS:     USED BY ACCAP40                                      *         
*                ORG TO END OF SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
STARTWRK DS    0A                  IN TWA                                       
ALINE1   DS    A                   A(FIRST LINE)                                
ALSTLINE DS    A                   A(LAST LINE)                                 
ALSTFLD  DS    A                   A(LAST FIELD)                                
ABOTSCR  DS    A                   A(BOTTOM SCREEN)                             
AINPELEM DS    A                   A(X'8B' INPUT DETAIL ELEM)                   
ATAXELEM DS    A                   A(X'8B' TAX DETAIL ELEM)                     
ANARELEM DS    A                   A(X'8B' NARRATIVE ELEM)                      
AXSTELEM DS    A                   A(X'8B' EXTRA STATUS ELEMENT)                
ASAVELEM DS    A                   A(X'8B' COMMENTED ELEM)                      
ATIMELEM DS    A                   A(X'8B' ETIME ELEM)                          
AMATELEM DS    A                   A(X'8B' ITEM/MATERIALS ELEM)                 
AFIELDH  DS    A                   A(FIELD HEADER)                              
AFLD1STH DS    A                   A(FIELD HEADER OF 1ST LINE FIELD)            
APRDH    DS    A                   A(PRODUCT HEADER)                            
AJOBH    DS    A                   A(JOB HEADER)                                
AVDTAB   DS    A                   A(VAL/DIS ROUTINE TABLE)                     
ADISLINE DS    A                   A(NEXT LINE NUMBER DISPLAYED)                
ASCRNITM DS    A                   A(SCREEN TABLE ITEM)                         
AHIDDEN  DS    A                   A(HIDDEN FIELD STORAGE)                      
ATAXBAS  DS    A                   A(TAX BASIS FIELD)                           
ATAXLOC  DS    A                   A(TAX LOCALITY FIELD)                        
ATAXWC   DS    A                   A(TAX WORKCODE FIELD)                        
ASCRNTAB DS    A                   A(SCREEN TABLE)                              
         DS    A                   N/D                                          
*                                                                               
*                                                                               
RECLEN   DS    H                                                                
FIELDLEN DS    H                                                                
RECSDIS  DS    H                                                                
DUPNUM   DS    H                                                                
DBOTSCR  DS    H                   DISP TO BOTTOM SCREEN                        
*                                                                               
PCSTAT   DS    XL1                 PROJECT CONTROL STATUS                       
PCSJ     EQU   X'80'                                                            
PC1R     EQU   X'40'                                                            
*                                                                               
SVSCROLL DS    XL2                 SAVED SCROLL AMOUNT                          
SVOPT1   DS    XL1                 SORT                                         
SVOPT2   DS    XL1                 XD=Y                                         
SVOPT3   DS    XL1                 ALL                                          
*                                                                               
SORTSTAT DS    XL1                 INDICATES HOW TSAR BUFFER IS SORTED          
SORTSJ   EQU   X'01'               SORTED BY SJ                                 
SORTTSN  EQU   X'02'               SORTED BY TIMESHEET NUMBER (DEFAULT)         
*                                                                               
PRDSTDTE DS    PL3                 PERIOD START DATE                            
PRDENDTE DS    PL3                 PERIOD ENDING DATE                           
PRDNUM   DS    XL1                 PERIOD NUMBER                                
PRDMON   DS    PL2                 PERIOD MONTH                                 
YYMMDD   DS    PL3                                                              
SVYYMMDD DS    PL3                                                              
SVTODAY  DS    PL3                 SAVED AREA FOR TODAY'S DATE                  
*                                                                               
LISTDTES DS    8PL3                DATES VALID FOR CHANGE FROM LIST             
*                                                                               
SVDGRP   DS    CL1                 DEFAULT DIRECT ACCOUNT GROUP                 
*                                                                               
SAVPERDT DS    PL3                 SAVED PERIOD DATE                            
SAVPERSN DS    CL8                 KEY - SAVED PERSON CODE                      
         DS    XL1                 N/D                                          
SVBOTSCR DS    XL1                 SAVED BOTTOM SCREEN                          
*                                                                               
SCRNSTAT DS    XL1                 SCREEN STATUS BYTE                           
SCRNCHNG EQU   X'01'               SCREEN HAS CHANGED                           
*                                                                               
LINESTAT DS    XL1                 WHAT HAS BEEN ENTERED ON LINE                
LBADDREC EQU   X'80'               ADD REC FOR THIS LINE(NOT WRITE)             
LBRATADJ EQU   X'40'               RATE IS ELEIGIBLE FOR ADJUSTMENT             
LBXJOB   EQU   X'20'               JOB IS AN X-JOB                              
LBSAVE   EQU   X'10'               LINE IS COMMENTED OUT                        
LB1N     EQU   X'08'               ENTERED A 1N ACCOUNT                         
LBSAVE#  EQU   X'04'               LINE IS COMMENTED OUT WITH A ?               
LBINCOMP EQU   X'02'               MISSING RATE OR INCOME ACOUNT                
LBDELETE EQU   X'01'               DELETE LINE                                  
*                                                                               
ONSCREEN DS    XL2                 FIELDS ON SCREEN                             
*                                                                               
SCRREQS  DS    XL2                 SCREEN -> REQUIRED FIELDS                    
FLDREQS  DS    XL2                 LINE ---> REQUIRED FIELDS                    
FLDINPS  DS    XL2                 LINE ---> INPUT FIELDS                       
*                                                                               
SVLSEL   DS    CL1                 SAVED AREA FOR SEL OFF OF TIME LIST          
*                                                                               
STATUS   DS    XL1                                                              
STINCOMP EQU   X'80'               TSAR REC IS MISSING SOME INFO                
DISPED   EQU   X'40'               DISPLAY BLOCK BUILT                          
STPROTCT EQU   X'10'               PROTECT CURRENT LINE                         
STINCTAX EQU   X'08'               TSAR REC IS MISSING TAX INFO                 
NEWINPUT EQU   X'04'               NEW INPUT ON CURRENT LINE                    
NOREC    EQU   X'02'               THERE ARE NO RECORDS - MUST ADD              
STINSERT EQU   X'01'               FORCE CURSOR TO INSERTED LINE                
*                                                                               
STATUS2  DS    XL1                                                              
TRNCMPAR EQU   X'80'               CALL TIMETRN WITH ACTION COMPARE             
ONLNUPDT EQU   X'40'               ON LINE UP[DATE OF BUCKETS IN FORCE          
TRNBUILD EQU   X'20'               CALL TIMETRN WITH ACTION BUILD               
DAILYTIM EQU   BCPRIDTM            X'10' DAILY TIME                             
HASDATE  EQU   X'08'               HAS DATE TO PUT OUT FOR DAILY TIME           
MCSTIME  EQU   BCPRIMCT            X'04' MCS TIME                               
*                                                                               
TMSUBLOK DS    0CL165                                                           
TMSUNTAB DS    F                   NUMBER OF ENTRIES IN TABLE                   
TMSUTMAX EQU   10                  MAX NUMBER OF ENTRIES                        
TMSUTAB  DS    (TMSUTMAX*16)C                                                   
TMSUTABX DS    C                                                                
STARTWKQ EQU   *-STARTWRK                                                       
         EJECT                                                                  
***********************************************************************         
* DDGENTWA                                                            *         
***********************************************************************         
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
STORED   DSECT                     SAVED STORAGE                                
RELO     DS    A                                                                
STADDR   DS    F                   START ADDRESS                                
SVADDR   DS    F                   SAVED ADDRESS                                
WORK2    DS    10F                                                              
ACADET   DS    A                   A(CADET)                                     
*                                                                               
TOTALH   DS    0PL4                                                             
PBHOURS  DS    PL4                 PREVIOUS B HOURS INPUT                       
PNHOURS  DS    PL4                 PREVIOUS N HOURS INPUT                       
PRHOURS  DS    PL4                 PREVIOUS R HOURS INPUT                       
CBHOURS  DS    PL4                 CURRENT B HOURS INPUT                        
CNHOURS  DS    PL4                 CURRENT N HOURS INPUT                        
CRHOURS  DS    PL4                 CURRENT R HOURS INPUT                        
OBHOURS  DS    PL4                 OTHER(DIFF LOCATION) B HOURS INPUT           
ONHOURS  DS    PL4                 OTHER N HOURS INPUT                          
ORHOURS  DS    PL4                 OTHER R HOURS INPUT                          
PRETOT   DS    PL4                 PREVIOUS HOURS TOTAL                         
CURTOT   DS    PL4                 CURRENT HOURS TOTAL                          
OTHTOT   DS    PL4                 OTHER HOURS TOTAL                            
BILTOT   DS    PL4                 B-TIME HOURS TOTAL                           
RTMTOT   DS    PL4                 R-TIME HOURS TOTAL                           
NONTOT   DS    PL4                 N-TIME HOURS TOTAL                           
TOTHRS   DS    PL4                 GRAND TOTAL                                  
TOTALHQ  EQU   (*-TOTALH)/L'TOTALH                                              
*                                                                               
HRSWRK   DS    PL4                 TOTAL HOURS WORKED ON ADD                    
HRSDIF   DS    PL4                 DIFFERENCE BETWEEN TOTAL & INPUT             
*                                                                               
SVRVSN#  DS    XL2                 REVISION #                                   
SVTSNUM  DS    XL2                 TIMESHEET#                                   
SVTS#NXT DS    XL2                 NEXT TIMESHEET #                             
*                                                                               
SCXLINE# DS    0XL4                SCREEN LINE/SUB LINE #                       
SCXLINE  DS    XL2                 LINE #                                       
SCXSUB   DS    XL2                 SUB LINE #                                   
*                                                                               
SCXST#   DS    0XL4                                                             
SCXSTLIN DS    XL2                 START OF PAGE LINE NUMBER                    
SCXSTSUB DS    XL2                 START OF PAGE SUB LINE NUMBER                
*                                                                               
SCXNEXT# DS    0XL4                                                             
SCXNEXT  DS    XL2                 NEXT LINE NUMBER TO USE FOR NEXT ADD         
SCXNXSUB DS    XL2                 HIGH SUB NUM TO USE FOR NEXT INSERT          
*                                                                               
SCRLINES DS    XL1                 NUBER OF LINES ON SCREEN                     
DISLINES DS    XL50                TABLE OF LINES DISPLAYED ON SCREEN           
*                                                                               
LAP#     DS    XL1                 COUNT # LAPS THROUGH DISPREC                 
*                                                                               
SVTIMST1 DS    XL1                 SAVED TIMKSTAT                               
*                                                                               
SVFLAG5  DS    XL1                 SAVED AREA FOR BCFLAG5                       
*                                                                               
SVTMSYS  DS    XL1                 SYSTEM FOR ERROR MESSAGES                    
SVVKBLCK DS    0C                  *** SAVE TOP OF SCREEN ***                   
SV1RPRSN DS    CL8                 PERSON CODE                                  
SV1ROFFC DS    CL2                 OFFICE CODE                                  
SV1RDPT  DS    CL3                 DEPARTMENT CODE                              
SV1RSDPT DS    CL3                 SUB DEPARTMENT CODE                          
SV1RDTES DS    0PL6                SAVE AREA FOR LOC START/END DATES            
SV1RSTDT DS    PL3                 LOCATION START DATE                          
SV1RENDT DS    PL3                 LOCATION END   DATE                          
SVOMOA   DS    PL2                 OPEN MONTH OF ACTIVITY                       
SVFMOA   DS    PL2                 FUTURE MONTH                                 
SV1RACT  DS    0XL15               1R ACCOUNT                                   
SV1RCPY  DS    XL1                 1R COMPANY CODE                              
SV1RULA  DS    0CL14               1R UNIT/LEDGER/ACCOUNT                       
SV1RUL   DS    CL2                 1R UNIT/LEDGER                               
SV1RCODE DS    CL12                1R ACCOUNT CODE                              
SVSPINC  DS    CL15                SPECIAL INCOME ACCOUNT                       
SVCSTGRP DS    CL1                 COSTING GROUP                                
SVCLIPOS DS    XL1                 CLIENT POSITION                              
SVCPOSN  DS    XL1                 COSTING REPLACE POSITION                     
SVCCNTER DS    XL3                 COSTING CENTER                               
SVPIDNO  DS    XL2                 PERSON ID #                                  
SVDFTASK DS    CL2                 DEFAULT TASK CODE                            
SVDFSCRN DS    XL1                 DEFAULT SCREEN CODE                          
*                                                                               
SVDFBRTE DS    XL1                 DEFAULT TYPE 'B' RATE                        
SVDFRRTE DS    XL1                 DEFAULT TYPE 'N'/'R' RATE                    
SVDFRCST EQU   X'01'               N = COST RATE                                
SVDFRSAL EQU   X'02'               Y = SALES RATE                               
*                                                                               
SVBTL    DS    XL1                 BILLABLE TIME LEDGER (I/S)                   
SVDLK    DS    XL1                 DEFAULT INCOME/SUSPENCE (Y/N/O)              
SVTUP    DS    XL1                 UPDATE TIME WHEN:(SA/SU/CL/LM/FA)            
*                                                   (1 /2 /3 /4 /5 )            
SVASTAT1 DS    XL1                 SAVED AREA FOR BCASTAT1                      
SVMCSRNO DS    XL2                 SAVED ROW NO. IF MCS TIME                    
*                                                                               
SVCNFRM  DS    XL1                 DEFAULT CONFIRM TIMESHEET UPDATE             
SVCNFRMY EQU   X'01'                                                            
*                                                                               
SVVRBLCK DS    0C                  *** SAVED BOTTOM OF SCREEN ***               
SVTYPECH DS    CL1                 TYPE OF TIME (B/N/R)                         
SVTYPE   DS    CL1                 TYPE OF TIME (BIT EQUIVALENT)                
SVIND    DS    XL1                 TYPE OF TIME INDICATOR BYTE                  
SVHOURS  DS    PL4                 HOURS                                        
SV1NACT  DS    0XL15               1N ACCOUNT                                   
SV1NCPY  DS    XL1                 1N COMPANY CODE                              
SV1NULA  DS    0CL14               1N UNIT/LEDGER/ACCOUNT                       
SV1NUL   DS    CL2                 1N UNIT/LEDGER                               
SV1NCODE DS    CL12                1N ACCOUNT CODE                              
SVSJACT  DS    0XL15               SJ ACCOUNT                                   
SVSJCPY  DS    XL1                 SJ COMPANY CODE                              
SVSJULA  DS    0CL14               SJ UNIT/LEDGER/ACCOUNT                       
SVSJUL   DS    CL2                 SJ UNIT/LEDGER                               
SVSJCODE DS    CL12                SJ ACCOUNT CODE                              
SVTASK   DS    CL2                 TASK CODE                                    
SV1CACT  DS    0XL15               1C ACCOUNT                                   
SV1CCPY  DS    XL1                 1C COMPANY CODE                              
SV1CULA  DS    0CL14               1C UNIT/LEDGER/ACCOUNT                       
SV1CUL   DS    CL2                 1C UNIT/LEDGER                               
SV1CCODE DS    CL12                1C ACCOUNT CODE                              
SVSIACT  DS    0XL15               SI ACCOUNT                                   
SVSICPY  DS    XL1                 SI COMPANY CODE                              
SVSIULA  DS    0CL14               SI UNIT/LEDGER/ACCOUNT                       
SVSIUL   DS    CL2                 SI UNIT/LEDGER                               
SVSICODE DS    CL12                SI ACCOUNT CODE                              
SVPCACT  DS    CL15                PROJECT CONTROL ACCOUNT                      
SVMOA    DS    PL2                 MOA                                          
SVRATE   DS    PL4                 RATE                                         
SVRATIND DS    XL1                 RATE BITS AS IN RECORD                       
SVRATEFF DS    PL3                 RATE EFFECTIVE DATE                          
SVAMOUNT DS    PL8                 AMOUNT                                       
SVCRATE  DS    PL4                 COST RATE                                    
SVCRATEF DS    PL3                 COST RATE EFFECTIVE DATE                     
SVERATE  DS    PL4                 EURO CHARGE RATE                             
SVECRATE DS    PL4                 EURO COST RATE                               
SVCLIOFF DS    CL2                 CLIENT OFFICE                                
SVMED    DS    CL1                 MEDIA CODE                                   
SVSJROFF DS    CL2                 CLIENT OFFICE FOR GETTING RATES              
SVNARRLN DS    XL1                                                              
SVNARR   DS    CL60                                                             
SV1NNAME DS    0CL36                                                            
SVSJCLNM DS    CL36                                                             
SVSJPRNM DS    CL36                                                             
SVERROR  DS    XL2                 ERROR STATUS                                 
SVSTAT1  DS    XL1                 LINE STATUS #1                               
SVSRFRSH EQU   X'80'               REFRESH RATE & INCOME ACCOUNT                
SVTXRFSH EQU   X'40'               REFRESH TAX INFORMATION                      
SVKSTAT  DS    XL1                 SAVED TSAR KEY STATUS                        
*                                                                               
SVTMPLN# DS    XL2                 SAVED TEMPO LINE #                           
SVDTWC   DS    CL2                 DEFAULT TAX WORKCODE                         
SVDTLOC  DS    CL8                 DEFAULT TAX LOCALITY                         
SVTAXSTA DS    XL1                 TAX STATUS                                   
SVTAXOVR EQU   X'01'               TAX INFORMATION WAS OVERRIDDEN               
SVTAXREQ EQU   X'02'               TAX INFORMATION IS REQUIRED                  
SVTAXNO  EQU   X'04'               NO TAX ON THIS ITEM                          
*                                                                               
SVTX     DS    0C                                                               
SVTXWC   DS    CL2                 TAX WORKCODE                                 
SVTXLOC  DS    CL8                 TAX LOCALITY                                 
SVTXBAS  DS    PL6                 TAX BASIS                                    
SVTXMINI DS    XL1                 # MINI TAX ELEMENTS                          
SVTXBLCK DS    XL(TIMTMINQ*4)      MAX = 4 LEVEL TAX LOCALITY                   
         ORG   SVTXBLCK                                                         
SVTXACC  DS    CL14                TAX ACCOUNT                                  
SVTXEFF  DS    PL3                 TAX RATE EFFECTIVE DATE                      
SVTXRATE DS    PL4                 TAX RATE                                     
SVTXAMNT DS    PL6                 TAX AMOUNT                                   
         ORG   SVTXBLCK+L'SVTXBLCK                                              
SVTXLNQ  EQU   *-SVTX                                                           
*                                                                               
SVFLDBLK DS    XL255               FIELD SAVE BLOCK                             
*                                                                               
SVVKBLKQ EQU   *-SVVKBLCK                                                       
SVVRBLKQ EQU   *-SVVRBLCK                                                       
*                                                                               
TTRNBLK  DS    CL(TTRNLNQ)         TIMETRN CONTROL BLOCK                        
TTRNBUF  DS    CL(TRNBUFLN)        TIMETRN BUFFER                               
TRNBUFLN EQU   2048                                                             
TEND     DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
PFBD     EQU   PF1                 BD+                                          
PFNB     EQU   PF2                 NB                                           
PFNBC    EQU   PF3                 NB+                                          
PFB      EQU   PF4                 B                                            
PFBC     EQU   PF5                 B+                                           
PFUPDATE EQU   PF6                 UPDATE TIMESHEET                             
PFUP     EQU   PF7                 UP                                           
PFDOWN   EQU   PF8                 DOWN                                         
PFSAVE   EQU   PF9                 SAVE TIMESHEET                               
PFUNMARK EQU   PF10                UNMARK PAGE                                  
PFNEXT   EQU   PF11                NEXT ITEM                                    
PFRETURN EQU   PF12                RETURN FROM LIST                             
PFALTPFS EQU   PF13                ALTERNATE PF KEY LINE                        
PFCLEAR  EQU   PF14                REFRESH                                      
PFTAX    EQU   PF15                TAX SCREEN                                   
PFTIMREP EQU   PF16                TIME/REPORT                                  
PFTEMPO  EQU   PF19                TEMPO UPLOAD SCREEN                          
         EJECT                                                                  
***********************************************************************         
* GLOBAL STORAGE                                                      *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE ACGETRATED                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDLANGEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117ACCAP40   11/05/20'                                      
         END                                                                    
