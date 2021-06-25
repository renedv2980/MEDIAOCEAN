*          DATA SET ACCAP42    AT LEVEL 028 AS OF 10/27/17                      
*PHASE T61D42A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP42 -- TIME/LIST SCREEN                          *         
*                                                                     *         
*  COMMENTS:     LISTS TIME BY WEEK/O/D/S/CLI/PRO/JOB                 *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       N/A                                                  *         
*                                                                     *         
*  OUTPUTS:      LIST                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
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
* NSHE 012 23OCT07 FIX TO CLIENT FILTERING FOR 5 CHARACTER CODES      *         
* SGAV 028 27JUL17 <SPEC-14474> COST/TIME TO LIST 0 HOUR T/S RECORDS  *         
***********************************************************************         
         TITLE 'T61D42 - TIME/LIST'                                             
T61D42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D42**,R7,RR=R3                                              
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORGAE)                     
         ST    R3,RELO                                                          
*                                                                               
         LA    R2,CONRECH                                                       
*&&US*&& TM    BCCPYST7,CPYSTMSY   TIME MAGANGEMENT SYSTEM IN USE               
*&&US*&& BNO   ENATMS              NOT AUTHORIZED FOR TMS                       
*                                                                               
         TWAXC CONWHENH,CONWHENH   THE PRINT FIELD                              
         MVI   CONWHENH+5,0        ZERO OUT LENGTH                              
         OI    CONWHENH+6,X'80'                                                 
         XC    SVPERDTE,SVPERDTE   CLEAR PERIOD EVERY TIME THRU LIST            
         XC    SVFLTS,SVFLTS       CLEAR SAVED AREA FOR FILTERS                 
         NI    CONOPTH+1,X'FF'-X'20'  UNPROTECT                                 
         LA    R2,CONOPTH                                                       
         GOTO1 AVALOPTS,(R2)       EXTRACT OPTIONS - BCOPTS SET                 
         BNE   ACCERRX                                                          
         TM    4(R2),X'80'         WAS FIELD INPUTED THIS TIME?                 
         BNO   *+10                                                             
         XC    SAVEKEY,SAVEKEY     IF IT WAS-CLEAR KEY                          
         MVC   SVOPT1,BCOPT1                                                    
         MVC   SVOPT2,BCOPT2                                                    
         MVC   SVFLTCLI,BCFLTCLI                                                
         MVC   SVFLTPRO,BCFLTPRO                                                
         MVC   SVFLTJOB,BCFLTJOB                                                
         MVC   SVFLTTSK,BCFLTTSK                                                
         MVC   SVFLTMOA,BCFLTMOA                                                
         MVC   SVFLTTYP,BCFLTTYP                                                
         MVC   SVFLTCA,BCFLTCA                                                  
         OI    4(R2),X'20'         VALIDATED OPTIONS                            
*                                                                               
MAIN10   MVC   AIO,AIO1            DEFAULT AIO                                  
         MVI   TSARSTAT,0                                                       
         MVI   BOTSCR,0            INITIALIZE BOTTOM SCREEN FOR TMS             
*                                                                               
         LA    R2,PFTABLE          INITIALIZE PFKEYS TO PFKEY TABLE             
         LA    R3,TSLPFKYH                                                      
PFKEYS   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LISTR                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING PERVALD,R5                                                       
VK       DS    0H                  *** VALIDATE YEAR ***                        
         NI    LSTSTAT,LSTPRNTD    RESET EVERYTHING BUT PRINTED                 
         TM    TSLYEARH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK050                                                            
         NI    LSTSTAT,X'FF'-LSTPRNTD    RESET PRINTED                          
         NI    FLDFINP,X'FF'-FLDALLYR                                           
         LA    R2,TSLYEARH                                                      
         OC    AC@ALL,BCSPACES     CHEATING - IF PROBLEM GET AHYD               
         CLC   TSLYEAR,AC@ALL      NO RESTRICTIONS                              
         BNE   VK010                                                            
         OI    FLDFINP,FLDALLYR    ALL YEAR                                     
         NI    TSLSDTEH+4,X'DF'    FORCE VALIDATION IN CASE OF PERS             
         NI    TSLEDTEH+4,X'DF'    FORCE VALIDATION IN CASE OF PERS             
         XC    CALSDATE,CALSDATE                                                
         XC    CALEDATE,CALEDATE                                                
         B     VK020                                                            
VK010    XC    CALDATE,CALDATE                                                  
         LA    R5,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(TSLYEARH+5,TSLYEAR),(X'60',BLOCK)                   
         CLI   DMCB+4,X'04'        ONLY ONE DATE                                
         BE    VK015                                                            
         CLI   DMCB+4,X'03'        TODAY'S DATE                                 
         BE    VK013                                                            
         CLI   DMCB+4,0            OK                                           
         BE    VK015                                                            
         B     ERRINV                                                           
*                                                                               
VK013    OI    LSTSTAT,USETODAY    USES TODAY'S DATE                            
         MVC   CALDATE,PVALPSTA    TODAY'S DATE                                 
         B     *+10                                                             
VK015    MVC   CALYEAR,PVALPSTA    JUST THE YEAR                                
         BAS   RE,GETCAL                                                        
VK020    OI    TSLYEARH+4,X'20'    VALIDATED                                    
         OI    TSLYEARH+6,X'80'    TRANSMIT                                     
*                                                                               
VK050    XC    SVDSKDA,SVDSKDA     CLEAR SAVE DISK ADDRESS                      
*                                  *** VALIDATE PERSON CODE ***                 
         TM    TSLCODEH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK100                                                            
         NI    LSTSTAT,X'FF'-LSTPRNTD    RESET PRINTED                          
         XC    SAVEKEY,SAVEKEY                                                  
         LA    R2,TSLCODEH                                                      
         CLI   TSLCODEH+5,0                                                     
         BE    ERRPLS                                                           
         MVI   BCIFMIN,1           MINIMUM LENGTH - PERSON REQUIRED             
         MVC   BCIFMAX,BC1RLEV4    MAXIMUM LENGTH                               
         GOTO1 AFVAL,TSLCODEH                                                   
         BH    ACCERRX                                                          
         GOTO1 AVALPRSN,TSLCODEH                                                
         BNE   EINVPER                                                          
         MVC   TSLNAME,BCWORK      DISPLAY PERSON NAME                          
*                                                                               
*&&US                                                                           
         MVI   BCBYTE1,NAMEFLDQ    CHECK ACCESS TO VIEW NAME                    
         BAS   RE,FLDSEC           SECURITY TO VIEW NAME                        
         BNL   *+8                                                              
         OI    TSLNAMEH+1,X'0C'    LOW INTENSITY                                
*&&                                                                             
         OI    TSLNAMEH+6,X'80'                                                 
         OI    TSLCODEH+4,X'20'    VALIDATED                                    
         OI    TSLCODEH+6,X'80'    TRANSMIT                                     
*                                                                               
VK100    TM    TSLSDTEH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK150               YES, SKIP IT                                 
         NI    LSTSTAT,X'FF'-LSTPRNTD    RESET PRINTED                          
         LA    R2,TSLSDTEH                                                      
         MVC   FLTSTDTE,BCEFFS                                                  
         SR    R1,R1                                                            
         ICM   R1,1,TSLSDTEH+5                                                  
         BZ    VK150                                                            
         GOTO1 =A(VALDATE),DMCB,TSLSDTEH,FLTSTDTE,RR=RELO                       
         BH    ACCERRX                                                          
         OI    TSLSDTEH+4,X'20'    VALIDATED                                    
         OI    TSLSDTEH+6,X'80'    TRANSMIT                                     
*                                                                               
VK150    TM    TSLEDTEH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK200               YES, SKIP IT                                 
         NI    LSTSTAT,X'FF'-LSTPRNTD    RESET PRINTED                          
         LA    R2,TSLEDTEH                                                      
         XC    FLTENDTE,FLTENDTE                                                
         SR    R1,R1                                                            
         ICM   R1,1,TSLEDTEH+5                                                  
         BZ    VK200                                                            
         GOTO1 =A(VALDATE),DMCB,TSLEDTEH,(X'80',FLTENDTE),RR=RELO               
         BH    ACCERRX                                                          
         OI    TSLEDTEH+4,X'20'    VALIDATED                                    
         OI    TSLEDTEH+6,X'80'    TRANSMIT                                     
*                                                                               
VK200    NI    FLDFINP,X'FF'-FLDFCLT-FLDFPRD-FLDFJOB-FLDFTSK-FLD1NCLT           
         XC    FLTCLTLN(4),FLTCLTLN                                             
         CLI   TSLCLNTH+5,0        SET UP INPUT FLAGS                           
         BZ    *+14                                                             
         OI    FLDFINP,FLDFCLT                                                  
         MVC   FLTCLTLN,TSLCLNTH+5                                              
         CLI   TSLPRODH+5,0                                                     
         BZ    *+14                                                             
         OI    FLDFINP,FLDFPRD                                                  
         MVC   FLTPRDLN,TSLPRODH+5                                              
         CLI   TSLJOBH+5,0                                                      
         BZ    *+14                                                             
         OI    FLDFINP,FLDFJOB                                                  
         MVC   FLTJOBLN,TSLJOBH+5                                               
         CLI   TSLTASKH+5,0                                                     
         BZ    *+14                                                             
         OI    FLDFINP,FLDFTSK                                                  
         MVC   FLTTSKLN,TSLTASKH+5                                              
*                                                                               
         CLC   FLTCLTLN,BCSJLNQ1   IF CLIENT>SJ CLI LN-ASSUME ACCT              
         BNH   VK200A                                                           
         OI    FLDFINP,FLD1NCLT                                                 
         TM    FLDFINP,FLDFPRD+FLDFJOB+FLDFTSK                                  
         BZ    VK200A                                                           
         LA    R2,TSLPRODH         ERROR IF PRD/JOB/TSK USED                    
         TM    FLDFINP,FLDFPRD                                                  
         BO    ERRINV                                                           
         LA    R2,TSLJOBH                                                       
         TM    FLDFINP,FLDFJOB                                                  
         BO    ERRINV                                                           
         LA    R2,TSLTASKH                                                      
         B     ERRINV                                                           
*                                                                               
VK200A   LA    R2,TSLCLNTH                                                      
         SR    R1,R1                                                            
         TM    TSLCLNTH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK210               YES, SKIP IT                                 
         ICM   R1,1,TSLCLNTH+5                                                  
         BNZ   *+16                                                             
*&&US*&& TM    FLDFINP,FLDFPRD+FLDFJOB                                          
*&&UK*&& TM    FLDFINP,FLDFPRD+FLDFJOB+FLDFTSK                                  
         BNZ   ERRMISS                                                          
         B     VK200X                                                           
         MVC   FLTCLT,BCSPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTCLT(0),TSLCLNT                                                
VK200X   OI    TSLCLNTH+4,X'20'    VALIDATED                                    
         OI    TSLCLNTH+6,X'80'    TRANSMIT                                     
*                                                                               
VK210    TM    FLDFINP,FLD1NCLT    1N ACCT, SKIP PROD/JOB/TASK                  
         BO    VK300                                                            
         TM    TSLPRODH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK220               YES, SKIP IT                                 
         LA    R2,TSLPRODH         POINT TO FIELD FOR ERROR                     
         ICM   R1,1,TSLPRODH+5                                                  
         BNZ   *+16                                                             
*&&US*&& TM    FLDFINP,FLDFJOB                                                  
*&&UK*&& TM    FLDFINP,FLDFJOB+FLDFTSK                                          
         BNZ   ERRMISS                                                          
         B     VK220                                                            
         MVC   FLTPRD,BCSPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTPRD(0),TSLPROD                                                
         OI    TSLPRODH+4,X'20'    VALIDATED                                    
         OI    TSLPRODH+6,X'80'    TRANSMIT                                     
*                                                                               
VK220    TM    TSLJOBH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BO    VK230               YES, SKIP IT                                 
         LA    R2,TSLJOBH          POINT TO FIELD FOR ERROR                     
         ICM   R1,1,TSLJOBH+5                                                   
*&&US*&& BZ    VK230                                                            
*&&UK                                                                           
         BNZ   *+16                                                             
         TM    FLDFINP,FLDFTSK                                                  
         BNZ   ERRMISS                                                          
         B     VK230                                                            
*&&                                                                             
         MVC   FLTJOB,BCSPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTJOB(0),TSLJOB                                                 
         OI    TSLJOBH+4,X'20'     VALIDATED                                    
         OI    TSLJOBH+6,X'80'     TRANSMIT                                     
*                                                                               
VK230    TM    TSLTASKH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BO    VK300               YES, SKIP IT                                 
         LA    R2,TSLTASKH         POINT TO FIELD FOR ERROR                     
         ICM   R1,1,TSLTASKH+5                                                  
         BZ    VK300                                                            
         MVC   FLTTSK,BCSPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTTSK(0),TSLTASK                                                
         OI    TSLTASKH+4,X'20'    VALIDATED                                    
         OI    TSLTASKH+6,X'80'    TRANSMIT                                     
*                                                                               
VK300    LA    R2,TSLACTNH         POINT CURSOR TO FIRST LINE                   
         XC    ACURFORC,ACURFORC   SO WE HAVE TO ZERO THIS OUT                  
VKX      B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CHECK FIELD SECURITY FOR DISPLAYING FIELDS                         *          
**********************************************************************          
         SPACE 1                                                                
*&&US                                                                           
FLDSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R2,BCBYTE1                                                       
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    ROUTE                                                            
         CLI   DMCB,SECPREAD                                                    
         BE    ROUTH                                                            
         B     ROUTL                                                            
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
* GET CALENDAR YEAR, DEFAULT CURRENT YEAR                            *          
**********************************************************************          
         SPACE 1                                                                
GETCAL   NTR1                                                                   
         LA    R2,TSLYEARH                                                      
         XC    CALSDATE,CALSDATE   INIT                                         
         XC    CALEDATE,CALEDATE                                                
*                                                                               
         USING CASRECD,R6          READ FOR CALENDAR                            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA,CALDATE    DATE TO READ HIGH FOR                        
         GOTO1 HIGH                                                             
*                                                                               
* AS PER PSHA - IF YEAR ENTERED ONLY SHOW THAT YEAR IF ANY                      
*                                                                               
         LA    R1,3                DONT CHECK YR                                
         TM    LSTSTAT,USETODAY    USE'S TODAYS DATE                            
         BO    *+8                       DONT CHECK YEAR                        
         LA    R1,4                      CHECK YR                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BIGKEY(0),KEYSAVE     SAME COMP                                  
         BNE   ENOCAL                                                           
*        CLI   CALDATE+1,0         COMPARE ON YEAR IF 0                         
*        BE    *+14                                                             
*        CLC   CASKEMOA(1),CALDATE SAME YEAR                                    
*        BH    EFLDRQ              FIELD IS REQUIRED                            
         MVC   CALKEY,BIGKEY       SAVE KEY                                     
         MVC   AIO,AIO3                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING CASRECD,R6                                                       
         L     R6,AIO3                                                          
         MVC   CALYEAR,CASKEMOA    REAL YEAR                                    
*                                                                               
         USING TMRELD,R6           SAVE CALENDAR START AND END DATES            
         L     R6,AIO3                                                          
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL            GET START YEAR                               
         BE    *+6                                                              
         DC    H'0'                TELL ANN MARIE ABOUT THIS RECORD             
*                                                                               
         ZICM  R1,TMRSTART,3                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,CALSDATE                                                    
         ZICM  R1,TMREND,3                                                      
         LNR   R1,R1                                                            
         STCM  R1,7,CALEDATE                                                    
*                                                                               
GCX      MVC   8(2,R2),BCSPACES    DISPLAY                                      
         MVC   BCWORK(1),CALYEAR                                                
         MVC   BCWORK+1(2),=X'0101'                                             
         GOTO1 DATCON,DMCB,(1,BCWORK),(11,WORK)                                 
         MVC   8(2,R2),WORK+6      CHARACTER YEAR                               
         MVI   5(R2),X'02'                                                      
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* VALDATE - VALIDATES PERIOD OR DATE                                 *          
*           CALLED W/ GOTO1, PARM1=FIELD                             *          
*                            PARM2=FILTER ADDRESS (HOB, USE ENDDATE) *          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
VALDATE  NTR1                                                                   
         L     R2,0(R1)            FIELD ADDRESS                                
         L     R4,4(R1)            FILTER ADDRESS                               
*                                                                               
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK+L'PVALOUTB                                              
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK+L'PVALOUTB),C',=-='                   
         CLI   DMCB+4,1                                                         
         BNE   VDBAD                                                            
         CLI   5(R2),3             L'INPUT>3 MEANS DATE  EX/'051594'            
         BH    VD130                                                            
         TM    SC1STVAL,SCNUMQ     DID THEY ENTER A PERIOD NUMBER               
         BZ    VD130                                                            
*                                                                               
         TM    FLDFINP,FLDALLYR    DOES YEAR = ALL?                             
         BO    VDBADPER                                                         
         L     R6,AIO3                                                          
         CLC   CALKEY,0(R6)        DO WE STILL HAVE CALENDAR RECORD?            
         BE    VD050               YES, DON'T HAVE TO READ AGAIN                
         MVC   BIGKEY,CALKEY       USE CALENDAR KEY                             
         MVC   AIO,AIO3                                                         
         GOTO1 READ                READ RECORD IN CASE IT IS GONE               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING TMPELD,R6                                                        
VD050    L     R6,AIO3                                                          
         MVI   ELCODE,TMPELQ       GET TIMESHEET PERIODS ELEMENT                
         BAS   RE,GETEL                                                         
VD100    BNE   VDBAD                                                            
         CLC   TMPNUMB,SC1STNUM+3  CHECK PERIOD #                               
         BE    VD110                                                            
         BAS   RE,NEXTEL           BUMP TO NEXT ONE                             
         B     VD100                                                            
*                                                                               
VD110    SR    R1,R1                                                            
         ICM   R1,7,TMPSTART                                                    
         CLM   R4,8,=X'80'                                                      
         BNE   *+8                                                              
         ICM   R1,7,TMPEND                                                      
         B     VD135                                                            
*                                                                               
         USING PERVALD,R5                                                       
VD130    LA    R5,BCWORK                                                        
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'60',BCWORK)                         
         TM    DMCB+4,3            FIELD 1 INVALID/FIELD 2 INVALID              
         BNZ   VDBAD                                                            
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(17,8(R2))                              
         SR    R1,R1                                                            
         ICM   R1,7,PVALPSTA       GET THE DATE                                 
VD135    LNR   R1,R1                                                            
         STCM  R1,7,0(R4)                                                       
         B     ROUTE                                                            
*                                                                               
VDBAD    MVC   GERROR,=AL2(ACEINV) INVALID INPUT FIELD                          
         B     ROUTH                                                            
VDBADPER MVC   GERROR,=AL2(ACEENTDT) ENTER DATE                                 
         B     ROUTH                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*              LIST - USING 1R RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
LISTR    DS    0H                                                               
         LA    R4,STMBLOCK                                                      
         USING SVTIMED,R4                                                       
         XC    CALBLOCK,CALBLOCK                                                
         MVI   NLISTS,MAXLINES     # LINES/SCREEN                               
         MVC   AIO,AIO1            USE AIO1                                     
         ZAP   AMTBILL,=P'0'       INIT AMOUNTS                                 
         ZAP   AMTNBILL,=P'0'                                                   
         ZAP   AMTREAL,=P'0'                                                    
         ZAP   SCRBILL,=P'0'       INIT AMOUNTS                                 
         ZAP   SCRNBILL,=P'0'                                                   
         ZAP   SCRREAL,=P'0'                                                    
         MVC   TSLSCRT,BCSPACES                                                 
         OI    TSLSCRTH+6,X'80'                                                 
         MVC   TSLTTOT,BCSPACES                                                 
         OI    TSLTTOTH+6,X'80'                                                 
         NI    LSTSTAT,X'FF'-LASTLINE                                           
*                                                                               
*              BUILD X'3E0F' PASSIVE KEY                                        
*                                                                               
         USING TSWRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         CLI   LISTSW,C'T'         COMING BACK FROM SOMEWHERE?                  
         BNE   LISTR10                                                          
         BAS   RE,UP2GRAND         FIND GRAND TOTAL UPTO LASTKEY                
         MVC   BIGKEY,LASTKEY                                                   
         B     LISTRHI                                                          
*                                                                               
LISTR10  OI    LSTSTAT,FRSTSCRN                                                 
         OC    BIGKEY,BIGKEY       1ST TIME IN?                                 
         BZ    LISTR40                                                          
         OC    SAVEKEY,SAVEKEY     CLEARED FROM NEW INPUT?                      
         BZ    LISTR40                                                          
         NI    LSTSTAT,X'FF'-FRSTSCRN                                           
         OI    LSTSTAT,GOTGRAND    GOT GRAND TOTALS BEFORE                      
         MVC   BIGKEY,SAVEKEY                                                   
         MVC   LASTKEY,SAVEKEY                                                  
         BAS   RE,BLDSVTIM         BUILD SAVE TIME BLOCK                        
*                                                                               
LISTR20  OC    SVWKEND,SVWKEND     ANY SAVED RECORDS?                           
         BZ    LISTR30                                                          
         CLC   SVWKEND,BIGKEY+(TSWKEND-TSWRECD)                                 
         BNL   LISTR30                                                          
         LA    R4,SVTIMELN(R4)     SKIP THOSE WE PAST                           
         B     LISTR20                                                          
*                                                                               
LISTR30  B     LISTRHI                                                          
*                                                                               
LISTR40  ZAP   TOTBILL,=P'0'       INIT GRAND TOTALS                            
         ZAP   TOTNBILL,=P'0'                                                   
         ZAP   TOTREAL,=P'0'                                                    
         XC    TSWKEY,TSWKEY                                                    
         BAS   RE,BLDSVTIM         BUILD SAVE TIME BLOCK                        
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,CMPY        COMPANY CODE                                 
         MVC   TSWKPER,BCSPACES                                                 
         ZIC   R1,TSLCODEH+5       PERSON CODE                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSWKPER(0),TSLCODE                                               
         MVC   TSWKEND,FLTENDTE                                                 
         OC    CALEDATE,CALEDATE                                                
         BZ    *+10                                                             
         MVC   TSWKEND,CALEDATE                                                 
*                                                                               
LISTRHI  GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    SVDSKDA,SVDSKDA     FIRST RECORD OF GROUP?                       
         BNZ   *+10                                                             
         MVC   LASTKEY,TSWKEY                                                   
         B     LISTR90                                                          
*                                                                               
LISTRSQ  GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEYSAVE(TSWKEND-TSWKEY),BIGKEY   COMPARE UP TO PERSON            
         BE    *+12                                                             
         OI    LSTSTAT,LASTLINE                                                 
         B     *+14                                                             
         CLC   KEYSAVE(TSWKULC-TSWKEY),BIGKEY   COMPARE UP TO ULC               
         BE    LISTR90                                                          
         MVC   DMDSKADD,SVDSKDA    INSURE CORRECT DA TABLE                      
         XC    SVDSKDA,SVDSKDA     PREPARE FOR NEXT ONE                         
         OC    LISTAR,LISTAR                                                    
         BZ    LISTR90                                                          
         MVC   SAVEKEY,BIGKEY                                                   
         AP    SCRBILL,AMTBILL                                                  
         AP    SCRNBILL,AMTNBILL                                                
         AP    SCRREAL,AMTREAL                                                  
         TM    LSTSTAT,GOTGRAND    GOT GRAND TOTALS?                            
         BO    LISTR50             YES, NO NEED TO ADD                          
         AP    TOTBILL,AMTBILL                                                  
         AP    TOTNBILL,AMTNBILL                                                
         AP    TOTREAL,AMTREAL                                                  
LISTR50  OI    GLSTSTAT,RETEXTRA   RETURN EXTRA TIME IF END OF PAGE             
         OI    LSTSTAT,LSTPRNTD                                                 
         GOTO1 LISTMON                                                          
         BE    LISTR80                                                          
*                                                                               
LISTR60  TM    LSTSTAT,LASTLINE+FRSTSCRN                                        
         BO    LISTR70             JUST SHOW GRAND TOTAL                        
         ZAP   SCRTOTAL,SCRBILL                                                 
         AP    SCRTOTAL,SCRNBILL                                                
         AP    SCRTOTAL,SCRREAL                                                 
         LA    R5,TSLSCRT                                                       
         USING LSTLINED,R5                                                      
         MVC   LSTDEPT,BCSPACES                                                 
         MVC   LSTDEPT(L'BC@LSCRN),BC@LSCRN                                     
         CURED SCRBILL,(L'LSTBILL,LSTBILL),2,ZERO=NOBLANK,MINUS=YES             
         CURED SCRNBILL,(L'LSTNBILL,LSTNBILL),2,ZERO=NOBLANK,MINUS=YES          
         CURED SCRREAL,(L'LSTREAL,LSTREAL),2,ZERO=NOBLANK,MINUS=YES             
         CURED SCRTOTAL,(L'LSTTOTAL,LSTTOTAL),2,ZERO=NOBLANK,MINUS=YES          
         OI    TSLSCRTH+6,X'80'    TRANSMIT                                     
         TM    LSTSTAT,GOTGRAND                                                 
         BO    LISTR70                                                          
*                                                                               
         BAS   RE,CALCGRND                                                      
*                                                                               
LISTR70  ZAP   TOTTOTAL,TOTBILL                                                 
         AP    TOTTOTAL,TOTNBILL                                                
         AP    TOTTOTAL,TOTREAL                                                 
         LA    R5,TSLTTOT                                                       
         USING LSTLINED,R5                                                      
         MVC   LSTDEPT,BCSPACES                                                 
         MVC   LSTDEPT(L'BC@LTOTL),BC@LTOTL                                     
         CURED TOTBILL,(L'LSTBILL,LSTBILL),2,ZERO=NOBLANK,MINUS=YES             
         CURED TOTNBILL,(L'LSTNBILL,LSTNBILL),2,ZERO=NOBLANK,MINUS=YES          
         CURED TOTREAL,(L'LSTREAL,LSTREAL),2,ZERO=NOBLANK,MINUS=YES             
         CURED TOTTOTAL,(L'LSTTOTAL,LSTTOTAL),2,ZERO=NOBLANK,MINUS=YES          
         OI    TSLTTOTH+6,X'80'    TRANSMIT                                     
         TM    LSTSTAT,LASTLINE                                                 
         BO    LISTRX              HAVE TO LEAVE                                
         GOTO1 LISTMON                                                          
         DROP  R5                                                               
*                                                                               
LISTR80  XC    LISTAR,LISTAR                                                    
         ZAP   AMTBILL,=P'0'       INIT AMOUNTS                                 
         ZAP   AMTNBILL,=P'0'                                                   
         ZAP   AMTREAL,=P'0'                                                    
*                                                                               
LISTR90  DS    0H                                                               
         NI    LSTSTAT,X'FF'-RECFOUND                                           
         LA    R6,BIGKEY                                                        
         CLC   KEYSAVE(TSWKEND-TSWKEY),BIGKEY   CMP UP TO PERSON CODE           
         BNE   LISTR120                                                         
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         LA    R3,OFFAWORK         OFFICE LIST                                  
*                                                                               
         LA    RE,OFFAOLDA                                                      
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   *+8                                                              
         LA    RE,OFFANEWA                                                      
         OC    0(L'OFFANEWA,RE),0(RE)                                           
         BZ    LISTR110            UNLIMITED ACCESS-DONT CHECK FURTHER          
         DROP  R1                                                               
*                                                                               
         LA    RF,32               MAX # OF OLD OFFICES (16/PAGE*2)             
         LA    RE,1                ONE BYTE LENGTH                              
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   *+16                # OF OFFICES IS NOT INCL IN LIST             
         LA    RE,2                ONE BYTE LENGTH                              
         LH    RF,0(R3)            RF=# OF OFFICES WHICH ARE VALID              
         LA    R3,2(R3)            BUMP PAST THIS                               
         LR    R1,RE               SAVE LENGTH FOR COMPARE                      
*                                                                               
         BCTR  R1,0                                                             
LISTR100 EXCLC R1,0(R3),BCSPACES   ANYTHING IN LIST?                            
         BNH   LISTRSQ             NOT IN LIST-SKIP                             
         EXCLC R1,0(R3),TSWKODS    COMPARE OFFICE IN KEY TO LIST                
         BE    LISTR110            OFFICE IN LIST-CONTINUE                      
         AR    R3,RE               BUMP TO NEXT POSITION IN TABLE               
         BCT   RF,LISTR100                                                      
         B     LISTRSQ             NOT IN LIST-SKIP                             
*                                                                               
LISTR110 CLC   TSWKEND,FLTENDTE    FILTER                                       
         BL    LISTRSQ                                                          
         OC    CALEDATE,CALEDATE                                                
         BZ    *+14                                                             
         CLC   TSWKEND,CALEDATE                                                 
         BL    LISTRSQ                                                          
         CLC   TSWKEND,FLTSTDTE    QUIT IF OUT OF RANGE                         
         BH    LISTR120                                                         
         OC    CALSDATE,CALSDATE                                                
         BZ    LISTR140                                                         
         CLC   TSWKEND,CALSDATE                                                 
         BNH   LISTR140                                                         
*                                                                               
LISTR120 TM    LSTSTAT,LSTPRNTD    ANYTHING AT ALL?                             
         BO    LISTR130            LEAVE                                        
         TM    LSTSTAT,FRSTSCRN    AND FIRST MEANS NOTHING                      
         BZ    LISTRX                                                           
         MVC   LASTKEY,KEYSAVE                                                  
LISTR130 OI    LSTSTAT,LASTLINE+NOTMRECS                                        
         B     LISTR150            MAY HAVE MORE SAVED RECORDS                  
*                                                                               
LISTR140 OC    SVDSKDA,SVDSKDA     FIRST RECORD OF GROUP?                       
         BNZ   *+10                                                             
         MVC   SVDSKDA,TSWKDA                                                   
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING TIMRECD,R6                                                       
         CLC   TIMKREF,=C'*TIME*'  HAS TO BE A TIME RECORD                      
         BE    *+10                                                             
         CLC   TIMKEY(TIMKSBR-TIMKEY),SAVEKEY    OR MATCH THE LAST ONE          
         BNE   LISTRSQ                                                          
         BAS   RE,FILTER           FILTER OUT OPTIONS                           
         BNE   LISTRSQ                                                          
         BAS   RE,EXTRAMTS         EXTRACT AMOUNTS FROM RECORD                  
         TM    LSTSTAT,RECFOUND                                                 
         BZ    LISTRSQ                                                          
*                                                                               
LISTR150 OC    SVWKEND,SVWKEND     ANY SAVED RECORD?                            
         BZ    LISTR180                                                         
         TM    LSTSTAT,RECFOUND    ONLY SHOW IF RECORD FOUND                    
         BZ    LISTR180                                                         
         TM    LSTSTAT,NOTMRECS    NO TIME RECORDS?                             
         BO    LISTR160            DON'T CHECK AGAINST KEY                      
         OC    SVFLTS,SVFLTS       IF THERE ARE OPTS DATES                      
         BNZ   LISTR190               MUST MATCH                                
         CLC   SVWKEND,BIGKEY+(TSWKEND-TSWRECD)                                 
         BH    LISTR180                                                         
         BL    LISTR160                                                         
         LA    R4,SVTIMELN(R4)     NEXT ONE                                     
         B     LISTR170                                                         
*                                                                               
LISTR160 LA    R5,LISTAR                                                        
         USING LSTLINED,R5                                                      
         MVI   LSTSVREC,C'*'                                                    
         ZICM  R1,SVWKEND,3                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TEMPDATE                                                    
         GOTO1 DATCON,BCDMCB,(1,TEMPDATE),(17,LSTPRDND)                         
         LA    R3,SVODS                                                         
         BAS   RE,SHOWODS          SHOW OFFICE/DEPT/SUBDEPT                     
         MVC   SAVEKEY,BIGKEY                                                   
         MVC   TEMPKEY,KEYSAVE     OR ELSE CALCGRAND WON'T WORK                 
         BAS   RE,SHOWPERD         SHOW PERIOD NUMBER                           
         MVC   KEYSAVE,TEMPKEY                                                  
         MVC   BIGKEY,SAVEKEY      GETCAL DESTROYS BIGKEY                       
         MVC   DMDSKADD,SVDSKADD   POINT GENCON TO THE D/A                      
         GOTO1 HIGH                RESTORE LAST POSITION                        
         CURED =P'0',(L'LSTBILL,LSTBILL),2,ZERO=NOBLANK,MINUS=YES               
         CURED =P'0',(L'LSTNBILL,LSTNBILL),2,ZERO=NOBLANK,MINUS=YES             
         CURED =P'0',(L'LSTREAL,LSTREAL),2,ZERO=NOBLANK,MINUS=YES               
         CURED =P'0',(L'LSTTOTAL,LSTTOTAL),2,ZERO=NOBLANK,MINUS=YES             
         OI    GLSTSTAT,RETEXTRA   RETURN EXTRA TIME                            
         GOTO1 LISTMON                                                          
         BNE   LISTR60                                                          
         XC    LISTAR,LISTAR       HAVE TO CLEAR IT                             
*                                                                               
         LA    R4,SVTIMELN(R4)     NEXT ONE                                     
         B     LISTR150                                                         
*                                                                               
LISTR170 MVI   LISTAR,C'*'                                                      
         B     LISTR190                                                         
*                                                                               
LISTR180 TM    LSTSTAT,LASTLINE                                                 
         BO    LISTR60                                                          
*                                                                               
LISTR190 MVC   SAVEKEY,BIGKEY      SAVE KEY FOR LATER                           
         LA    R5,LISTAR                                                        
         CLI   LSTPRDNM,C'#'       DID WE FILL IN STUFF BEFORE?                 
         BE    LISTR200            JUST SHOW NEW AMOUNTS                        
         MVC   LSTPRDNM(L'LISTAR-1),BCSPACES                                    
         GOTO1 DATCON,BCDMCB,(1,TIMKPEDT),(17,LSTPRDND)                         
*                                                                               
         LA    R3,TIMKACT                                                       
         BAS   RE,SHOWODS          SHOW IT ON SCREEN                            
         MVC   TEMPDATE,TIMKPEDT                                                
         BAS   RE,SHOWPERD         SHOW PERIOD NUMBER                           
*                                                                               
LISTR200 CURED AMTBILL,(L'LSTBILL,LSTBILL),2,ZERO=NOBLANK,MINUS=YES             
         CURED AMTNBILL,(L'LSTNBILL,LSTNBILL),2,ZERO=NOBLANK,MINUS=YES          
         CURED AMTREAL,(L'LSTREAL,LSTREAL),2,ZERO=NOBLANK,MINUS=YES             
         CURED AMTTOTAL,(L'LSTTOTAL,LSTTOTAL),2,ZERO=NOBLANK,MINUS=YES          
         MVC   BIGKEY,SAVEKEY      RESTORE KEY KILLED BY GETCAL                 
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                RESTORE POSITION                             
         B     LISTRSQ                                                          
*                                                                               
LISTRX   LA    R2,TSLACTNH         FORCE CURSOR TO 1ST LIST ENTRY               
         ST    R2,ACURFORC                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BLDSVTIM - BUILD SAVE TIME RECORD BLOCK                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TSSRECD,R6                                                       
BLDSVTIM NTR1                                                                   
         LA    R6,BIGKEY                                                        
         LA    R4,STMBLOCK                                                      
         MVC   TEMPKEY,BIGKEY      HAVE TO RESET WHEN WE LEAVE                  
         XC    STMBLOCK,STMBLOCK                                                
*                                                                               
         MVI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         MVI   TSSKSUB,TSSKSUBQ    X'11'                                        
         MVC   TSSKCPY,CMPY        COMPANY CODE                                 
         MVC   TSSKPER,BCSPACES                                                 
         ZIC   R1,TSLCODEH+5       PERSON CODE                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSSKPER(0),TSLCODE                                               
         MVC   TSSKEND,FLTENDTE                                                 
         OC    CALEDATE,CALEDATE                                                
         BZ    *+10                                                             
         MVC   TSSKEND,CALEDATE                                                 
*                                                                               
BLDSV010 LA    R5,MAXLINES                                                      
BLDSV020 GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BLDSV040                                                         
*                                                                               
BLDSV030 GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDSV040 CLC   KEYSAVE(TSWKEND-TSWKEY),BIGKEY   CMP UP TO PERSON CODE           
         BNE   BLDSVXIT                                                         
         CLC   TSSKEND,FLTENDTE    FILTER                                       
         BL    BLDSV030                                                         
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         LA    R3,OFFAWORK         OFFICE LIST                                  
*                                                                               
         LA    RE,OFFAOLDA                                                      
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   *+8                                                              
         LA    RE,OFFANEWA                                                      
         OC    0(L'OFFANEWA,RE),0(RE)                                           
         BZ    BLDSV065            UNLIMITED ACCESS-DONT CHECK FURTHER          
         DROP  R1                                                               
*                                                                               
         LA    RF,32               MAX # OF OLD OFFICES (16/PAGE*2)             
         LA    RE,1                ONE BYTE LENGTH                              
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   *+16                # OF OFFICES IS NOT INCL IN LIST             
         LA    RE,2                ONE BYTE LENGTH                              
         LH    RF,0(R3)            RF=# OF OFFICES WHICH ARE VALID              
         LA    R3,2(R3)            BUMP PAST THIS                               
         LR    R1,RE               SAVE LENGTH FOR COMPARE                      
*                                                                               
         BCTR  R1,0                                                             
BLDSV050 EXCLC R1,0(R3),BCSPACES   ANYTHING IN LIST?                            
         BNH   BLDSV030            NOT IN LIST-SKIP                             
         EXCLC R1,0(R3),TSSKODS    COMPARE OFFICE IN KEY TO LIST                
         BE    BLDSV065            OFFICE IN LIST-CONTINUE                      
         AR    R3,RE               BUMP TO NEXT POSITION IN TABLE               
         BCT   RF,BLDSV050                                                      
         B     BLDSV030            NOT IN LIST-SKIP                             
*                                                                               
BLDSV065 OC    CALEDATE,CALEDATE                                                
         BZ    *+14                                                             
         CLC   TSSKEND,CALEDATE                                                 
         BL    BLDSV030                                                         
         CLC   TSSKEND,FLTSTDTE    QUIT IF OUT OF RANGE                         
         BH    BLDSVXIT                                                         
         OC    CALSDATE,CALSDATE                                                
         BZ    BLDSV070                                                         
         CLC   TSSKEND,CALSDATE                                                 
         BNH   BLDSV070                                                         
         B     BLDSVXIT                                                         
*                                                                               
BLDSV070 MVC   SVWKEND,TSSKEND                                                  
         MVC   SVODS,TSSKODS                                                    
         MVC   SVDSKADD,TSSKDA                                                  
         LA    R4,SVTIMELN(R4)                                                  
         BCT   R5,BLDSV030                                                      
*                                                                               
BLDSVXIT MVC   BIGKEY,TEMPKEY                                                   
         B     ROUTE                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SHOWODS - SHOWS OFFICE, DEPT AND SUBDEPT ON SCREEN, R3 -> INPUT     *         
***********************************************************************         
         SPACE 1                                                                
SHOWODS  SR    R1,R1                                                            
         IC    R1,BC1RLEV1                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   LSTOFFC(0),0(R3)                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,BC1RLEV2                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTDEPT(0),0(R3)                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,BC1RLEV3                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTSUB(0),0(R3)                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SHOWPERD - SHOWS PERIOD NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
SHOWPERD NTR1                                                                   
         LA    R3,CALBLOCK         FILL CALENDAR BLOCK                          
         USING CALD,R3                                                          
         MVC   AIO,AIO2            USED FOR GETCAL                              
         OC    CALBLOCK,CALBLOCK   NEVER USED CAL RECORD                        
         BZ    SPER050                                                          
         CLC   LASTOFFC,LSTOFFC    NEW OFFICE, NEW CALENDAR                     
         BNE   SPER050                                                          
         CLC   TEMPDATE,CALCSTRT   OUTSIDE CALENDAR RANGE, NEW ONE              
         BL    SPER050                                                          
         CLC   TEMPDATE,CALCEND                                                 
         BNH   SPER100                                                          
SPER050  XC    CALBLOCK(CALDQ),CALBLOCK                                         
         MVC   CALPYMD,TEMPDATE                                                 
         OI    CALSTAT,CALYMDQ     DATE ENTERED                                 
         MVC   CALOFF,LSTOFFC                                                   
         GOTO1 AGETCAL,CALBLOCK                                                 
         BNE   SPERXIT             ERROR DON'T SHOW PERIOD #                    
         MVC   LASTOFFC,LSTOFFC    NEW OFFICE SAVED                             
         B     SPER300                                                          
*                                                                               
SPER100  DS    0H                  WE HAVE CALENDAR ALREADY                     
         SR    R0,R0                                                            
         L     R1,AIO                                                           
         AH    R1,=Y(ACTRFST-ACTRECD)                                           
SPER110  CLI   0(R1),0                                                          
         BE    SPERXIT                                                          
         CLI   0(R1),TMPELQ        X'88'  TS PERIODS ELEMENT                    
         B     SPER120                                                          
SPER115  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     SPER110                                                          
*                                                                               
         USING TMPELD,R1                                                        
SPER120  CLC   TEMPDATE,TMPSTART   IN THE RANGE?                                
         BL    SPER115                                                          
         CLC   TEMPDATE,TMPEND                                                  
         BH    SPER115                                                          
         MVC   CALRNUM,TMPNUMB     SAVE PERIOD NUMBER                           
         DROP  R1                                                               
*                                                                               
SPER300  DS    0H                                                               
         MVI   LSTPRDNM,C'#'                                                    
         LA    RF,LSTPRDNM+1                                                    
         EDIT  CALRNUM,(2,(RF)),ALIGN=LEFT                                      
SPERXIT  B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CALCGRND - CALCULATES GRAND TOTALS                                  *         
***********************************************************************         
         SPACE 1                                                                
CALCGRND NTR1                                                                   
CALC010  NI    LSTSTAT,X'FF'-RECFOUND                                           
         LA    R6,BIGKEY                                                        
         USING TSWRECD,R6                                                       
         CLC   KEYSAVE(TSWKEND-TSWKEY),BIGKEY   CMP UP TO PERSON CODE           
         BNE   CALCX                                                            
         CLC   TSWKEND,FLTENDTE    FILTER                                       
         BL    CALCSEQ                                                          
         OC    CALEDATE,CALEDATE                                                
         BZ    *+14                                                             
         CLC   TSWKEND,CALEDATE                                                 
         BL    CALCSEQ                                                          
         CLC   TSWKEND,FLTSTDTE    QUIT IF OUT OF RANGE                         
         BH    CALCX                                                            
         OC    CALSDATE,CALSDATE                                                
         BZ    CALC020                                                          
         CLC   TSWKEND,CALSDATE                                                 
         BH    CALCX                                                            
*                                                                               
CALC020  GOTO1 GETREC                                                           
         ZAP   AMTBILL,=P'0'       INIT AMOUNTS                                 
         ZAP   AMTNBILL,=P'0'                                                   
         ZAP   AMTREAL,=P'0'                                                    
         L     R6,AIO                                                           
         USING TIMRECD,R6                                                       
         BAS   RE,FILTER           FILTER OUT OPTIONS                           
         BNE   CALCSEQ                                                          
         BAS   RE,EXTRAMTS                                                      
         TM    LSTSTAT,RECFOUND                                                 
         BZ    CALCSEQ                                                          
         AP    TOTBILL,AMTBILL                                                  
         AP    TOTNBILL,AMTNBILL                                                
         AP    TOTREAL,AMTREAL                                                  
*                                                                               
CALCSEQ  GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    CALC010                                                          
         DC    H'0'                                                             
*                                                                               
CALCX    B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* UP2GRAND - FIND GRAND TOTAL UPTO WHERE LASTKEY POINTS               *         
***********************************************************************         
UP2GRAND NTR1                                                                   
         OI    LSTSTAT,FRSTSCRN    ASSUME FIRST TIME                            
         ZAP   TOTBILL,=P'0'       INIT GRAND TOTALS                            
         ZAP   TOTNBILL,=P'0'                                                   
         ZAP   TOTREAL,=P'0'                                                    
         LA    R6,BIGKEY                                                        
         USING TSWRECD,R6                                                       
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,CMPY        COMPANY CODE                                 
         MVC   TSWKPER,BCSPACES                                                 
         ZIC   R1,TSLCODEH+5       PERSON CODE                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSWKPER(0),TSLCODE                                               
         MVC   TSWKEND,FLTENDTE                                                 
         OC    CALEDATE,CALEDATE                                                
         BZ    *+10                                                             
         MVC   TSWKEND,CALEDATE                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UP2010   CLC   BIGKEY,LASTKEY      UPTO LASTKEY                                 
         BNL   UP2X                                                             
         NI    LSTSTAT,X'FF'-FRSTSCRN   CAN'T BE FIRST TIME                     
         GOTO1 GETREC                                                           
         ZAP   AMTBILL,=P'0'       INIT AMOUNTS                                 
         ZAP   AMTNBILL,=P'0'                                                   
         ZAP   AMTREAL,=P'0'                                                    
         L     R6,AIO                                                           
         USING TIMRECD,R6                                                       
         BAS   RE,FILTER           FILTER OUT OPTIONS                           
         BNE   UP2SEQ                                                           
         BAS   RE,EXTRAMTS                                                      
         TM    LSTSTAT,RECFOUND                                                 
         BZ    UP2SEQ                                                           
         AP    TOTBILL,AMTBILL                                                  
         AP    TOTNBILL,AMTNBILL                                                
         AP    TOTREAL,AMTREAL                                                  
*                                                                               
UP2SEQ   GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    UP2010                                                           
         DC    H'0'                                                             
*                                                                               
UP2X     ZAP   AMTBILL,=P'0'       DON'T LEAVE LINGERING AMOUNTS                
         ZAP   AMTNBILL,=P'0'                                                   
         ZAP   AMTREAL,=P'0'                                                    
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRAMTS - EXTRACT AMOUNTS FROM RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
EXTRAMTS NTR1                                                                   
         OC    SVFLTS,SVFLTS                                                    
         BNZ   EXTRX               OPTIONS TAKE PRECEDENCE                      
         NI    LSTSTAT,X'FF'-RECFOUND                                           
         L     R6,AIO                                                           
         USING TIMELD,R6                                                        
         MVI   ELCODE,TIMELQ       FIND TIME ELEMENTS                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EXTR10   BAS   RE,NEXTEL                                                        
         BNE   EXTR90              LEAVE                                        
         CLI   TIMETYP,TIMEINP     DATA TYPE HAS TO BE INPUT DETAIL             
         BNE   EXTR10                                                           
*&&US                                                                           
         CLI   TIMTTYP,TIMTEM      TIME TYPE = EMPTY TIMESHEET?                 
         BNE   *+8                 NO: CHECK TIMTTYP = B,R,N OR NC              
         OI    LSTSTAT,RECFOUND    YES : RECORD GOOD                            
*&&                                                                             
         LA    RF,TIMTABL                                                       
EXTR20   CLI   0(RF),X'FF'         EOT?                                         
         BE    EXTR10              NO MATCH                                     
         CLC   TIMTTYP,0(RF)       MATCH?                                       
         BE    EXTR30                                                           
         LA    RF,3(RF)            NEXT ONE                                     
         B     EXTR20                                                           
*                                                                               
EXTR30   DS    0H                                                               
         TM    FLDFINP,FLDFCLT     DO WE HAVE A CLIENT FILTER?                  
         BZ    EXTR60                                                           
         CLC   FLTCLTLN,BCSJLEV1   FILTER MORE THAN LENGTH?                     
         BNH   EXTR40              YES, DON'T NEED TO CHECK                     
         CLC   TIMACC(2),=C'1N'    HAS TO BE 1N ACCT                            
         BNE   EXTR10                                                           
EXTR40   ZIC   R1,FLTCLTLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIMACC+2(0),FLTCLT  MATCH ON CLIENT?                             
         BNE   EXTR10              NOPE, NEXT ONE                               
*                                                                               
         TM    FLDFINP,FLDFPRD     DO WE HAVE A PRODUCT FILTER?                 
         BZ    EXTR50                                                           
         CLC   FLTPRDLN,BCSJLEV2   FILTER MORE THAN LENGTH?                     
         BH    EXTR10              YES, DON'T NEED TO CHECK                     
         LA    R3,TIMACC+2         POINT RF TO PRODUCT PART                     
         ZIC   R1,BCSJLNQ1                                                      
         AR    R3,R1                                                            
         ZIC   R1,FLTPRDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),FLTPRD      MATCH ON PRODUCT?                            
         BNE   EXTR10              NOPE, NEXT ONE                               
*                                                                               
         TM    FLDFINP,FLDFJOB     DO WE HAVE A JOB FILTER?                     
         BZ    EXTR50                                                           
         CLC   FLTJOBLN,BCSJLEV3   FILTER MORE THAN LENGTH?                     
         BH    EXTR10              YES, DON'T NEED TO CHECK                     
         LA    R3,TIMACC+2         POINT RF TO PRODUCT PART                     
         ZIC   R1,BCSJLNQ2                                                      
         AR    R3,R1                                                            
         ZIC   R1,FLTJOBLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),FLTJOB      MATCH ON JOB?                                
         BNE   EXTR10              NOPE, NEXT ONE                               
EXTR50   OI    LSTSTAT,RECFOUND    SATISFACTORY RECORD                          
*                                                                               
EXTR60   TM    FLDFINP,FLDFTSK     DO WE HAVE A TASK FILTER?                    
         BZ    EXTR70                                                           
         ZIC   R1,FLTTSKLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIMTSK(0),FLTTSK    MATCH ON TASK?                               
         BNE   EXTR10              NOPE, NEXT ONE                               
         OI    LSTSTAT,RECFOUND    SATISFACTORY RECORD                          
*                                                                               
EXTR70   OC    SVFLTMOA,SVFLTMOA   ARE WE FILTERING ON MOA?                     
         BZ    EXTR80                                                           
         CLC   TIMMOA(L'SVFSTMOA),SVFSTMOA                                      
         BL    EXTR10                                                           
         CLC   TIMMOA(L'SVFENMOA),SVFENMOA                                      
         BH    EXTR10                                                           
*                                                                               
EXTR80   TM    FLDFINP,FLDFCLT+FLDFPRD+FLDFJOB+FLDFTSK                          
         BNZ   *+8                                                              
         OI    LSTSTAT,RECFOUND    RECORD GOOD IF NO FILTERS                    
         ZICM  R1,1(RF),2                                                       
         LA    R4,AMTTOTS                                                       
         AR    R1,R4               GET ADDRESS OF AMOUNT COUNTER                
         AP    0(L'AMTBILL,R1),TIMHRS                                           
         B     EXTR10                                                           
*                                                                               
EXTR90   ZAP   AMTTOTAL,AMTBILL    KEEP TOTAL                                   
         AP    AMTTOTAL,AMTNBILL                                                
         AP    AMTTOTAL,AMTREAL                                                 
EXTRX    B     EXIT                CC EQUAL                                     
         EJECT                                                                  
***********************************************************************         
* FILTER ROUTINE                                                      *         
***********************************************************************         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         NI    LSTSTAT,X'FF'-RECFOUND                                           
         OC    SVFLTS,SVFLTS                                                    
         BZ    FILTXX                                                           
*                                                                               
         XC    WORK,WORK           BUILD SAVE LINES IN WORK                     
         L     R6,AIO                                                           
         USING TIMELD,R6                                                        
         MVI   ELCODE,TIMELQ       FIND TIME ELEMENTS                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FILT10   BAS   RE,NEXTEL                                                        
         BNE   FILTX               LEAVE                                        
*                                                                               
         CLI   TIMETYP,TIMEINP     MAKE SURE THAT IT IS INPUT DETAIL            
         BNE   FILT10                                                           
*                                                                               
         CLI   SVFLTTYP,0          *** FILTER BY TYPE OF TIME ***               
         BE    FILT20                                                           
         CLI   SVFLTTYP,TIMTCR                                                  
         BH    *+18                                                             
         CLC   SVFLTTYP,TIMTTYP                                                 
         BNE   FILT10                                                           
         B     FILT20                                                           
         CLI   TIMTTYP,TIMTCN                                                   
         BL    FILT10                                                           
*                                                                               
FILT20   CLC   SVFLTCLI,BCSPACES   *** FILTER BY CLIENT ***                     
         BNH   FILT30                                                           
         SR    R0,R0                                                            
         IC    R0,BCSJLEV1                                                      
         LA    RE,SVFLTCLI         FILTER FIELD - CLIENT                        
         LA    RF,TIMACC+2         TMS LINE FIELD - CLIENT                      
FILT25   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   FILT10                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT25                                                        
*                                                                               
FILT30   CLC   SVFLTPRO,BCSPACES   *** FILTER BY PRODUCT ***                    
         BNH   FILT40                                                           
         LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ1                                                      
         AR    RF,R1                                                            
         SR    R0,R0                                                            
         IC    R0,BCSJLEV2                                                      
         LA    RE,SVFLTPRO         FILTER FIELD - PRODUCT                       
FILT35   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   FILT10                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT35                                                        
*                                                                               
FILT40   CLC   SVFLTJOB,BCSPACES   *** FILTER BY JOB ***                        
         BNH   FILT50                                                           
         LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ2                                                      
         AR    RF,R1                                                            
         SR    R0,R0                                                            
         IC    R0,BCSJLEV3                                                      
         LA    RE,SVFLTJOB         FILTER FIELD - JOB                           
FILT45   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   FILT10                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT45                                                        
*                                                                               
FILT50   CLC   SVFLTTSK,BCSPACES   *** FILTER BY TASK ***                       
         BNH   FILT60                                                           
         LA    RE,SVFLTTSK         FILTER FIELD - TASK                          
         LA    RF,TIMTSK                                                        
         LA    R0,L'TIMTSK                                                      
FILT55   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   FILT10                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT55                                                        
*                                                                               
FILT60   CLC   SVFLTCA,BCSPACES   *** FILTER BY CONTRA ***                      
         BNH   FILT70                                                           
*                                                                               
         USING TIMRECD,R1                                                       
         L     R1,AIO                                                           
         LA    RE,SVFLTCA          ONLY COMPARE ON SIGNIFICANT CHAR             
         LA    RF,TIMKULC                                                       
         LA    R0,L'SVFLTCA                                                     
FILT65   CLI   0(RE),C'?'                                                       
         BE    FILT67                                                           
         CLI   0(RE),C' '                                                       
         BE    FILT67                                                           
         CLC   0(1,RE),0(RF)       SAME CONTRA?                                 
         BNE   FILT10                                                           
FILT67   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT65                                                        
         DROP  R1                                                               
*                                                                               
FILT70   OC    SVFLTMOA,SVFLTMOA   *** FILTER BY MOA ***                        
         BZ    FILT80                                                           
         CLC   TIMMOA(L'SVFSTMOA),SVFSTMOA                                      
         BL    FILT10                                                           
         CLC   TIMMOA(L'SVFENMOA),SVFENMOA                                      
         BH    FILT10                                                           
*                                                                               
FILT80   OI    LSTSTAT,RECFOUND                                                 
*                                                                               
         LA    RF,TIMTABL                                                       
FILT90   CLI   TIMTTYP,X'FF'       EOT?                                         
         BE    FILT10              NO MATCH                                     
         CLC   TIMTTYP,0(RF)       MATCH?                                       
         BE    *+12                                                             
         LA    RF,3(RF)            NEXT ONE                                     
         B     FILT90                                                           
*                                                                               
         ZICM  R1,1(RF),2                                                       
         LA    R4,AMTTOTS                                                       
         AR    R1,R4               GET ADDRESS OF AMOUNT COUNTER                
         AP    0(L'AMTBILL,R1),TIMHRS                                           
         B     FILT10                                                           
*                                                                               
FILTX    ZAP   AMTTOTAL,AMTBILL    KEEP TOTAL                                   
         AP    AMTTOTAL,AMTNBILL                                                
         AP    AMTTOTAL,AMTREAL                                                 
*                                                                               
         TM    LSTSTAT,RECFOUND                                                 
         BNO   ROUTH                                                            
FILTXX   B     ROUTE                                                            
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
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
ENATMS   MVC   GERROR,=AL2(ACENATMS)                                            
         B     ACCERRX                                                          
ERRINV   MVC   GERROR,=AL2(ACEINV)     INVALID INPUT FIELD                      
         B     ACCERRX                                                          
ENOCAL   MVC   GERROR,=AL2(ACENOCAL)     NO CALENDAR                            
         B     ACCERRX                                                          
EFLDRQ   MVC   GERROR,=AL2(ACEFLDRQ)     FIELD IS REQUIRED                      
         B     ACCERRX                                                          
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ACCERRX                                                          
ERRPLS   MVC   GERROR,=Y(ACIPLSE)                                               
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         B     *+12                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR               DEFAULT IS ACCOUNTING MSG SYSTEM             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES/LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
MAXLINES EQU   14                  MAX NUMBER OF LINES PER SCREEN               
*                                                                               
*              TIME TYPE TABLE WITH ACCUMULATOR                                 
*                                                                               
TIMTABL  DC    AL1(TIMTCB),AL2(AMTBILL-AMTTOTS)                                 
         DC    AL1(TIMTCR),AL2(AMTREAL-AMTTOTS)                                 
         DC    AL1(TIMTCN),AL2(AMTNBILL-AMTTOTS)                                
         DC    AL1(TIMTNC),AL2(AMTNBILL-AMTTOTS)                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY DEFINITIONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
PFTABLE  DS    0C                                                               
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,(PPF01X-PPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01    DC    AL1(KEYTYTWA,L'TSLCODE-1),AL2(TSLCODE-T61DFFD)                   
PPF01X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF02X-*,02,PFTCPROG,(PPF02X-PPF02)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
PPF02    DC    AL1(KEYTYTWA,L'TSLCODE-1),AL2(TSLCODE-T61DFFD)                   
PPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,(PPF03X-PPF03)/KEYLNQ,PFTSETPN)         
         DCDD  AC#RPT,3                                                         
         DC    CL8'    '                                                        
         DCDD  AC#RPT,8                                                         
PPF03    DC    AL1(KEYTYCUR,L'LSTPRDND-1),AL2(LSTPRDND-LSTLINED)                
PPF03X   EQU   *                                                                
*                                                                               
*        DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
*        DC    CL3' ',CL8' ',CL8' '                                             
*PF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              LIST SCREEN DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTLINED DSECT                                                                  
LSTSVREC DS    CL1                 * = SAVED TIME RECORD                        
LSTPRDNM DS    CL3                 PERIOD NUMBER                                
         DS    CL1                                                              
LSTPRDND DS    CL8                 PERIOD END DATE                              
         DS    CL3                                                              
LSTOFFC  DS    CL2                 OFFICE CODE                                  
         DS    CL3                                                              
LSTDEPT  DS    CL6                 DEPARTMENT CODE                              
         DS    CL2                                                              
LSTSUB   DS    CL6                 SUBDEPARTMENT                                
         DS    CL1                                                              
LSTNBILL DS    CL8                 NON-BILLABLE                                 
         DS    CL1                                                              
LSTBILL  DS    CL8                 BILLIABLE                                    
         DS    CL1                                                              
LSTREAL  DS    CL8                 REALIZATION                                  
         DS    CL1                                                              
LSTTOTAL DS    CL9                 TOTAL                                        
         SPACE 3                                                                
SVTIMED  DSECT                                                                  
SVWKEND  DS    CL3                 WEEK ENDING                                  
SVODS    DS    CL8                 OFFICE/DEPT/SUBDEPT                          
SVDSKADD DS    CL4                 DISK ADDRESS                                 
SVTIMELN EQU   *-SVWKEND                                                        
         EJECT                                                                  
       ++INCLUDE ACCAPWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
* SCREENS                                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPD2D                                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION SAVED STORAGE                                           *         
***********************************************************************         
         SPACE 1                                                                
STARTWRK DS    0A                  IN TWA                                       
RELO     DS    A                                                                
SVDSKDA  DS    A                   SAVE DISK ADDRESS                            
         DS    3A                  N/D                                          
SAVEKEY  DS    CL56                                                             
LASTKEY  DS    CL56                                                             
TEMPKEY  DS    CL56                                                             
*                                                                               
AMTTOTS  DS    0PL4                                                             
AMTBILL  DS    PL4                 BILLABLE AMOUNT                              
AMTNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
AMTREAL  DS    PL4                 REALIZATION AMOUNT                           
AMTTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
SCRBILL  DS    PL4                 BILLABLE AMOUNT  (SCREEN TOTALS)             
SCRNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
SCRREAL  DS    PL4                 REALIZATION AMOUNT                           
SCRTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
TOTBILL  DS    PL4                 BILLABLE AMOUNT  (GRAND TOTAL)               
TOTNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
TOTREAL  DS    PL4                 REALIZATION AMOUNT                           
TOTTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
FLTSTDTE DS    PL3                 FILTER START DATE                            
FLTENDTE DS    PL3                 FILTER END DATE                              
FLTCLT   DS    CL6                 FILTER CLIENT                                
FLTPRD   DS    CL4                 FILTER PRODUCT                               
FLTJOB   DS    CL6                 FILTER JOB                                   
FLTTSK   DS    CL2                 FILTER TASK                                  
FLTCLTLN DS    XL1                 FILTER CLIENT LENGTH                         
FLTPRDLN DS    XL1                 FILTER PRODUCT LENGTH                        
FLTJOBLN DS    XL1                 FILTER JOB LENGTH                            
FLTTSKLN DS    XL1                 FILTER TASK LENGTH                           
FLDFINP  DS    CL1                 FIELD INPUT                                  
FLDFCLT  EQU   X'80'               CLIENT INPUTTED                              
FLDFPRD  EQU   X'40'               PRODUCT INPUTTED                             
FLDFJOB  EQU   X'20'               JOB INPUTTED                                 
FLDFTSK  EQU   X'10'               TASK INPUTTED                                
FLDALLYR EQU   X'08'               YEAR=ALL                                     
FLD1NCLT EQU   X'04'               1N CLIENT, PRD/JOB/TSK INVALID               
CALDATE  DS    0PL3                CALENDAR DATE                                
CALYEAR  DS    CL1                 CALENDAR YEAR                                
         DS    CL2                                                              
CALSDATE DS    PL3                 CALENDAR START DATE                          
CALEDATE DS    PL3                 CALENDAR END DATE                            
CALKEY   DS    CL56                CALENDAR KEY                                 
*                                                                               
OPTFLDS  DS    0C                  OPTION FIELDS                                
SVSCROLL DS    XL2                 SAVED SCROLL AMOUNT                          
SVOPT1   DS    XL1                                                              
SVOPT2   DS    XL1                 XD=Y                                         
SVFLTS   DS    0CL40                                                            
SVFLTCLI DS    CL7                 CLIENT FILTER                                
SVFLTPRO DS    CL4                 PRODUCT FILTER                               
SVFLTJOB DS    CL6                 JOB FILTER                                   
SVFLTTSK DS    CL2                 TASK FILTER                                  
SVFLTMOA DS    0PL4                MOA FILTERS                                  
SVFSTMOA DS    PL2                 MOA FILTER                                   
SVFENMOA DS    PL2                 N/D                                          
SVFLTTYP DS    XL1                 TYPE OF TIME                                 
SVFLTCA  DS    CL14                CONTRA ACCOUNT                               
         DS    XL2                 N/D                                          
OPTFLNQ  EQU   *-OPTFLDS                                                        
*                                                                               
LSTSTAT  DS    XL1                 LIST STATUS                                  
RECFOUND EQU   X'80'               FOUND GOOD RECORD                            
LASTLINE EQU   X'40'               LAST LINE                                    
FRSTSCRN EQU   X'20'               FIRST SCREEN                                 
GOTGRAND EQU   X'10'               GOT GRAND TOTALS                             
LSTPRNTD EQU   X'08'               SOMETHING WAS PRINTED                        
NOTMRECS EQU   X'04'               NO MORE TIME RECORDS                         
USETODAY EQU   X'02'               USES TODAYS DATE                             
LASTOFFC DS    CL2                 LAST OFFICE                                  
CALBLOCK DS    CL(CALDQ)           CALENDAR BLOCK                               
TEMPDATE DS    CL3                 TEMPORARY DATE                               
STMBLOCK DS    CL((MAXLINES+1)*SVTIMELN)  SAVE TIME RECORDS BLOCK               
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* DDGENTWA                                                                      
* ACCAP30GW                                                                     
* ACCAP30DST                                                                    
* ACCAPDSECT                                                                    
* FATIOB                                                                        
* DDCOMFACS                                                                     
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* DDSCANBLKD                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
* ACOFFALD                                                                      
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACCAP42   10/27/17'                                      
         END                                                                    
