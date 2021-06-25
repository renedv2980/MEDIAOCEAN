*          DATA SET ACREPZT02  AT LEVEL 014 AS OF 02/18/20                      
*PHASE ACZT02B,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'TIMETIME ELEMENT FIX FOR BAD JOB TO JOB XFERS'                  
**********************************************************************          
*   QOPT1=Y: PRINT ALL TIMES FOR BAD PERSONS ONLY                    *          
*   QOPT2=Y: QOPT1 MUST BE Y, PRINT BAD TIMES FOR BAD PERSONS ONLY   *          
*   QOPT6=Y: QOPT6 MUST BE Y, TO PRINT BAD 8B ELEMENT SEQUENCE ISSUE *          
**********************************************************************          
ACZT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZT**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZTD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
*                                                                               
EXITL    XR    RE,RE               SET CC LOW                                   
         B     EXITCC                                                           
*                                                                               
EXITH    LHI   RE,2                SET CC HIGH                                  
         B     EXITCC                                                           
*                                                                               
EXITE    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         USING ACCRECD,RE                                                       
         L     RE,AIO1                                                          
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FLAG,0              INITIALIZE FLAG                              
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
*                                                                               
         XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    REQF10                                                           
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
*                                                                               
REQF10   MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    REQFX                                                            
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   REQF20                                                           
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
REQF20   GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
*                                                                               
         TM    CPYSTAT4,CPYSOFF2   IS NEW 2 CHAR OFFICE IN USE                  
         BNO   *+8                                                              
         OI    FLAG,FLGNOFF        NEW 2 CHAR OFFICE IN USE                     
*                                                                               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
*                                                                               
         BAS   RE,GETLEVS          GET LEVELS                                   
*                                                                               
LDGFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL A FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         L     R2,ADLVANAM                                                      
         LA    R3,LEVANME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVAFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL B FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         L     R2,ADLVBNAM                                                      
         LA    R3,LEVBNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVBFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL C FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ADLVCNAM                                                      
         LA    R3,LEVCNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVCFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         BAS   RE,SETCDE           SET LEVEL CODES                              
*                                                                               
         L     R2,ADACCNAM                                                      
         LA    R3,LEVDNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
         MVC   SVACCT,SPACES       CLEAR ACCOUNT                                
         XC    SVPEDT,SVPEDT       CLEAR PERIOD END DATE                        
         MVC   LASTKEY,SPACES      LAST KEY FOR EVERY NEW KEY                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTOR PROCPER                                                          
         JNE   PACCX                                                            
*                                                                               
         GOTOR CALPRDS             GET CALENDAR INFORMATION                     
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTOR PTIME                                                            
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BE    PACC10                                                           
*                                                                               
         MVI   RCSUBPRG,1                                                       
         GOTOR FIXTS               REPORT AND FIX TIMESHEET                     
         B     PACCX                                                            
*                                                                               
PACC10   MVI   RCSUBPRG,2                                                       
         GOTOR FIXT8B              REPORT AND FIX TIMESHEET                     
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET PERSON RECORD INFORMATION                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
PROCPER  NTR1                                                                   
         L     R3,ADACC                                                         
*                                                                               
         USING LOCTABD,R4                                                       
         L     R4,AIO4                                                          
         XC    0(LOCDATL,R4),0(R4)                                              
         XC    DNUMLOC,DNUMLOC                                                  
*                                                                               
         MVC   AIO,AIO2            Set IO area                                  
*                                                                               
         USING IOAREAD,R5                                                       
         L     R5,AIO                                                           
*                                                                               
         USING PERRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   PERKEY,SPACES       Clear Key                                    
         MVI   PERKTYP,PERKTYPQ    Record Type - X'0F'                          
         MVC   PERKCPY,ACTKCPY     Company Code                                 
         MVC   PERKCODE,LEVDCDE    Person Code                                  
*                                                                               
*        MVC   MSG,=CL10'ACCOUNT'                                               
*        GOTO1 ADUMP,DMCB,(RC),SVKEY,15                                         
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(L'PERKEY),IOKEY      SAME KEY?                             
         JNE   PPERSNO                                                          
         DROP  R5                                                               
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         L     R2,AIO                                                           
*                                                                               
         LA    R2,PERRFST                                                       
         XR    R0,R0                                                            
PPERS10  CLI   0(R2),0                                                          
         BE    PPERS24                                                          
         CLI   0(R2),LOCELQ                                                     
         BE    PPERS22                                                          
PPERS12  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PPERS10                                                          
*                                                                               
         USING LOCELD,R2                                                        
PPERS22  CLC   LOCOFF,LEVACDE                                                   
         BNE   PPERS12                                                          
         CLC   LOCDEPT,LEVBCDE                                                  
         BNE   PPERS12                                                          
         CLC   LOCSUB,LEVCCDE                                                   
         BNE   PPERS12                                                          
         MVC   LOCDSTRT,LOCSTART                                                
         MVC   LOCDEND,LOCEND                                                   
         MVC   LOCDLOCK,LOCLOCK                                                 
         LA    R4,LOCDATL(R4)                                                   
         LH    RE,DNUMLOC                                                       
         AHI   RE,1                                                             
         CHI   RE,500                                                           
         JH    PPERSYES                                                         
         STH   RE,DNUMLOC                                                       
         XC    0(LOCDATL,R4),0(R4)                                              
         B     PPERS12                                                          
*                                                                               
PPERS24  L     R4,AIO4                                                          
         OC    0(LOCDATL,R4),0(R4)                                              
         BZ    PPERSNO                                                          
*                                                                               
PPERSYES B     EXITE                                                            
PPERSNO  B     EXITL                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS TIME                                                       *          
**********************************************************************          
*&&DO                                                                           
         SPACE 1                                                                
PTIME    NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADACC                                                         
*                                                                               
         MVC   AIO,AIO1            Set IO area                                  
*                                                                               
         USING IOAREAD,R5                                                       
         L     R5,AIO                                                           
*                                                                               
         USING TIMRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES       CLEAR KEY                                    
         MVC   TIMKCPY,ACTKCPY     COMPANY CODE                                 
         MVC   TIMKUNT(2),=C'1R'   U/L                                          
         MVC   TIMKACT,ACTKACT     MOVE IN ACCOUNT CODE                         
*                                                                               
*        MVC   MSG,=CL10'ACCOUNT'                                               
*        GOTO1 ADUMP,DMCB,(RC),ACTKEY,15                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     PTIME020                                                         
*                                                                               
PTIME010 GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
PTIME020 CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY      SAME KEY?                       
         BNE   PTIMEX                                                           
*                                                                               
         SR    R9,R9                                                            
         LA    R2,IOKEY                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   PTIME010                                                         
         DROP  R5                                                               
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    PTIME010            IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    PTIME010            IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         L     R2,AIO                                                           
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         XC    SVASTDTE,SVASTDTE   SAVE AREA FOR ACTUAL START DATE              
         NI    FLAG,X'FF'-FLGCHG   Init update flag                             
*                                                                               
         LA    R3,TIMRFST                                                       
         USING TIMELD,R3                                                        
PTIME030 CLI   TIMEL,0                                                          
         BE    PTIME100                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   PTIME040                                                         
         CLI   TIMETYP,TIMEINP                                                  
         BE    PTIME050                                                         
         CLI   TIMETYP,TIMETIME                                                 
         BE    PTIME060                                                         
PTIME040 SR    R1,R1                                                            
         IC    R1,1(R3)             GET LENGTH OF THIS ELEMENT                  
         AR    R3,R1                                                            
         B     PTIME030                                                         
*                                                                               
PTIME050 MVI   BYTE,0                                                           
         MVC   PPERCDE,LEVDCDE       PERSON CODE                                
         MVC   PPERNME,LEVDNME       PERSON NAME                                
         EDIT  TIMLINE#,PTMSLIN      PRINT TMS LINE NO.                         
         EDIT  (P3,TIMHRS),PHOURS,2,MINUS=YES                                   
         ZAP   PKHRS,TIMHRS                                                     
         B     PTIME040                                                         
*                                                                               
PTIME060 STCM  R3,15,SVADDR                                                     
         OC    TIMETPDT,TIMETPDT   ANY ACTUAL ENDDATE?                          
         JZ    PTIME065                                                         
         OC    SVASTDTE,SVASTDTE   ANY SAVE DATE?                               
         JNZ   PTIME040                                                         
         MVC   SVASTDTE,TIMETDT1   SAVE OFF START DATE                          
         BE    PTIME040                                                         
*                                                                               
PTIME065 GOTOR GETCAL                                                           
         OI    FLAG,FLGCHG         Set Flag to show we have an update           
         MVC   SVELMHDR,0(R3)      SAVE OFF ELEMENT HEADER                      
         MVC   MSG,=CL15'BAD ELEM'                                              
         SR    R6,R6                                                            
         IC    R6,1(R3)                                                         
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
         MVI   TIMEL,DELELQ                                                     
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DELELQ',TIMRECD),0                   
*                                                                               
TIME     USING TIMELD,R4                                                        
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(L'SVELMHDR),SVELMHDR                                     
         MVC   TIME.TIMETPDT,TIMKPEDT                                           
         GOTOR GETLOC              GET LOCATION DATES                           
         OC    LOCENDT,LOCENDT     Any location end date?                       
         JZ    PTIME066                                                         
         CLC   TIMKPEDT,LOCENDT                                                 
         JNH   PTIME066                                                         
         MVC   TIME.TIMETPDT,LOCENDT                                            
PTIME066 MVC   TIME.TIMETDT1,SVASTDTE                                           
         ZAP   TIME.TIMEHRS1,PKHRS      Set Hours in Day 1                      
         GOTO1 DATCON,DMCB,(1,TIME.TIMETPDT),(0,WORK+12)                        
         GOTO1 DATCON,DMCB,(1,SVASTDTE),(0,WORK)                                
         LA    R6,TIME.TIMETDT2         for loop                                
*                                                                               
PTIME067 CLC   WORK+12(6),WORK          Do not exceed Period End Date           
         JNH   PTIME070                                                         
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,0(R6))                                 
         MVC   WORK(6),WORK+6           Update Date in WORK                     
         ZAP   3(L'TIMEHRS1,R6),=P'0'                                           
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         J     PTIME067                                                         
*                                                                               
PTIME070 LR    RE,R4                                                            
         SR    R6,RE                                                            
         STC   R6,1(R4)            Save off length                              
         ICM   R6,15,SVADDR                                                     
         LA    RF,=CL8'ADD=HERE'                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,ELEMENT,(RF),0,(R6)            
         MVC   MSG,=CL15'FIX ELEM'                                              
         SR    R6,R6                                                            
         IC    R6,1(R3)                                                         
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
         GOTO1 PRINTIT                                                          
         B     PTIME040                                                         
*                                                                               
PTIME100 TM    FLAG,FLGCHG         Was anything chenaged?                       
         JNO   PTIME010                                                         
         CLI   QOPT1,C'Y'                                                       
         BNE   PTIME010                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    PTIME010                                                         
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
         B     PTIME010                                                         
*                                                                               
PTIMEX   B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
*&&                                                                             
**********************************************************************          
* REPORT AND FIX TIMESHEET                                           *          
**********************************************************************          
         SPACE 1                                                                
FIXTS    NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADACC                                                         
*                                                                               
         MVC   AIO,AIO1            Set IO area                                  
*                                                                               
         USING IOAREAD,R5                                                       
         L     R5,AIO                                                           
*                                                                               
         USING TIMRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES       CLEAR KEY                                    
         MVC   TIMKCPY,ACTKCPY     COMPANY CODE                                 
         MVC   TIMKUNT(2),=C'1R'   U/L                                          
         MVC   TIMKACT,ACTKACT     MOVE IN ACCOUNT CODE                         
*                                                                               
*        MVC   MSG,=CL10'ACCOUNT'                                               
*        GOTO1 ADUMP,DMCB,(RC),ACTKEY,15                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     FIXTS020                                                         
*                                                                               
FIXTS010 GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
FIXTS020 CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY      SAME KEY?                       
         BNE   FIXTSX                                                           
*                                                                               
         SR    R9,R9                                                            
         LA    R2,IOKEY                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   FIXTS010                                                         
         DROP  R5                                                               
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    FIXTS010            IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    FIXTS010            IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         L     R2,AIO                                                           
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         XC    SVASTDTE,SVASTDTE   SAVE AREA FOR ACTUAL START DATE              
         NI    FLAG,X'FF'-FLGCHG   Init update flag                             
*                                                                               
         LA    R3,TIMRFST                                                       
         USING TIMELD,R3                                                        
FIXTS030 CLI   TIMEL,0                                                          
         BE    FIXTS100                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   FIXTS040                                                         
         CLI   TIMETYP,TIMEINP                                                  
         BE    FIXTS050                                                         
         CLI   TIMETYP,TIMETIME                                                 
         BE    FIXTS060                                                         
FIXTS040 SR    R1,R1                                                            
         IC    R1,1(R3)             GET LENGTH OF THIS ELEMENT                  
         AR    R3,R1                                                            
         B     FIXTS030                                                         
*                                                                               
FIXTS050 MVI   BYTE,0                                                           
         MVC   PPERCDE,LEVDCDE       PERSON CODE                                
         MVC   PPERNME,LEVDNME       PERSON NAME                                
         EDIT  TIMLINE#,PTMSLIN      PRINT TMS LINE NO.                         
         EDIT  (P3,TIMHRS),PHOURS,2,MINUS=YES                                   
         ZAP   PKHRS,TIMHRS                                                     
         B     FIXTS040                                                         
*                                                                               
FIXTS060 STCM  R3,15,SVADDR                                                     
         GOTOR GETCAL              GET PERIOD DATES FROM CALENDAR               
         XR    R0,R0               Calculate days of given period below         
         LLC   R1,TIMLN                                                         
         SHI   R1,TIMEDLNQ                                                      
         D     R0,=F'7'                                                         
         AHI   R1,1                Actual no. of days in time record            
         ST    R1,WORK+18          save number of days for the period           
*                                                                               
         GOTOR GETLOC              GET LOCATION DATES                           
         OC    LOCENDT,LOCENDT     Any location end date?                       
         JZ    *+20                                                             
         CLC   TIMETPDT,LOCENDT                                                 
         JNH   *+10                                                             
         MVC   TIMETPDT,LOCENDT    set location end date if present             
*                                                                               
         OC    LOCSTDT,LOCSTDT     Any location start date?                     
         JZ    *+20                                                             
         CLC   SVASTDTE,LOCSTDT                                                 
         JH    *+10                                                             
         MVC   SVASTDTE,LOCSTDT    set location end date if present             
         OC    TIMETPDT,TIMETPDT   Any actual EndDate?                          
         JZ    FIXTS062                                                         
         GOTO1 DATCON,DMCB,(1,SVASTDTE),(8,PSDATE)   Start Date                 
         GOTO1 DATCON,DMCB,(1,TIMETPDT),(8,PEDATE)   End Date                   
         GOTO1 DATCON,DMCB,(1,SVASTDTE),(0,WORK+6)   Start date YYMMDD          
         GOTO1 DATCON,DMCB,(1,TIMETPDT),(0,WORK+12)  End date YYMMDD            
         GOTO1 VPERVERT,DMCB,WORK+6,WORK+12    Diff of Start and End            
         XR    R9,R9                                                            
         LH    R9,DMCB+8           Get diff of Start and End dates              
         L     R1,WORK+18          Load number of days for the period           
         CR    R1,R9               Flag if number of days not equal             
         JNE   FIXTS062                                                         
*                                                                               
         LA    R6,TIMETDT1         Loop to check if invalid dates in TS         
FIXTS061 GOTO1 DATCON,DMCB,(1,0(R6)),(0,WORK)                                   
         CLC   WORK(6),WORK+6      Exceed Period Start Date?                    
         JL    FIXTS062                                                         
         CLC   WORK(6),WORK+12     Exceed Period End Date?                      
         JH    FIXTS062                                                         
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         BCT   R9,FIXTS061         Next day element                             
         J     FIXTS040                                                         
*                                                                               
FIXTS062 L     R4,WORK+18          Loop and print all dates since ...           
         LA    R6,TIMETDT1                   ... invalid date found             
         LA    R9,PHRSD1                                                        
FIXTS063 GOTO1 DATCON,DMCB,(1,0(R6)),(8,0(R9))                                  
         LA    R9,10(R9)                                                        
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         BCT   R4,FIXTS063                                                      
*                                                                               
FIXTS065 OI    FLAG,FLGCHG         Set Flag to show we have an update           
         MVC   MSG,=CL15'BAD ELEM'                                              
         SR    R6,R6                                                            
         IC    R6,1(R3)                                                         
         BCTR  R6,0                Adjust for EX                                
         EXMVC R6,SVELMHD2,0(R3)   Save this element                            
         AHI   R6,1                Adjust back to original length               
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
         MVI   TIMEL,DELELQ                                                     
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DELELQ',TIMRECD),0                   
*                                                                               
TIME     USING TIMELD,R4                Build new element                       
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(L'SVELMHD2),SVELMHD2                                     
FIXTS066 MVC   TIME.TIMETDT1,SVASTDTE                                           
         ZAP   TIME.TIMEHRS1,PKHRS      Set Hours in Day 1                      
         MVC   WORK(6),WORK+6                                                   
         LA    R6,TIME.TIMETDT2         for loop                                
*                                                                               
FIXTS067 CLC   WORK+12(6),WORK          Do not exceed Period End Date           
         JNH   FIXTS068                                                         
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,0(R6))                                 
         MVC   WORK(6),WORK+6           Update Date in WORK                     
         ZAP   3(L'TIMEHRS1,R6),=P'0'                                           
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         J     FIXTS067                                                         
*                                                                               
FIXTS068 GOTO1 HEXOUT,DMCB,SVDA,PHRSTOT,L'SVDA                                  
FIXTS070 LR    RE,R4                                                            
         SR    R6,RE                                                            
         STC   R6,1(R4)            Save off length                              
         ICM   R6,15,SVADDR                                                     
         LA    RF,=CL8'ADD=HERE'                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,ELEMENT,(RF),0,(R6)            
         MVC   MSG,=CL15'FIX ELEM'                                              
         SR    R6,R6                                                            
         IC    R6,1(R3)                                                         
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
         GOTO1 PRINTIT                                                          
         J     FIXTS040                                                         
*                                                                               
FIXTS100 TM    FLAG,FLGCHG         Was anything chenaged?                       
         JNO   FIXTS010                                                         
         CLI   QOPT1,C'Y'                                                       
         BNE   FIXTS010                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    FIXTS010                                                         
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
         B     FIXTS010                                                         
*                                                                               
FIXTSX   B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
**********************************************************************          
* REPORT AND FIX 8B ELEMENT IN TIMESHEET                                        
* IT WILL FIX THE SEQUENCEING ISSUE IN THE TIMESHEET                            
* TIMEINP AND TIMITIME SHOULD BE IN ONE AFTER OTHER FOR A LINE NUMBER,          
* THEIR SEQ NUMBER SHOULD ALSO BE MATCHED                                       
**********************************************************************          
         SPACE 1                                                                
FIXT8B   NTR1                                                                   
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADACC                                                         
*                                                                               
         MVC   AIO,AIO1            Set IO area                                  
*                                                                               
         USING IOAREAD,R5                                                       
         L     R5,AIO                                                           
*                                                                               
         USING TIMRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES       CLEAR KEY                                    
         MVC   TIMKCPY,ACTKCPY     COMPANY CODE                                 
         MVC   TIMKUNT(2),=C'1R'   U/L                                          
         MVC   TIMKACT,ACTKACT     MOVE IN ACCOUNT CODE                         
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     FIXT8B20                                                         
*                                                                               
FIXT8B10 GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
FIXT8B20 CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY      SAME KEY?                       
         BNE   FIXT8BX                                                          
*                                                                               
         SR    R9,R9                                                            
         LA    R2,IOKEY                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   FIXT8B10                                                         
         DROP  R5                                                               
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    FIXT8B10            IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    FIXT8B10            IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         L     R2,AIO                                                           
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         XC    SVASTDTE,SVASTDTE   SAVE AREA FOR ACTUAL START DATE              
         NI    FLAG,X'FF'-FLGCHG   Init update flag                             
         ZAP   TOTTIM07,=P'0'                                                   
         ZAP   TOTTIM01,=P'0'                                                   
* READ ELEMENT AND FOUND BAD ELEMENT                                            
FIXT8B25 LA    R3,TIMRFST                                                       
         USING TIMELD,R3                                                        
FIXT8B30 CLI   TIMEL,0                                                          
         BE    FIXT8B99                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   FIXT8B40                                                         
         CLI   TIMETYP,TIMEINP                                                  
         BE    FIXT8B50                                                         
         CLI   TIMETYP,TIMETIME                                                 
         JE    FIXT8B55                                                         
*                                                                               
FIXT8B40 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         ZAP   TOTTIM07,=P'0'                                                   
         J     FIXT8B30                                                         
*                                                                               
FIXT8B50 ZAP   TOTTIM01,TIMHRS                                                  
         ZAP   TOTTIM07,=P'0'                                                   
         MVC   LINE#001,TIMLINE#                                                
         EDIT  TIMLINE#,PRTLIN01                                                
         MVC   PRTACCT(16),TIMACC                                               
         J     FIXT8B40                                                         
*                                                                               
FIXT8B55 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         LA    R9,12           LOAD TILL TIMETDT1 length                        
         LA    R4,TIMETDT1                                                      
         LA    R6,15                                                            
*                                                                               
FIXT8B60 CR    R9,R1                                                            
         BE    FIXT8B70                                                         
         BH    FIXT8B70                                                         
         CLC   0(3,R4),=X'000000'                                               
         JE    FIXT8B70                                                         
         CLC   3(4,R4),=X'00000000'                                             
         JE    FIXT8B65                                                         
         AP    TOTTIM07,3(4,R4)                                                 
FIXT8B65 CLC   TIMETPDT,0(R4)                                                   
         JE    FIXT8B70              PERSON CODE                                
         LA    R4,7(R4)                                                         
         LA    R9,7(R9)                                                         
         BCT   R6,FIXT8B60           LOOP THROUGH ALL DAYS TIME HRS             
*                                                                               
FIXT8B70 CP    TOTTIM01,TOTTIM07     TOTAL MATCH?                               
         JE    FIXT8B40              YES, READ NEXT                             
* BAD ELEMENT FOUND                                                             
* DELETE AND ADD BACK THE BAD ELEMENT                                           
         MVC   LINE#007,TIMEIDNO                                                
         EDIT  TIMEIDNO,PRTLIN07     PRINT TMS LINE NO.                         
         MVC   PRTPCODE,LEVDCDE       PERSON CODE                               
         GOTO1 DATCON,DMCB,(1,TIMETPDT),(8,PRTENDDT) End Date                   
         EDIT  (P3,TOTTIM01),PRTHOR01,2,MINUS=YES                               
         EDIT  (P3,TOTTIM07),PRTHOR07,2,MINUS=YES                               
         GOTO1 HEXOUT,DMCB,SVDA,PRTDA,L'SVDA                                    
         GOTO1 PRINTIT                                                          
         ZAP   TOTTIM01,=P'0'                                                   
*                                                                               
* DELETE ELEMNET                                                                
FIXT8B75 LA    R4,TIMRFST                                                       
U        USING TIMELD,R4                                                        
FIXT8B80 CLI   U.TIMEL,0                                                        
         BE    FIXT8B90                                                         
         CLI   U.TIMEL,TIMELQ                                                   
         BNE   FIXT8B82                                                         
         CLI   U.TIMETYP,TIMEINP                                                
         BE    FIXT8B84                                                         
         CLI   U.TIMETYP,TIMETIME                                               
         JE    FIXT8B86                                                         
*                                                                               
FIXT8B82 SR    R1,R1                                                            
         IC    R1,1(R4)             GET LENGTH OF THIS ELEMENT                  
         AR    R4,R1                                                            
         J     FIXT8B80                                                         
*                                                                               
FIXT8B84 CLC   U.TIMLINE#,LINE#001                                              
         BNE   FIXT8B85                                                         
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         BCTR  R6,0                Adjust for EX                                
         EXMVC R6,SAVE101,0(R4)    Save this element                            
         MVI   U.TIMEL,DELELQ                                                   
         J     FIXT8B82                                                         
*                                                                               
FIXT8B85 CLC   U.TIMLINE#,LINE#007                                              
         BNE   FIXT8B82                                                         
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         BCTR  R6,0                Adjust for EX                                
         EXMVC R6,SAVE201,0(R4)    Save this element                            
         MVI   U.TIMEL,DELELQ                                                   
         J     FIXT8B82                                                         
*                                                                               
FIXT8B86 CLC   U.TIMEIDNO,LINE#001                                              
         BNE   FIXT8B87                                                         
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         BCTR  R6,0                Adjust for EX                                
         EXMVC R6,SAVE107,0(R4)    Save this element                            
         MVI   U.TIMEL,DELELQ                                                   
         J     FIXT8B82                                                         
*                                                                               
FIXT8B87 CLC   U.TIMEIDNO,LINE#007                                              
         BNE   FIXT8B82                                                         
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         BCTR  R6,0                Adjust for EX                                
         EXMVC R6,SAVE207,0(R4)    Save this element                            
         MVI   U.TIMEL,DELELQ                                                   
         J     FIXT8B82                                                         
*                                                                               
FIXT8B90 GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DELELQ',TIMRECD),0                   
*                                                                               
* CORRECT AND ADD ELEMENT                                                       
         ZAP   TOTTIM07,=P'0'                                                   
         LA    R4,SAVE107                                                       
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R1,R4                                                            
         LA    R4,U.TIMETDT1                                                    
         LA    R6,15                                                            
*                                                                               
FIXT8B92 CR    R4,R1                                                            
         BE    FIXT8B94                                                         
         BH    FIXT8B94                                                         
         AP    TOTTIM07,3(4,R4)                                                 
         LA    R4,7(R4)                                                         
         BCT   R6,FIXT8B92                                                      
*                                                                               
FIXT8B94 LA    R4,SAVE101                                                       
         ZAP   U.TIMHRS,TOTTIM07                                                
         ZAP   U.TIMAMNT,U.TIMRATE                                              
         MP    U.TIMAMNT,U.TIMHRS                                               
         SRP   U.TIMAMNT,64-2,5         / 100 (hours is 2 dec places)           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,SAVE101,0,0,0                  
         MVC   SAVE107+2(1),SAVE101+2                                           
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,SAVE107,0,0,0                  
*                                                                               
         ZAP   TOTTIM07,=P'0'                                                   
         LA    R4,SAVE207                                                       
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R1,R4                                                            
         LA    R4,U.TIMETDT1                                                    
         LA    R6,15                                                            
*                                                                               
FIXT8B96 CR    R4,R1                                                            
         BE    FIXT8B98                                                         
         BH    FIXT8B98                                                         
         AP    TOTTIM07,3(4,R4)                                                 
         LA    R4,7(R4)                                                         
         BCT   R6,FIXT8B96                                                      
*                                                                               
FIXT8B98 LA    R4,SAVE201                                                       
         ZAP   U.TIMHRS,TOTTIM07                                                
         ZAP   U.TIMAMNT,U.TIMRATE                                              
         MP    U.TIMAMNT,U.TIMHRS                                               
         SRP   U.TIMAMNT,64-2,5         / 100 (hours is 2 dec places)           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,SAVE201,0,0,0                  
         MVC   SAVE207+2(1),SAVE201+2                                           
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,SAVE207,0,0,0                  
         OI    FLAG,FLGCHG         Set Flag to show we have an update           
*                                                                               
         J     FIXT8B25                                                         
* PUT RECORD                                                                    
FIXT8B99 TM    FLAG,FLGCHG         Was anything chenaged?                       
         JNO   FIXT8B10                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    FIXT8B10                                                         
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
         B     FIXT8B10                                                         
*                                                                               
FIXT8BX  B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
         EX    R1,*-6                                                           
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC            A(ACCOUNT RECORD)                            
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         MVC   0(0,R2),0(R1)                                                    
         EX    R4,*-6                                                           
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME ROUTINE                                                   *          
*     R2 = NAME ELEMENT (SOURCE)                                     *          
*     R3 = NAME FIELD   (DESTINATION)                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
GETNME   NTR1                                                                   
         MVC   0(L'LEVNMES,R3),SPACES                                           
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNX                                                            
         MVC   0(0,R3),NAMEREC                                                  
         EX    R1,*-6                                                           
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD3+1(10),=CL10'LOGIN ID :'                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         USING HEADD,R3                                                         
         LA    R3,XHEAD4                                                        
         MVC   HEADDESC,LEVADSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVACDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVANME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD5                                                        
         MVC   HEADDESC,LEVBDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVBCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVBNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD6                                                        
         MVC   HEADDESC,LEVCDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVCCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVCNME      LEVEL NAME                                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(HELLO)            HELLO CALL                                   
         DC    V(PERVERT)          PERVERT                                      
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    A(IO3)                                                           
         DC    A(IO4)                                                           
         DC    A(IO5)                                                           
         SPACE 2                                                                
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ADUMP    DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCMST   DC    C'ACCMST '                                                       
         SPACE 2                                                                
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* I/O Areas                                                          *          
**********************************************************************          
         SPACE 1                                                                
IO1      DS    0CL2042                                                          
IOKEY1   DS    CL42                KEY                                          
IODATA1  DS    CL2000              DATA                                         
IOLNQ1   EQU   *-IO1               LENGTH                                       
*                                                                               
IO2      DS    0CL2042                                                          
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
*                                                                               
IO3      DS    0CL2042                                                          
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
*                                                                               
IO4      DS    0CL2042                                                          
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
*                                                                               
IO5      DS    0CL2042                                                          
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
         SPACE 2                                                                
***********************************************************************         
* Calendar periods retrieval                                          *         
*   This routine is used to build a list of time periods using the    *         
*   company calendar                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALPRDS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CALPRDS'                                                      
*                                                                               
         USING CALTABD,R4                                                       
         MVC   OFFCODE,LEVACDE                                                  
CALP005  L     R4,AIO3                                                          
         XC    CALTENDT,CALTENDT                                                
         XC    DNUMPRD,DNUMPRD                                                  
         NI    FLAG,X'FF'-FLGCAL                                                
*                                                                               
         USING CASRECD,R2                                                       
         LA    R2,SVKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASPTYP,CASPTYPQ                                                 
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE,START                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         USING IOAREAD,R5                                                       
         L     R5,AIO                                                           
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         JE    CALP020                                                          
         DC    H'0'                                                             
*                                                                               
CALP010  GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
         JE    CALP020                                                          
         DC    H'0'                                                             
*                                                                               
CALP020  CLC   SVKEY(CASPEDTE-CASKEY),IOKEY                                     
         JNE   CALPRDSX                                                         
         L     R2,AIO                                                           
         CLC   CASPOFC,OFFCODE                                                  
         JNE   CALP010                                                          
         DROP  R5                                                               
*                                                                               
         CLC   CASPSDTE,END                                                     
         JH    CALPRDSX                                                         
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         USING TMPELD,R3                                                        
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
CALP030  CLI   TMPEL,0                                                          
         BE    CALP010                                                          
         CLI   TMPEL,TMPELQ                                                     
         BE    CALP050                                                          
         CLI   TMPEL,TMRELQ                                                     
         BNE   CALP040                                                          
         ST    R3,FULL1                                                         
                                                                                
CALP040  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         B     CALP030                                                          
                                                                                
CALP050  CLC   TMPSTART,START      IS START DATE LOWER THAN PERIOD DATE         
         BNL   CALP060             NO - CHECK END DATE                          
         CLC   TMPEND,START        IS START DATE LOWER THAN PERIOD END          
         BL    CALP040             NO - NOT INTERESTED                          
         B     CALP070                                                          
CALP060  CLC   TMPEND,END          IS PERIOD END DATE LOWER THAN END            
         BNH   CALP070             YES                                          
         CLC   TMPSTART,END        IS PERIOD START DATE HIGHER THAN END         
         BH    CALP040             YES - NOT INTERESTED                         
*                                                                               
CALP070  CLC   TMPEND,TODAYP       NOTHING HIGHER THAN TODAY                    
         BH    CALP040                                                          
*                                                                               
         OI    FLAG,FLGCAL                                                      
         MVC   CALTEMOA,CASKEMOA   End MOA                                      
         MVC   CALTSMOA,CASKSMOA   Start MOA                                    
         MVC   CALTOFC,CASKOFC     Office Code                                  
         MVC   CALTENDT,TMPEND                                                  
         MVC   CALTSTRT,TMPSTART                                                
         LA    R4,CALTABL(R4)                                                   
         XR    RE,RE                                                            
         ICM   RE,3,DNUMPRD                                                     
         AHI   RE,1                                                             
         CHI   RE,500                                                           
         JH    CALPRDSY                                                         
         STCM  RE,3,DNUMPRD                                                     
         XC    CALTENDT,CALTENDT                                                
         B     CALP040                                                          
         DROP  R3                                                               
*                                                                               
CALPRDSX TM    FLAG,FLGCAL         Did we get a calendar Record?                
         JNO   CALPRDS1                                                         
*                                                                               
CALPRDSY J     EXITE                                                            
*                                                                               
CALPRDS1 CLC   OFFCODE,SPACES      Did we LOOK FOR DEFAULT ?                    
         JE    CALPRDSN                                                         
         MVC   OFFCODE,SPACES      CLEAR OFFICE CODE                            
         J     CALP005             LOOK AGAIN                                   
CALPRDSN J     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Get Correct Calendar and Period Start/End combo                     *         
*     R2 = TIME Record                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
GETCAL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GETCAL*'                                                      
*                                                                               
         USING CALTABD,R4                                                       
         L     R4,AIO3                                                          
         OC    CALTENDT,CALTENDT                                                
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE               Clear for Postition                          
         SR    R0,R0                                                            
         ICM   R0,3,DNUMPRD                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETC010  CLC   CALTEMOA,TIMKPEDT   Test ends before request end                 
         JL    GETC999                                                          
         CLC   CALTSMOA,TIMKPEDT   Test starts after request Start              
         JH    GETC999                                                          
         CLC   CALTOFC,SPACES      Any Office?                                  
         JNH   GETC020                                                          
         CLC   CALTOFC,LEVACDE     Match on Office                              
         JNE   GETC999                                                          
*                                                                               
GETC020  CLC   CALTSTRT,TIMKPEDT   Test starts after request Start              
         JH    GETC999                                                          
         CLC   CALTENDT,TIMKPEDT   Test ends before request end                 
         JL    GETC999                                                          
         MVC   SVASTDTE,CALTSTRT   Save off period start date.                  
         J     GETCX                                                            
*                                                                               
GETC999  LA    R4,CALTABL(R4)                                                   
         JCT   R0,GETC010                                                       
*                                                                               
         OC    SVASTDTE,SVASTDTE   ANY SAVE DATE?                               
         JNZ   GETCX                                                            
         DC    H'0'                                                             
*                                                                               
GETCX    XMOD1                                                                  
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Get Correct Location Start and End date for given period end date   *         
*     R2 = TIME Record                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
GETLOC   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GETLOC*'                                                      
*                                                                               
         USING LOCTABD,R4                                                       
         L     R4,AIO4                                                          
         OC    0(LOCDATL,R4),0(R4)                                              
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         SR    R0,R0                                                            
         ICM   R0,3,DNUMLOC                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETL010  CLC   LOCDSTRT,TIMKPEDT                                                
         JH    GETLX                                                            
         MVC   LOCSTDT,LOCDSTRT                                                 
         MVC   LOCENDT,LOCDEND                                                  
         MVC   LOCDLOC,LOCDLOCK                                                 
         LA    R4,LOCDATL(R4)                                                   
         JCT   R0,GETL010                                                       
*                                                                               
GETLX    XMOD1                                                                  
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,AIO,0              
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,AIO,0              
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,AIO,0              
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         L     R3,AIO                                                           
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,AIO,DMWORK                   
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST ',SVDA,AIO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZTD    DSECT                                                                  
VTYPES   DS    0A                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VHELLO   DS    V                   HELLO                                        
VPERVERT DS    V                   HELLO                                        
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
AIO4     DS    A                                                                
AIO5     DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
AIO      DS    A                   Address of spot to put new ELEM              
*                                                                               
FULL1    DS    F                                                                
SVDA     DS    F                                                                
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
DELELQ   EQU   X'FF'               DELETE ELEMENT FF                            
*                                                                               
PKHRS    DS    PL(L'TIMHRS)                                                     
*                                                                               
ELEMENT  DS    XL255                                                            
*                                                                               
MSG      DS    CL15                DUMP MESSAGE                                 
*                                                                               
FLAG     DS    XL1                                                              
FLGCAL   EQU   X'80'               Found a Calendar record                      
FLGNOFF  EQU   X'40'               Company on NEW OFFICE                        
FLGCHG   EQU   X'20'               Record was changed                           
*                                                                               
TOTTIM01 DS    PL3                 TOTAL TIME 8B 01                             
TOTTIM07 DS    PL3                 DAY TOTAL TIME 8B 07                         
TODAYP   DS    PL3                 Todays Date (Packed)                         
DNUMPRD  DS    XL2                 Number of Periods Saved                      
DNUMLOC  DS    XL2                 Number of Locations saved for person         
*                                                                               
SVADDR   DS    AL4                                                              
SVKEY    DS    CL56                                                             
SVACCT   DS    CL12                                                             
SVPEDT   DS    CL3                                                              
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
OFFCODE  DS    CL2                 Office code                                  
LINE#001 DS    XL2                                                              
LINE#007 DS    XL2                                                              
SAVE107  DS    XL124                                                            
SAVE101  DS    XL124                                                            
SAVE207  DS    XL124                                                            
SAVE201  DS    XL124                                                            
*                                                                               
SVASTDTE DS    CL(L'TIMETDT1)      SAVED AREA FOR ACTUAL START DATE             
SVELMHDR DS    CL(TIMHLNQ+L'TIMEIDNO+L'TIMEPST1+L'TIMEPIDC)                     
SVELMHD2 DS    CL255                                                            
*                                                                               
LASTKEY  DS    CL(TIMKEND)                                                      
LSTTS    DS    XL3                 LAST T/S DATE                                
LSTTMPTS DS    XL3                 LAST TEMPO T/S DATE                          
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVNMES  DS    0CL36               LEVEL NAMES                                  
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVDNME  DS    CL36                LEVEL D NAME                                 
*                                                                               
LOCDATE  DS    0X                                                               
LOCSTDT  DS    PL3                 LOCATION START DATE                          
LOCENDT  DS    PL3                 LOCATION END DATE                            
LOCDLOC  DS    PL3                 LOCATION LOCK DATE                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL1                                                              
PPERCDE  DS    CL6                 PERSON CODE                                  
         DS    CL2                                                              
PPERNME  DS    CL36                PERSON NAME                                  
         DS    CL4                                                              
PEDATE   DS    CL8                 PERIOD END DATE                              
         DS    CL4                                                              
PSDATE   DS    CL8                 PERIOD START DATE (CALENDAR)                 
         DS    CL3                                                              
PTMSLIN  DS    CL6                 TMS LINE NUMBER                              
         DS    CL3                                                              
PHOURS   DS    CL8                 TOTAL HOURS                                  
         DS    CL4                                                              
PHRSD1   DS    CL8                       - DAY 1 HOURS                          
         DS    CL2                                                              
PHRSD2   DS    CL8                       - DAY 2 HOURS                          
         DS    CL2                                                              
PHRSD3   DS    CL8                       - DAY 3 HOURS                          
         DS    CL2                                                              
PHRSD4   DS    CL8                       - DAY 4 HOURS                          
         DS    CL2                                                              
PHRSD5   DS    CL8                       - DAY 5 HOURS                          
         DS    CL2                                                              
PHRSD6   DS    CL8                       - DAY 6 HOURS                          
         DS    CL2                                                              
PHRSD7   DS    CL8                       - DAY 7 HOURS                          
         DS    CL2                                                              
PHRSTOT  DS    CL8                       - PERIOD TOTAL                         
         DS    CL2                                                              
         ORG   PRTLINE                                                          
         DS    CL1                                                              
PRTPCODE DS    CL6                                                              
         DS    CL3                                                              
PRTACCT  DS    CL14                                                             
         DS    CL6                                                              
PRTENDDT DS    CL8                                                              
         DS    CL14                                                             
PRTLIN01 DS    CL6                                                              
         DS    CL13                                                             
PRTHOR01 DS    CL8                                                              
         DS    CL11                                                             
PRTLIN07 DS    CL6                                                              
         DS    CL13                                                             
PRTHOR07 DS    CL8                                                              
         DS    CL12                                                             
PRTDA    DS    CL8                                                              
         DS    CL2                                                              
         ORG                                                                    
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADLINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADDESC DS    CL15                                                             
         DS    CL1                                                              
HEADCODE DS    CL6                 1R OFFICE                                    
         DS    CL1                                                              
HEADNAME DS    CL36                DECSRIPTION                                  
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* CALENDAR TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
CALTABD  DSECT                                                                  
CALTEMOA DS    PL2                 END MOA                                      
CALTSMOA DS    PL2                 START MOA                                    
CALTOFC  DS    CL2                 OFFICE/OFFICE GROUP                          
CALTENDT DS    PL3                 Period end date 2's complement               
CALTSTRT DS    PL3                 Period start date 2's complement             
CALTABL  EQU   *-CALTABD                                                        
         EJECT                                                                  
***********************************************************************         
* LOCATION TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
LOCTABD  DSECT                                                                  
LOCDSTRT DS    PL3                 LOCATION START DATE                          
LOCDEND  DS    PL3                 LOCATION END DATE                            
LOCDLOCK DS    PL3                 LOCATION LOCK DATE                           
LOCDATL  EQU   *-LOCTABD                                                        
         EJECT                                                                  
***********************************************************************         
* IO AREA DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
IOAREAD  DSECT                                                                  
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IOKEY             LENGTH                                       
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPZT02 02/18/20'                                      
         END                                                                    
