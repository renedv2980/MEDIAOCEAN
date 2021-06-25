*          DATA SET ACCAP05    AT LEVEL 126 AS OF 01/05/11                      
*PHASE T61D05A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP05 -- CALENDAR MAINTENANCE                      *         
*                                                                     *         
*  COMMENTS:     DEFINES CALENDAR PERIODS FOR TIMESHEETS AND SALARY   *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPFC                                      *         
*                                                                     *         
*  OUTPUTS:      UPDATED CALENDAR RECORDS                             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- THIRD BASE                                     *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* DCUR L108 - FIXED BUG IN GTSTENDT AND FORCE CONTIGUOUS MONTHS FROM  *         
*             ONE YEAR TO THE NEXT                                    *         
* DCUR L109 - CALL PERVAL TO GET NEXT MONTH IN CHKMON                 *         
* DCUR L112 - FIXED SEMIIT ROUTINE TO HANDLE ALL START DATES          *         
*             REBUILD ELEMS IN MAKEPER, NOT ADDELEM (SORTS)           *         
*             DO NOT ALLOW CHANGING OF MOA WHEN TMS START DATE        *         
* DCUR L113 - GIVE MESSAGE 'REC ALREADY EXISTS' WHEN ADDING A CAL THAT*         
*             ALREADY EXISTS.  FORCE USER TO ENTER A PERIOD DEFINITION*         
* TKLU L118 - CALENDAR PER YEAR CHANGE <LO01-2834>                    *         
* NSHE L119 05OCT04 - SOX ALLOW CLIENT TO CHANGE CALENDAR             *         
* NSHE L128 21FEB05 - SPEED UP TIME CHECKING ROUTINE                  *         
* JFOS L129 23AUG05 - READ TDT PASSIVES TO CHECK TIME                 *         
* DKEL L129 17JUL06 <DU01-5479> DISABLE OFFICE LEVEL CALENDAR RECORDS *         
*                               FOR MCS TIMESHEETS USERS              *         
* NSHE L130 12SEP06 - DON'T DELETE SELECTED CAL RECORDS/DON'T ERROR   *         
*                     WHEN CHANGING EXISTING CALENDAR RECORDS         *         
* TKLU L133 17NOV06 <BR10017D> ALLOW TO ADD WEEKMO TYPE CALENDAR      *         
* NSHE L135 01APR08 <LO01-7453> ALLOW ALL PERIOD TYPES FOR BRANDOCEAN *         
* MPEN L136 31APR09 <BR24690L> FIX FOR READING CALENDAR RECORD        *         
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         EJECT                                                                  
         TITLE 'CALENDAR/TIMESHEET PERIODS'                                     
T61D05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D05**,R7,R5,RR=R3                                           
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T61DFFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO                                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BAS   RE,DK                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*&&UK*&& CLI   MODE,XRECADD        ADD PASSIVE POINTER                          
*&&UK*&& BE    XA                                                               
*&&UK*&& CLI   MODE,XRECPUT        CHANGE PASSIVE POINTER                       
*&&UK*&& BE    XP                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALKEY - DETERMINE START AND END DATES OF CALENDAR                     
***********************************************************************         
*                                                                               
VK       NTR1                                                                   
         GOTO1 GETLDG,DMCB,C'1R'                                                
         MVC   LEVELLN,ABCDLEN     GET 1R LEVEL LENGTHS                         
*                                                                               
         MVI   BITS3,0                                                          
*&&US                                                                           
         CLI   ACTEQU,ACTDIS                                                    
         BE    VK00                DISPLAY REC                                  
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED FROM LIST?               
         BNE   VK00                                                             
*&&                                                                             
         GOTO1 =A(UNPROT),DMCB,RR=RELO  UNPROTECT FIELDS FOR CHANGE             
VK00     MVI   OFFTAB,C' '                                                      
         MVC   OFFTAB+1(L'OFFTAB-1),OFFTAB                                      
         XC    STMONTH,STMONTH                                                  
         XC    NDMONTH,NDMONTH                                                  
         XC    STDATE,STDATE                                                    
         XC    ENDATE,ENDATE                                                    
         XC    PSTMON,PSTMON       PACKED START MON                             
         XC    PNDMON,PNDMON                                                    
         XC    PREVEND,PREVEND                                                  
         XC    PREVSTRT,PREVSTRT                                                
         MVC   CADPER,SPACES                                                    
         OI    CADPERH+6,X'80'                                                  
*                                                                               
         MVI   BITS,0                                                           
         CLI   ACTEQU,ACTADD       IF ACTION ADD                                
         BNE   VK01                                                             
*&&US*&& OI    BITS,GUESS          NO, SO SET GUESS FLAG                        
         GOTO1 =A(CLRSCRN),DMCB,RR=RELO    CLEAR SCREEN                         
         LA    R2,CADOFCH                                                       
*&&UK*&& CLC   CADOFC,SPACES          IS OFFICE BLANK                           
*&&UK*&& BH    EINCALOF               NO - NOT ALLOWED ANYMORE(MCS)             
*                                                                               
VK01     MVC   SVACTEQU,ACTEQU                                                  
         LA    R2,CONOPTH                                                       
*                                                                               
VK05     CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLC   AC@STRTU,CONOPT     IS START = OPT ON                            
         BE    VK10                                                             
         B     EINVOPT             NO VALID OPTIONS                             
*                                                                               
VK10     DS    0H                  CHECK CPY REC FOR FISCAL YEAR START          
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CMPY                                                     
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR ',BIGKEY,BIGKEY,0             
         CLC   BIGKEY(L'CPYKEY),KEYSAVE                                         
         BNE   VK14                                                             
         MVC   AIO,AIO2                                                         
         LA    R2,BIGKEY                                                        
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST ',(R2),AIO,WORK               
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    TMSTDATE,TMSTDATE                                                
         L     R6,AIO2                                                          
         USING CPYELD,R6                                                        
         MVI   ELCODE,CPYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VK14                                                             
         OC    CPYSFST,CPYSFST     ANY FISCAL START MONTH                       
         BZ    VK14                                                             
         CLI   CPYSFST,C'1'        JAN                                          
         BE    VK14                                                             
*                                                                               
         OC    CPYTMSSD,CPYTMSSD   ANY TMS START DATE?                          
         BZ    VK11                                                             
         GOTO1 DATCON,DMCB,(2,CPYTMSSD),(1,TMSTDATE) SAVE DATE                  
*                                                                               
         USING MONTABD,R3                                                       
VK11     LA    R3,MONTAB                                                        
VK12LP   CLC   MONCODE,CPYSFST     MATCH ON MONTH CODE (1-9,A,B,C)              
         BE    VK12                                                             
         LA    R3,MONTABLN(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BE    VK14                                                             
         B     VK12LP                                                           
*                                                                               
VK12     MVC   STMONTH,MONCHAR     SAVE CHARACTER MONTH                         
         DROP  R3,R6                                                            
*                                                                               
VK14     DS    0H                                                               
         OC    STMONTH,STMONTH     IS THERE A START MONTH                       
         BZ    VK18                NO THEN DEFAULT TO JAN                       
*                                                                               
*        GET START AND END MONTHS FROM MONTH ENTERED                            
*                                                                               
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         XC    WORK,WORK                                                        
*&&US                                                                           
         MVC   WORK(L'STMONTH),STMONTH                                          
         MVC   WORK+L'STMONTH(2),=C'01'                                         
*&&                                                                             
*&&UK                                                                           
         MVC   WORK(2),=C'01'                                                   
         MVC   WORK+2(L'STMONTH),STMONTH                                        
*&&                                                                             
         MVC   WORK+L'STMONTH+2(6),=C'-(12M)'                                   
*                                                                               
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'10'                                                       
         GOTO1 PERVAL,DMCB,(11,WORK),(BYTE,BLOCK)                               
*                                                                               
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   PSTMONTH,PVALPSTA+1                                              
         GOTO1 DATCON,DMCB,(1,PVALPEND),(11,WORK)                               
         MVC   NDMONTH,WORK        END MONTH                                    
*                                                                               
*        VALIDATE YEAR                                                          
*                                                                               
VK18     LA    R2,CADYRH                                                        
* WAS CAUSING ALL THE PERIODS TO BE REBUILT IN VR OVERWRITING ANY               
* PERIOD CHANGES MADE TO THE CALENDAR.                                          
*        TM    CADYRH+4,X'80'      IF ENTERED THIS TIME                         
*        BNO   *+8                                                              
*        OI    BITS,GUESS          SET GUESS FLAG                               
*                                                                               
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         LA    R3,BLOCK                                                         
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(BYTE,BLOCK)                           
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT                          
         BE    VK21                                                             
         CLI   DMCB+4,X'03'        TODAYS DATE RETURNED ON NO INPUT             
         BE    VK20                                                             
         CLI   DMCB+4,X'00'        EVERYTHING OK                                
         BE    VK21                                                             
         B     EINVYR                                                           
*                                                                               
VK20     OC    STMONTH,STMONTH                                                  
         BZ    VK21                                                             
         CLC   PSTMONTH,PVALPSTA+1   IS TODAY AFTER START MONTH                 
         BH    VK21                                                             
         MVC   WORK(7),=C'T(+12M)'                                              
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         GOTO1 PERVAL,DMCB,(7,WORK),(X'62',BLOCK)                               
*                                                                               
*                                    CONVERT FA TO F0, FB TO F1, ETC.           
VK21     GOTO1 DATCON,DMCB,(0,PVALESTA),(X'20',PVALESTA)                        
         MVC   PVALEEND,PVALESTA                                                
         XC    WORK,WORK           GET FULL YEAR                                
         MVC   WORK(2),PVALEEND                                                 
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+10)                                
         XC    WORK(4),WORK                                                     
         MVC   WORK(4),WORK+10     WORK+10=YYYYMMDD                             
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         GOTO1 PERVAL,DMCB,(4,WORK),(X'42',BLOCK)                               
*                                    CONVERT FA TO F0, FB TO F1, ETC.           
         GOTO1 DATCON,DMCB,(0,PVALEEND),(X'20',PVALEEND)                        
         MVC   NDYEAR,PVALEEND     END CHARACTER YEAR                           
*                                                                               
         XC    WORK,WORK           PUT START AND END TOGETHER                   
         OC    STMONTH,STMONTH                                                  
         BNZ   *+14                                                             
         MVC   WORK(3),DDJAN       JAN                                          
         B     *+10                                                             
         MVC   WORK(3),STMONTH                                                  
         MVI   WORK+3,C'-'                                                      
         OC    NDMONTH,NDMONTH                                                  
         BNZ   *+14                                                             
         MVC   WORK+4(3),DDDEC     DEC                                          
         B     *+10                                                             
         MVC   WORK+4(3),NDMONTH                                                
         MVI   WORK+7,C'/'                                                      
         MVC   WORK+8(2),NDYEAR                                                 
         MVC   BYTE,LANGCODE                                                    
         LA    R1,10               LENGTH                                       
         STC   R1,BYTE2                                                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,BLOCK)                            
*                                                                               
VK24     MVC   STDATE,PVALPSTA     SAVE START DATE                              
         MVC   ESTDATE,PVALESTA    SAVE EBCDIC START DATE                       
         MVC   ENDATE,PVALPEND     AND END DATE                                 
         MVC   NDYEAR,PVALEEND     END CHARACTER YEAR                           
         MVC   STYEAR,PVALESTA     START YEAR FOR PERIODS                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),NDYEAR      CONVERT NDYEAR FOR Y2K                       
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   NDYEAR,WORK                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(2),STYEAR      CONVERT STYEAR FOR Y2K                       
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   STYEAR,WORK                                                      
*                                                                               
VK26     MVC   8(4,R2),SPACES      DISPLAY                                      
         MVC   8(2,R2),NDYEAR      CHARACTER YEAR                               
         MVI   5(R2),X'02'                                                      
         OI    6(R2),X'80'                                                      
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ANY OFFICE ENTERED                                                     
*----------------------------------------------------------------------         
*                                                                               
         MVC   OFFICE,SPACES                                                    
         LA    R2,CADOFCH                                                       
         CLI   CADOFCH+5,0                                                      
         BE    VK30                                                             
*                                                                               
         CLC   CADOFCH+5(1),LEVELLN CORRECT LENGTH                              
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'0'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),CADOFC                                                 
         OC    OFFICE,SPACES                                                    
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6          BUILD KEY                                    
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT(L'OFFICE),OFFICE                                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BNE   EINVOFF                                                          
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUILD KEY                                                              
*----------------------------------------------------------------------         
*                                                                               
VK30     DS    0H                  READ HIGH ON END MONTH                       
*                                                                               
         CLC   SVNDYEAR,NDYEAR                                                  
         BNE   VK31                                                             
         CLC   SVOFFICE,OFFICE                                                  
         BE    *+8                                                              
VK31     MVI   SAPERNUM,0                                                       
*        MVC   SVOFFICE,OFFICE    CAN'T DO THIS HERE OR WILL NOT                
         MVC   PSTMON,STDATE      DISPLAY NEW RECORD IN VR                      
         MVC   PNDMON,ENDATE                                                    
         CLI   ACTEQU,ACTDIS       DON'T DEL/CHANGE ON ACT DISP                 
         BE    VK32                                                             
*&&US                                                                           
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED?                         
         BNE   VK32                                                             
*&&                                                                             
         GOTO1 =A(GTSTENDT),DMCB,RR=RELO                                        
         GOTO1 =A(CKSTDHR),DMCB,RR=RELO                                         
         GOTO1 =A(CKEDTHR),DMCB,RR=RELO                                         
*                                                                               
VK32     XC    BIGKEY,BIGKEY                                                    
         LA    R2,OFFTAB                                                        
         USING CASRECD,R6          BUILD KEY                                    
         LA    R6,BIGKEY                                                        
         XC    CASKEY,CASKEY       BUILD TIMESHEET PERIOD RECORD KEY            
         MVI   CASKTYP,CASKTYPQ    RECORD TYPE                                  
         MVI   CASKSUB,CASKSUBQ    SUB RECORD TYPE                              
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA(1),PNDMON     END YEAR                                  
         MVI   CASKEMOA+1,X'00'                                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(CASKSMOA-CASKEY-1),BIGKEY                                
         BNE   VK50                                                             
*                                                                               
VK35     DS    0H                  DID STARTING MONTH CHANGE                    
         CLC   CASKEMOA,PNDMON                                                  
         BE    VK37                YES THEN READ SEQUENTIALLY                   
         CLC   OFFICE,CASKOFC      SAME OFFICE                                  
         BNE   VK50                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'       DELETE OLD REC                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         MVC   PREVSTRT,TMRSTART   SAVE TO DELETE POINTER                       
         MVC   PREVEND,TMREND                                                   
         L     R6,AIO                                                           
         USING CASRECD,R6          BUILD KEY                                    
         OI    CASRSTAT,X'80'                                                   
         CLI   ACTEQU,ACTDIS       DON'T DEL/CHANGE ON ACT DISP                 
         BE    VK50                                                             
         CLI   ACTEQU,ACTSEL       ACTION SELECT?                               
         BNE   VK36                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED?                         
         BNE   VK50                NO - DONT DELETE RECORD                      
VK36     GOTO1 PUTREC                                                           
         LA    R6,BIGKEY                                                        
         OI    CASKSTAT,X'80'                                                   
         GOTO1 WRITE                                                            
         B     VK50                                                             
*                                                                               
VK37     LA    R6,BIGKEY                                                        
         CLC   OFFICE,CASKOFC      SAME OFFICE                                  
         BNE   VK38                                                             
         MVC   PNDMON,CASKEMOA    END MOA                                       
         MVC   PSTMON,CASKSMOA    START MOA                                     
         B     VK40                                                             
*                                                                               
VK38     CLC   OFFICE,SPACES                                                    
         BNE   VK40                                                             
         MVC   0(L'TRNOFFC,R2),CASKOFC                                          
         LA    R2,L'TRNOFFC(R2)                                                 
*                                                                               
VK40     OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 SEQ                                                              
         CLC   KEYSAVE(CASKSMOA-CASKEY-1),BIGKEY                                
         BE    VK35                                                             
         B     VK50                                                             
*                                                                               
VK50     XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         XC    CASKEY,CASKEY       BUILD TIMESHEET PERIOD RECORD KEY            
         MVI   CASKTYP,CASKTYPQ    RECORD TYPE                                  
         MVI   CASKSUB,CASKSUBQ    SUB RECORD TYPE                              
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA,PNDMON     END MOA                                      
         MVC   CASKSMOA,PSTMON     START MOA                                    
         MVC   CASKOFC,OFFICE      OFFICE                                       
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
*&&UK*&& MVC   SVNDYEAR,NDYEAR                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALREC - VALIDATE RECORD                                               
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
*                                                                               
*        BAS   RE,VK               VALIDATE KEY EVERY TIME                      
         OI    GENSTAT2,RETEQSEL   RETURN TO SAME SELECTION                     
*                                                                               
         CLI   ACTEQU,ACTDIS                                                    
         BE    VR00                DISPLAY REC                                  
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED FROM LIST?               
         BNE   VR00                                                             
         GOTO1 =A(UNPROT),DMCB,RR=RELO  UNPROTECT FIELDS FOR CHANGE             
                                                                                
*IF USER DID NOT ENTER PASSWORD FIRST TIME THROUGH MUST FORCE TO REDISP         
*                                                                               
         TM    TRANSTAT,RACHANG    RECORD OR ACTION CHANGE                      
         BNZ   VR00                REDISPLAY                                    
         CLC   OFFICE,SVOFFICE                                                  
         BNE   VR00                REDISPLAY                                    
         CLC   NDYEAR,SVNDYEAR                                                  
         BE    VR00A               REDISPLAY                                    
                                                                                
VR00     MVC   SVNDYEAR,NDYEAR                                                  
         MVC   SVOFFICE,OFFICE                                                  
         CLI   ACTEQU,ACTADD       NOTHING TO DISPLAY                           
         BE    VR00A                                                            
         B     DR                                                               
*                                  GET END OF PREV CAL AND ST OF NEXT           
VR00A    GOTO1 =A(GTSTENDT),DMCB,RR=RELO                                        
*                                  CANNOT CHANGE IF STD/EDT HOURS XIST          
         GOTO1 =A(CKSTDHR),DMCB,RR=RELO                                         
         GOTO1 =A(CKEDTHR),DMCB,RR=RELO                                         
*                                                                               
         USING CASRECD,R6          BUILD KEY                                    
         NI    BITS,X'FF'-NEWREC                                                
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         OI    DMINBTS,X'08'       PASS BACK DELETED RECS                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VR01                RECORD DOESN'T EXISTS                        
         GOTO1 GETREC                                                           
         LA    R6,BIGKEY                                                        
         TM    CASKSTAT,X'80'      MARKED DELETED                               
         BO    *+12                                                             
         TM    CASRSTAT,X'80'                                                   
         BZ    VR04                                                             
         NI    CASKSTAT,X'FF'-X'80'        UNDELETE                             
         L     R6,AIO                                                           
         NI    CASRSTAT,X'FF'-X'80'        UNDELETE                             
         B     VR04A                                                            
*                                                                               
VR01     DS    0H                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   ERRNOREC            RECORD DOESN'T EXIST                         
*&&US                                                                           
         CLI   ACTEQU,ACTDIS                                                    
         BE    VR02                DISPLAY REC                                  
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED FROM LIST?               
         BNE   VR02                                                             
*&&                                                                             
         GOTO1 =A(UNPROT),DMCB,RR=RELO  UNPROTECT FIELDS FOR CHANGE             
VR02     L     RE,AIO              CLEAR IO                                     
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),SAVEKEY    MOVE IN KEY                            
         OI    BITS,NEWREC         FLAG TO ADD REC                              
         B     VR04A                                                            
*                                                                               
VR04     CLI   ACTEQU,ACTADD                                                    
         BNE   *+12                                                             
         LA    R2,CADYRH                                                        
         B     ERYESREC            RECORD ALREADY EXISTS                        
VR04A    L     R6,AIO                                                           
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         BE    VR05                                                             
         OI    BITS,GUESS          NEW RECORD GUESS AT FORM                     
         BAS   RE,ADDRULE          ADD ELEM                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL            POINT TO ELEM                                
         B     VR06                                                             
*                                                                               
VR05     DS    0H                                                               
         MVC   PREVSTRT,TMRSTART   SAVE PREV FOR PASSIVE KEY UPDATES            
         MVC   PREVEND,TMREND                                                   
         MVC   TMRSTART,STDATE     FILL IN DATES IN ELEM                        
         MVC   TMREND,ENDATE                                                    
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE PERIOD DEFINITION                                             
*---------------------------------------------------------------------          
*                                                                               
VR06     LA    R3,DEFDEFLT         DEFAULT TO WEEKLY IF NO INPUT                
         LA    R2,CADDEFH                                                       
         CLI   5(R2),0                                                          
         BNE   VR08                                                             
         CLI   ACTEQU,ACTADD                                                    
         BE    VR08                                                             
         MVC   CADDEF,SVTIMDEF                                                  
         MVC   TMRDEF,SVTMRDEF                                                  
*                                                                               
VR08     LA    R3,DEFTABLE         TABLE OF VALID PERIOD DEFINITIONS            
         USING DEFTBD,R3                                                        
*                                                                               
VR10     CLI   CADDEFH+5,5         IF LENGTH ENTERED IS 5 OR >                  
         BNH   VR12                                                             
         ZIC   R1,CADDEFH+5        COMPARE ON LONG NAME                         
         SH    R1,=H'1'                                                         
         BM    VR12                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DEFDESCR(0),CADDEF                                               
         BE    VR20                GOT A MATCH                                  
         B     VR15                                                             
*                                                                               
VR12     ZIC   R1,DEFMIN           MIN NUMBER FOR COMPARE                       
         SH    R1,=H'1'                                                         
         BM    VR15                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DEFCODE(0),CADDEF   COMPARE ON SHORT NAME                        
         BE    VR20                GOT A MATCH                                  
VR15     LA    R3,DEFTBLN(R3)                                                   
         CLI   0(R3),EOT                                                        
         BNE   VR10                                                             
         B     EINVDEF                                                          
*                                                                               
VR20     MVC   BUILDRT,DEFRTRN               ROUTINE TO BUILD CALENDAR          
         MVC   CADDEF(L'DEFDESCR),DEFDESCR   DISPLAY FULL DESCRIPTION           
         MVC   SVTIMDEF,CADDEF                                                  
         OI    CADDEFH+6,X'80'               XMIT                               
*                                                                               
         CLC   TMRDEF,DEFBYTE                                                   
         BE    *+8                                                              
         OI    BITS,GUESS          NEW DEFINITION TAKE GUESS AT FORMAT          
         MVC   TMRDEF,DEFBYTE      MOVE INTO ELEMENT                            
         MVC   SVTMRDEF,TMRDEF                                                  
*                                                                               
         XC    ENDDAY,ENDDAY       CLEAR DAY PERIOD ENDS                        
         CLI   DEFDAY,X'01'        IS DAY PERIOD ENDS REQUIRED                  
         BE    VR30                YES                                          
         XC    TMRPEND,TMRPEND     CLEAR                                        
         B     VR40                AND SKIP                                     
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE DAY PERIOD ENDS                                               
*---------------------------------------------------------------------          
*                                                                               
VR30     DS    0H                                                               
         USING DAYTBD,R3                                                        
         LA    R3,DAYDEFLT         DEFAULT TO SUNDAY IF NO INPUT                
         LA    R2,CADDAYH                                                       
*&&UK                                                                           
         CLI   5(R2),0                                                          
         BE    VR35                                                             
*&&                                                                             
*&&US                                                                           
         ZIC   R1,0(R2)            CAN'T COUNT ON CHECKING HEADER+5             
         AHI   R1,-8               B/C MAY SHOW INPUT LEN AS ZERO               
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXCLC R1,8(R2),SPACES                                                  
         BNH   VR35                                                             
*&&                                                                             
*                                                                               
         LA    R3,DAYTABLE         TABLE OF VALID DAYS                          
*                                                                               
VR33     ZIC   R1,DAYMIN                                                        
         SH    R1,=H'0'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,DICLIST                                                       
         ZICM  R0,DAYDCODE,2       DISP TO DAY CODE                             
         AR    R4,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),CADDAY                                                   
         BE    VR35                                                             
*                                                                               
         LA    R3,DAYTBLN(R3)                                                   
         CLI   0(R3),EOT                                                        
         BNE   VR33                                                             
         B     EINVDAY                                                          
*                                                                               
VR35     DS    0H                                                               
         MVC   ENDDAY,DAYBYTE                                                   
*                                                                               
         CLC   TMRPEND,DAYBYTE     DID DAY CHANGE                               
         BE    *+8                                                              
         OI    BITS,GUESS          YES, TAKE BEST GUESS AT PERIODS              
         MVC   TMRPEND,DAYBYTE     FOR ELEM                                     
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        IF PERIOD ELEMS EXIST THEN CHECK FOR CHANGES                           
*                                                                               
VR40     DS    0H                                                               
         L     R6,AIO                                                           
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         BNE   VR100               NEED TO CHANGE RECORD                        
*                                                                               
         TM    BITS,GUESS          RULES CHANGED SO CREATE FROM SCRATCH         
         BO    VR100                                                            
*                                                                               
*        UPDATE REC- MAKE RECORD FROM SCREEN                                    
*                                                                               
VR50     OI    BITS,FIRST          FIRST PERIOD                                 
         XC    LASTEND,LASTEND                                                  
         XC    SAVEPMON,SAVEPMON                                                
         XC    PERYRMON,PERYRMON   PERIOD MONTH AND YR                          
         MVC   PERYRMON(1),STDATE  SET YEAR-START                               
*                                                                               
         XC    LSTMONYR,LSTMONYR                                                
         OC    MUSTSTMO,MUSTSTMO   MUST START MONTH                             
         BZ    VR52                                                             
         MVC   WORK(2),MUSTSTMO                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(11,WORK+10)                                
         MVC   LSTMONYR(3),WORK+10                                              
         MVC   LSTMONYR+3(3),WORK+15                                            
         MVC   PERYRMON,MUSTSTMO                                                
*                                                                               
*                                                                               
VR52     LA    R2,CADCOL1H         COLUMN 1 HEADER                              
         USING DSPLINED,R2                                                      
*                                                                               
VR55     DS    0H                                                               
         BAS   RE,MAKEPER          ADDS TIMESHEET PERIOD ELEM                   
         BNE   VR72                NO MORE                                      
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND1H                                                      
         CR    R2,R1                                                            
         BNH   VR55                                                             
*                                                                               
         LA    R2,CADCOL2H         COLUMN 2 HEADER                              
VR60     BAS   RE,MAKEPER          ADDS TIMESHEET PERIOD ELEM                   
         BNE   VR72                                                             
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND2H                                                      
         CR    R2,R1                                                            
         BNH   VR60                                                             
*                                                                               
         LA    R2,CADCOL3H         COLUMN 3 HEADER                              
VR70     BAS   RE,MAKEPER          ADDS TIMESHEET PERIOD ELEM                   
         BNE   VR72                                                             
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND3H                                                      
         CR    R2,R1                                                            
         BNH   VR70                                                             
*                                                                               
VR72     DS    0H                  RENUMBERS PERIOD IN CASE DELETED ONE         
         GOTO1 =A(RENUMB),DMCB,RR=RELO                                          
         BAS   RE,GETRANGE         GET START AND END DATES                      
*                                                                               
         OC    MUSTSTDT,MUSTSTDT   PERIODS MUST BE CONTIGUOUS                   
         BZ    VR75                WITH OTHER YEARS                             
         CLC   MUSTSTDT,SVSTART                                                 
         BE    VR75                                                             
         LA    R2,CADCOL1H         COLUMN 1 HEADER                              
         B     ENOTCONT                                                         
VR75     OC    MUSTENDT,MUSTENDT                                                
         BZ    VR80                                                             
         CLC   MUSTENDT,SVEND                                                   
         BE    VR80                                                             
         L     R2,ALSTPER          A(LAST PERIOD)                               
         B     ENOTCONT                                                         
*                                                                               
VR80     L     R6,AIO                                                           
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         MVC   TMRSTART,SVSTART    USER MIGHT HAVE CHANGED START                
         MVC   TMREND,SVEND        AND END DATES                                
         MVC   STDATE,SVSTART                                                   
         MVC   ENDATE,SVEND                                                     
         DROP  R2                                                               
         B     VRX                                                              
         EJECT                                                                  
*                                                                               
*        MAKE NEW RECORD                                                        
*                                                                               
VR100    DS    0H                                                               
         MVI   ELCODE,TMPELQ       START FROM NEW                               
         GOTO1 REMELEM                                                          
*                                                                               
*                                  MUST START WITH A CONTIGUOUS MONTH           
         GOTO1 =A(CHKMON),DMCB,RR=RELO                                          
*                                                                               
         OC    MUSTSTDT,MUSTSTDT                                                
         BZ    VR102                                                            
         CLC   AC@STRTU,CONOPT     IS START = OPT ON                            
         BNE   VR101                                                            
         CLC   STDATE,MUSTSTDT   IF NO PREVIOUS CALENDAR                        
         BE    VR101               THAN ABLE TO USE START OPTION                
         LA    R2,CONOPTH                                                       
         B     ENOTCONT            NOT CONTIGUOUS                               
VR101    MVC   STDATE,MUSTSTDT                                                  
VR102    OC    MUSTENDT,MUSTENDT                                                
         BZ    VR103                                                            
         MVC   ENDATE,MUSTENDT                                                  
VR103    GOTO1 DATCON,DMCB,(1,STDATE),(17,CADPER)                               
         GOTO1 DATCON,DMCB,(1,ENDATE),(17,CADPER+9)                             
         MVI   CADPER+8,C'-'                                                    
         OI    CADPERH+6,X'80'     XMIT                                         
*                                                                               
         L     R6,AIO                                                           
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         MVC   TMRSTART,STDATE     START AND END DATES MAY HAVE CHANGED         
         MVC   TMREND,ENDATE       AND END DATES                                
*                                                                               
         L     RF,BUILDRT          ROUTINE TO BUILD CALENDAR                    
         L     R1,RELO                                                          
         AR    RF,R1                                                            
         BASR  RE,RF                                                            
*                                                                               
VRX      DS    0H                                                               
         NI    BITS,X'FF'-GUESS                                                 
         TM    BITS,NEWREC                                                      
         BO    VRXADD                                                           
*                                                                               
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         BAS   RE,XP               CHANGE PASSIVE POINTER                       
         MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                                                               
*                                                                               
VRXADD   GOTO1 ADDREC                                                           
         BAS   RE,XA               ADD PASSIVE POINTER                          
         MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH WEEKLY PERIODS                                       
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
*        END DAY EQIVALENT IN ENDDAY (X'01'=MON...X'07'=SUN)                    
***********************************************************************         
*                                                                               
WEEKIT   NTR1                                                                   
         USING PERVALD,R3                                                       
         NI    BITS,X'FF'-LASTPER     RESET                                     
         MVI   PERNUM,X'01'           START WITH PERIOD NUMBER 1                
         MVC   PERYRMON(2),SVSTMO     YEAR AND MONTH                            
         MVC   PERSTART,STDATE        FIRST START DATE                          
         MVC   SVPERST,PERSTART                                                 
*                                                                               
*        FIRST TWO PERIODS ARE A LITTLE TRICKY                                  
*                                                                               
*                                  GET EBCIDIC START DATE FOR GETDAY            
         XC    EBDATE,EBDATE                                                    
         GOTO1 DATCON,DMCB,(1,STDATE),(0,EBDATE)                                
*                                  GETDAY GETS DAY NUMBER OF START DATE         
         GOTO1 GETDAY,DMCB,EBDATE,WORK                                          
         MVC   STDAY,DMCB          START DAY NUMBER                             
*                                                                               
         ZIC   R2,ENDDAY                                                        
         ZIC   R1,STDAY                                                         
         CR    R1,R2               IS START DAY < END DAY                       
         BNH   WK10                THEN SUBTRACT TO GET # DAYS                  
         LA    R2,7(R2)            ELSE ADD 7 BEFORE SUBTRACTING                
WK10     SR    R2,R1               R2 HAS NUMBER OF DAYS TO ADD                 
*                                                                               
         GOTO1 ADDAY,DMCB,EBDATE,WORK,(R2)        GETS END DATE                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PEREND)    CHANGE TO PWOS                
*                                                                               
         BAS   RE,ADDPER           FIRST PERIOD                                 
*                                                                               
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
*        CALCULATE SECOND PERIOD                                                
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(1D)'             ADDS ONE DAY TO END DATE          
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(4),=C'(1T)'             ADDS ONE DAY TO END DATE          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)    FOR START DATE           
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         CLC   PERSTART(2),SVPERST         SAME START MONTH AS LAST PER         
         BE    WK15                                                             
         CLC   PERSTART(2),SVSTMO          SAME START MONTH AS LAST MOA         
         BE    WK15                                                             
         GOTO1 =A(BUMPMO),DMCB,RR=RELO                                          
WK15     MVC   PERYRMON(2),SVSTMO                                               
         MVC   SVPERST,PERSTART                                                 
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(1W)'           ADDS ONE WEEK TO END DATE           
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)     FOR END DATE            
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPSTA                                                  
*                                                                               
         BAS   RE,ADDPER                                                        
*                                                                               
WK20     DS    0H                  NOW CAN JUST BUMP BY WEEKS                   
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)                               
         MVC   WORK+8(5),=C'(1W)-'   ADDS ONE WEEK TO START DATE                
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK+13)                              
         MVC   WORK+21(4),=C'(1W)'   ADDS ONE WEEK TO END DATE                  
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(25,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         MVC   PEREND,PVALPEND                                                  
         CLC   PERSTART(2),SVPERST        SAME START MONTH AS LAST PER          
         BE    WK25                                                             
         CLC   PERSTART(2),SVSTMO         SAME START MONTH AS LAST MOA          
         BE    WK25                                                             
         GOTO1 =A(BUMPMO),DMCB,RR=RELO                                          
WK25     MVC   PERYRMON(2),SVSTMO                                               
         MVC   SVPERST,PERSTART                                                 
*                                                                               
*        MVC   PERYRMON(2),PERSTART       YEAR AND MONTH                        
*                                                                               
         CLC   PEREND,STDATE       TO HANDLE YR 2000                            
         BL    WK40                                                             
*                                                                               
         CLC   PEREND,ENDATE       HAVE WE REACHED END DATE                     
         BL    WK50                NO                                           
WK40     OI    BITS,LASTPER        LAST PERIOD                                  
         MVC   PEREND,ENDATE                                                    
         B     WK50                                                             
*                                                                               
WK50     DS    0H                  ADD THE ELEMENT                              
         BAS   RE,ADDPER                                                        
*                                                                               
         TM    BITS,LASTPER        LAST PERIOD?                                 
         BZ    WK20                GET NEXT PERIOD                              
*                                                                               
WKX      DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH WEEKLY PERIODS THAT DON'T OVERLAP CAL MONTHS         
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
*        END DAY EQIVALENT IN ENDDAY (X'01'=MON...X'07'=SUN)                    
***********************************************************************         
*                                                                               
WKMONIT  NTR1                                                                   
         USING PERVALD,R3                                                       
         NI    BITS,X'FF'-LASTPER     RESET                                     
         MVI   PERNUM,X'01'           START WITH PERIOD NUMBER 1                
         MVC   PERYRMON(2),SVSTMO     YEAR AND MONTH                            
         MVC   PERSTART,STDATE        FIRST START DATE                          
         MVC   SVPERST,PERSTART    SAVE PERIOD START DATE                       
         B     WM06                                                             
*                                                                               
*        FIRST PERIODS OF EACH MONTH ARE A LITTLE TRICKY                        
*                                                                               
*                                  GET EBCIDIC START DATE FOR GETDAY            
WM05     DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(1D)'             ADDS ONE DAY TO END DATE          
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(4),=C'(1T)'             ADDS ONE DAY TO END DATE          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)   FOR START DATE            
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         CLC   PERSTART(2),PEREND    IS PERIOD IN SAME MONTH                    
         BE    WM21                                                             
*                                                                               
WM06     XC    EBDATE,EBDATE       GET EBCIDIC START DATE FOR GETDAY            
         GOTO1 DATCON,DMCB,(1,PERSTART),(0,EBDATE)                              
*                                  GETDAY GETS DAY NUMBER OF START DATE         
         GOTO1 GETDAY,DMCB,EBDATE,WORK                                          
         MVC   STDAY,DMCB          START DAY NUMBER                             
*                                                                               
         ZIC   R2,ENDDAY                                                        
         ZIC   R1,STDAY                                                         
         CR    R1,R2               IS START DAY < END DAY                       
         BNH   WM10                THEN SUBTRACT TO GET # DAYS                  
         LA    R2,7(R2)            ELSE ADD 7 BEFORE SUBTRACTING                
WM10     SR    R2,R1               R2 HAS NUMBER OF DAYS TO ADD                 
*                                                                               
         GOTO1 ADDAY,DMCB,EBDATE,WORK,(R2)        GETS END DATE                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PEREND)    CHANGE TO PWOS                
         B     WM25                                                             
*                                                                               
WM21     XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(1W)'           ADDS ONE WEEK TO END DATE           
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)     FOR END DATE            
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPSTA                                                  
*                                                                               
WM25     CLC   PERSTART(2),PEREND    PERIOD MUST BE IN SAME MONTH               
         BE    WM30                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK+10)                            
         MVC   WORK(3),WORK+10                                                  
         MVC   WORK+3(3),WORK+15                                                
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(6,WORK),(BYTE,PVALBLK)                              
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPEND     LAST DAY IN MONTH                            
         B     WM25                                                             
*                                                                               
WM30     CLC   PERSTART(2),SVPERST     SAME START MONTH AS LAST PER             
         BE    WM35                                                             
         CLC   PERSTART(2),SVSTMO      SAME START MONTH AS LAST MOA             
         BE    WM35                                                             
         GOTO1 =A(BUMPMO),DMCB,RR=RELO  BUMP MOA TO NEXT MONTH                  
WM35     MVC   PERYRMON(2),SVSTMO                                               
         MVC   SVPERST,PERSTART                                                 
*                                                                               
         CLC   PEREND,STDATE       TO HANDLE YR 2000                            
         BL    WM40                                                             
*                                                                               
         CLC   PEREND,ENDATE       HAVE WE REACHED END DATE                     
         BL    WM50                NO                                           
WM40     OI    BITS,LASTPER        LAST PERIOD                                  
         MVC   PEREND,ENDATE                                                    
         B     WM50                                                             
*                                                                               
WM50     DS    0H                  ADD THE ELEMENT                              
         BAS   RE,ADDPER                                                        
         TM    BITS,LASTPER        LAST PERIOD?                                 
         BNZ   WMX                                                              
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
         B     WM05                GET NEXT PERIOD                              
*                                                                               
WMX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH BI-WEEKLY PERIODS                                    
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
*        END DAY EQIVALENT IN ENDDAY (X'01'=MON...X'07'=SUN)                    
***********************************************************************         
*                                                                               
BIWEKIT  NTR1                                                                   
         USING PERVALD,R3                                                       
         NI    BITS,X'FF'-LASTPER     RESET                                     
         MVI   PERNUM,X'01'           START WITH PERIOD NUMBER 1                
         MVC   PERYRMON(2),SVSTMO     YYMM                                      
         MVC   PERSTART,STDATE        FIRST START DATE                          
         MVC   SVPERST,PERSTART                                                 
*                                                                               
*        FIRST TWO PERIODS ARE A LITTLE TRICKY                                  
*                                                                               
*                                  GET EBCIDIC START DATE FOR GETDAY            
         GOTO1 DATCON,DMCB,(1,STDATE),(0,EBDATE)                                
*                                  GETDAY GETS DAY NUMBER OF START DATE         
         GOTO1 GETDAY,DMCB,EBDATE,WORK                                          
         MVC   STDAY,DMCB          START DAY NUMBER                             
*                                                                               
         ZIC   R2,ENDDAY                                                        
         ZIC   R1,STDAY                                                         
         CR    R1,R2               IS START DAY < END DAY                       
         BL    BIWK10              THEN SUBTRACT TO GET # DAYS                  
         LA    R2,7(R2)            ELSE ADD 7 BEFORE SUBTRACTING                
BIWK10   SR    R2,R1               R2 HAS NUMBER OF DAYS TO ADD                 
         LA    R2,7(R2)            ADD ANOTHER WEEK FOR BI-WEEKLY               
*                                                                               
         GOTO1 ADDAY,DMCB,EBDATE,WORK,(R2)        GETS END DATE                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PEREND)    CHANGE TO PWOS                
*                                                                               
         BAS   RE,ADDPER           FIRST PERIOD                                 
*                                                                               
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
*        CALCULATE SECOND PERIOD                                                
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(1D)'             ADDS ONE DAY TO END DATE          
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(4),=C'(1T)'             ADDS ONE DAY TO END DATE          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)    FOR START DATE           
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         CLC   PERSTART(2),SVPERST         SAME START MONTH AS LAST PER         
         BE    BIWK15                                                           
         CLC   PERSTART(2),SVSTMO          SAME START MONTH AS LAST MOA         
         BE    BIWK15                                                           
         GOTO1 =A(BUMPMO),DMCB,RR=RELO     BUMP MOA BY ONE MONTH                
BIWK15   MVC   PERYRMON(2),SVSTMO                                               
         MVC   SVPERST,PERSTART                                                 
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK)                                 
         MVC   WORK+8(4),=C'(2W)'           ADDS TWO WEEKS TO END DATE          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)      FOR END DATE           
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPSTA                                                  
*                                                                               
         BAS   RE,ADDPER                                                        
*                                                                               
BIWK20   DS    0H                  NOW CAN JUST BUMP BY WEEKS                   
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)                               
         MVC   WORK+8(5),=C'(2W)-'   ADDS TWO WEEKS TO START DATE               
         GOTO1 DATCON,DMCB,(1,PEREND),(11,WORK+13)                              
         MVC   WORK+21(4),=C'(2W)'   ADDS TWO WEEKS TO END DATE                 
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(25,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         MVC   PEREND,PVALPEND                                                  
*                                                                               
         CLC   PERSTART(2),SVPERST         SAME START MONTH AS LAST PER         
         BE    BIWK30                                                           
         CLC   PERSTART(2),SVSTMO          SAME START MONTH AS LAST MOA         
         BE    BIWK30                                                           
         GOTO1 =A(BUMPMO),DMCB,RR=RELO     BUMP MOA BY ONE MONTH                
BIWK30   MVC   PERYRMON(2),SVSTMO                                               
         MVC   SVPERST,PERSTART                                                 
*                                                                               
         CLC   PEREND,STDATE       WATCH OUT FOR YR 2000                        
         BL    BIWK40                                                           
*                                                                               
         CLC   PEREND,ENDATE       HAVE WE REACHED END DATE                     
         BL    BIWK50              NO                                           
BIWK40   OI    BITS,LASTPER        LAST PERIOD                                  
         MVC   PEREND,ENDATE                                                    
*                                                                               
BIWK50   DS    0H                  ADD THE ELEMENT                              
         BAS   RE,ADDPER                                                        
*                                                                               
         TM    BITS,LASTPER        LAST PERIOD?                                 
         BZ    BIWK20              GET NEXT PERIOD                              
*                                                                               
BIWKX    DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH SEMI-MONTHLY (HALF) PERIODS ON 15TH AND LAST         
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
***********************************************************************         
*                                                                               
SEMIIT   NTR1                                                                   
         USING PERVALD,R3                                                       
         NI    BITS,X'FF'-LASTPER     RESET                                     
         MVI   PERNUM,X'01'           START WITH PERIOD NUMBER 1                
         MVC   PERYRMON(2),SVSTMO                                               
         MVC   PERSTART,STDATE        FIRST START DATE                          
*                                                                               
SM10     CLC   PERSTART+2(1),=X'10' IS DATE BETWEEN 01-10                       
         BNH   SM20                THEN GET 15TH FOR END                        
         CLC   PERSTART+2(1),=X'20' IS DATE BETWEEN 11-20                       
         BNH   SM30                THEN GET END OF CURR MONTH FOR END           
         XC    WORK,WORK           ELSE GET 15TH OF NEXT MONTH FOR END          
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)                               
         MVC   WORK+3(2),=C'01'           ADD 1 MONTH TO 1ST                    
         MVC   WORK+8(4),=C'(1M)'      TO GET LAST DAY OF MONTH                 
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPSTA                                                  
         MVI   PEREND+2,X'15'                                                   
         B     SM40                                                             
*                                                                               
SM20     MVC   PEREND,PERSTART     ALREADY HAVE FIRST OF MONTH                  
         MVI   PEREND+2,X'15'                                                   
         B     SM40                                                             
*                                                                               
SM30     MVC   WORK(L'PERSTART),PERSTART     GET LAST DAY OF MONTH              
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+5)    C'YYMMDD'                     
         GOTO1 ADDAY,DMCB,(C'Y',WORK+5),(X'80',WORK+10),0                       
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,PEREND)                               
*                                                                               
SM40     BAS   RE,ADDPER                ADD PERIOD ELEMENT                      
*                                                                               
         ZIC   R1,PERNUM           BUMP NUMBER                                  
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
         CLC   PEREND+2(1),=X'15'  DID THE PERIOD END ON THE 15TH               
         BE    SM50                THEN START NEXT PERIOD ON 16TH               
         XC    WORK,WORK           ELSE GET START OF NEXT MONTH                 
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)    C'MM/DD/YY'                
         MVC   WORK+3(2),=C'01'           ADD 1 MONTH TO 1ST                    
         MVC   WORK+8(4),=C'(1M)'      TO GET 1ST OF NEXT MONTH                 
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         MVC   PEREND,PERSTART                                                  
         MVI   PEREND+2,X'15'                                                   
         MVC   PERYRMON(2),SVSTMO                                               
         BAS   RE,ADDPER           ADD PERIOD ELEMENT                           
*                                                                               
* GET READY FOR NEXT PERIOD                                                     
*                                                                               
         MVC   PERSTART,PEREND                                                  
         MVI   PERSTART+2,X'16'    SET START DATE FOR NEXT PERIOD               
         B     SM60                                                             
*                                                                               
SM50     MVC   PERSTART,PEREND                                                  
         MVI   PERSTART+2,X'16'                                                 
         MVC   WORK(L'PERSTART),PERSTART     GET LAST DAY OF MONTH              
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+5)    C'YYMMDD'                     
         GOTO1 ADDAY,DMCB,(C'Y',WORK+5),(X'80',WORK+10),0                       
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,PEREND)                               
         MVC   PERYRMON(2),SVSTMO                                               
         BAS   RE,ADDPER           ADD PERIOD ELEMENT                           
*                                                                               
* GET READY FOR NEXT PERIOD                                                     
*                                                                               
         XC    WORK,WORK           NOW NEED START OF NEXT MONTH                 
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)    C'MM/DD/YY'                
         MVC   WORK+3(2),=C'01'           ADD 1 MONTH TO 1ST                    
         MVC   WORK+8(4),=C'(1M)'      TO GET 1ST OF NEXT MONTH                 
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
*                                                                               
SM60     ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
         GOTO1 =A(BUMPMO),DMCB,RR=RELO      BUMP MOA TO NEXT MONTH              
         MVC   PERYRMON(2),SVSTMO                                               
         CLI   PERNUM,25                                                        
         BE    SMX                                                              
         B     SM10                                                             
*                                                                               
SMX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH MONTHLY PERIODS                                      
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
***********************************************************************         
*                                                                               
MONTHIT  NTR1                                                                   
         MVI   PERNUM,X'01'        START WITH PERIOD NUMBER 1                   
         MVC   PERYRMON(2),SVSTMO                                               
         MVC   PERSTART,STDATE        FIRST START DATE                          
         LA    R4,12               ONLY HAVE 12 MONTHS                          
*                                                                               
         USING PERVALD,R3                                                       
*                                                                               
         DS    0H                  GETS END OF FIRST MONTH                      
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)                               
         MVC   WORK+8(5),=C'-(1M)'   GETS NEXT MONTH, X'10' PREV DAY            
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'10'          MINUS 1 DAY                                  
         GOTO1 PERVAL,DMCB,(13,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PEREND,PVALPEND                                                  
*                                                                               
MON10    DS    0H                                                               
         BAS   RE,ADDPER           ADD ELEMENT                                  
*                                                                               
MON20    DS    0H                  GETS BEGINNING OF NEXT MONTH                 
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK)                               
         MVC   WORK+8(5),=C'(1M)-'       ADD 1 MONTH TO START                   
         GOTO1 DATCON,DMCB,(1,PERSTART),(11,WORK+13)                            
         MVC   WORK+21(4),=C'(2M)'       ADD 2 MONTH - 1 DAY TO START           
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'10'          MINUS 1 DAY                                  
         GOTO1 PERVAL,DMCB,(25,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   PERSTART,PVALPSTA                                                
         MVC   PEREND,PVALPEND                                                  
*                                                                               
         MVC   PERYRMON(2),PEREND         YEAR AND MONTH                        
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
         BCT   R4,MON10            WILL MAKE 12 ELEMENTS                        
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD WITH BROADCAST MONTH  PERIODS                             
*        PACKED START DATE IN STDATE AND END DATE IN ENDATE                     
***********************************************************************         
*                                                                               
BROADIT  NTR1                                                                   
         MVI   PERNUM,X'01'        START WITH PERIOD NUMBER 1                   
         LA    R4,12               ONLY HAVE 12 MONTHS                          
         MVC   DUMDATE(2),SVSTMO                                                
         MVI   DUMDATE+2,X'15'     ALWAYS PASS GETBROAD THE 15TH                
*                                                                               
         USING PERVALD,R3                                                       
*                                                                               
BR10     DS    0H                                                               
         XC    EBDATE,EBDATE       GET EBCIDIC MMDDYY FOR GETBROAD              
         GOTO1 DATCON,DMCB,(1,DUMDATE),(0,EBDATE)                               
         XC    WORK,WORK                                                        
         GOTO1 VGTBROAD,DMCB,(X'01',EBDATE),WORK,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,PERSTART)   GET PWOS                     
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,PEREND)                                
*                                                                               
         CLC   PERSTART(2),DUMDATE                                              
         BE    BR15                                                             
         CLC   PERSTART(2),SVSTMO                                               
         BE    BR15                                                             
         GOTO1 =A(BUMPMO),DMCB,RR=RELO                                          
BR15     MVC   PERYRMON(2),SVSTMO                                               
*                                                                               
         BAS   RE,ADDPER           ADD ELEMENT                                  
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
*                                                                               
BR20     DS    0H                  GET NEXT MONTH'S DUMMY DATE                  
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,DUMDATE),(11,WORK)                                
         MVC   WORK+8(4),=C'(1M)'        ADD 1 MONTH TO START                   
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'10'          MINUS 1 DAY                                  
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,PVALBLK)                             
         LA    R3,PVALBLK                                                       
         MVC   DUMDATE,PVALPSTA                                                 
*                                                                               
         BCT   R4,BR10             WILL MAKE 12 ELEMENTS                        
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD TIMESHEET PERIOD ELEMENT                                           
*        PERSTART=START DATE : PEREND=END DATE : PERNUM=PERIOD NUMBER           
*        PERYRMON = PERIOD MONTH AND YEAR                                       
***********************************************************************         
*                                                                               
ADDPER   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TMPELD,R6                                                        
         MVI   TMPEL,TMPELQ                                                     
         MVI   TMPLN,TMPLNQ                                                     
         MVC   TMPMTH,PERYRMON                                                  
         MVC   TMPNUMB,PERNUM                                                   
         MVC   TMPSTART,PERSTART                                                
         MVC   TMPEND,PEREND                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        MAKEPER - ADDS TIMESHEET PERIOD ELEM FROM SCREEN                       
***********************************************************************         
*                                                                               
MAKEPER  NTR1                                                                   
         USING DSPLINED,R2                                                      
         ST    R2,ALSTPER                                                       
         NI    BITS,X'FF'-ADDPERD                                               
*----------------------------------------------------------------------         
*        GET PERIOD NUMBER FROM SCREEN                                          
*----------------------------------------------------------------------         
*                                                                               
         OC    DSPNUM,DSPNUM       ANY PERIOD NUMBER                            
         BNZ   MPA                 NO MORE                                      
         OC    DSPSTDT,DSPSTDT     ANY PERIOD START DATE                        
         BZ    XNO                                                              
         MVI   PERNUM,X'FF'        MUST BE ADDING A NEW PERIOD                  
         B     MPB                 MOVE HIGHEST PERIOD # FOR NOW                
*                                                                               
MPA      MVI   DSPNUMH+5,X'02'                                                  
         GOTO1 SCANNER,DMCB,DSPNUMH,(1,SCANBLK)                                 
         CLI   DMCB+4,0                                                         
         BE    EINVOPT                                                          
         MVC   PERNUM,SCANBLK+7  FIRST PERIOD ON SCREEN                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CHECK RECORD FOR PERIOD                                                
*----------------------------------------------------------------------         
*                                                                               
MPB      L     R6,AIO                                                           
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MP1NX    BAS   RE,NEXTEL                                                        
         BNE   MPC                                                              
*                                                                               
         CLC   TMPNUMB,PERNUM MATCH                                             
         BE    MP2                                                              
         BH    MPC                                                              
         MVC   LASTEND,TMPEND                                                   
         MVC   PERYRMON,TMPMTH                                                  
         MVC   WORK(2),TMPMTH                                                   
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(11,WORK+10)                                
         MVC   LSTMONYR(3),WORK+10                                              
         MVC   LSTMONYR+3(3),WORK+15                                            
         NI    BITS,X'FF'-FIRST                                                 
         MVC   SVPERNUM,TMPNUMB                                                 
         B     MP1NX                                                            
*                                                                               
MP2      ST    R6,ADDRELEM         SAVE ADDRESS OF THIS ELEMENT                 
         OC    DSPSTDT(11),DSPSTDT NO DATES                                     
         BNZ   MP3                                                              
         MVI   0(R6),X'FF'         REMOVE ELEM                                  
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
MP3      MVI   ELCODE,TMPELQ       RESET                                        
         OC    DSPSTDT(11),DSPSTDT                                              
         BZ    XYES                NOTHING ENTERED                              
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD NEW PERIOD - VALIDATE MONTH                                        
*----------------------------------------------------------------------         
*                                                                               
MPC      CLI   PERNUM,X'FF'                                                     
         BNE   MP10                                                             
         ZIC   R1,SVPERNUM                                                      
         LA    R1,1(R1)                                                         
         STC   R1,PERNUM                                                        
         OI    BITS,ADDPERD        BIT TO ADD PERIOD                            
*                                                                               
         USING TMPELD,R6                                                        
         USING PERVALD,R3                                                       
MP10     LA    R3,PVALBLK          GET PACKED MONTH ENTERED                     
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TMPEL,TMPELQ                                                     
         MVI   TMPLN,TMPLNQ                                                     
*                                                                               
         CLC   DSPMON,SPACES       NEW MONTH                                    
         BNH   MP50                                                             
*                                                                               
         XC    PVALBLK,PVALBLK     GET PACKED MONTH ENTERED                     
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'20'                                                       
         GOTO1 PERVAL,DMCB,(DSPMONH+5,DSPMON),(BYTE,PVALBLK)                    
         CLI   DMCB+4,X'01'                                                     
         BE    EINVDATE                                                         
         CLI   DMCB+4,X'03'                                                     
         BE    EINVDATE                                                         
         MVC   SAVEPMON,PVALPSTA+1                                              
*                                                                               
MP20     CLC   SAVEPMON,PERYRMON+1 SAME AS LAST IS OK                           
         BE    MP50                                                             
         OC    LSTMONYR,LSTMONYR   PREVIOUS MONTH EXIST                         
         BNZ   MP24                                                             
         MVC   PERYRMON+1(1),SAVEPMON   ALREADY HAS YEAR                        
         B     MP50                                                             
*                                                                               
MP24     XC    WORK,WORK                                                        
         MVC   WORK(6),LSTMONYR                                                 
         MVC   WORK+6(5),=C'-(1M)'                                              
         XC    PVALBLK,PVALBLK                                                  
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(11,WORK),(BYTE,PVALBLK)                             
         CLC   SAVEPMON,PVALPEND+1                                              
         BNE   EINVMON                                                          
         MVC   PERYRMON,PVALPEND      ENTERED NEXT MONTH                        
*                                                                               
MP50     CLI   PERYRMON+1,X'00'       BETTER HAVE A MONTH                       
         BE    EINVMON                                                          
         MVC   WORK(2),PERYRMON                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(11,WORK+10)                                
         MVC   LSTMONYR(3),WORK+10                                              
         MVC   LSTMONYR+3(3),WORK+15                                            
         MVC   TMPMTH,PERYRMON                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        VALIDATE PERIOD                                                        
*----------------------------------------------------------------------         
*                                                                               
*                                                                               
         TM    BITS,FIRST          FIRST PERIOD?                                
         BNO   MP80                NO                                           
*                                                                               
         BAS   RE,CHKSTPER                                                      
         XC    PVALBLK,PVALBLK                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(5),DSPSTDT     FIRST PERIOD                                 
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),STYEAR    START YEAR                                   
         MVC   WORK+8(6),DSPHYPH                                                
         MVC   BYTE,LANGCODE                                                    
         LA    R1,14               LENGTH                                       
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'         ASSUME MMYY NOT DDMM                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,PVALBLK)                          
         CLI   DMCB+4,X'00'                                                     
         BNE   EINVDATE                                                         
         OC    TMSTDATE,TMSTDATE   ANY TMS START DATE?                          
         BZ    MP60                                                             
         CLC   TMSTDATE,PVALPSTA                                                
         BNE   MP60                                                             
         CLC   DSPMON,SPACES       MUST BE THE START OF A MONTH                 
         BNH   EINVTMS                                                          
MP60     MVC   TMPSTART,PVALPSTA                                                
         MVC   TMPEND,PVALPEND                                                  
         B     MP100                                                            
*                                                                               
MP80     XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,LASTEND),(11,WORK)                                
         MVC   WORK+8(4),=C'(1D)'        ADDS ONE DAY TO LAST END DATE          
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(4),=C'(1T)'        ADDS ONE DAY TO LAST END DATE          
         MVC   BYTE,LANGCODE                                                    
         LA    R1,12               LENGTH                                       
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'         ASSUME MMYY NOT DDMM                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,PVALBLK)    NEW START             
         GOTO1 DATCON,DMCB,(0,PVALESTA),(X'20',PVALESTA)  CONV FOR Y2K          
*        CLI   PVALESTA,C'9'                                                    
*        BNH   MP82                                                             
*        SR    R1,R1                                                            
*        IC    R1,PVALESTA                                                      
*        SH    R1,=H'10'                                                        
*        STC   R1,PVALESTA                                                      
MP82     MVC   TMPSTART,PVALPSTA                                                
*                                                                               
         MVC   WORK(5),DSPSTDT                                                  
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),PVALESTA  CHARACTER YEAR                               
         MVC   WORK+8(6),DSPHYPH   END DATE                                     
         MVC   BYTE,LANGCODE                                                    
         LA    R1,14               LENGTH                                       
         STC   R1,BYTE2                                                         
         OI    BYTE2,X'40'         ASSUME MMYY NOT DDMM                         
         GOTO1 PERVAL,DMCB,(BYTE2,WORK),(BYTE,PVALBLK)                          
         CLI   DMCB+4,X'00'                                                     
         BNE   EINVDATE                                                         
         CLC   TMPSTART,PVALPSTA                                                
         BNE   ENOTCONT            PERIODS NOT CONTIGUOUS                       
         OC    TMSTDATE,TMSTDATE   ANY TMS START DATE?                          
         BZ    MP90                                                             
         CLC   TMSTDATE,PVALPSTA                                                
         BNE   MP90                                                             
         CLC   DSPMON,SPACES       MUST BE THE START OF A MONTH                 
         BNH   EINVTMS                                                          
         CLC   PREVPMON,SAVEPMON   MUST BE THE START OF A MONTH                 
         BE    EINVTMS                                                          
MP90     MVC   TMPSTART,PVALPSTA                                                
         MVC   TMPEND,PVALPEND                                                  
*                                                                               
MP100    MVC   LASTEND,TMPEND                                                   
         NI    BITS,X'FF'-FIRST                                                 
         MVC   PREVPMON,SAVEPMON                                                
         MVC   TMPNUMB,PERNUM                                                   
         TM    BITS,ADDPERD        ADDING A PERIOD?                             
         BO    MP110                                                            
         L     R6,ADDRELEM         POINT TO WHERE ELEM SHOULD BE ADDED          
         MVC   0(TMPLNQ,R6),ELEM                                                
         B     MPX                                                              
MP110    GOTO1 ADDELEM                                                          
MPX      B     XYES                                                             
         DROP  R6,R3,R2                                                         
         EJECT                                                                  
***********************************************************************         
*        SEE IF STARTING PERIOD BEGINS IN PREVIOUS YEAR                         
***********************************************************************         
CHKSTPER NTR1                                                                   
*                                                                               
*** ONLY CALENDARS BEGINNING IN JANUARY ARE VALID                               
*                                                                               
         USING DSPLINED,R2                                                      
         USING MONTABD,R3                                                       
         USING PERVALD,R4                                                       
         LA    R2,CADCOL1H                                                      
         LA    R3,MONTAB                                                        
         LA    R4,BLOCK                                                         
         CLC   STDATE+1(1),MONPACK IS IT JAN?                                   
         BNE   CHKSTPRX                                                         
*                                                                               
         CLC   ESTDATE+2(2),DSPSTDT    IS THE PER STRT DATE >                   
         BNL   CHKSTPRX                THAN THE START MONTH                     
*                                                                               
         XC    BLOCK(L'PVALOUTB),BLOCK   YES, SO MUST START FROM                
         XC    WORK,WORK                 PREV YEAR                              
         MVC   WORK(L'STYEAR),STYEAR                                            
         MVC   WORK+L'STYEAR(6),=C'(-12M)'                                      
         GOTO1 PERVAL,DMCB,(8,WORK),(X'02',BLOCK)                               
         MVC   STYEAR,PVALESTA                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),STYEAR      CONVERT STYEAR FOR Y2K                       
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   STYEAR,WORK                                                      
*                                                                               
         XC    WORK,WORK           OVERWRITE THE OLD START DATE                 
         MVC   WORK(5),DSPSTDT                                                  
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6,STYEAR                                                    
         GOTO1 DATCON,DMCB,(4,WORK),(1,STDATE)                                  
*                                                                               
CHKSTPRX XIT1                                                                   
         DROP  R2,R3,R4                                                         
*                                                                               
***********************************************************************         
*        GET START AND END DATES FROM REC IN AIO                                
***********************************************************************         
*                                                                               
GETRANGE NTR1                                                                   
         XC    SVSTART,SVSTART                                                  
         XC    SVEND,SVEND                                                      
*                                                                               
         L     R6,AIO                                                           
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         BNE   GRX                 NEED TO CHANGE RECORD                        
         MVC   SVSTART,TMPSTART                                                 
         MVC   SVEND,TMPEND                                                     
GR10NX   BAS   RE,NEXTEL                                                        
         BNE   GRX                                                              
         MVC   SVEND,TMPEND                                                     
         B     GR10NX                                                           
*                                                                               
GRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDRULE - ADDS EMPTY TIMESHEET RULES ELEMENT (X'87') TO AIO            
***********************************************************************         
*                                                                               
ADDRULE  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING TMRELD,R6                                                        
         MVI   TMREL,TMRELQ                                                     
         MVI   TMRLN,TMRLNQ                                                     
         MVC   TMRSTART,STDATE     FILL IN DATES IN ELEM                        
         MVC   TMREND,ENDATE                                                    
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        BUMP9  -  BUMPS 9 FIELDS, TO GET TO NEXT IN COLUMN                     
*                  R2 SHOULD BE POINTING TO FIRST HEADER IN COLUMN              
***********************************************************************         
*                                                                               
BUMP9    NTR1                                                                   
         LA    R4,9                                                             
BUMPLP   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,BUMPLP                                                        
         XIT1  REGS=(R2)           RETURN R2                                    
         EJECT                                                                  
***********************************************************************         
*        DISPKEY - DISPLAY KEY                                                  
***********************************************************************         
*                                                                               
DK       NTR1                                                                   
*        CLI   ACTEQU,ACTSEL       ACTION SELECT?                               
*        BNE   DK50                                                             
*        CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED?                         
*        BNE   DK50                NO - DONT LOAD CHANGE SCREEN                 
*        GOTO1 CALLOV,DMCB,(X'FC',CONTAGH),0                                    
*        CLI   DMCB+4,X'FF'                                                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        CLI   ACTEQU,ACTSEL                                                    
*        BNE   DK50                                                             
*        CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED?                         
*        BNE   DK50                                                             
*        GOTO1 =A(UNPROT),DMCB,RR=RELO THEN UNPROTECT SOME FIELDS               
*                                                                               
*        LA    R2,CONOPTH                                                       
*        CLC   =C'PW=RADNELAC',8(R2)    CHANGE PASSWORD USED?                   
*        BE    DK50                                                             
*        LA    R2,CONACTH                                                       
*        B     EINVACT                                                          
*                                                                               
         USING CASRECD,R2                                                       
DK50     MVC   SAVEKEY,BIGKEY                                                   
         LA    R2,BIGKEY                                                        
         MVC   WORK(2),CASKEMOA                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(10,WORK+4)                                 
         MVC   CADYR,WORK+10       YEAR                                         
         MVC   CADOFC,CASKOFC      OFFICE                                       
         OI    CADYRH+6,X'80'                                                   
         MVI   CADYRH+5,2                                                       
         OI    CADOFCH+6,X'80'                                                  
         GOTO1 GETLDG,DMCB,C'1R'                                                
         MVC   LEVELLN,ABCDLEN     GET 1R LEVEL LENGTHS                         
         MVC   CADOFCH+5(1),LEVELLN                                             
         BAS   RE,VK                                                            
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPREC - DISPLAY RECORD                                               
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
*&&US                                                                           
         CLI   ACTEQU,ACTDIS       ACTION DISPLAY                               
         BE    DR00                                                             
         CLI   ACTEQU,ACTSEL                                                    
         BNE   DR00A                                                            
         CLC   THISLSEL,AC@CHAU    WAS CHANGE SELECTED FROM LIST?               
         BE    DR00A                                                            
DR00     BAS   RE,PROTECT          PROTECT FIELDS                               
         B     DR00B                                                            
*&&                                                                             
DR00A    GOTO1 =A(UNPROT),DMCB,RR=RELO THEN UNPROTECT SOME FIELDS               
DR00B    XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         MVI   DMINBTS,X'00'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    DR01                                                             
         CLI   ACTEQU,ACTADD                                                    
         BNE   ERRNOREC            RECORD DOESN'T EXISTS                        
*                                                                               
DR01     GOTO1 GETREC                                                           
*                                                                               
DR02     GOTO1 =A(CLRSCRN),DMCB,RR=RELO    CLEAR SCREEN                         
*                                                                               
*        DISPLAY RULES                                                          
*                                                                               
         L     R6,AIO                                                           
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PREVSTRT,TMRSTART   SAVE PREV FOR PASSIVE KEY UPDATES            
         MVC   PREVEND,TMREND                                                   
         XC    YYMMDD2,YYMMDD2     FIELD FOR HIGHEST DATE                       
         TM    COMPYST5,CPYSNCST                                                
         BNZ   DR04                                                             
         GOTO1 =A(DRAFTRNS),DMCB,RR=RELO                                        
*                                  PERIOD                                       
DR04     GOTO1 DATCON,DMCB,(1,TMRSTART),(17,CADPER)                             
         GOTO1 DATCON,DMCB,(1,TMREND),(17,CADPER+9)                             
         MVI   CADPER+8,C'-'                                                    
         OI    CADPERH+6,X'80'     XMIT                                         
*                                                                               
*                                  PERIOD DEFINITION                            
         MVC   CADDEF,SPACES                                                    
         OI    CADDEFH+6,X'80'                                                  
         LA    R3,DEFTABLE         TABLE OF VALID PERIOD DEFINITIONS            
         USING DEFTBD,R3                                                        
*                                                                               
DR10     DS    0H                                                               
         CLC   DEFBYTE,TMRDEF                                                   
         BE    DR20                GOT A MATCH                                  
         LA    R3,DEFTBLN(R3)                                                   
         CLI   0(R3),EOT                                                        
         BNE   DR10                                                             
         B     DR30                                                             
*                                                                               
DR20     DS    0H                                                               
         MVC   CADDEF(L'DEFDESCR),DEFDESCR                                      
         MVC   SVTIMDEF,CADDEF                                                  
         MVC   SVTMRDEF,DEFBYTE                                                 
         TM    BITS3,TIMEXST       HAVE WE FOUND ANY TIME                       
         BZ    DR21                NO                                           
         OI    CADDEFH+1,X'20'     YES - PROTECT PERIOD DEFINITION              
DR21     CLI   DEFDAY,X'01'        END DAY NEEDED                               
         BNE   DR30                                                             
         DROP  R3                                                               
*                                  DISPLAY END DAY                              
         MVC   CADDAY,SPACES                                                    
         OI    CADDAYH+6,X'80'                                                  
         LA    R3,DAYTABLE                                                      
         USING DAYTBD,R3                                                        
*                                                                               
DR23     CLC   DAYBYTE,TMRPEND                                                  
         BE    DR25                                                             
         LA    R3,DAYTBLN(R3)                                                   
         CLI   0(R3),EOT                                                        
         BNE   DR23                                                             
         B     DR30                                                             
*                                                                               
DR25     LA    R1,DICLIST                                                       
         ZICM  R0,DAYDCODE,2       DISP TO DAY CODE                             
         AR    R1,R0                                                            
         MVC   CADDAY(3),0(R1)                                                  
         TM    BITS3,TIMEXST       HAVE WE FOUND ANY TIME                       
         BZ    DR30                NO                                           
         OI    CADDAYH+1,X'20'     YES - PROTECT END DAY                        
         DROP  R3,R6                                                            
*                                                                               
DR30     DS    0H                  DISPLAY PERIOD ELEMENTS                      
*                                                                               
         XC    SVMONTH,SVMONTH                                                  
*                                                                               
         USING TMPELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
*                                                                               
         MVC   STPERNUM,SAPERNUM   GET LAST DISPLAYED                           
         CLI   PFKEY,0                                                          
         BE    DR33                                                             
         MVI   STPERNUM,1          DEFAULT TO FIRST                             
         MVI   SAPERNUM,1          DEFAULT TO FIRST                             
         CLI   PFKEY,7             PAGE UP = START AT BEGINNING                 
         BE    DR36                                                             
         CLI   PFKEY,8             PAGE DOWN START WITH PERNUM                  
         BNE   DR33                DISPLAY SAME SCREEN                          
*                                                                               
DR32     MVC   STPERNUM,PERNUM     GET LAST DISPLAYED +1                        
*                                                                               
* PERIOD # 51 (X'33') IS THE LAST TO FIT ON THE 1ST PAGE.  IF EXISTS            
* WANT TO START WITH THIS PERIOD # TO ALLOW ADDT'L PERIODS TO BE ADDED          
* TO A 2ND PAGE.                                                                
*                                                                               
         MVC   SAPERNUM,STPERNUM                                                
         CLI   PERNUM,X'33'        PERIOD NUMBER 51 IS THE LAST TO              
         BE    DR33                FIT ON THE 1ST PAGE                          
         ZIC   R1,STPERNUM                                                      
         LA    R1,1(R1)                                                         
         STC   R1,STPERNUM                                                      
         MVC   SAPERNUM,STPERNUM                                                
*                                                                               
DR33     BAS   RE,GETEL                                                         
         B     DR34                                                             
DR34NX   BAS   RE,NEXTEL                                                        
DR34     BNE   DR36                                                             
         CLC   TMPNUMB,STPERNUM                                                 
         BNE   DR34NX                                                           
         B     DR40                                                             
*                                                                               
DR36     L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
DR40     DS    0H                  DISPLAY ELEMENTS                             
         MVC   STPERNUM,TMPNUMB    SAVE START PERIOD                            
         LA    R2,CADCOL1H         COLUMN 1 HEADER                              
         USING DSPLINED,R2                                                      
*                                                                               
DR45     DS    0H                                                               
         BAS   RE,DISPPER          DISPLAY NEXT ELEM                            
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DR65                                                             
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND1H                                                      
         CR    R2,R1                                                            
         BNH   DR45                                                             
*                                                                               
         LA    R2,CADCOL2H         COLUMN 2 HEADER                              
DR50     BAS   RE,DISPPER          DISP TIMESHEET PERIOD ELEM                   
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DR65                                                             
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND2H                                                      
         CR    R2,R1                                                            
         BNH   DR50                                                             
*                                                                               
         LA    R2,CADCOL3H         COLUMN 3 HEADER                              
DR55     BAS   RE,DISPPER          DISP TIMESHEET PERIOD ELEM                   
         BAS   RE,NEXTEL           NEXT ELEM                                    
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DR65                                                             
         BAS   RE,BUMP9            NEXT IN COLUM                                
         LA    R1,CADEND3H                                                      
         CR    R2,R1                                                            
         BNH   DR55                                                             
*                                                                               
DR65     TM    BITS3,TIMEXST       HAVE WE FOUND ANY TIME                       
         BZ    DRX                 NO                                           
         OI    CADDEFH+1,X'20'     YES - PROTECT PERIOD DEFINITION              
         OI    CADDAYH+1,X'20'     AND PROTECT DAY WEEK ENDS                    
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PROTECT SOME FIELDS                                                    
***********************************************************************         
*                                                                               
PROTECT  NTR1                                                                   
         LA    R2,CADDEFH          PERIOD DEFINITION FIELD                      
         OI    1(R2),X'20'                                                      
         LA    R2,CADDAYH          DAY PEIOD ENDS FIELD                         
         OI    1(R2),X'20'                                                      
*                                                                               
         LA    R2,CADCOL1H         1ST LINE                                     
         LA    R3,CADPFKYH         PF KEY                                       
*                                                                               
PROT10   OI    1(R2),X'20'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BL    PROT10                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPPER - DISPLAYS TIMESHEET PERIOD ELEM                               
***********************************************************************         
*                                                                               
DISPPER  NTR1                                                                   
         USING DSPLINED,R2         R2 = SCREEN LINE                             
         USING TMPELD,R6           R6 = ELEMENT                                 
*                                                                               
DP10     DS    0H                                                               
         CLC   SVMONTH,TMPMTH+1                                                 
         BE    DP20                IF SAME MONTH DON'T DISPLAY                  
*                                                                               
         USING MONTABD,R3                                                       
         LA    R3,MONTAB           TRANSLATE TABLE                              
DP12     CLC   TMPMTH+1(1),MONPACK  MATCH ON PACKED MONTH                       
         BE    DP14                                                             
         LA    R3,MONTABLN(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   DP12                                                             
         DC    H'0'                                                             
DP14     MVC   DSPMON(3),MONCHAR      DISPLAYS MONTH                            
         MVI   DSPMONH+5,3                                                      
         DROP  R3                                                               
*                                                                               
DP20     DS    0H                  DISPALY PERIOD                               
         MVI   BYTE,10                                                          
         CLI   LANGCODE,3                                                       
         BNE   *+8                                                              
         MVI   BYTE,17                                                          
         GOTO1 DATCON,DMCB,(1,TMPSTART),(BYTE,WORK)                             
         MVC   DSPSTDT,WORK                                                     
         MVI   DSPHYPH,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,TMPEND),(BYTE,WORK)                               
         MVC   DSPENDT,WORK                                                     
*                                                                               
         MVC   SVMONTH,TMPMTH+1    SAVE MONTH                                   
*                                                                               
         EDIT  (B1,TMPNUMB),(2,DSPNUM),FILL=0                                   
         MVC   PERNUM,TMPNUMB                                                   
         OI    DSPNUMH+6,X'80'                                                  
         OI    DSPPERH+6,X'80'     XMIT                                         
         OI    DSPMONH+6,X'80'                                                  
*                            LOOK FOR ANY TDT PASSIVES FOR THIS PERIOD          
         XC    KEY2,KEY2                                                        
         LA    R3,KEY2                                                          
         USING TDTPASD,R3                                                       
         XC    TDTPAS,TDTPAS                                                    
         MVI   TDTPTYP,TDTPTYPQ                                                 
         MVI   TDTPSUB,TDTPSUBQ                                                 
         MVC   TDTPCPY,CMPY                                                     
         SR    R1,R1                                                            
         ICM   R1,7,TMPEND                                                      
         LNR   R1,R1                                                            
         STCM  R1,7,TDTPPEDT       PERIOD END DATE, 2'S COMP.                   
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(TDTPPEDT-TDTPAS),KEYSAVE                                    
         BNE   DPX                 NO (MORE) TIME DATE PASSIVES                 
         SR    R1,R1                                                            
         ICM   R1,7,TMPSTART                                                    
         LNR   R1,R1                                                            
         CLM   R1,7,TDTPPEDT       TEST TIME POSTED WITHIN THIS PERIOD          
         BH    DP30                YES, PROTECT FIELDS                          
         TM    COMPYST5,CPYSNCST   NO, TEST AGENCY IS ON NEW COST               
         BNZ   DPX                 YES, FINISHED                                
         OC    YYMMDD2,YYMMDD2     ELSE TEST ANY OLD-STYLE DRAFT TIME           
         BZ    DPX                                                              
         CLC   TMPEND,YYMMDD2      FOR THE PERIOD IN QUESTION                   
         BH    DPX                                                              
DP30     OI    DSPMONH+1,X'20'     YES - SET FIELDS PROTECTED                   
         OI    DSPPERH+1,X'20'                                                  
         OI    BITS3,TIMEXST                                                    
*                                                                               
DPX      B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        CHANGE PASSIVE POINTER                                                 
***********************************************************************         
*                                                                               
XP       NTR1                                                                   
         OC    PREVSTRT,PREVSTRT   ANYTHING TO DELETE                           
         BNZ   XP1                 NO GO ADD                                    
         BAS   RE,XA                                                            
         B     XPX                                                              
*                                                                               
XP1      CLC   STDATE,PREVSTRT     IF START/END DATES ARE SAME                  
         BNE   XP10                DON'T NEED TO CHANGE POINTER                 
         CLC   ENDATE,PREVEND                                                   
         BE    XPX                                                              
*                                                                               
XP10     DS    0H                  DELETE OLD POINTER                           
         LA    R6,BIGKEY                                                        
         USING CASRECD,R6                                                       
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,CMPY                                                     
         MVC   CASPEDTE,PREVEND                                                 
         MVC   CASPSDTE,PREVSTRT                                                
         MVC   CASPOFC,OFFICE                                                   
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',BIGKEY,BIGKEY         
         CLI   DMCB+8,0                                                         
         BE    XP20                IF THERE DELETE THEN ADD NEW POINTER         
         BAS   RE,XA                                                            
         B     XPX                                                              
*                                                                               
XP20     DS    0H                                                               
         OI    CASPSTAT,X'80'      DELETE IT                                    
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    XP30                                                             
         DC    H'0'                                                             
XP30     BAS   RE,XA                                                            
*                                                                               
XPX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD PASSIVE POINTER                                                    
***********************************************************************         
*                                                                               
XA       NTR1                                                                   
         USING CASRECD,R6                                                       
*                                                                               
         LA    R6,BIGKEY           GET DA OR REC JUST ADDED                     
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',BIGKEY,BIGKEY                 
         CLI   DMCB+8,0                                                         
         BNE   XAX                                                              
         MVC   DISKADDR,CASKDA                                                  
*                                                                               
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,CMPY                                                     
         MVC   CASPEDTE,ENDATE                                                  
         MVC   CASPSDTE,STDATE                                                  
         MVC   CASPOFC,OFFICE                                                   
         MVC   KEY2,BIGKEY         SAVE TO ADD                                  
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',BIGKEY,BIGKEY         
         CLI   DMCB+8,0            NO ERRORS, THEN KEY ALREADY THERE            
         BE    XAX                                                              
         CLI   DMCB+8,X'02'        DELETED                                      
         BNE   XA20                NO, GO ADD                                   
         NI    CASPSTAT,X'FF'-X'80' UNDELETE                                    
         MVC   CASPDA,DISKADDR                                                  
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    XAX                                                              
         DC    H'0'                                                             
*                                                                               
XA20     DS    0H                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEY2                                            
         MVC   CASPDA,DISKADDR                                                  
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    XAX                                                              
         DC    H'0'                                                             
*                                                                               
XAX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                           
***********************************************************************         
*                                  GENERAL MESSAGES                             
EINVACT  MVI   GERROR1,12         RECORD/ACTION COMBO INVALID                   
         B     ERRX                                                             
ERRMISS  DS    0H                                                               
         MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRINV   DS    0H                                                               
         MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRNOREC MVI   GERROR1,53         RECORD NOT FOUND                              
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
ERYESREC MVC   GERROR,=AL2(ACERECEX)   RECORD ALREADY EXISTS                    
         B     ACCERRX                                                          
EINVOFF  MVC   GERROR,=AL2(ACEIVOF)                                             
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
EYESSTD  MVC   GERROR,=AL2(ACECALST)                                            
         B     ACCERRX                                                          
EYESEDT  MVC   GERROR,=AL2(ACEEDTHR)    EDIT HOURS EXIST                        
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVYR   MVC   GERROR,=AL2(ACEINVYR)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVHRS  MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ACCERRX                                                          
EACTHRS  MVC   GERROR,=AL2(ACEACTHR)                                            
         B     ACCERRX                                                          
EQRTHRS  MVC   GERROR,=AL2(ACEQTRHR)                                            
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
EINVMETH MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
EINVLOC  MVC   GERROR,=AL2(ACEIVLOC)                                            
         B     ACCERRX                                                          
ETABDN   MVC   GERROR,=AL2(ACETABDN)                                            
         B     ACCERRX                                                          
EDUPEN   MVC   GERROR,=AL2(ACEDUPEN)                                            
         B     ACCERRX                                                          
EINVAMT  MVC   GERROR,=AL2(ACEAMNT)                                             
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EINVDAY  MVC   GERROR,=AL2(ACEIVDAY)                                            
         B     ACCERRX                                                          
EINVDEF  MVC   GERROR,=AL2(ACEPERDF)                                            
         B     ACCERRX                                                          
EINVMON  MVC   GERROR,=AL2(ACEIVMON)                                            
         B     ACCERRX                                                          
ENOTCONT MVC   GERROR,=AL2(ACEPERCT)                                            
         B     ACCERRX                                                          
EINVTMS  MVC   GERROR,=AL2(ACETMSDT)                                            
         B     ACCERRX                                                          
EINCALOF MVC   GERROR,=AL2(ACECALOF)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE                                                            
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
*                                                                               
*                *** MONTH CODE TABLE ***                                       
MONTAB   DC    CL1'1',X'01'                                                     
DDJAN    DCDD  AC#JAN,3                                                         
         DC    CL1'2',X'02'                                                     
         DCDD  AC#FEB,3                                                         
         DC    CL1'3',X'03'                                                     
         DCDD  AC#MAR,3                                                         
         DC    CL1'4',X'04'                                                     
         DCDD  AC#APR,3                                                         
         DC    CL1'5',X'05'                                                     
         DCDD  AC#MAY,3                                                         
         DC    CL1'6',X'06'                                                     
         DCDD  AC#JUN,3                                                         
         DC    CL1'7',X'07'                                                     
         DCDD  AC#JUL,3                                                         
         DC    CL1'8',X'08'                                                     
         DCDD  AC#AUG,3                                                         
         DC    CL1'9',X'09'                                                     
         DCDD  AC#SEP,3                                                         
         DC    CL1'A',X'10'                                                     
         DCDD  AC#OCT,3                                                         
         DC    CL1'B',X'11'                                                     
         DCDD  AC#NOV,3                                                         
         DC    CL1'C',X'12'                                                     
DDDEC    DCDD  AC#DEC,3                                                         
         DC    AL1(EOT)                                                         
*                                                                               
*                *** PERIOD ENDING TABLE ***                                    
DAYTABLE DC    AL2(AC@DNMSU-DICLIST),AL1(TMRMON),AL1(1)                         
         DC    AL2(AC@DNMSU-DICLIST+3),AL1(TMRTUE),AL1(1)                       
         DC    AL2(AC@DNMSU-DICLIST+6),AL1(TMRWED),AL1(1)                       
         DC    AL2(AC@DNMSU-DICLIST+9),AL1(TMRTHU),AL1(1)                       
         DC    AL2(AC@DNMSU-DICLIST+12),AL1(TMRFRI),AL1(1)                      
         DC    AL2(AC@DNMSU-DICLIST+15),AL1(TMRSAT),AL1(1)                      
DAYDEFLT DC    AL2(AC@DNMSU-DICLIST+18),AL1(TMRSUN),AL1(1)                      
         DC    AL1(EOT)                                                         
*                                                                               
*                *** PERIOD DEFINITION TABLE ***                                
         DS    0F                                                               
DEFTABLE DCDD  AC#CAL1,5           MTH                                          
         DCDD  AC#CAL1,8           MONTHLY                                      
         DC    AL1(TMRMNTH,1,0),A(MONTHIT)                                      
*                                                                               
         DCDD  AC#CAL2,5           WEEK-                                        
         DCDD  AC#CAL2,8           WEEK-MO                                      
         DC    AL1(TMRWKMON,5,1),A(WKMONIT)                                     
*                                                                               
         DCDD  AC#CAL16,5          WEEKM                                        
         DCDD  AC#CAL16,8          WEEKMO                                       
         DC    AL1(TMRWKMON,5,1),A(WKMONIT)                                     
*                                                                               
DEFDEFLT DCDD  AC#CAL3,5           WEEK                                         
         DCDD  AC#CAL3,8           WEEKLY                                       
         DC    AL1(TMRWEEK,2,1),A(WEEKIT)                                       
*                                                                               
         DCDD  AC#CAL4,5           WK-MO                                        
         DCDD  AC#CAL4,8           WEEK-MO                                      
         DC    AL1(TMRWKMON,5,1),A(WKMONIT)                                     
*                                                                               
         DCDD  AC#CAL5,5           HALF                                         
         DCDD  AC#CAL5,8           HALF MTH                                     
         DC    AL1(TMRHMTH,1,0),A(SEMIIT)                                       
*&&US                                                                           
         DCDD  AC#CAL6,5           BCST                                         
         DCDD  AC#CAL6,8           BROADCST                                     
         DC    AL1(TMRBDCST,2,0),A(BROADIT)                                     
*                                                                               
         DCDD  AC#CAL7,5           BROA                                         
         DCDD  AC#CAL7,8           BROADCST                                     
         DC    AL1(TMRBDCST,2,0),A(BROADIT)                                     
*&&                                                                             
         DCDD  AC#CAL8,5           BIWK                                         
         DCDD  AC#CAL8,8           BI-WEEK                                      
         DC    AL1(TMRBIWK,1,1),A(BIWEKIT)                                      
*                                                                               
         DCDD  AC#CAL9,5           BI-WK                                        
         DCDD  AC#CAL9,8           BI-WEEK                                      
         DC    AL1(TMRBIWK,1,1),A(BIWEKIT)                                      
*                                                                               
         DCDD  AC#CAL10,5          BI WK                                        
         DCDD  AC#CAL10,8          BI-WEEK                                      
         DC    AL1(TMRBIWK,1,1),A(BIWEKIT)                                      
*                                                                               
         DCDD  AC#CAL11,5          BIWE                                         
         DCDD  AC#CAL11,8          BI-WEEK                                      
         DC    AL1(TMRBIWK,1,1),A(BIWEKIT)                                      
*                                                                               
         DCDD  AC#CAL12,5          2WKS                                         
         DCDD  AC#CAL12,8          BI-WEEK                                      
         DC    AL1(TMRBIWK,1,1),A(BIWEKIT)                                      
*                                                                               
         DCDD  AC#CAL13,5          SEMI                                         
         DCDD  AC#CAL13,8          HALF MTH                                     
         DC    AL1(TMRHMTH,1,0),A(SEMIIT)                                       
*                                                                               
         DCDD  AC#CAL14,5          1/2                                          
         DCDD  AC#CAL14,8          HALF MTH                                     
         DC    AL1(TMRHMTH,3,0),A(SEMIIT)                                       
*                                                                               
         DCDD  AC#CAL15,5          MONTH                                        
         DCDD  AC#CAL15,8          MONTHLY                                      
         DC    AL1(TMRMNTH,3,0),A(MONTHIT)                                      
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        UNPROTECT SOME FIELDS FOR CHANGE                                       
***********************************************************************         
*                                                                               
UNPROT   NMOD1 0,*UNPROT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         TM    BITS3,TIMEXST                                                    
         BNZ   UNPR02                                                           
*                                                                               
         LA    R2,CADDEFH          PERIOD DEFINITION FIELD                      
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'         XMIT                                         
         LA    R2,CADDAYH          DAY PEIOD ENDS FIELD                         
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
UNPR02   LA    R2,CADCOL1H         1ST LINE                                     
         LA    R3,CADPFKYH         PF KEY                                       
*                                                                               
UNPR10   NI    1(R2),X'FF'-X'20'   MONTH FIELD                                  
         ZIC   R1,0(R2)            BUMP PAST ## FIELD-DO NOT UNPROTECT          
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BNL   UNPRX                                                            
         NI    1(R2),X'FF'-X'20'   PERIOD FIELD                                 
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BL    UNPR10                                                           
*                                                                               
UNPRX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        RE NUMBER PERIODS IN REC IN AIO                                        
***********************************************************************         
*                                                                               
RENUMB   NMOD1 0,*RENUMB*                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         MVI   WORK,X'01'        START WITH 1                                   
         L     R6,AIO                                                           
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ       TIMESHEET PERIODS ELEM                       
         BAS   RE,GETEL                                                         
         B     RN10                NEED TO CHANGE RECORD                        
RN10NX   BAS   RE,NEXTEL                                                        
RN10     BNE   RNX                                                              
         MVC   TMPNUMB,WORK                                                     
         ZIC   R1,WORK                                                          
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         B     RN10NX                                                           
*                                                                               
RNX      XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK IF STANDARD RECS HAVE BEEN SET UP FOR THIS CALENDAR              
***********************************************************************         
*                                                                               
CKSTDHR  NMOD1 0,*CKSTDH*                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         USING STDRECD,R6          BUILD KEY                                    
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVC   STDKEY,SPACES       BUILD STD HOURS RECORD KEY                   
         MVI   STDKTYP,STDKTYPQ    RECORD TYPE                                  
         MVI   STDKSUB,STDKSUBQ    SUB RECORD TYPE                              
         MVC   STDKCPY,CMPY        COMPANY                                      
         MVC   STDKOFC,OFFICE                                                   
         MVI   STDKYR,0            CLEAR OUT NEW YEAR FIELD                     
         XC    STDKSEQ,STDKSEQ     SEQ NUM= X'00'                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2,0                  
         B     CS10                                                             
*                                                                               
CS10SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2,0                  
CS10     CLC   KEY2(STDKDPT-STDKEY),KEYSAVE    SAME THRU OFFICE                 
         BNE   CS50                                                             
*                                                                               
         CLC   STDKSTDT,ENDATE                                                  
         BH    CS10SEQ                                                          
         OC    MUSTSTDT,MUSTSTDT                                                
         BZ    CS18                                                             
         CLC   STDKENDT,MUSTSTDT                                                
         BL    CS10SEQ                                                          
         B     CS20                MATCH, GO CHECK ELEMENTS                     
*                                                                               
CS18     CLC   STDKENDT,STDATE                                                  
         BL    CS10SEQ                                                          
*                                                                               
CS20     MVC   AIO,AIO2            CHECK ELEMENTS                               
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST ',(R2),AIO,WORK               
         MVC   AIO,AIO1                                                         
*                                                                               
         USING SHRELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,SHRELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CSNXT    BAS   RE,NEXTEL                                                        
         BNE   CS50                OKAY                                         
*                                                                               
         CLC   ENDATE(1),SHREND    CLC FOR YEAR ONLY                            
         BH    CSNXT                                                            
         CLC   SHRSTART,ENDATE                                                  
         BH    CSNXT                                                            
         B     EYESSTD             CAN'T CHANGE                                 
*                                                                               
CS50     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK IF EDIT HRS RECS HAVE BEEN SET UP FOR THIS CALENDAR              
***********************************************************************         
*                                                                               
CKEDTHR  NMOD1 0,*CKEDTH*                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         USING EDTRECD,R6          BUILD KEY                                    
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVC   EDTKEY,SPACES       BUILD EDT HOURS RECORD KEY                   
         MVI   EDTKTYP,EDTKTYPQ    RECORD TYPE                                  
         MVI   EDTKSUB,EDTKSUBQ    SUB RECORD TYPE                              
         MVC   EDTKCPY,CMPY        COMPANY                                      
         MVC   EDTKOFC,OFFICE                                                   
         MVI   EDTKYR,0            CLEAR OUT NEW YEAR FIELD                     
         XC    EDTKSEQ,EDTKSEQ     SEQ NUM= X'00'                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2,0                  
         B     CE10                                                             
*                                                                               
CE10SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2,0                  
CE10     CLC   KEY2(EDTKDPT-EDTKEY),KEYSAVE    SAME THRU OFFICE                 
         BNE   CE100                                                            
*                                                                               
         CLI   EDTKKSTA,EDTKSDAY   IS IT DAILT EDIT HOURS                       
         BE    CE10SEQ             YES - DON'T NEED TO WORRY ABOUT THEM         
         CLC   EDTKSTDT,ENDATE                                                  
         BH    CE10SEQ                                                          
         OC    MUSTSTDT,MUSTSTDT                                                
         BZ    CE18                                                             
         CLC   EDTKENDT,MUSTSTDT                                                
         BL    CE10SEQ                                                          
         B     CE20                MATCH, GO CHECK ELEMENTS                     
*                                                                               
CE18     CLC   EDTKENDT,STDATE                                                  
         BL    CE10SEQ                                                          
*                                                                               
CE20     MVC   AIO,AIO2                                                         
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST ',(R2),AIO,WORK               
         MVC   AIO,AIO1                                                         
*                                                                               
         USING SHRELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,SHRELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CENXT    BAS   RE,NEXTEL                                                        
         BNE   CE100               OKAY                                         
*                                                                               
         CLC   ENDATE(1),SHREND    CLC FOR YEAR ONLY                            
         BH    CENXT                                                            
         CLC   SHRSTART,ENDATE                                                  
         BH    CENXT                                                            
         B     EYESEDT             CAN'T CHANGE                                 
*                                                                               
CE100    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET END OF PREVIOUS CALENDAR AND START OF NEXT CALENDAR                
***********************************************************************         
*                                                                               
GTSTENDT NMOD1 0,**STEND*                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         XC    MUSTSTDT,MUSTSTDT                                                
         XC    MUSTENDT,MUSTENDT                                                
         XC    MUSTSTMO,MUSTSTMO                                                
         XC    MUSTENMO,MUSTENMO                                                
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         XC    WORK,WORK                                                        
         MVC   WORK(L'NDYEAR),NDYEAR                                            
         MVC   WORK+L'NDYEAR(6),=C'-(12M)'                                      
         GOTO1 PERVAL,DMCB,(8,WORK),(X'02',BLOCK)                               
         MVC   NXTYEAR,PVALPEND    NEXT YEAR                                    
*                                                                               
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         XC    WORK,WORK                                                        
         MVC   WORK(L'NDYEAR),NDYEAR                                            
         MVC   WORK+L'NDYEAR(6),=C'(-12M)'                                      
         GOTO1 PERVAL,DMCB,(8,WORK),(X'02',BLOCK)                               
         MVC   PREVYEAR,PVALPSTA   PREVIOUS YEAR                                
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         USING CASRECD,R6          BUILD KEY                                    
         LA    R6,BIGKEY                                                        
         XC    CASKEY,CASKEY       BUILD TIMESHEET PERIOD RECORD KEY            
         MVI   CASKTYP,CASKTYPQ    RECORD TYPE                                  
         MVI   CASKSUB,CASKSUBQ    SUB RECORD TYPE                              
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA(1),PREVYEAR   YEAR PACKED                               
         MVI   CASKEMOA+1,X'00'                                                 
         GOTO1 HIGH                                                             
         B     GSE7                                                             
*                                                                               
GSE5     CLC   OFFICE,CASKOFC                                                   
         BE    GSE10                                                            
         GOTO1 SEQ                                                              
GSE7     CLC   KEYSAVE(CASKSMOA-CASKEY-1),BIGKEY                                
         BNE   GSE20                                                            
         B     GSE5                                                             
*                                                                               
GSE10    GOTO1 GETREC              PREVIOUS YEAR'S CALENDAR                     
         L     R6,AIO                                                           
         MVC   MUSTSTMO,CASKEMOA   LAST MONTH IN PREVIOUS CALENDAR              
*                                                                               
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TMREND),(11,WORK)                                 
         MVC   WORK+8(5),=C'-(1D)'                                              
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(5),=C'-(1T)'                                              
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(13,WORK),(BYTE,BLOCK)                               
         GOTO1 DATCON,DMCB,(0,PVALEEND),(X'20',PVALEEND)  CONV FOR Y2K          
         LA    R3,BLOCK                                                         
*                                                                               
GSE18    MVC   MUSTSTDT,PVALPEND                                                
*                                                                               
GSE20    XC    BIGKEY,BIGKEY                                                    
         USING CASRECD,R6          BUILD KEY                                    
         LA    R6,BIGKEY                                                        
         XC    CASKEY,CASKEY       BUILD TIMESHEET PERIOD RECORD KEY            
         MVI   CASKTYP,CASKTYPQ    RECORD TYPE                                  
         MVI   CASKSUB,CASKSUBQ    SUB RECORD TYPE                              
         MVC   CASKCPY,CMPY        COMPANY                                      
         MVC   CASKEMOA(1),NXTYEAR    YEAR PACKED                               
         MVI   CASKEMOA+1,X'00'                                                 
         GOTO1 HIGH                                                             
         B     GSE27                                                            
*                                                                               
GSE25    CLC   OFFICE,CASKOFC                                                   
         BE    GSE30                                                            
         GOTO1 SEQ                                                              
GSE27    CLC   KEYSAVE(CASKSMOA-CASKEY-1),BIGKEY                                
         BNE   GSEX                                                             
         B     GSE25                                                            
*                                                                               
GSE30    GOTO1 GETREC              NEXT YEAR'S CALENDAR                         
         L     R6,AIO                                                           
         MVC   MUSTENMO,CASKSMOA   FIRST MONTH IN NEXT YEAR                     
*                                                                               
         USING TMRELD,R6                                                        
         MVI   ELCODE,TMRELQ       TIMESHEET RULES ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TMRSTART),(11,WORK)                               
         MVC   WORK+8(5),=C'(-1D)'                                              
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(5),=C'(-1T)'                                              
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(13,WORK),(BYTE,BLOCK)                               
         GOTO1 DATCON,DMCB,(0,PVALESTA),(X'20',PVALESTA)  CONV FOR Y2K          
*                                                                               
GSE40    MVC   MUSTENDT,PVALPSTA                                                
*                                                                               
GSEX     XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK TO MAKE SURE STARTING WITH THE RIGHT MONTH                       
*        MUSTSTMO HAS YYMM OF THE PREV YEAR                                     
*        STDATE HAS YYMMDD OF THE START DATE                                    
***********************************************************************         
*                                                                               
CHKMON   NMOD1 0,*CHKMON*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         XC    SVSTMO,SVSTMO                                                    
         OC    MUSTSTMO,MUSTSTMO                                                
         BZ    CHK50                                                            
         OC    STDATE,STDATE                                                    
         BZ    CHK50                                                            
*                                                                               
         CLI   MUSTSTMO+1,X'12'    DOES PREV CAL END IN DEC?                    
         BNE   CHK10                                                            
         MVC   SVSTMO(1),STDATE    THEN CURRENT CAL MUST BEGIN                  
         MVI   SVSTMO+1,X'01'   IN JAN                                          
         B     CHKMX                                                            
*                                                                               
CHK10    XC    WORK,WORK                                                        
         MVC   WORK(L'MUSTSTMO),MUSTSTMO                                        
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(11,WORK+10)                                
         MVC   WORK+18(4),=C'(1M)'                ADD 1 MONTH                   
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK+10),(BYTE,PVALBLK)                          
         LA    R3,PVALBLK                                                       
         USING PERVALD,R3                                                       
         MVC   SVSTMO,PVALPSTA     SAVE START MOA                               
         B     CHKMX                                                            
*                                                                               
CHK50    MVC   SVSTMO,STDATE                                                    
*                                                                               
CHKMX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        BUMP SVSTMO BY 1 FOR MOA                                               
***********************************************************************         
*                                                                               
BUMPMO   NMOD1 0,*BUMPMO*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   WORK+10(2),SVSTMO                                                
         MVI   WORK+12,X'01'                                                    
         GOTO1 DATCON,DMCB,(1,WORK+10),(11,WORK+15)                             
         MVC   WORK+23(4),=C'(1M)'      ADD ONE MONTH                           
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK+15),(BYTE,PVALBLK)                          
         LA    R3,PVALBLK                                                       
         USING PERVALD,R3                                                       
         MVC   SVSTMO,PVALPSTA                                                  
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR ALL UNPROTECTED FIELDS                                           
***********************************************************************         
*                                                                               
CLRSCRN  NMOD1 0,*CLRSCR*                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         LA    R2,CADCOL1H        FIRST FIELD                                   
         LA    R3,CADPFKYH         LAST FIELD                                   
*                                                                               
DRCLR    DS    0H                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   DRCLR05                                                          
         SH    R1,=H'17'           8 FOR HEADER, 1 FOR EX, 8 FOR EXT            
         BNM   *+6                                                              
         DC    H'0'                                                             
         B     DRCLR06                                                          
DRCLR05  SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         BNM   *+6                                                              
         DC    H'0'                                                             
DRCLR06  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DRCLR10  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    DRCLR               NO                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NMOD1 0,*SETUP**                                                       
         L     RC,SAVERC           RC=CONTROLLER STORAGE AREA                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN TO SAME SELECTION                     
         SR    R2,R2                                                            
         LA    R3,CADPFKYH                                                      
         CLI   PFKEY,7                                                          
         BE    SU10                                                             
         CLI   PFKEY,8                                                          
         BE    SU10                                                             
         LA    R2,PFTABLE                                                       
SU10     GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
         TM    SCRSTAT,RECCHG      DID RECORD CHANGE                            
         BNO   *+8                                                              
         MVI   STPERNUM,0          RESET START PERIOD                           
*                                                                               
         USING MONTABD,R3                                                       
         LA    R3,MONTAB           TRANSLATE TABLE                              
         CLI   MONCHAR,X'40'       ALREADY DONE                                 
         BH    SUX                                                              
SU20     GOTO1 DICTATE,DMCB,C'SU  ',MONCHAR,0                                   
         LA    R3,MONTABLN(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   SU20                                                             
         DROP  R3                                                               
*                                                                               
         USING DEFTBD,R3                                                        
         LA    R3,DEFTABLE         TABLE OF VALID PERIOD DEFINITIONS            
         CLI   DEFCODE,X'40'       ALREADY DONE                                 
         BH    SUX                                                              
SU30     GOTO1 DICTATE,DMCB,C'SU  ',DEFCODE,0                                   
         GOTO1 DICTATE,DMCB,C'SU  ',DEFDESCR,0                                  
         LA    R3,DEFTBLN(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   SU30                                                             
         DROP  R3                                                               
*                                                                               
SUX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DRAFT TIME TRANSACTIONS IF USER IS NOT ON NEW TIME           
***********************************************************************         
*                                                                               
         USING TIMRECD,R6                                                       
DRAFTRNS NMOD1 0,*DRTRANS                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   TIMKEY,SPACES       TRANSACTION RECORD                           
         MVC   TIMKCPY,CMPY        COMPANY                                      
         MVI   TIMKUNT,C'1'        UNIT 1                                       
         MVI   TIMKLDG,C'R'        LEDGER R                                     
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     DRF10                                                            
DRFNXT   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
DRF10    CLC   KEYSAVE(TIMKULA-TIMKEY),KEY2                                     
         BNE   DRAFTNO                                                          
         LA    R6,KEY2                                                          
         CLC   TIMKCULC(TIMKPEDT-TIMKULC),SPACES  MUST HAVE                     
         BE    DRFNXT                             CONTRA ACCNT                  
         OC    TIMKPEDT,TIMKPEDT   MUST HAVE A TRANSACTION DATE                 
         BZ    DRFNXT                                                           
         TM    TIMKSTA,X'40'      IS THIS A DRAFT TRANSACTION                   
         BZ    DRFNXT                                                           
*                                                                               
         CLC   SVOFFICE,SPACES     ARE WE LOOK AT AN OFFICE CALENDAR            
         BNE   DRF14               YES                                          
         LA    R2,OFFTAB           NO - MUST BE HIGHEST LEVEL                   
DRF12    CLC   0(L'TRNOFFC,R2),SPACES HAVE WE GOT SOME LOW LEVEL CALS           
         BE    DRF16               NO                                           
         SR    R1,R1               YES - CHECK WHETHER THIS TIME                
         IC    R1,LEVELLN          IS PART OF THIS LOWER CALENDAR               
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIMKACT(0),0(R2)    IS OFFICE SAME AS IN LIST                    
         BE    DRFNXT              YES - GET NEXT TIME RECORD                   
         LA    R2,L'TRNOFFC(R2)    NO - CHECK REST OF LIST                      
         B     DRF12                                                            
                                                                                
DRF14    SR    R1,R1                                                            
         IC    R1,LEVELLN                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIMKACT(0),SVOFFICE DOES OFFICE MATCH OFFICE OF CALENDAR         
         BNE   DRFNXT              NO - GET NEXT TIME RECORD                    
*                                                                               
DRF16    CLC   TIMKPEDT,PREVSTRT                                                
         BL    DRFNXT                                                           
         OC    PREVEND,PREVEND                                                  
         BZ    DRF18                                                            
         CLC   TIMKPEDT,PREVEND                                                 
         BH    DRFNXT                                                           
*                                                                               
DRF18    OI    BITS3,TIMEXST                                                    
         CLC   TIMKPEDT,YYMMDD2    IS THIS THE HIGHEST DATE WE HAVE             
         BNH   DRFNXT              YES                                          
         MVC   YYMMDD2,TIMKPEDT    NO - SAVE THIS NEW HIGHER DATE               
         B     DRFNXT              YES                                          
*                                                                               
DRAFTNO  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
DEFTBD   DSECT                     PERIOD ENDING TABLE                          
DEFCODE  DS    CL5                 3-4 CHAR CODE                                
DEFDESCR DS    CL8                 PERIOD DESCRIPTION                           
DEFBYTE  DS    XL1                 1 BYTE PERIOD CODE FOR TMRELD                
DEFMIN   DS    AL1                 MINIMUM LENGTH FOR COMPARE                   
DEFDAY   DS    AL1                 1= INPUT REQUIRED 0= NOT                     
DEFRTRN  DS    AL4                 A(BUILDING ROUTINE)                          
DEFTBLN  EQU   *-DEFTBD                                                         
*                                                                               
DAYTBD   DSECT                     DAY OF THE WEEK TABLE                        
DAYDCODE DS    XL2                 DISP TO 3 CHAR DAY CODE                      
DAYBYTE  DS    XL1                 1 BYTE PERIOD CODE FOR TMRELD                
DAYMIN   DS    AL1                 MINIMUM LENGTH FOR COMPARE                   
DAYTBLN  EQU   *-DAYTBD                                                         
*                                                                               
TRANSMIT EQU   X'80'                      TRANSMIT BIT                          
EOT      EQU   X'FF'                      END OF TABLE                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
* FAXTRAINF                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPFCD                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SAVERC   DS    F                                                                
BUILDRT  DS    F                   BUILDING ROUTINE                             
ALSTPER  DS    F                   A(LAST PERIOD)                               
DISKADDR DS    F                                                                
ADDRELEM DS    F                   A(ELEM)                                      
TMSTDATE DS    XL3                 TMS START DATE (YYMMDD)                      
STDATE   DS    PL3                 PWOS YEAR AND MONTH                          
ESTDATE  DS    CL6                 C'YYMMDD' START OF PERIOD                    
ENDATE   DS    PL3                 PWOS YEAR AND MONTH                          
ENDDAY   DS    XL1                 PERIOD END DAY                               
PSTMON   DS    PL2                 START MOA                                    
PNDMON   DS    PL2                 END MOA                                      
STMONTH  DS    CL3                 START MOA                                    
PSTMONTH DS    PL1                 PACKED START MONTH                           
NDMONTH  DS    CL3                 END MOA                                      
PERSTART DS    PL3                 PERIOD START                                 
SVPERST  DS    PL3                 SAVED PERIOD START                           
PEREND   DS    PL3                 PERIOD END                                   
PERYRMON DS    PL2                 YYMM FOR TMPEL                               
SAVEPMON DS    XL1                                                              
SVPERNUM DS    XL1                 SAVED PERIOD NUMBER                          
PREVPMON DS    XL1                 PREVIOUS PERIODS MONTH                       
LSTMONYR DS    CL6                 MMM/YY                                       
DUMDATE  DS    PL3                 DUMMY DATE FOR BROADCAST MONTHS              
LASTEND  DS    PL3                 PREVIOUS END DATE FOR VALIDATING             
SVSTART  DS    PL3                                                              
SVEND    DS    PL3                                                              
SVTIMDEF DS    CL8                                                              
SVTMRDEF DS    CL1                                                              
MUSTSTDT DS    PL3                 MUST START CAL ON THIS DATE                  
MUSTSTMO DS    PL2                 MUST START CAL MONTH                         
MUSTENDT DS    PL3                 MUST END CAL ON THIS DATE                    
MUSTENMO DS    PL2                 MUST END CAL MONTH                           
PREVSTRT DS    PL3                 PREVIOUS START DATE                          
PREVEND  DS    PL3                                                              
PERNUM   DS    XL1                 PERIOD NUMBER                                
SAPERNUM DS    XL1                 SAVED PERIOD NUMBER                          
STPERNUM DS    XL1                 START PERIOD NUMBER                          
STDAY    DS    XL1                 START DAY NUMBER                             
SVMONTH  DS    PL1                 SAVED MONTH                                  
STYEAR   DS    CL2                 CHARACTER YEAR                               
NDYEAR   DS    CL2                 CHARACTER YEAR                               
SVNDYEAR DS    CL2                                                              
NXTYEAR  DS    PL1                 NEXT YEAR                                    
PREVYEAR DS    PL1                 PREVIOUS YEAR                                
EBDATE   DS    CL10                EBCIDIC  YYMMDD                              
OFFICE   DS    CL2                 OFFICE                                       
SVOFFICE DS    CL2                                                              
LEVELLN  DS    CL4                                                              
ACCNT    DS    CL12                                                             
SVACTEQU DS    CL1                                                              
BYTE2    DS    XL1                                                              
SVSTMO   DS    XL2                 YYMM-CAL MUST START MONTH                    
YYMMDD   DS    PL3                 WORK AREA FOR PACKED DATE                    
YYMMDD2  DS    PL3                 WORK AREA FOR PACKED DATE                    
*                                                                               
BITS     DS    XL1                                                              
LASTPER  EQU   X'80'                                                            
GUESS    EQU   X'40'                                                            
FIRST    EQU   X'20'               OFF WHEN FIRST TIME THROUGH                  
NEWREC   EQU   X'10'                                                            
CHKSTART EQU   X'08'               CHECK VALID START DATE                       
CHKEND   EQU   X'04'               CHECK VALID END DATE                         
ADDPERD  EQU   X'02'               ADDING A PERIOD START/END                    
*                                                                               
BITS3    DS    XL1                                                              
TIMEXST  EQU   X'80'               TIME EXISTS FOR THIS CALENDER YEAR           
*                                                                               
KEY2     DS    XL54                ACCFILE KEY                                  
SAVEKEY  DS    XL54                                                             
OFFTAB   DS    CL52                                                             
PVALBLK  DS    CL100                                                            
SCANBLK  DS    CL100                                                            
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT - FOR 1 COLUMN            
DSPMONH  DS    CL8                                                              
DSPMON   DS    CL5                 MONTH                                        
DSPNUMH  DS    CL8                                                              
DSPNUM   DS    CL2                 NUMBER                                       
DSPPERH  DS    CL8                                                              
DSPSTDT  DS    CL5                 START OF PERIOD                              
DSPHYPH  DS    CL1                                                              
DSPENDT  DS    CL5                 END OF PERIOD                                
DSPLLEN  EQU   *-DSPLINED                                                       
*                                                                               
*        MONTH CODE TABLE DSCET                                                 
*                                                                               
MONTABD  DSECT                                                                  
MONCODE  DS    CL1                                                              
MONPACK  DS    PL1                                                              
MONCHAR  DS    CL3                                                              
MONTABLN EQU   *-MONTABD                                                        
*                                                                               
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126ACCAP05   01/05/11'                                      
         END                                                                    
