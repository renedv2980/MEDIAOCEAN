*          DATA SET SPREPM802  AT LEVEL 021 AS OF 11/21/19                      
*PHASE SPM802T                                                                  
*INCLUDE SPM8TP                                                                 
*INCLUDE COVAIL                                                                 
         SPACE 1                                                                
*===============================================================*               
* 17MAR98  FIX TO PRINT DAILY GRID FOR DAYS 43-53               *               
*                                                               *               
* 12MAR96  CHANGE TO ACCUMULATE CHILD SPOT PGR AND CLT TOTALS   *               
*          FROM PRODUCT TOTALS BECAUSE TALENT FACTORS NOW       *               
*          APPLIED AT PRODUCT LEVEL                             *               
*===============================================================*               
         TITLE 'MARKET MEDIA PLAN'                                              
SPM802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPM8**,RR=R2                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         LA    R7,2048(R9)                                                      
         LA    R7,2048(R7)                                                      
         USING SPM802+4096,R9,R7                                                
         ST    R2,RELO                                                          
         L     RA,0(R1)            RA/RC FOR WORKD                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         SPACE 1                                                                
         L     R8,BUFFBUFF         ALWAYS POINT TO BUFFER                       
         L     R6,MEDBUFF          R6 FOR BLOCK                                 
         USING MEDBLOCK,R6                                                      
         SPACE 1                                                                
         LA    R1,MYBUFFIO                                                      
         ST    R1,BUFFIO                                                        
         SPACE 1                                                                
         LA    R2,PLANHOOK                                                      
         ST    R2,HEADHOOK                                                      
         STM   R9,RC,SPM8R9                                                     
         ST    R7,SPM8R7                                                        
         CLI   MODE,MKTLAST                                                     
         BL    MP1                                                              
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         SPACE 2                                                                
MP1      CLI   MODE,RUNFRST        RUNFRST ROUTINES                             
         BNE   MP2                                                              
         MVI   ALLOWCWM,C'N'                                                    
         L     R2,=A(LODAREA)                                                   
         A     R2,RELO                                                          
         GOTO1 LOADER,DMCB,=CL8'SPM8CS',(R2),(C'M',(R2))                        
         L     R8,=A(BUFFALOC)     R8 FOR BUFFALO CSECT/DSECT                   
         A     R8,RELO                                                          
         ST    R8,BUFFBUFF                                                      
         GOTO1 =V(COVAIL),DMCB,C'SETB',65000,800000,(R8)                        
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFBUFF,12(R1)     SET BUFFALO AREA                             
         L     R8,BUFFBUFF                                                      
         USING BUFFALOD,R8                                                      
         L     RF,BUFFLALL         AND LENGTH OF IT                             
         SR    RE,RE                                                            
         M     RE,BUFFCRMX                                                      
         ST    RF,LNBUFFER                                                      
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',(R8)                                        
         MVC   MEDNUMWK,=F'75'                                                  
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDSEED,DMCB,(RA)                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   ASPM804,MEDTABLE                                                 
         MVI   MEDDAILY,C'N'                                                    
         MVC   ATHISGP,=A(THISGP)                                               
         MVC   ATHISAD,=A(THISAD)                                               
         MVC   ATHISAP,=A(THISAP)                                               
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE REQFRST CONDITIONS (AT ESTFRST)                           
         SPACE 3                                                                
MP2      CLI   MODE,REQFRST                                                     
         BNE   MP4                                                              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
*                                                                               
         OI    RQOPTS,RQOPTS_1DEC  FORCE 1 DECIMAL RATINGS                      
         XC    PGRTOTS,PGRTOTS                                                  
         XC    CLTTOTS,CLTTOTS                                                  
*                                                                               
         BAS   RE,REQFILT                                                       
         CLI   QDPTDET,C' '        ADJUST REQUESTS FROM PROFILES                
         BNE   MP2B                                                             
         CLI   PROGPROF,0          DAY-PART DETAIL                              
         BE    MP2B                                                             
         MVC   QDPTDET,PROGPROF                                                 
         SPACE 2                                                                
MP2B     CLI   QOPT2,C' '          RECAP OPTION                                 
         BNE   MP2D                                                             
         CLI   PROGPROF+1,0                                                     
         BE    MP2D                                                             
         MVC   QOPT2,PROGPROF+1                                                 
         SPACE 2                                                                
MP2D     CLI   QOPT3,C' '          DATA OPTION                                  
         BNE   MP2F                                                             
         CLI   PROGPROF+2,0                                                     
         BE    MP2F                                                             
         MVC   QOPT3,PROGPROF+2                                                 
         SPACE 2                                                                
MP2F     CLI   QOPT4,C' '          PRODUCT NAME PRINTING                        
         BNE   MP2G                                                             
         CLI   PROGPROF+4,0                                                     
         BE    MP2G                                                             
         MVC   QOPT4,PROGPROF+4                                                 
         SPACE 2                                                                
MP2G     MVC   SPILLOPT,QAREA+49   SUPPRESS SPILL                               
         MVI   QAREA+49,C' '                                                    
         CLI   SPILLOPT,C' '                                                    
         BNE   MP2H                                                             
         CLI   PROGPROF+7,0                                                     
         BE    MP2H                                                             
         MVC   SPILLOPT,PROGPROF+7                                              
         SPACE 2                                                                
MP2H     MVI   ESTSW,C'N'                                                       
         LA    R2,OPTLIST                                                       
         SPACE 2                                                                
MP3      CLC   0(1,R2),QOPT3       CHECK LIST TO SEE IF WE NEED                 
         BE    MP3B                TO READ BUYS, GOALS OR BOTH                  
         CLI   0(R2),X'FF'                                                      
         BE    MP3B                                                             
         LA    R2,8(R2)                                                         
         B     MP3                                                              
         SPACE 2                                                                
MP3B     MVC   FCRDBUYS,1(R2)                                                   
         MVC   FCRDGOAL,2(R2)                                                   
*                                                                               
MP3C     MVI   TPWRITE,C'N'                                                     
         CLC   =C'JW',AGY          TEST FOR JWT TAPE WRITE OPTION               
         BNE   XIT                                                              
         CLC   =C'TAPE-',QUESTOR                                                
         BNE   XIT                                                              
         MVI   TPWRITE,C'Y'        TURN ON TAPE INDICATOR                       
         B     XIT                                                              
*                                                                               
*                                                                               
MP4      CLI   MODE,REQLAST                                                     
         BNE   MP5                                                              
         CLI   TPWRITE,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 =V(SPM8TP),DMCB,(C'X',(RA))    CLOSE THE TAPE                    
         B     XIT                                                              
*                                                                               
*                                                                               
TPWRITE  DS    CL1                                                              
         EJECT                                                                  
MP5      CLI   MODE,ESTFRST                                                     
         BNE   MP30                                                             
*                                                                               
         L     RE,ADBLOCK                                                       
         USING DBLOCK,RE                                                        
         XC    0(256,RE),0(RE)                                                  
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     MP5DEMX                                                          
*                                                                               
         L     RF,ADCLT                                                         
         CLI   CEXTRA-CLTHDR(RF),C'U' TEST US DEMOS                             
         BE    MP5DEMX                                                          
*                                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   MP5DEMX                                                          
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
MP5DEMX  L     RF,=A(PWPREFIX)                                                  
         MVC   0(5,RF),SPACES                                                   
         L     RE,ADEST                                                         
         USING ESTHDRD,RE                                                       
         OC    EPWPCT,EPWPCT                                                    
         BNZ   *+14                                                             
         OC    ECOST2,ECOST2                                                    
         BZ    MP5NOPW                                                          
         MVC   0(3,RF),=C'IM '                                                  
         CLC   QAGY,=C'WI'                                                      
         BE    *+10                                                             
         MVC   0(3,RF),=C'AGY'                                                  
         CLI   QPWCV,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(3,RF),=C'CLT'                                                  
         DROP  RE                                                               
*                                                                               
MP5NOPW  CLI   ESTSW,C'N'                                                       
         BNE   XIT                                                              
         MVI   ESTSW,C'Y'                                                       
         MVC   PAGE,=H'1'                                                       
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
         CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         CLI   EDAILY,C'Y'         TEST FOR DAILY ESTIMATE                      
         BNE   MP5A                                                             
         DROP  RE                                                               
         CLI   MEDDAILY,C'Y'       YES-TEST DAILY ALREADY                       
         BE    MP5C                YES                                          
         MVI   MEDDAILY,C'Y'       NO-USE SPM8041 FOR MEDTABLE                  
         MVC   ATHISGP,=A(THISGPD)                                              
         MVC   ATHISAD,=A(THISADD)                                              
         MVC   ATHISAP,=A(THISAPD)                                              
         ICM   R4,15,ASPM8041                                                   
         BNZ   MP5B                                                             
         GOTO1 LOADER,DMCB,=C'SPM8041 ',=V(M8041BLK)                            
         ICM   R4,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,ASPM8041                                                      
         B     MP5B                                                             
*                                                                               
MP5A     CLI   MEDDAILY,C'Y'       WEEKLY ESTIMATE -TEST ALREADY WEEKLY         
         BNE   MP5C                YES                                          
         L     R4,ASPM804          NO-SET MEDTABLE TO SPM804                    
         MVI   MEDDAILY,C'N'                                                    
         MVC   ATHISGP,=A(THISGP)                                               
         MVC   ATHISAD,=A(THISAD)                                               
         MVC   ATHISAP,=A(THISAP)                                               
*                                                                               
MP5B     ST    R4,MEDTABLE                                                      
         GOTO1 MEDSEED,DMCB,(RA)                                                
*                                                                               
MP5C     DS    0H                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         BAS   RE,SETDLIST                                                      
         MVC   MEDDFORM,PROGPROF+3                                              
         GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         EJECT                                                                  
*              TRIM DOWN THE BUFFALO TABLES WHERE WE CAN                        
         SPACE 3                                                                
         CLI   QDPTDET,C'B'                                                     
         BE    MP6                                                              
         CLI   QDPTDET,C'C'                                                     
         BNE   MP8                                                              
         LA    R2,LISTB            SUPPRESS SPOT/LENGTH DAY-PART DETAIL         
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP6      LA    R2,LISTA            SUPPRESS SPOTLENGTH DETAIL                   
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP8      CLI   QOPT1,C' '                                                       
         BE    MP15                                                             
         CLI   QOPT1,C'W'                                                       
         BE    MP15                                                             
         CLI   QOPT1,C'1'                                                       
         BE    MP10                                                             
         LA    R2,LIST1            SUPPRESS FIRST QUARTER DETAIL                
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP10     CLI   QOPT1,C'2'                                                       
         BE    MP12                                                             
         LA    R2,LIST2            SUPPRESS SECOND QUARTER DETAIL               
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP12     CLI   QOPT1,C'3'                                                       
         BE    MP14                                                             
         LA    R2,LIST3            SUPPRESS THIRD QUARTER DETAIL                
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP14     CLI   QOPT1,C'4'                                                       
         BE    MP16                                                             
         LA    R2,LIST4            SUPPRESS FOURTH QUARTER DETAIL               
         BAS   RE,TRIM                                                          
         B     MP16                                                             
         SPACE 2                                                                
MP15     CLI   MEDDAILY,C'Y'                                                    
         BE    MP15W                                                            
         LA    R2,LIST2                                                         
         OC    MEDMON04(4),MEDMON04                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST3                                                         
         OC    MEDMON07(4),MEDMON07                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST4                                                         
         OC    MEDMON10(4),MEDMON10                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         B     MP16                                                             
*                                                                               
MP15W    LA    R2,LIST2            THESE WORK FOR DAILY !                       
         OC    MEDMON03(4),MEDMON03                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST3                                                         
         OC    MEDMON05(4),MEDMON05                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST4                                                         
         OC    MEDMON07(4),MEDMON07                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP16     CLI   QOPT1,C'M'                                                       
         BE    MP20                                                             
         CLI   QOPT1,C'Y'                                                       
         BE    MP20                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    MP17                                                             
         CLI   QOPT2,C'M'                                                       
         BE    MP17                                                             
         CLI   QOPT2,C' '          THE DEFAULT IS TO GET A RECAP IF             
         BNE   MP18                MORE THAN ONE QUARTER IS REQUESTED           
         SPACE 2                                                                
MP17     OC    MEDMON04(4),MEDMON04                                             
         BNZ   MP20                                                             
         SPACE 2                                                                
MP18     LA    R2,LIST5            SUPPRESS MONTHLY RECAP                       
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP20     CLI   QOPT2,C'C'                                                       
         BE    MP22                                                             
         CLI   QOPT2,C'Q'                                                       
         BNE   MP21                                                             
*                                  SUPPRESS QUARTERLY IF REQUEST                
*                                  IS ONLY FOR A SINGLE QUARTER                 
         OC    MEDMON04(4),MEDMON04                                             
         BNZ   MP22                                                             
         SPACE 2                                                                
MP21     LA    R2,LIST6            SUPPRESS QUARTERLY RECAP                     
         BAS   RE,TRIM                                                          
         EJECT                                                                  
*              OTHER OPTIMIZATION                                               
         SPACE 3                                                                
MP22     LA    R2,OPTLIST                                                       
         SPACE 2                                                                
MP24     CLC   0(1,R2),QOPT3       MATCH ON DATA OPTION                         
         BE    MP26                                                             
         CLI   0(R2),X'FF'                                                      
         BE    MP26                                                             
         LA    R2,8(R2)                                                         
         B     MP24                                                             
         SPACE 2                                                                
MP26     MVC   MEDEXTDM,3(R2)      HOW MANY DEMOS NEEDED (0/1)                  
         MVC   BUFFCOLS,4(R2)      HOW MANY COLUMNS                             
         MVC   BUFFROWS,=F'1'      ONE ROW IF MARKET IS SPECIFIED               
         CLC   =C'ALL',QMKT                                                     
         BNE   MP28                                                             
         MVC   BUFFROWS,=F'2'      AND 2 UNLESS MARKET GROUPS ARE               
         CLI   QMGR,C' '           INVOLVED                                     
         BE    MP28                                                             
         CLI   QOPT5,C'Y'          OPTION TO SUPPRESS SUMMARIES                 
         BE    MP28                                                             
         MVC   BUFFROWS,=F'5'      WHEN WE NEED 5                               
         SPACE 2                                                                
MP28     L     R3,BUFFCOLS         WORK OUT HOW MANY FIT IN CORE                
         M     R2,BUFFROWS                                                      
         SLL   R3,2                                                             
         A     R3,BUFFLKEY                                                      
         L     R4,LNBUFFER         IN BUFFER AREA                               
         SRDA  R4,32                                                            
         DR    R4,R3                                                            
         BCTR  R5,0                                                             
         ST    R5,BUFFCRMX                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R8)                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TRIM TABLE ENTRIES                                    
         SPACE 3                                                                
TRIM     NTR1                                                                   
         SPACE 2                                                                
TRIM2    CLI   0(R2),X'FF'         R2 HAS A(LINE NO. LIST)                      
         BE    XIT                                                              
         SR    R3,R3                                                            
         IC    R3,0(R2)                                                         
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         A     R3,MEDTABLE                                                      
         OI    0(R3),X'80'                                                      
         LA    R2,1(R2)                                                         
         B     TRIM2                                                            
         EJECT                                                                  
*              ROUTINE TO SET-UP DAYPART FILTER ETC                             
         SPACE 3                                                                
REQFILT  NTR1                                                                   
         MVC   DPFILT,SPACES                                                    
         LA    R2,QUESTOR                                                       
         LA    R3,8                                                             
         SPACE 2                                                                
REQFILT2 CLC   0(2,R2),=C'D='                                                   
         BNE   *+10                                                             
         MVC   DPFILT,2(R2)                                                     
         LA    R2,1(R2)                                                         
         BCT   R3,REQFILT2                                                      
         BAS   RE,SETPCADJ                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT PERCENT ADJUSTMENT                            
         SPACE 3                                                                
SETPCADJ NTR1                                                                   
         XC    REQPCT,REQPCT                                                    
         XC    CSHPCT,CSHPCT                                                    
         CLI   QUESTOR+3,C'.'                                                   
         BNE   XIT                                                              
         LA    R2,REQPCT                                                        
         CLI   QUESTOR+6,C'%'      PERCENT SIGN FOR POINTS                      
         BE    SPC2                                                             
         LA    R2,CSHPCT                                                        
         CLI   QUESTOR+6,C'$'      DOLLAR FOR CASH ADJUSTMENT                   
         BNE   XIT                                                              
         SPACE 2                                                                
SPC2     MVC   WORK,QUESTOR                                                     
         OC    WORK(6),=6X'F0'                                                  
         PACK  DUB,WORK+1(2)                                                    
         CVB   R1,DUB                                                           
         PACK  DUB,WORK+4(2)                                                    
         CVB   R0,DUB                                                           
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         CLI   QUESTOR,C'-'                                                     
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              LISTS FOR OPTIMIZATION ROUTINES                                  
         SPACE 3                                                                
LISTA    DC    AL1(1,4,7,10,13,16,19,22,25)                                     
         DC    AL1(28,31,34,37,40,43,46,49,52)                                  
         DC    X'FF'                                                            
LISTB    DC    AL1(2,5,8,11,14,17,20,23,26)                                     
         DC    AL1(29,32,35,38,41,44,47,50,53)                                  
         DC    X'FF'                                                            
LIST1    DC    9AL1(*-LIST1+1),X'FF'                                            
LIST2    DC    9AL1(*-LIST2+10),X'FF'                                           
LIST3    DC    9AL1(*-LIST3+19),X'FF'                                           
LIST4    DC    9AL1(*-LIST4+28),X'FF'                                           
LIST5    DC    9AL1(*-LIST5+37),X'FF'                                           
LIST6    DC    9AL1(*-LIST6+46),X'FF'                                           
         SPACE 2                                                                
OPTLIST  DS    0D                                                               
         DC    C' NY',AL1(1),F'36'                                              
         DC    C'1NY',AL1(1),F'36'                                              
         DC    C'2NY',AL1(1),F'36'                                              
         DC    C'3NY',AL1(0),F'18'                                              
         DC    C'4YN',AL1(1),F'72'                                              
         DC    C'5YN',AL1(1),F'72'                                              
         DC    C'6YN',AL1(0),F'54'                                              
         DC    C'7YY',AL1(1),F'72'                                              
         DC    C'8YY',AL1(1),F'72'                                              
         DC    C'9YY',AL1(0),F'54'                                              
         DC    C'ANY',AL1(0),F'18'                                              
         DC    C'BYN',AL1(0),F'54'                                              
         DC    C'CYY',AL1(0),F'54'                                              
         DC    C'DYN',AL1(1),F'72'                                              
         DC    C'EYY',AL1(1),F'72'                                              
         DC    C'JYY',AL1(1),F'72'                                              
         DC    C'KYY',AL1(1),F'72'                                              
         DC    C'LYY',AL1(0),F'54'                                              
         DC    X'FF'                                                            
         DC    C'NY',AL1(1),F'36'                                               
         EJECT                                                                  
*              PROCESS A GOAL RECORD                                            
         SPACE 3                                                                
MP30     CLI   MODE,PROCGOAL                                                    
         BNE   MP32                                                             
         L     R5,ADGOAL                                                        
         USING GOALREC,R5                                                       
         TM    GKEYAGY-GKEY+KEY,X'80'  IGNORE PASSIVE PIGGYBACK POINTER         
         BO    XIT                                                              
*                                                                               
         XR    R0,R0               CLEAR R0                                     
         LA    R6,24(R5)           R6=A(FIRST GOAL ELEMENT)                     
MP30A    CLI   0(R6),0             END OF RECORD?                               
         BE    XIT                 YES - EXIT                                   
         CLI   0(R6),X'21'         HAVE A WEEK ELEMENT?                         
         BE    MP30AA              YES - OK TO PROCESS                          
         IC    R0,1(R6)            LENGTH OF ELEMENT                            
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         B     MP30A               KEEP SEARCHING FOR X'21' ELEMENTS            
*                                                                               
MP30AA   L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         TM    GKEYAGY,X'40'       TEST PRD2 IS IDREF                           
         BO    *+10                                                             
         MVC   MEDPIGGY,GKEYPRD2   PARTNER BRAND                                
         ZIC   RE,MEDBRAND         BYPASS BRANDS NOT IN PRDBUFF                 
         BCTR  RE,0                 1. INCONSISTANT EST FILTERS                 
         MH    RE,PRDBUFLN          2. INCONSISTANT ADV POINTERS                
         A     RE,PRDBUFF                                                       
         CLI   0(RE),0                                                          
         BE    XIT                                                              
         MVC   MEDSPTLN,GKEYSLN                                                 
         MVC   MEDTSPLN,GKEYSEC    TOTAL SPOT LENGTH                            
         CLI   MEDPIGGY,0                                                       
         BE    *+10                                                             
         MVC   MEDSPTLN,MEDTSPLN                                                
         MVI   MEDNOPIG,C'Y'       PRETEND THERE IS NO PARTNER BRAND            
*                                                                               
         MVI   MEDEXTPW,C'N'                                                    
         CLI   QPWCV,C'Y'          TEST PW CLT$ REQUEST                         
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         MVC   MEDSPTLN,GKEYSLN                                                 
         CLC   DPFILT,SPACES                                                    
         BE    *+14                                                             
         CLC   DPFILT,MEDDPART                                                  
         BNE   XIT                                                              
         JIF   QOPT3,EQ,7,OR,8,OR,9,MP30B,JUMP=N                                
         CLI   QOPT3,C'J'                                                       
         BE    MP30B                                                            
         CLI   QOPT3,C'K'                                                       
         BE    MP30B                                                            
         CLI   QOPT3,C'L'                                                       
         BE    MP30B                                                            
         MVC   MEDDPGRP,MEDDPART                                                
         SPACE 2                                                                
MP30B    CLC   TODAYP,GACTDATE     WAS IT ACTIVE TODAY                          
         BNE   MP31                                                             
         L     R2,=A(ACTPOOL)                                                   
         A     R2,RELO             YES - SO LEAVE A MEMO IN POOL                
         CLC   0(4,R2),4(R2)                                                    
         BE    MP31                                                             
         L     R3,4(R2)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,4(R2)                                                         
         BCTR  R3,0                                                             
         MH    R3,=H'06'                                                        
         LA    R3,8(R2,R3)                                                      
         MVC   0(1,R3),MEDBRAND                                                 
         MVC   1(1,R3),MEDPIGGY                                                 
         MVC   2(3,R3),MEDDPART                                                 
         MVC   5(1,R3),MEDTSPLN                                                 
         DROP  R6                                                               
         SPACE 2                                                                
MP31     GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,DIGPROD                                                       
         BAS   RE,SETGSPLN         SET GOAL SPOT LENGTHS                        
         BAS   RE,CHEKPRIM                                                      
         GOTO1 MEDPOST,DMCB,(RA)                                                
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A BUY RECORD                                             
         SPACE 3                                                                
MP32     CLI   MODE,PROCBUY                                                     
         BNE   MP40                                                             
         L     R2,=A(PSLIST)                                                    
         LR    RE,R2                                                            
         LA    RF,L'PSLIST                                                      
         XCEF                                                                   
         GOTO1 MEDPSL,DMCB,(RA),(R2)                                            
         SPACE 2                                                                
MP34     OC    0(2,R2),0(R2)       LOOP ON BRAND/SPOTLENGTH                     
         BE    XIT                                                              
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         MVI   MEDPIGGY,0                                                       
*                                                                               
         MVI   MEDEXTPW,C'N'                                                    
         CLI   QPWCV,C'Y'          TEST PW CLT$ REQUEST                         
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         CLI   SPILLOPT,C'Y'       TEST SUPPRESS SPILL OPTION                   
         BNE   *+12                                                             
         CLI   MEDSPILL,C'Y'       ON - CHECK FOR SPILL                         
         BE    XIT                      YES - EXIT                              
         CLC   DPFILT,SPACES                                                    
         BE    *+14                                                             
         CLC   DPFILT,MEDDPART                                                  
         BNE   XIT                                                              
         JIF   QOPT3,EQ,7,OR,8,OR,9,MP36,JUMP=N                                 
         CLI   QOPT3,C'J'                                                       
         BE    MP36                                                             
         CLI   QOPT3,C'K'                                                       
         BE    MP36                                                             
         CLI   QOPT3,C'L'                                                       
         BE    MP36                                                             
         MVC   MEDDPGRP,MEDDPART                                                
         DROP  R6                                                               
         SPACE 2                                                                
MP36     GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,DIGPROD                                                       
         BAS   RE,SETBSPLN         SET BUY SPOT LENGTHS                         
         BAS   RE,CHEKPRIM                                                      
         GOTO1 MEDPOST,DMCB,(RA)                                                
         LA    R2,2(R2)                                                         
         B     MP34                                                             
         EJECT                                                                  
*              ROUTINE TO LOOK UP PRODUCT CODE                                  
         SPACE 3                                                                
         USING MEDBLOCK,R6                                                      
DIGPROD  NTR1                                                                   
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
         L     R6,MEDBUFF                                                       
         MVC   WORK(1),MEDBRAND                                                 
         MVI   WORK+1,0                                                         
         CLI   MEDBRAND,219                                                     
         BNL   DIG8                                                             
*                                                                               
DIG1     LA    R0,2                                                             
         MVI   WORK,X'FF'                                                       
         MVI   WORK+1,X'FF'                                                     
         CLI   MEDPIGGY,0                                                       
         BNE   *+12                                                             
         LA    R0,1                                                             
         MVI   WORK+1,0                                                         
         LA    RE,1                                                             
*                                                                               
DIG2     CLC   MEDBRAND,3(RF)                                                   
         BNE   DIG4                                                             
         STC   RE,WORK          WORK(1) = PSN IN PRODUCT LIST IN CLIENT         
         BCT   R0,DIG6                    HEADER FOR PRODUCT 1                  
         B     DIG8                                                             
*                                                                               
DIG4     CLI   MEDPIGGY,0                                                       
         BE    DIG6                                                             
         CLC   MEDPIGGY,3(RF)                                                   
         BNE   DIG6                                                             
         STC   RE,WORK+1        WORK+1(1) = PSN IN PRODUCT LIST IN              
         BCT   R0,DIG6                      CLIENT HEADER FOR PRODUCT 2         
         B     DIG8                                                             
*                                                                               
DIG6     LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    DIG2                                                             
*                                                                               
DIG8     ZIC   R1,MEDBRAND         SET MEDPRIMY TO RELATIVE DEMO NO             
         LA    R1,DLIST-1(R1)                                                   
         MVC   MEDPRIMY,0(R1)                                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SET GOAL SPOT LENGTHS                                 
         SPACE 3                                                                
         USING MEDBLOCK,R6                                                      
SETGSPLN DS    0H                                                               
         L     R6,MEDBUFF                                                       
         ZIC   R1,MEDTSPLN                                                      
         ZIC   RF,MEDSPTLN                                                      
         STC   R1,WORK+2           WORK+2(1) = TOTAL SPOT LENGTH                
         STC   RF,WORK+3           WORK+3(1) = SPOT LENGTH 1                    
         SR    R1,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         STC   R1,WORK+4           WORK+4(1) = SPOT LENGTH 2                    
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO SET BUY SPOT LENGTHS                                  
*              NOTE: TOTAL SPOT LENGTH MEDTSPLN IS NOT SET FOR BUYS             
         SPACE 3                                                                
         USING MEDBLOCK,R6                                                      
SETBSPLN DS    0H                                                               
         L     R6,MEDBUFF                                                       
         ZIC   RF,MEDSPTLN                                                      
         STC   RF,WORK+2           WORK+2(1) = TOTAL SPOT LENGTH                
         STC   RF,WORK+3           WORK+3(1) = SPOT LENGTH 1                    
         MVI   WORK+4,0            WORK+4(1) = SPOT LENGTH 2 = 0                
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD A LIST OF RELATIVE DEMO NO / PRODUCT            
         SPACE 3                                                                
SETDLIST NTR1                                                                   
         CLC   PRDBUFLN,=H'57'                                                  
         BL    XIT                                                              
         XC    DLIST,DLIST                                                      
         XC    PDLIST,PDLIST                                                    
         L     R2,PRDBUFF                                                       
         LA    R3,DLIST                                                         
         LA    R4,220                                                           
         SPACE 1                                                                
SETD2    BAS   RE,SETD4                                                         
         AH    R2,PRDBUFLN                                                      
         LA    R3,1(R3)                                                         
         BCT   R4,SETD2                                                         
         MVI   DLIST+254,1         (SET POOL TO REL 1)                          
         B     XIT                                                              
         SPACE 1                                                                
SETD4    NTR1                                                                   
         USING PTBUFFD,R2                                                       
         CLI   PTPRDN,0            PICK OUT TARGET FOR THIS BRAND               
         BE    XIT                                                              
         MVC   WORK(3),PTDEMO                                                   
         LA    R2,PDLIST           GET RELATIVE NUMBER FROM LIST                
         LA    R4,1                                                             
         LA    R5,60                                                            
         SPACE 1                                                                
SETD6    STC   R4,0(R3)            RETURN RELATIVE NUMBER                       
         CLC   0(3,R2),WORK                                                     
         BE    XIT                                                              
         OC    0(3,R2),0(R2)                                                    
         BZ    SETD8                                                            
         LA    R2,3(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,SETD6                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
SETD8    MVC   0(3,R2),WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FUDGE PRODUCT GROUPS FOR CHILD SPOT                   
         SPACE 2                                                                
CHEKPRIM NTR1                                                                   
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         CLI   QOPT2,C'C'                                                       
         BNE   XIT                                                              
         MVI   MEDPRIMY,1                                                       
         L     R2,PRDBUFF                                                       
         SPACE 2                                                                
CH2      CLC   0(1,R2),MEDBRAND                                                 
         BE    XIT                                                              
         AH    R2,PRDBUFLN                                                      
         B     CH2                                                              
         EJECT                                                                  
*              CONTROL OF REPORT PRINTING                                       
         SPACE 3                                                                
MP40     CLI   MODE,MKTFRST                                                     
         BNE   MP44                                                             
         L     R2,=A(ACTPOOL)                                                   
         A     R2,RELO                                                          
         XC    4(4,R2),4(R2)                                                    
         MVI   MCOMSW,C'N'                                                      
         CLI   PROGPROF+8,C'Y'     TEST MARKET COMMENTS REQUIRED                
         BNE   XIT                                                              
         BAS   RE,GETMCOM          YES-GET MARKET COMMENTS                      
         B     XIT                                                              
         SPACE 2                                                                
MP44     MVI   RCSUBPRG,1                                                       
         LA    R2,1                                                             
         CLI   MODE,MKTLAST                                                     
         BNE   MP46                                                             
         B     SUM                                                              
         SPACE 2                                                                
MP46     MVI   RCSUBPRG,2                                                       
         CLI   QOPT5,C'Y'                                                       
         BE    MP52                                                             
         LA    R2,2                                                             
         CLI   MODE,MGR3LAST                                                    
         BE    SUM                                                              
         SPACE 2                                                                
MP48     LA    R2,3                                                             
         CLI   MODE,MGR2LAST                                                    
         BE    SUM                                                              
         SPACE 2                                                                
MP50     LA    R2,4                                                             
         CLI   MODE,MGR1LAST                                                    
         BE    SUM                                                              
         SPACE 2                                                                
MP52     MVI   RCSUBPRG,3                                                       
         LA    R2,5                                                             
         CLI   QMGR,C' '                                                        
         BNE   *+8                                                              
         LA    R2,2                                                             
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         LA    R2,2                                                             
         CLI   MODE,PRDLAST                                                     
         BNE   XIT                                                              
         SPACE 2                                                                
SUM      CLI   SPDUPTOT,C'Y'                                                    
         BE    SUM1                                                             
         BAS   RE,EDIT                                                          
         SPACE 2                                                                
SUM1     C     R2,BUFFROWS                                                      
         BE    SUM6                                                             
         CH    R2,=H'1'            IF WE'RE AT HIGHEST LEVEL                    
         BNE   SUM6                                                             
         ST    R2,DMCB+8                                                        
         LA    R3,1(R2)            ADD TO LOWER LEVELS IF NEEDED                
         LA    R4,DMCB+12                                                       
         SPACE 2                                                                
SUM2     ST    R3,0(R4)                                                         
         C     R3,BUFFROWS                                                      
         BE    SUM4                                                             
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         B     SUM2                                                             
         SPACE 2                                                                
SUM4     OI    0(R4),X'80'                                                      
         GOTO1 BUFFALO,DMCB,=C'ADD',(R8)                                        
         SPACE 2                                                                
SUM6     GOTO1 BUFFALO,DMCB,=C'CLEAR',(R8),(X'80',(R2))                         
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO READ AND EDIT BUFFALO RECORDS                        
         SPACE 3                                                                
EDIT     NTR1                                                                   
         MVI   PRDCNT,0                                                         
         ZAP   PDGCOUNT,=P'0'                                                   
         XC    LASTKEY,LASTKEY                                                  
         MVC   THISKEY,LASTKEY                                                  
         ST    R2,BUFFLEVL                                                      
         L     RE,BUFFIO                                                        
         LR    R3,RE                                                            
         LA    RF,1450                                                          
         XCEF                                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),(R3),(R2)                             
         B     EDIT4                                                            
         SPACE 2                                                                
EDIT2    L     R2,BUFFLEVL                                                      
         L     R3,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),(R3),(R2)                              
         SPACE 2                                                                
EDIT4    TM    DMCB+8,X'80'                                                     
         BO    XIT                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,COMPDISP                                                      
         CLC   THISKEY(4),LASTKEY  FIRST RECORD FOR PRODUCT                     
         BE    EDIT20                                                           
         CLC   THISKEY(1),LASTKEY                                               
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   THISPRD1,X'FF'                                                   
         BE    EDIT10                                                           
         MVI   CHUNKSW,C'Y'                                                     
         BAS   RE,PAGESET                                                       
         L     R6,PAGEMARK                                                      
*                                                                               
         ZIC   R1,PRDCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PRDCNT                                                        
*                                                                               
         MVI   SVBRAND,0                                                        
         MVI   SVPIGGY,0                                                        
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)   RF = A(PROD LIST IN CLT HDR)              
         CLI   THISPRD1,X'FF'      TEST FOR UNRECOGNIZED PRODUCT                
         BNE   *+14                                                             
         MVC   0(3,R6),=C'***'                                                  
         B     EDIT5                                                            
         MVC   0(3,R6),=C'POL'                                                  
         ZIC   R2,THISPRD1                                                      
         CLI   THISPRD1,219                                                     
         BNL   EDIT4A                                                           
         ZIC   RE,THISPRD1                                                      
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         AR    RE,RF                                                            
         MVC   0(3,R6),0(RE)                                                    
         ZIC   R2,3(RE)                                                         
*                                                                               
EDIT4A   BCTR  R2,0                                                             
         MH    R2,PRDBUFLN                                                      
         A     R2,PRDBUFF                                                       
         MVC   SVBRAND,0(R2)       SAVE THE BRAND CODE                          
         L     RF,=A(LODAREA)                                                   
         GOTO1 (RF),DMCB,QEND,(SVBRAND,TALFAC)                                  
*                                                                               
         CLI   QOPT4,C'N'          OPTION TO SUPPRESS NAME                      
         BE    EDIT5                                                            
         MVI   8(R6),C'-'                                                       
         MVC   21(20,R6),4(R2)     PRODUCT 1 NAME                               
         OC    21(20,R6),SPACES                                                 
*                                                                               
EDIT5    LA    R4,42(R6)                                                        
         CLI   THISPRD2,0                                                       
         BNE   *+16                                                             
         CLI   THISPRD1,X'FF'                                                   
         BE    EDIT8                                                            
         B     EDIT6                                                            
         BAS   RE,DEMOUT                                                        
         CLI   THISPRD2,X'FF'      TEST FOR UNRECOGNIZED PIGGYBACK              
         BNE   *+12                                                             
         LA    RE,=C'***'                                                       
         B     EDIT5A                                                           
         ZIC   RE,THISPRD2                                                      
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)   RF = A(PROD LIST IN CLT HDR)              
         AR    RE,RF                                                            
*                                                                               
EDIT5A   LA    R3,3(R6)                                                         
         CLI   2(R6),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         MVC   1(3,R3),0(RE)                                                    
         MVC   8(12,R6),=C'*PIGGYBACKS*'                                        
         CLI   THISPRD2,X'FF'                                                   
         BE    EDIT8                                                            
         ZIC   R2,3(RE)                                                         
         BCTR  R2,0                                                             
         MH    R2,PRDBUFLN                                                      
         A     R2,PRDBUFF                                                       
         MVC   SVPIGGY,0(R2)       SAVE THE PIGGYBACK BRAND CODE                
         CLI   QOPT4,C'N'          OPTION TO SUPPRESS NAME                      
         BE    EDIT16                                                           
         MVI   70(R6),C'/'                                                      
         MVC   72(20,R6),4(R2)     PRODUCT 2 NAME                               
         OC    72(20,R6),SPACES                                                 
         LA    R4,93(R6)                                                        
*                                                                               
EDIT6    BAS   RE,DEMOUT           GET DEMO NAMES                               
*                                                                               
EDIT8    GOTO1 SQUASHER,DMCB,(R6),132                                           
         LA    R6,132(R6)                                                       
         ST    R6,PAGEMARK                                                      
         B     EDIT16                                                           
         SPACE 2                                                                
EDIT10   CLI   THISDEM,X'FF'       FIRST FOR PRIMARY DEMO GROUP                 
         BE    EDIT14                                                           
         AP    PDGCOUNT,=P'1'                                                   
         MVC   P,SPACES                                                         
         MVI   CHUNKSW,C'Y'        SUPPRESS IF ONLY 1 PRODUCT IN GROUP          
         CLI   PRDCNT,1                                                         
         MVI   PRDCNT,0                                                         
         BH    EDIT11                                                           
         MVI   CHUNKSW,C'N'                                                     
         B     EDIT2                                                            
         SPACE 2                                                                
EDIT11   CLI   QOPT4,C'N'          SKIP A LINE IF WE WERE DOING                 
         BNE   EDIT12              SINGLE SPACING PRODUCT REPORT                
         CLI   QDPTDET,C'C'                                                     
         BNE   EDIT12                                                           
         GOTO1 REPORT                                                           
         SPACE 2                                                                
EDIT12   BAS   RE,PAGESET                                                       
         L     R6,PAGEMARK                                                      
         MVC   0(12,R6),=C'GROUP TOTALS'                                        
         CLI   QOPT4,C'N'                                                       
         BE    EDIT16                                                           
         MVC   WORK(1),THISDEM                                                  
         BAS   RE,DEMEXP                                                        
         MVC   0(7,R6),WORK                                                     
         MVC   7(7,R6),=C' BRANDS'                                              
         LA    R6,132(R6)                                                       
         ST    R6,PAGEMARK                                                      
         B     EDIT16                                                           
         SPACE 2                                                                
EDIT14   BAS   RE,PAGESET                                                       
         L     R6,PAGEMARK                                                      
         MVI   CHUNKSW,C'Y'        SUPPRESS TOTALS IF ONLY 1 PD GROUP           
         CP    PDGCOUNT,=P'1'                                                   
         BH    *+8                                                              
         MVI   CHUNKSW,C'N'                                                     
         ZAP   PDGCOUNT,=P'0'                                                   
         CLI   QOPT3,C'2'          AND SUPPRESS IF RATINGS ONLY                 
         BNE   *+8                                                              
         MVI   CHUNKSW,C'N'                                                     
         CLI   QOPT3,C'5'                                                       
         BNE   *+8                                                              
         MVI   CHUNKSW,C'N'                                                     
         CLI   QOPT3,C'8'                                                       
         BNE   *+8                                                              
         MVI   CHUNKSW,C'N'                                                     
         CLI   QOPT3,C'K'                                                       
         BNE   *+8                                                              
         MVI   CHUNKSW,C'N'                                                     
         MVC   0(7,R6),=C'SUMMARY'                                              
         CLI   QOPT4,C'N'                                                       
         BE    EDIT16                                                           
         MVC   0(132,R6),SPACES                                                 
         MVC   57(18,R6),=C'MEDIA PLAN SUMMARY'                                 
         LA    R6,132(R6)                                                       
         MVC   57(18,R6),DASHES                                                 
         LA    R6,264(R6)                                                       
         ST    R6,PAGEMARK                                                      
         SPACE 2                                                                
EDIT16   MVI   DPCOUNT,0                                                        
         MVI   SLCOUNT,0                                                        
         B     EDIT20                                                           
         EJECT                                                                  
*              ROUTINE TO OUTPUT DEMOS                                          
         SPACE 3                                                                
DEMOUT   NTR1                                                                   
         LA    R2,28(R2)                                                        
         ZIC   R3,PROGPROF+6       MAXIMUM NUMBER TO SHOW                       
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         CLI   THISPRD2,0                                                       
         BE    *+8                                                              
         LA    R3,3                3 DEMOS FOR PIGGYBACKS                       
         MVI   0(R4),C'('                                                       
         LA    R4,1(R4)                                                         
         CLC   PRDBUFLN,=H'56'                                                  
         BH    DEMOUT6                                                          
         SPACE 2                                                                
DEMOUT2  ZIC   R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    DEMOUT4                                                          
         STC   R5,WORK                                                          
         BAS   RE,DEMEXP                                                        
         MVC   0(7,R4),WORK                                                     
         MVI   7(R4),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R4,9(R4)                                                         
         BCT   R3,DEMOUT2                                                       
         SPACE 2                                                                
DEMOUT4  BCTR  R4,R0                                                            
         BCTR  R4,R0                                                            
         MVI   0(R4),C')'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DEMOUT6  AHI   R2,-28                                                           
         USING PTBUFFD,R2                                                       
         LA    R2,PTDEMO                                                        
         L     R1,ADEST                                                         
         MVC   SPUSRNMS,EUSRNMS-ESTHDR(R1)                                      
         L     RE,=A(SVNONTDMS)       POINT TO NONT DEMO SAVE AREA              
         LA    RF,ENONTDMS-ESTHDR(R1)                                           
         MVC   0(140,RE),0(RF)        MOVE NONT DEMO LIST TO IT                 
         ST    RE,DMCB+16             AND SET AS PARAM5                         
*                                                                               
DEMOUT8  OC    0(3,R2),0(R2)                                                    
         BZ    DEMOUT4                                                          
*                                                                               
DEMOUT10 GOTO1 DEMOCON,DMCB,(0,(R2)),(2,(R4)),(C'S',ADBLOCK),          X        
               (SPOTPROF+9,SPUSRNMS)                                            
         MVI   7(R4),C','                                                       
         LA    R2,3(R2)                                                         
         LA    R4,9(R4)                                                         
         BCT   R3,DEMOUT8                                                       
         B     DEMOUT4                                                          
         EJECT                                                                  
*              ROUTINE TO EXPAND 1 CHARACTER DEMO NUMBER                        
         SPACE 1                                                                
DEMEXP   NTR1                                                                   
         CLC   PRDBUFLN,=H'56'                                                  
         BH    DEMEXP2                                                          
         ZIC   R1,WORK             OLD STYLE                                    
         BCTR  R1,0                                                             
         MHI   R1,7                                                             
         A     R1,DEMTABLE                                                      
         MVC   WORK(7),0(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
DEMEXP2  ZIC   R1,WORK                                                          
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,PDLIST(R1)                                                    
         MVC   WORK(3),0(R1)                                                    
         L     RE,=A(SVNONTDMS)       POINT TO NONT DEMO SAVE AREA              
         ST    RE,DMCB+16             AND SET AS PARAM5                         
         GOTO1 DEMOCON,DMCB,WORK,(2,WORK+3),(C'S',ADBLOCK),            X        
               (SPOTPROF+9,SPUSRNMS)                                            
         MVC   WORK(7),WORK+3                                                   
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL THE DETAIL LINE EDIT                                     
         SPACE 3                                                                
EDIT20   L     R6,PAGEMARK                                                      
         BAS   RE,UNWEIGHT                                                      
         L     R2,=A(EDBLOCK)      CHECK WE HAVEN'T HIT 40 LINES YET            
         A     R2,RELO                                                          
         AH    R2,=H'5280'                                                      
         CR    R6,R2                                                            
         BL    EDIT21                                                           
         BAS   RE,CHUNK            WE HAVE - SO EXHAUST                         
         BAS   RE,PAGESET                                                       
         L     R6,PAGEMARK                                                      
         SPACE 2                                                                
EDIT21   CLI   THISDPT+1,C''      TEST FOR $ DAYPART                           
         BNE   *+16                                                             
         CLI   THISTYPE,3          YES-SKIP EXCEPT FOR TOTAL                    
         BE    EDIT26                                                           
         B     EDIT28                                                           
         CLI   THISTYPE,1          DP/SL DETAIL                                 
         BNE   EDIT22                                                           
         SR    R2,R2                                                            
         IC    R2,SLCOUNT                                                       
         LA    R2,1(R2)                                                         
         STC   R2,SLCOUNT                                                       
         L     R2,=A(ACTPOOL)      WAS THIS PRD/DPT/SL ACTIVE TODAY             
         A     R2,RELO                                                          
         L     R3,4(R2)                                                         
         LTR   R3,R3                                                            
         BZ    EDIT21D                                                          
         MVI   4(R6),C'*'          ASSUME IT IS                                 
         MVC   WORK(1),SVBRAND                                                  
         MVC   WORK+1(1),SVPIGGY                                                
         MVC   WORK+2(3),THISDPT+1                                              
         MVC   WORK+5(1),THISTLN                                                
         LA    R2,8(R2)                                                         
         SPACE 2                                                                
EDIT21B  CLC   0(6,R2),WORK                                                     
         BE    EDIT21D                                                          
         LA    R2,6(R2)                                                         
         BCT   R3,EDIT21B                                                       
         MVI   4(R6),C' '          NO MATCH - SO IT IS'NT                       
         SPACE 2                                                                
EDIT21D  MVC   5(3,R6),THISDPT+1                                                
         MVI   8(R6),C'-'                                                       
         EDIT  (1,THISSLN1),(3,9(R6)),ALIGN=LEFT                                
         CLI   THISSLN2,0                                                       
         BE    EDIT21E                                                          
         LA    R3,12(R6)                                                        
         CLI   11(R6),C' '                                                      
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (1,THISSLN2),(3,1(R3)),ALIGN=LEFT                                
*                                                                               
EDIT21E  BAS   RE,EDIT30                                                        
         B     EDIT28                                                           
         SPACE 2                                                                
EDIT22   CLI   THISTYPE,2          DAYPART TOTAL                                
         BNE   EDIT26                                                           
         SR    R2,R2                                                            
         IC    R2,DPCOUNT                                                       
         LA    R2,1(R2)                                                         
         STC   R2,DPCOUNT                                                       
         CLI   SLCOUNT,1                                                        
         MVI   SLCOUNT,0                                                        
         BE    EDIT28                                                           
         MVC   5(3,R6),THISDPT+1                                                
         BL    EDIT24                                                           
         MVC   8(4,R6),=C'-TOT'                                                 
         SPACE 2                                                                
EDIT24   BAS   RE,EDIT30                                                        
         MVI   SLCOUNT,0                                                        
         B     EDIT28                                                           
         SPACE 2                                                                
EDIT26   CLI   THISTYPE,3          CHUNK TOTAL                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DPCOUNT,2                                                        
         BL    *+10                                                             
         MVC   5(5,R6),=C'TOTAL'                                                
         CLI   DPCOUNT,1                                                        
         BE    *+8                                                              
         BAS   RE,EDIT30                                                        
         MVI   SLCOUNT,0                                                        
         MVI   DPCOUNT,0                                                        
         BAS   RE,CHUNK                                                         
         MVI   CHUNKSW,C'Y'                                                     
         BAS   RE,PAGESET                                                       
         MVI   FLTSW,C'Y'                                                       
         BAS   RE,FLIGHT                                                        
         MVI   FLTSW,C'N'                                                       
         SPACE 2                                                                
EDIT28   MVC   LASTKEY,THISKEY                                                  
         B     EDIT2                                                            
         EJECT                                                                  
*              ROUTINES TO SPLASH OUT THE NUMBERS                               
         SPACE 3                                                                
EDIT30   NTR1                                                                   
         L     R6,PAGEMARK                                                      
         LA    R2,DECTABLE                                                      
         SPACE 2                                                                
EDIT32   CLC   0(1,R2),QOPT3       MATCH DATA TYPE IN DECISION TABLE            
         BE    EDIT34                                                           
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     EDIT32                                                           
         SPACE 2                                                                
EDIT34   LA    R2,2(R2)            LOOP DOWN AND FIND OUT WHAT TO DO            
         CLI   0(R2),X'FF'                                                      
         BNE   EDIT36                                                           
         ST    R6,PAGEMARK                                                      
         B     XIT                                                              
         SPACE 2                                                                
EDIT36   CLI   1(R2),C'P'                                                       
         BNE   EDIT37                                                           
         CLI   THISDEM,X'FF'       DONT HANDLE POINTS AT CLIENT LEVEL           
         BE    EDIT34                                                           
         SPACE 2                                                                
EDIT37   CLC   0(2,R2),=C'GP'                                                   
         BE    EDIT40                                                           
         CLC   0(2,R2),=C' P'                                                   
         BE    EDIT40                                                           
         CLC   0(2,R2),=C'G$'                                                   
         BE    EDIT42                                                           
         CLC   0(2,R2),=C' $'                                                   
         BE    EDIT42                                                           
         CLC   0(2,R2),=C'G+'                                                   
         BE    EDIT42                                                           
         CLC   0(2,R2),=C' +'                                                   
         BE    EDIT42                                                           
         CLC   0(2,R2),=C'AP'                                                   
         BE    EDIT44                                                           
         CLC   0(2,R2),=C'A$'                                                   
         BE    EDIT46                                                           
         CLC   0(2,R2),=C'A+'                                                   
         BE    EDIT46                                                           
         CLC   0(2,R2),SPACES                                                   
         BE    EDIT48                                                           
         CLC   0(2,R2),=C'IP'                                                   
         BE    EDIT50                                                           
         CLC   0(2,R2),=C'I$'                                                   
         BE    EDIT52                                                           
         CLC   0(2,R2),=C'DP'                                                   
         BE    EDIT50                                                           
         CLC   0(2,R2),=C'D$'                                                   
         BE    EDIT52                                                           
         CLC   0(2,R2),=C'I+'                                                   
         BE    EDIT52                                                           
         CLC   0(2,R2),=C'GC'                                                   
         BE    EDIT54                                                           
         CLC   0(2,R2),=C'AC'                                                   
         BE    EDIT56                                                           
         B     EDIT34                                                           
         EJECT                                                                  
*              DECISION TABLE FOR EDITING                                       
         SPACE 3                                                                
DECTABLE DS    0D                                                               
         DC    C' ',AL1(09),C' PGC $',X'FF'                                     
         DC    C'1',AL1(09),C' PGC $',X'FF'                                     
         DC    C'2',AL1(05),C' P',X'FF'                                         
         DC    C'3',AL1(05),C' $',X'FF'                                         
         DC    C'4',AL1(09),C'APACA$',X'FF'                                     
         DC    C'5',AL1(05),C'AP',X'FF'                                         
         DC    C'6',AL1(05),C'A$',X'FF'                                         
         DC    C'7',AL1(19),C'GPAPIP  G$A$I$  ',X'FF'                           
         DC    C'8',AL1(11),C'GPAPIP  ',X'FF'                                   
         DC    C'9',AL1(11),C'G$A$I$  ',X'FF'                                   
         DC    C'A',AL1(05),C' +',X'FF'                                         
         DC    C'B',AL1(05),C'A+',X'FF'                                         
         DC    C'C',AL1(11),C'G+A+I+  ',X'FF'                                   
         DC    C'D',AL1(09),C'APA+  ',X'FF'                                     
         DC    C'E',AL1(11),C'G+A+AP  ',X'FF'                                   
         DC    C'J',AL1(19),C'GPAPDP  G$A$D$  ',X'FF'                           
         DC    C'K',AL1(11),C'GPAPDP  ',X'FF'                                   
         DC    C'L',AL1(11),C'G$A$D$  ',X'FF'                                   
         EJECT                                                                  
*              SPECIFIC EDITING ROUTINES                                        
         SPACE 3                                                                
EDIT40   MVC   14(2,R6),0(R2)      GOAL POINTS                                  
         L     R3,ATHISGP                                                       
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         L     R3,=A(THISMGP)                                                   
         BE    EDMON                                                            
         L     R3,=A(THISQGP)                                                   
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT42   MVC   14(2,R6),0(R2)      GOAL DOLLARS                                 
         LA    R3,THISGD                                                        
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         LA    R3,THISMGD                                                       
         BE    EDMON                                                            
         MVI   EDQUTGD,C'Y'        SET ON ZEN'S FLAG                            
         LA    R3,THISQGD                                                       
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT44   MVC   14(2,R6),0(R2)      ACTUAL POINTS                                
         MVI   14(R6),C'P'         SHOW PP NOT AP                               
         L     R3,ATHISAP                                                       
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         L     R3,=A(THISMAP)                                                   
         BE    EDMON                                                            
         L     R3,=A(THISQAP)                                                   
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT46   MVC   14(2,R6),0(R2)      ACTUAL DOLLARS                               
         MVI   14(R6),C'P'         SHOW P$ NOT A$                               
         L     R3,ATHISAD                                                       
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         L     R3,=A(THISMAD)                                                   
         BE    EDMON                                                            
         L     R3,=A(THISQAD)                                                   
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT48   LA    R6,132(R6)          SPACE A LINE                                 
         B     EDIT34                                                           
         SPACE 2                                                                
EDIT50   MVC   14(2,R6),0(R2)      PERCENTAGE POINTS                            
         L     R3,ATHISAP                                                       
         L     R4,ATHISGP                                                       
         CLI   THISPAGE,5                                                       
         BL    PERWEEK                                                          
         L     R3,=A(THISMAP)                                                   
         L     R4,=A(THISMGP)                                                   
         BE    PERMON                                                           
         L     R3,=A(THISQAP)                                                   
         L     R4,=A(THISQGP)                                                   
         B     PERQUT                                                           
         SPACE 2                                                                
EDIT52   MVC   14(2,R6),0(R2)      PERCENTAGE DOLLARS                           
         L     R3,ATHISAD                                                       
         LA    R4,THISGD                                                        
         CLI   THISPAGE,5                                                       
         BL    PERWEEK                                                          
         L     R3,=A(THISMAD)                                                   
         LA    R4,THISMGD                                                       
         BE    PERMON                                                           
         L     R3,=A(THISQAD)                                                   
         LA    R4,THISQGD                                                       
         B     PERQUT                                                           
         SPACE 2                                                                
EDIT54   L     R3,ATHISGP          GOAL COST PER POINT                          
         LA    R3,68(R3)                                                        
         LA    R4,THISGD                                                        
         LA    R4,68(R4)                                                        
         CLI   THISPAGE,5                                                       
         BL    CPP                                                              
         L     R3,=A(THISMGP)                                                   
         LA    R3,48(R3)                                                        
         LA    R4,THISMGD                                                       
         LA    R4,48(R4)                                                        
         BE    CPP                                                              
         L     R3,=A(THISQGP)                                                   
         LA    R3,16(R3)                                                        
         LA    R4,THISQGD                                                       
         LA    R4,16(R4)                                                        
         B     CPP                                                              
         SPACE 2                                                                
EDIT56   L     R3,ATHISAP          ACTUAL COST PER POINT                        
         LA    R3,68(R3)                                                        
         L     R4,ATHISAD                                                       
         LA    R4,68(R4)                                                        
         CLI   THISPAGE,5                                                       
         BL    CPP                                                              
         L     R3,=A(THISMAP)                                                   
         LA    R3,48(R3)                                                        
         L     R4,=A(THISMAD)                                                   
         LA    R4,48(R4)                                                        
         BE    CPP                                                              
         L     R3,=A(THISQAP)                                                   
         LA    R3,16(R3)                                                        
         L     R4,=A(THISQAD)                                                   
         LA    R4,16(R4)                                                        
         B     CPP                                                              
         EJECT                                                                  
*              ROUTINE TO UNWEIGHT A DETAIL LINE                                
         SPACE 3                                                                
UNWEIGHT NTR1                                                                   
         CLI   SPOTPROF+1,C'N'                                                  
         BE    XIT                                                              
         MVC   WORK(1),THISDEM                                                  
         BAS   RE,DEMEXP                                                        
         CLI   WORK,C'R'           ONLY INTERESTED IN RATINGS                   
         BE    UNWB                                                             
         CLI   WORK,C'E'           OR EXTENDED RATINGS                          
         BE    UNWB                                                             
         CLI   QOPT2,C'C'                                                       
         BNE   XIT                 (CHILDSPOT SPECIAL)                          
         SPACE 2                                                                
UNWB     L     R3,SPWEIGHT         R3=TOTAL WEIGHTS                             
         SPACE 2                                                                
         SPACE 2                                                                
UNWA     BAS   RE,UNDO                                                          
         B     XIT                                                              
         SPACE 2                                                                
UNDO     NTR1                                                                   
         CLI   THISPAGE,5                                                       
         BE    UNDO2                                                            
         BH    UNDO4                                                            
         L     R4,ATHISGP          WEEKLY                                       
         LA    R5,18                                                            
         CLI   MEDDAILY,C'Y'       OR DAILY                                     
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         BAS   RE,UNDO6                                                         
         L     R4,ATHISAP                                                       
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO2    L     R4,=A(THISMGP)      MONTHLY                                      
         LA    R5,14                                                            
         BAS   RE,UNDO6                                                         
         L     R4,=A(THISMAP)                                                   
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO4    L     R4,=A(THISQGP)                                                   
         LA    R5,5                                                             
         BAS   RE,UNDO6                                                         
         L     R4,=A(THISQAP)                                                   
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO6    NTR1                                                                   
         LTR   R3,R3                                                            
         BNZ   UNDO8                                                            
         SPACE 2                                                                
UNDO7    XC    0(4,R4),0(R4)       ZERO WEIGHT MEANS SUPPRESS POINTS            
         LA    R4,4(R4)                                                         
         BCT   R5,UNDO7                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO8    L     R0,0(R4)                                                         
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,UNDO8                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE FLIGHT TOTALS                                  
         SPACE 3                                                                
FLIGHT   NTR1                                                                   
         CLI   THISPAGE,4                                                       
         BH    XIT                                                              
         CLI   QOPT3,C'4'          ACTUAL ONLY DOESNT NEED FLIGHTS              
         BE    XIT                                                              
         CLI   QOPT3,C'5'                                                       
         BE    XIT                                                              
         CLI   QOPT3,C'6'                                                       
         BE    XIT                                                              
         XC    FLTOT1(16),FLTOT1                                                
         CLI   THISDEM,X'FF'                                                    
         BNE   XIT                                                              
         CLI   PROGPROF+5,C'Y'                                                  
         BNE   XIT                                                              
         LA    R2,THISGD           POSITION TO WEEK 1                           
         L     R3,ATHISGP                                                       
         L     R4,ATHISAD                                                       
         L     R5,ATHISAP                                                       
         LA    R6,1                                                             
         SPACE 2                                                                
FLIGHT2  CH    R6,=H'14'           ARE WE AT WEEK 14                            
         BE    FLIGHT4                                                          
         CLC   0(4,R2),4(R2)       ARE GOAL DOLLRS FOR WEEKN=WEEKN+1            
         BNE   FLIGHT4                                                          
         CLC   0(4,R3),4(R3)       ARE GOAL POINTS FOR WEEKN=WEEKN+1            
         BNE   FLIGHT4                                                          
         L     R1,0(R2)            YES SO ADD INTO FLIGHT TOTALS                
         A     R1,FLTOT1                                                        
         ST    R1,FLTOT1                                                        
         L     R1,0(R3)                                                         
         A     R1,FLTOT2                                                        
         ST    R1,FLTOT2                                                        
         L     R1,0(R4)                                                         
         A     R1,FLTOT3                                                        
         ST    R1,FLTOT3                                                        
         L     R1,0(R5)                                                         
         A     R1,FLTOT4                                                        
         ST    R1,FLTOT4                                                        
         XC    0(4,R2),0(R2)       AND CLEAR WEEK TOTALS                        
         XC    0(4,R3),0(R3)                                                    
         XC    0(4,R4),0(R4)                                                    
         XC    0(4,R5),0(R5)                                                    
         B     FLIGHT6                                                          
         SPACE 2                                                                
FLIGHT4  L     R1,0(R2)            NO  SO ADD IN PREVIOUS FOR FLIGHT            
         A     R1,FLTOT1                                                        
         ST    R1,0(R2)                                                         
         L     R1,0(R3)                                                         
         A     R1,FLTOT2                                                        
         ST    R1,0(R3)                                                         
         L     R1,0(R4)                                                         
         A     R1,FLTOT3                                                        
         ST    R1,0(R4)                                                         
         L     R1,0(R5)                                                         
         A     R1,FLTOT4                                                        
         ST    R1,0(R5)                                                         
         XC    FLTOT1(16),FLTOT1   AND CLEAR FLIGHT TOTALS                      
         CH    R6,=H'14'                                                        
         BE    FLIGHT8                                                          
         SPACE 2                                                                
FLIGHT6  LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         B     FLIGHT2                                                          
         SPACE 2                                                                
FLIGHT8  L     R6,PAGEMARK                                                      
         MVC   0(13,R6),=C'FLIGHT TOTALS'                                       
         BAS   RE,EDIT30                                                        
         BAS   RE,CHUNK                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COMPUTE DISPLACEMENT OF WEEKS                         
         SPACE 3                                                                
COMPDISP NTR1                                                                   
         MVC   COLDISP,=F'6'                                                    
         XC    DISP,DISP                                                        
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         CLI   THISPAGE,5                                                       
         BH    XIT                                                              
         BE    MONDISP                                                          
         SR    R2,R2                                                            
         IC    R2,THISPAGE         (1-4)                                        
         BCTR  R2,0                (0-3)                                        
         MH    R2,=H'15'           DISPLACE TO WEEK LIST                        
         MH    R2,=H'12'                                                        
         LA    R2,MEDWEEKS(R2)                                                  
         LA    R2,156(R2)          POSITION TO WEEK 14                          
         OC    0(4,R2),0(R2)                                                    
         BNZ   XIT                 IF ITS THERE, NO DISPLACEMENT                
         LA    R3,6                IF NOT DISPLACE AT LEAST 6                   
         LA    R4,14                                                            
         SPACE 2                                                                
CMPD2    SH    R2,=H'12'                                                        
         OC    0(4,R2),0(R2)                                                    
         BNZ   CMPD3                                                            
         LA    R3,3(R3)            PLUS 3 FOR OTHER INACTIVE WEEKS              
         BCT   R4,CMPD2                                                         
         SPACE 2                                                                
CMPD3    CH    R4,=H'11'           IF NO MORE THAN 11 WEEKS                     
         BH    CMPD4                                                            
         MVC   COLDISP,=F'7'       SPACE COLUMNS 7 APART                        
         LR    R5,R4                                                            
         SRL   R5,1                                                             
         SR    R3,R5               AND ADJUST DISPLACEMENT                      
         CH    R4,=H'9'            IF NO MORE THAN 9 WEEKS                      
         BH    CMPD4                                                            
         MVC   COLDISP,=F'8'       SPACE COLUMNS 8 APART                        
         SR    R3,R5               AND ADJUST DISPLACEMENT                      
         SPACE 2                                                                
CMPD4    ST    R3,DISP                                                          
         B     XIT                                                              
         SPACE 2                                                                
MONDISP  LA    R2,MEDMON12                                                      
         SR    R3,R3                                                            
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+10                                                             
         MVC   COLDISP,=F'9'                                                    
         SPACE 2                                                                
CMPD6    OC    0(4,R2),0(R2)                                                    
         BNZ   CMPD4                                                            
         LA    R3,4(R3)            DISPLACE 4 FOR INACTIVE MONTHS               
         SH    R2,=H'12'                                                        
         B     CMPD6                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              NUMBER EDITING LOOPS                                             
         SPACE 3                                                                
EDWEEK   LA    R4,16(R6)                                                        
         A     R4,DISP                                                          
         LA    R5,14                                                            
         SPACE 2                                                                
EDWEEK2  BAS   RE,LOAD                                                          
         EDIT  (R1),(8,DMCB)                                                    
         MVC   0(6,R4),DMCB+2                                                   
         CLI   DMCB+2,C' '         WILL IT FIT ANYWAY                           
         BE    EDWEEK3                                                          
         SH    R4,=H'2'            IS THE PREVIOUS FIELD FILLED                 
         CLC   0(2,R4),SPACES                                                   
         BNE   EDWEEK2B                                                         
         MVC   0(8,R4),DMCB        NO - SO ITS OK TO MOVE FIELD IN              
         LA    R4,2(R4)                                                         
         B     EDWEEK3                                                          
         SPACE 2                                                                
EDWEEK2B MVC   132(8,R4),DMCB      YES - SO WE'LL HAVE TO PUT IT BELOW          
         LA    R4,2(R4)                                                         
         MVC   0(6,R4),SPACES                                                   
         SPACE 2                                                                
EDWEEK3  LA    R3,4(R3)                                                         
         A     R4,COLDISP                                                       
         BCT   R5,EDWEEK2                                                       
         LA    R4,100(R6)                                                       
         LA    R5,4                                                             
         SPACE 2                                                                
EDWEEK4  BAS   RE,LOAD                                                          
         EDIT  (R1),(8,0(R4))                                                   
         LA    R3,4(R3)                                                         
         LA    R4,8(R4)                                                         
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         CLI   MEDDAILY,C'Y'                                                    
         BNE   EDWEEK6                                                          
         CH    R5,=H'3'                                                         
         BNE   EDWEEK6                                                          
         BCTR  R5,0                                                             
         LA    R4,8(R4)                                                         
EDWEEK6  BCT   R5,EDWEEK4                                                       
         B     EDBUMP                                                           
         DROP  R6                                                               
         SPACE 2                                                                
EDMON    CLI   QOPT1,C'Y'          YEAR TO DATE OPTION                          
         BE    EDYTD                                                            
         CLI   QOPT2,C'Y'                                                       
         BNE   EDMON1                                                           
         SPACE 2                                                                
EDYTD    SR    R0,R0               R0=CUMULATIVE UNITS                          
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         LA    R4,MEDMON01                                                      
         DROP  R1                                                               
         LR    R1,R3                                                            
         LA    R5,13                                                            
         SPACE 2                                                                
EDYTD2   OC    0(4,R4),0(R4)       WAS MONTH REQUESTED                          
         BZ    EDYTD4                                                           
         A     R0,0(R3)            MAKE THIS MONTH                              
         ST    R0,0(R3)                 INTO YTD                                
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,EDYTD2                                                        
         SPACE 2                                                                
EDYTD4   LR    R3,R1                                                            
         SPACE 2                                                                
EDMON1   LA    R4,16(R6)                                                        
         A     R4,DISP                                                          
         LA    R5,13                                                            
         SPACE 2                                                                
EDMON2   BAS   RE,LOAD                                                          
         EDIT  (R1),(8,0(R4))                                                   
         LA    R3,4(R3)                                                         
         LA    R4,9(R4)                                                         
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         OC    MEDMON13(4),MEDMON13                                             
         BZ    *+6                                                              
         BCTR  R4,0                                                             
         DROP  R1                                                               
         BCT   R5,EDMON2                                                        
         BAS   RE,LOAD                                                          
         EDIT  (R1),(9,123(R6))                                                 
         B     EDBUMP                                                           
         SPACE 2                                                                
EDQUT    LA    R4,22(R6)                                                        
         LA    R5,5                                                             
         SPACE 2                                                                
EDQUT2   BAS   RE,LOAD4            GET TIME DOLLARS                             
         EDIT  (R1),(11,0(R4))                                                  
* GET THE TALENT FACTOR NOW !                                                   
         M     R0,TALFAC           MAY NEED TO UP FOR TIME+TALENT               
         D     R0,TALBASE                                                       
*                                                                               
         CLI   EDQUTGD,C'Y'                                                     
         BNE   EDQUT20                                                          
         CLI   THISPRD1,X'FF'      TEST PGR OR CLT TOTAL                        
         BE    EDQUT10                                                          
         CLI   THISDEM,X'FF'       TEST CLT TOTAL                               
         BE    EDQUT12                                                          
* ADD VALUE TO PGR AND CLT ACCUM                                                
         L     RF,=A(THISQGD)                                                   
         SR    RF,R3               GET CURRENT DSPL                             
         LPR   RF,RF                                                            
*                                                                               
         L     R0,PGRTOTS(RF)      GET ACCUM VALUE                              
         AR    R0,R1               ADD CURRENT                                  
         ST    R0,PGRTOTS(RF)                                                   
*                                                                               
         L     R0,CLTTOTS(RF)      GET ACCUM VALUE                              
         AR    R0,R1               ADD CURRENT                                  
         ST    R0,CLTTOTS(RF)                                                   
         B     EDQUT20                                                          
*                                                                               
EDQUT10  LA    RE,PGRTOTS          DIG THE VALUE OUT OF OUR ACCUM               
         B     *+8                                                              
EDQUT12  LA    RE,CLTTOTS                                                       
         L     RF,=A(THISQGD)      GET R3 BASE ADDRESS                          
         SR    RF,R3                                                            
         LPR   RF,RF               THIS GIVES DSPL                              
         LA    RE,0(RE,RF)         POINT TO VALUE                               
         L     R1,0(RE)            CHANGE R1 VALUE                              
         XC    0(4,RE),0(RE)       AND CLEAR                                    
*                                                                               
EDQUT20  CLI   QOPT2,C'C'                                                       
         BE    *+6                                                              
         SR    R1,R1                                                            
         CLI   1(R2),C'P'                                                       
         BNE   *+6                                                              
         SR    R1,R1                                                            
EDQUT22  DS    0H                                                               
         EDIT  (R1),(11,11(R4))                                                 
         LA    R3,4(R3)                                                         
         LA    R4,22(R4)                                                        
         BCT   R5,EDQUT2                                                        
         MVI   EDQUTGD,C'N'        UNSET FLAG                                   
         B     EDBUMP                                                           
EDQUTGD  DC    X'00'                                                            
         EJECT                                                                  
*              PERCENT EDITING LOOPS                                            
         SPACE 3                                                                
PERWEEK  LA    R6,16(R6)                                                        
         A     R6,DISP                                                          
         LA    R5,14                                                            
         SPACE 2                                                                
PERWEEK2 BAS   RE,PCT                                                           
         MVC   0(6,R6),WORK                                                     
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         A     R6,COLDISP                                                       
         BCT   R5,PERWEEK2                                                      
         L     R6,PAGEMARK                                                      
         LA    R6,100(R6)                                                       
         LA    R5,4                                                             
         SPACE 2                                                                
PERWEEK4 BAS   RE,PCT                                                           
         MVC   2(6,R6),WORK                                                     
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,PERWEEK4                                                      
         B     EDBUMP                                                           
         SPACE 2                                                                
PERMON   LA    R6,16(R6)                                                        
         A     R6,DISP                                                          
         LA    R5,13                                                            
         SPACE 2                                                                
PERMON2  BAS   RE,PCT                                                           
         MVC   2(6,R6),WORK                                                     
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R6,9(R6)                                                         
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         OC    MEDMON13(4),MEDMON13                                             
         BZ    *+6                                                              
         BCTR  R6,0                                                             
         DROP  R1                                                               
         BCT   R5,PERMON2                                                       
         BAS   RE,PCT                                                           
         L     R6,PAGEMARK                                                      
         MVC   126(6,R6),WORK                                                   
         B     EDBUMP                                                           
         SPACE 2                                                                
PERQUT   LA    R6,22(R6)                                                        
         LA    R5,5                                                             
         SPACE 2                                                                
PERQUT2  BAS   RE,PCT                                                           
         MVC   5(6,R6),WORK                                                     
         CLI   QOPT2,C'C'                                                       
         BNE   *+10                                                             
         MVC   16(6,R6),WORK                                                    
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R6,22(R6)                                                        
         BCT   R5,PERQUT2                                                       
         B     EDBUMP                                                           
         EJECT                                                                  
*              SUBSIDIARY FACILITIES FOR EDITING                                
         SPACE 3                                                                
EDBUMP   CLI   TPWRITE,C'Y'        TEST FOR TAPE WRITE                          
         BNE   EDBUMP2                                                          
         CLI   MODE,MKTLAST        YES - ONLY ON MARKET DETAIL                  
         BH    EDBUMP2                                                          
         CLC   =C'AP',0(R2)              AND ONLY PP AND P$                     
         BE    *+14                                                             
         CLC   =C'A$',0(R2)                                                     
         BNE   EDBUMP2                                                          
         ZIC   R0,1(R2)            POINTS/DOLLARS INDICATOR                     
         LR    R5,R1               R1 HAS TOTAL POINTS/DOLLARS                  
         GOTO1 =V(SPM8TP),DMCB,((R0),(RA)),MYBUFFIO,(R5)                        
*                                                                               
EDBUMP2  L     R6,PAGEMARK                                                      
         LA    R6,132(R6)                                                       
         ST    R6,PAGEMARK                                                      
         CLC   0(132,R6),SPACES                                                 
         BNE   EDBUMP                                                           
         B     EDIT34                                                           
         SPACE 2                                                                
LOAD     L     R1,0(R3)                                                         
         LPR   R1,R1                                                            
         SR    R0,R0                                                            
         TM    0(R3),X'C0'                                                      
         BNM   LOAD1                                                            
         TM    0(R3),X'80'                                                      
         BNO   LOAD1                                                            
         L     R1,0(R3)                                                         
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         CLI   1(R2),C'+'          CHILD SPOT FEATURE                           
         BNE   *+12                                                             
         M     R0,TALFAC                                                        
         D     R0,TALBASE                                                       
         M     R0,=F'100'                                                       
         B     LOAD6                                                            
         SPACE 2                                                                
LOAD1    CLI   1(R2),C'+'          CHILD SPOT FEATURE                           
         BNE   LOAD6                                                            
         M     R0,TALFAC                                                        
         D     R0,TALBASE                                                       
         SR    R0,R0                                                            
         B     LOAD6                                                            
         SPACE 2                                                                
LOAD4    L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         TM    0(R3),X'C0'                                                      
         BNM   LOAD6                                                            
         TM    0(R3),X'80'                                                      
         BNO   LOAD6                                                            
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         M     R0,=F'100'                                                       
         SPACE 2                                                                
LOAD6    LA    RF,5                SCALE TO POINT OR DOLLARS ROUNDED            
         CLI   1(R2),C'P'                                                       
         BE    *+8                                                              
         LA    RF,50                                                            
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         L     R0,REQPCT           OPTIONAL ADJUSTMENT TO POINTS                
         CLI   1(R2),C'P'                                                       
         BE    *+8                                                              
         L     R0,CSHPCT           OR DOLLARS                                   
         LR    RF,R1                                                            
         MR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         AR    R1,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
PCT      MVC   WORK(6),SPACES      PERCENTAGE EDITING                           
         CLI   0(R2),C'D'                                                       
         BE    DIFF                                                             
         OC    0(4,R3),0(R3)                                                    
         BCR   8,RE                                                             
         OC    0(4,R4),0(R4)                                                    
         BCR   8,RE                                                             
         XC    BIGNUM,BIGNUM                                                    
         CVDX  BIGDUB,0(R3)                                                     
         CVDX  DUB,0(R4)                                                        
         MP    BIGNUM,=P'1000'                                                  
         DP    BIGNUM,DUB                                                       
         EDIT  (P7,BIGNUM),(6,DMCB),1                                           
         MVC   WORK(6),DMCB                                                     
         CLI   WORK,C' '                                                        
         BCR   8,RE                                                             
         MVC   WORK(6),=C'  HIGH'                                               
         BR    RE                                                               
         SPACE 2                                                                
DIFF     ST    RE,DIFFRE                                                        
         ST    R3,DIFFR3                                                        
         LR    R3,R4                                                            
         BAS   RE,LOAD                                                          
         ST    R1,DIFFR1                                                        
         L     R3,DIFFR3                                                        
         BAS   RE,LOAD                                                          
         S     R1,DIFFR1                                                        
         EDIT  (R1),(6,DMCB),FLOAT=-                                            
         MVC   WORK(6),DMCB                                                     
         L     RE,DIFFRE                                                        
         BR    RE                                                               
         SPACE 2                                                                
DIFFR1   DS    F                                                                
DIFFR3   DS    F                                                                
DIFFRE   DS    F                                                                
         SPACE 1                                                                
CPP      OC    0(4,R3),0(R3)       R3 HAS A(POINTS 1 DEC)                       
         BZ    EDIT34              R4 HAS A(CASH - 2 DEC)                       
         OC    0(4,R4),0(R4)                                                    
         BZ    EDIT34                                                           
         L     R1,0(R4)                                                         
         L     R0,CSHPCT           POSSIBLE CASH ADJUSTMENT                     
         LR    RF,R1                                                            
         MR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         AR    R1,RF                                                            
         M     R0,=F'20'                                                        
         L     RF,0(R3)            POSSIBLE POINT ADJUSTMENT                    
         M     RE,REQPCT                                                        
         D     RE,=F'10000'                                                     
         A     RF,0(R3)                                                         
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   THISDEM,X'FF'                                                    
         BE    EDIT34                                                           
         EDIT  (R1),(8,6(R6)),2,ALIGN=LEFT                                      
         MVI   5(R6),C'('                                                       
         LA    R1,6(R6)                                                         
         SPACE 2                                                                
CPP2     CLI   0(R1),C' '                                                       
         BE    CPP4                                                             
         LA    R1,1(R1)                                                         
         B     CPP2                                                             
         SPACE 2                                                                
CPP4     MVI   0(R1),C')'                                                       
         B     EDIT34                                                           
         EJECT                                                                  
*              PAGE MAINTENANCE                                                 
         SPACE 3                                                                
PAGESET  NTR1                                                                   
         L     R2,=A(EDBLOCK)      CLEAR A PAGE                                 
         A     R2,RELO                                                          
         ST    R2,PAGEMARK                                                      
         LA    R3,50                                                            
         SPACE 2                                                                
PAGESET2 MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,PAGESET2                                                      
         B     XIT                                                              
         SPACE 2                                                                
CHUNK    NTR1                                                                   
         CLI   CHUNKSW,C'N'                                                     
         BE    XIT                                                              
         L     R2,=A(EDBLOCK)                                                   
         A     R2,RELO                                                          
         LA    R3,49               DISPLACE TO LAST LINE                        
         MH    R3,=H'132'                                                       
         AR    R3,R2                                                            
         LA    R4,50               FIND HOW MANY WE NEED TO PRINT               
         SPACE 2                                                                
CHUNK2   CLC   0(132,R3),SPACES                                                 
         BNE   CHUNK4                                                           
         SH    R3,=H'132'                                                       
         BCT   R4,CHUNK2                                                        
         B     XIT                 (NONE)                                       
         SPACE 2                                                                
CHUNK4   CLI   MCOMSW,C'Y'         TEST MARKET COMMENTS WAITING                 
         BNE   *+8                                                              
         BAS   RE,PRTMCOM          YES-PRINT THEM NOW                           
         STC   R4,ALLOWLIN         ENSURE BLOCK WILL FIT                        
         SPACE 2                                                                
CHUNK6   MVC   P(132),0(R2)        NOW OUTPUT AND PRINT                         
         GOTO1 REPORT                                                           
         MVI   ALLOWLIN,0                                                       
         LA    R2,132(R2)                                                       
         BCT   R4,CHUNK6                                                        
         CLI   THISPRD1,X'FF'                                                   
         BE    CHUNK8                                                           
         CLI   QOPT4,C'N'                                                       
         BNE   CHUNK8                                                           
         CLI   QDPTDET,C'C'                                                     
         BE    XIT                                                              
         SPACE 2                                                                
CHUNK8   GOTO1 REPORT              USUALLY SPACE AFTER CHUNK                    
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK ROUTINES                                           
         SPACE 3                                                                
         DROP  R7                                                               
         DROP  R9                                                               
         DROP  RB                                                               
         USING *,RF                                                             
         SPACE 2                                                                
PLANHOOK NTR1                                                                   
         LM    R9,RC,SPM8R9                                                     
         L     R7,SPM8R7                                                        
         DROP  RF                                                               
         USING SPM802,RB,R9,R7                                                  
         GOTO1 =A(HEDHK),DMCB,DISP,COLDISP                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MARKET COMMENTS                                      *         
***********************************************************************         
         SPACE 1                                                                
GETMCOM  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING COMHDRD,R2                                                       
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,BAGYMD                                                   
         MVI   COMCTYPE,C'M'                                                    
         MVC   COMKCLT,BCLT                                                     
         MVC   COMKPRD+2(1),BPRD                                                
         MVC   COMKEST,BEST                                                     
         MVC   COMKMKT,BMKT                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST COMMENT EXISTS                          
         BE    GETM2                                                            
         MVC   KEY,KEYSAVE         RESTORE                                      
         XC    COMKMKT,COMKMKT     TRY FOR EST LEVEL COMMENT                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETMX               NO-NO PRINT                                  
GETM2    L     R2,ADCOMREC         GET COMMENT HEADER                           
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
         MVI   MCOMSW,C'Y'                                                      
GETMX    B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PRINT MARKET COMMENTS                                    *         
***********************************************************************         
         SPACE 1                                                                
PRTMCOM  NTR1  ,                                                                
         MVI   MCOMSW,C'N'                                                      
         L     R2,ADCOMREC                                                      
         LA    R2,24(R2)                                                        
         SR    R0,R0                                                            
*                                                                               
PRTM2    CLI   0(R2),0             FIND COMMENT ELEMENT                         
         BNE   PRTM4                                                            
         GOTO1 REPORT              PRINT BLANK LINE AFTER COMMENTS              
         B     PRTMX                                                            
*                                                                               
PRTM4    CLI   0(R2),5                                                          
         BE    PRTM8                                                            
*                                                                               
PRTM6    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PRTM2                                                            
*                                                                               
PRTM8    ZIC   RE,1(R2)                                                         
         SH    RE,=H'2'                                                         
         BNP   PRTM6                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),2(R2)                                                       
         GOTO1 REPORT                                                           
         B     PRTM6                                                            
*                                                                               
PRTMX    B     XIT                                                              
         EJECT                                                                  
*              STORAGE FOR PROGRAM                                              
         SPACE 3                                                                
ASPM804  DS    A                                                                
ASPM8041 DC    A(0)                                                             
ATHISGP  DS    A                                                                
ATHISAD  DS    A                                                                
ATHISAP  DS    A                                                                
PAGEMARK DS    F                                                                
SLCOUNT  DS    CL1                                                              
DPCOUNT  DS    CL1                                                              
SPILLOPT DS    CL1                                                              
CENT     DC    F'0'                                                             
TALFAC   DC    F'0'                                                             
TALBASE  DC    F'0'                                                             
*                                                                               
FLTOT1   DC    F'0'                                                             
FLTOT2   DC    F'0'                                                             
FLTOT3   DC    F'0'                                                             
FLTOT4   DC    F'0'                                                             
SPM8R7   DS    F                                                                
SPM8R9   DS    F                                                                
SPM8RA   DS    F                                                                
SPM8RB   DS    F                                                                
SPM8RC   DS    F                                                                
COLDISP  DC    F'6'                                                             
DISP     DC    F'0'                                                             
BUFFLEVL DC    F'0'                                                             
DPFILT   DS    CL3                                                              
REQPCT   DS    F                                                                
CSHPCT   DS    F                                                                
LASTKEY  DC    CL12' '                                                          
CHUNKSW  DC    C'Y'                                                             
FLTSW    DC    C'N'                                                             
PDGCOUNT DC    PL4'0'                                                           
PRDCNT   DC    X'00'                                                            
         DS    0D                                                               
         DS    CL1                                                              
BIGNUM   DS    PL15                                                             
         ORG   BIGNUM+7                                                         
BIGDUB   DS    D                                                                
DLIST    DS    CL255                                                            
PDLIST   DS    CL180                                                            
SVBRAND  DS    C                                                                
SVPIGGY  DS    C                                                                
MCOMSW   DS    C                                                                
*                                                                               
DASHES   DC    18C'-'                                                           
         EJECT                                                                  
*              LTORG / CSECTS / DSECTS/ ETC                                     
         SPACE 3                                                                
RELO     DS    A                                                                
LNBUFFER DS    F                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUFFALO RECORD AREA                                              
         SPACE 3                                                                
MYBUFFIO DS    1452C                                                            
         ORG   MYBUFFIO                                                         
THISKEY  DS    0CL12                                                            
THISPAGE DS    CL1                                                              
THISDEM  DS    CL1                                                              
THISPRD1 DS    CL1                                                              
THISPRD2 DS    CL1                                                              
THISDPT  DS    CL4                                                              
THISTLN  DS    CL1                                                              
THISSLN1 DS    CL1                                                              
THISSLN2 DS    CL1                                                              
THISTYPE DS    CL1                                                              
         SPACE 2                                                                
THISGD   DS    CL72                                                             
THISGP   DS    CL72                                                             
THISAD   DS    CL72                                                             
THISAP   DS    CL72                                                             
THISAPX  EQU   *                                                                
         SPACE 2                                                                
         ORG   THISGD                                                           
THISGDD  DS    CL68                DAILY ACCUMULATORS                           
THISGPD  DS    CL68                                                             
THISADD  DS    CL68                                                             
THISAPD  DS    CL68                                                             
         SPACE 2                                                                
         ORG   THISGD                                                           
THISMGD  DS    CL56                                                             
THISMGP  DS    CL56                                                             
THISMAD  DS    CL56                                                             
THISMAP  DS    CL56                                                             
         SPACE 2                                                                
         ORG   THISGD                                                           
THISQGD  DS    CL20                                                             
THISQGP  DS    CL20                                                             
THISQAD  DS    CL20                                                             
THISQAP  DS    CL20                                                             
THISQCP  DS    CL20                                                             
THISQCN  DS    CL20                                                             
THISQCNX EQU   *                                                                
         ORG                                                                    
         EJECT                                                                  
PSLIST   DS    CL512                                                            
*                                                                               
         DS    0D                                                               
PGRTOTS  DS    CL20                                                             
PGRTOTX  EQU   *                                                                
CLTTOTS  DS    CL20                                                             
CLTTOTX  EQU   *                                                                
*                                                                               
M8041BLK CSECT                                                                  
         DS    10000C                                                           
*                                                                               
EDBLOCK  CSECT                                                                  
         DS    50CL132                                                          
         BUFF  LINES=1,ROWS=5,COLUMNS=72,FLAVOR=BINARY,KEYLIST=(12,A)           
         SPACE 2                                                                
         ENTRY ACTPOOL                                                          
         ENTRY LODAREA                                                          
ACTPOOL  DS    0D                                                               
         DC    F'800'                                                           
         DC    F'0'                                                             
         DC    4800X'00'                                                        
LODAREA  DS    0D                                                               
         DS    CL768                                                            
         EJECT                                                                  
* HEADLINE HOOK ROUTINE                                                         
*                                                                               
         SPACE 1                                                                
HEDHK    NMOD1 0,**M8HK**                                                       
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LM    RE,RF,0(R1)                                                      
         MVC   HKDISP,0(RE)                                                     
         MVC   HKCOLDSP,0(RF)                                                   
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   H10,SPACES                                                       
         MVC   H11,SPACES                                                       
         MVC   H12,SPACES                                                       
         MVC   H13,SPACES                                                       
*                                                                               
         LA    R4,H1+40                                                         
         SH    R4,=H'5'                                                         
         MVC   0(4,R4),PWPREFIX                                                 
*                                                                               
         LA    R5,62               CENTER ON H1+66                              
         GOTO1 SQUASHER,DMCB,(R4),(R5)                                          
         L     R0,4(R1)            GET SQUASHED LENGTH                          
         GOTO1 UNDERLIN,(R1),((R0),(R4)),132(R4)                                
         GOTO1 CENTER,DMCB,(R4),(R5)                                            
         LA    R4,132(R4)                                                       
         GOTO1 (RF),(R1),(R4),(R5)                                              
         EJECT                                                                  
*              LEFT HAND STUBS & GOAL EDITING                                   
         SPACE 3                                                                
         MVI   H10,C'-'            SELECT STUBS                                 
         MVC   H10+1(13),H10                                                    
         MVC   H11(7),=C'PRODUCT'                                               
         MVC   H12+5(9),=C'(CPP/CPM)'                                           
         CLI   QOPT3,C'2'          OK FOR SPACE, 1                              
         BL    HK1                                                              
         CLI   QOPT3,C'4'          OR 4                                         
         BE    HK1                                                              
         MVC   H12+5(9),SPACES                                                  
         SPACE 2                                                                
HK1      MVC   H13(14),H10                                                      
         CLI   QDPTDET,C'C'                                                     
         BE    HK2                                                              
         MVC   H11(8),=C'PRD  DPT'                                              
         CLI   QDPTDET,C'B'                                                     
         BE    HK2                                                              
         MVC   H11+8(4),=C'-LEN'                                                
         SPACE 2                                                                
HK2      L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
         MVC   SPUSRNMS,EUSRNMS                                                 
         MVC   HKAREA,SPACES                                                    
         LA    R2,EDEMOS                                                        
         DROP  R2                                                               
         LA    R3,HKAREA                                                        
         LA    R4,14                                                            
         CLC   PRDBUFLN,=H'56'                                                  
         BNH   HK4                                                              
         SPACE 1                                                                
HK3      OC    0(3,R2),0(R2)       NEW EST HEADER                               
         BZ    HK6                                                              
         L     RE,=A(SVNONTDMS)       POINT TO NONT DEMO SAVE AREA              
         ST    RE,DMCB+16             AND SET AS PARAM5                         
         GOTO1 DEMOCON,DMCB,(0,(R2)),(2,(R3)),(C'S',ADBLOCK),          X        
               (SPOTPROF+9,SPUSRNMS)                                            
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,HK3                                                           
         B     HK6                                                              
         SPACE 2                                                                
HK4      CLI   0(R2),0                                                          
         BE    HK6                                                              
         MVC   0(7,R3),2(R2)                                                    
         LA    R2,9(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,HK4                                                           
         SPACE 2                                                                
HK6      CLC   H7(20),SPACES       SHOW GOALS ON MID-LINES IF NO ROOM           
         BE    HK8                                                              
         MVC   MID1(7),=C'GOALS -'                                              
         GOTO1 CHOPPER,HKPARA,(112,HKAREA),(56,MID1+9),(C'P',2)                 
         B     HK8B                                                             
         SPACE 2                                                                
HK8      GOTO1 CHOPPER,HKPARA,(112,HKAREA),(39,H7+9),(C'P',3)                   
         MVC   H7(8),=C'GOALS - '                                               
         SPACE 2                                                                
HK8B     JIF   QOPT3,EQ,C'4',OR,C'5',OR,C'6',OR,C'B',HK9,JUMP=N                 
         JIF   QOPT3,EQ,C'7',OR,C'8',OR,C'9',OR,C'C',HK10,JUMP=N                
         JIF   QOPT3,EQ,C'J',OR,C'K',OR,C'L',HK10B,JUMP=N                       
         B     HK11                                                             
         SPACE 2                                                                
HK9      MVC   H7+99(13),=C'(P=PURCHASED)'                                      
         B     HK11                                                             
         SPACE 2                                                                
HK10     MVC   H7+99(28),=C'(G=GOAL P=PURCHASED I=INDEX)'                       
         B     HK11                                                             
         SPACE 2                                                                
HK10B    MVC   H7+99(27),=C'(G=GOAL P=PURCHASED D=DIFF)'                        
         EJECT                                                                  
*              QUARTER ROUTINES                                                 
         SPACE 3                                                                
HK11     CLI   THISPAGE,5                                                       
         BL    HK30                                                             
         BE    HK20                                                             
         CLI   QOPT2,C'C'          REGULAR QUARTER RECAP                        
         BE    HK12                                                             
         MVC   H11+27(5),=C'FIRST'                                              
         MVC   H11+48(6),=C'SECOND'                                             
         MVC   H11+71(5),=C'THIRD'                                              
         MVC   H11+92(7),=C'FOURTH'                                             
         MVC   H12+26(6),=C'PERIOD'                                             
         MVC   H12+48(6),=C'PERIOD'                                             
         MVC   H12+70(6),=C'PERIOD'                                             
         MVC   H12+70(6),=C'PERIOD'                                             
         MVC   H12+92(6),=C'PERIOD'                                             
         MVC   H10+115(6),=C'------'                                            
         MVC   H11+115(6),=C'ANNUAL'                                            
         MVC   H12+115(6),=C'TOTALS'                                            
         MVC   H13+115(6),=C'------'                                            
         B     HK14                                                             
         SPACE 2                                                                
HK12     MVI   H10+115,C'-'        CHILD SPOT QUARTER RECAP                     
         MVC   H10+116(16),H10+115                                              
         MVC   H13+115(17),H10+115                                              
         MVC   H11+029(12),=C'FIRST PERIOD'                                     
         MVC   H11+050(13),=C'SECOND PERIOD'                                    
         MVC   H11+073(12),=C'THIRD PERIOD'                                     
         MVC   H11+094(13),=C'FOURTH PERIOD'                                    
         MVC   H11+120(06),=C'ANNUAL'                                           
         MVC   H12+027(17),=C'TIME $    +TALENT'                                
         MVC   H12+049(17),=C'TIME $    +TALENT'                                
         MVC   H12+071(17),=C'TIME $    +TALENT'                                
         MVC   H12+093(17),=C'TIME $    +TALENT'                                
         MVC   H12+115(17),=C'TIME $    +TALENT'                                
         SPACE 2                                                                
HK14     DS    0H                                                               
         GOTO1 UNDERLIN,HKPARA,(84,H12+26),H10+26                               
         MVC   H13+26(84),H10+26                                                
         B     HKXIT                                                            
         EJECT                                                                  
*              MONTHLY RECAP EDITING                                            
         SPACE 3                                                                
HK20     MVC   H10+126(6),=C'------'                                            
         MVC   H11+126(6),=C'PERIOD'                                            
         MVC   H12+126(6),=C'TOTALS'                                            
         MVC   H13+126(6),=C'------'                                            
         LA    R2,MEDMON01                                                      
         LA    R3,H11+18                                                        
         A     R3,HKDISP                                                        
         LA    R4,13                                                            
         SPACE 2                                                                
HK22     OC    0(2,R2),0(R2)                                                    
         BZ    HK24                                                             
         MVC   HKSE,0(R2)                                                       
         BAS   RE,SERV                                                          
         MVC   0(6,R3),HKMON                                                    
         MVI   134(R3),C'('                                                     
         MVC   135(1,R3),HKNWKS    SHOW NUMBER OF WEEKS IN MONTH                
         MVI   136(R3),C')'                                                     
         CLI   MEDDAILY,C'Y'       OR NUMBER OF DAYS IN WEEK                    
         BNE   *+16                                                             
         MVC   0(6,R3),HKWK                                                     
         MVC   135(1,R3),HKNDAYS                                                
         OI    135(R3),X'F0'                                                    
         CLI   MEDDFORM,2                                                       
         BE    HK22B                                                            
         CLI   MEDDFORM,10         SHOW START AND END DATES                     
         BL    HK23                FOR SPECIAL DATE FORMULAE                    
         SPACE 2                                                                
HK22B    MVI   0(R3),C' '                                                       
         GOTO1 DATCON,HKPARA,(2,0(R2)),(7,1(R3))                                
         GOTO1 DATCON,HKPARA,(2,2(R2)),(7,133(R3))                              
         SPACE 2                                                                
HK23     CLI   QOPT1,C'Y'          SHOW BOTH DATES FOR YTD                      
         BE    HK23B                                                            
         CLI   QOPT2,C'Y'                                                       
         BNE   HK23D                                                            
         SPACE 2                                                                
HK23B    MVI   0(R3),C' '                                                       
         GOTO1 DATCON,HKPARA,(2,MEDMON01),(7,1(R3))                             
         GOTO1 DATCON,HKPARA,(2,2(R2)),(7,133(R3))                              
         SPACE 2                                                                
HK23D    LA    R2,12(R2)                                                        
         LA    R3,9(R3)                                                         
         OC    MEDMON13(4),MEDMON13                                             
         BZ    *+6                                                              
         BCTR  R3,0                                                             
         BCT   R4,HK22                                                          
         SPACE 2                                                                
HK24     GOTO1 UNDERLIN,HKPARA,(104,H11+19),H10+19                              
         MVC   H13+19(104),H10+19                                               
         B     HKXIT                                                            
         EJECT                                                                  
*              WEEKLY ROUTINES - POSITION TO RELEVANT QUARTER                   
         SPACE 3                                                                
HK30     LA    R2,MEDWEEKS                                                      
         LA    R3,MEDMON01                                                      
         LA    R4,15                                                            
         MH    R4,=H'12'                                                        
         LA    R1,MEDMON04                                                      
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LA    R1,MEDMON03                                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    *+10                                                             
         MVC   H4+57(16),=C' (FIRST PERIOD) '                                   
         CLI   MEDDFORM,10                                                      
         BNE   *+10                                                             
         MVC   H4+65(9),=C'12 WEEKS)'                                           
         CLI   THISPAGE,2                                                       
         BL    HK32                                                             
         AR    R2,R4                                                            
         LA    R3,MEDMON04                                                      
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LA    R3,MEDMON03                                                      
         MVC   H4+57(7),=C'(SECOND'                                             
         CLI   THISPAGE,2                                                       
         BE    HK32                                                             
         AR    R2,R4                                                            
         LA    R3,MEDMON07                                                      
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LA    R3,MEDMON05                                                      
         MVC   H4+57(7),=C' (THIRD'                                             
         CLI   THISPAGE,3                                                       
         BE    HK32                                                             
         AR    R2,R4                                                            
         LA    R3,MEDMON10                                                      
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LA    R3,MEDMON07                                                      
         MVC   H4+57(7),=C'(FOURTH'                                             
         SPACE 2                                                                
HK32     LA    R4,MEDWEEKS                                                      
         LA    R5,1                                                             
         SPACE 2                                                                
HK34     CLC   0(2,R2),0(R4)       FIND STARTING WEEK NO                        
         BE    HK36                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R4,12(R4)                                                        
         B     HK34                                                             
         SPACE 2                                                                
HK36     BAS   RE,HK40                                                          
         BAS   RE,HK50                                                          
         MVC   H10+126(6),=C'------'                                            
         MVC   H11+126(6),=C'PERIOD'                                            
         MVC   H12+126(6),=C'TOTALS'                                            
         MVC   H13+126(6),=C'------'                                            
         B     HKXIT                                                            
         EJECT                                                                  
*              WEEK AND MONTH EDITING ROUTINES                                  
         SPACE 3                                                                
HK40     NTR1                                                                   
         LA    R3,H11+17                                                        
         A     R3,HKDISP                                                        
         LA    R4,14                                                            
         SPACE 2                                                                
HK42     EDIT  (R5),(2,1(R3))                                                   
         GOTO1 DATCON,HKPARA,(2,0(R2)),(4,132(R3))                              
         LA    R2,12(R2)                                                        
         A     R3,HKCOLDSP                                                      
         LA    R5,1(R5)                                                         
         OC    0(2,R2),0(R2)                                                    
         BZ    HK44                                                             
         BCT   R4,HK42                                                          
         SPACE 2                                                                
HK44     GOTO1 UNDERLIN,HKPARA,(83,H12+17),H10+17                               
         MVC   H13+17(83),H10+17                                                
         B     HKXIT                                                            
         SPACE 2                                                                
HK50     NTR1                                                                   
         LA    R4,H11+102          R2=A(WEEK) R3=A(MONTH)                       
         LA    R8,3                R4=(OUTP) R5=WEEK NO.                        
         CLI   MEDDAILY,C'Y'                                                    
         BNE   HK52                                                             
         BCTR  R8,0                                                             
         SPACE 2                                                                
HK52     OC    0(4,R3),0(R3)                                                    
         BZ    HK57                                                             
         EDIT  (R5),(2,1(R4))      START WEEK NO                                
         MVI   3(R4),C'-'                                                       
         SPACE 2                                                                
HK54     CLC   2(2,R2),2(R3)       LOCATE END WEEK (END=END OF MONTH)           
         BE    HK56                                                             
         LA    R2,12(R2)                                                        
         LA    R5,1(R5)                                                         
         B     HK54                                                             
         SPACE 2                                                                
HK56     EDIT  (R5),(2,4(R4)),ALIGN=LEFT                                        
         MVC   HKSE,0(R3)                                                       
         BAS   RE,SERV                                                          
         CLI   MEDDAILY,C'Y'                                                    
         BE    HK56A                                                            
         CLI   SPOTPROF+2,0                                                     
         BNE   HK56A                                                            
         CLI   SPOTPROF+8,0                                                     
         BE    HK56B                                                            
         CLI   SPOTPROF+8,1                                                     
         BE    HK56B                                                            
*                                                                               
HK56A    MVC   132(6,R4),HKWK                                                   
         B     *+10                                                             
*                                                                               
HK56B    MVC   132(6,R4),HKMON                                                  
         CLI   MEDDFORM,10                                                      
         BNE   *+10                                                             
         MVC   132(6,R4),SPACES                                                 
         LA    R2,12(R2)                                                        
         LA    R3,12(R3)                                                        
         LA    R4,8(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R8,HK52                                                          
         SPACE 2                                                                
HK57     GOTO1 UNDERLIN,HKPARA,(22,H12+102),H10+102                             
         CLI   MEDDFORM,10                                                      
         BNE   HK57B                                                            
         GOTO1 UNDERLIN,HKPARA,(22,H11+102),H10+102                             
         SPACE 2                                                                
HK57B    MVC   H13+102(22),H10+102                                              
         CH    R8,=H'1'            (NOW SET TO 0/1/2)                           
         BH    HKXIT                                                            
         BE    HK58                                                             
         MVC   H10+107(13),=C'MONTHLY RECAP'                                    
         CLI   MEDDFORM,10                                                      
         BNE   *+10                                                             
         MVC   H10+107(7),=C'4 WEEK '                                           
         CLI   MEDDAILY,C'Y'                                                    
         BNE   HKXIT                                                            
         MVC   H10+104(16),=C'DAILY RECAP-    '                                 
         B     HKXIT                                                            
         SPACE 2                                                                
HK58     CLI   MEDDAILY,C'Y'                                                    
         BE    HKXIT                                                            
         MVC   H10+103(13),=C'MONTHLY RECAP'                                    
         CLI   MEDDFORM,10                                                      
         BNE   *+10                                                             
         MVC   H10+103(7),=C'4 WEEK '                                           
         B     HKXIT                                                            
         SPACE 2                                                                
HKXIT    XIT1  ,                                                                
         EJECT                                                                  
*              ROUTINE TO DIG OUT MONTH OF SERVICE & N'WEEKS                    
         SPACE 3                                                                
SERV     NTR1                                                                   
         LA    R2,HKSE                                                          
         MVI   HKWK,C' '                                                        
         GOTO1 DATCON,HKPARA,(2,0(R2)),(7,HKWK+1)                               
         GOTO1 DATCON,HKPARA,(2,0(R2)),(0,HKD1)                                 
         GOTO1 DATCON,HKPARA,(2,2(R2)),(0,HKD2)                                 
         L     R3,=F'-7'                                                        
         GOTO1 ADDAY,HKPARA,HKD2,HKD3,(R3)                                      
         CLC   HKD1,HKD3           IS START BEFORE END-7                        
         BL    *+10                                                             
         MVC   HKD3,HKD2           NO - USE END (OTHERWISE USE END-7)           
         MVI   HKMON,C' '                                                       
         GOTO1 DATCON,HKPARA,(0,HKD3),(6,HKMON+1)                               
         MVC   HKMON+4(2),HKMON+5  (S/B  MMMYY)                                 
         LA    R2,7                                                             
         LA    R4,HKNWKS                                                        
         BAS   R5,SERV2            FIND NUMBER OF WEEKS INVOLVED                
         LA    R2,1                                                             
         LA    R4,HKNDAYS                                                       
         BAS   R5,SERV2            FIND NUMBER OF DAYS INVOLVED                 
         B     HKXIT                                                            
         SPACE 2                                                                
SERV2    MVC   HKD3,HKD1                                                        
         LA    R3,1                                                             
*                                                                               
SERV4    STC   R3,0(R4)            FIND NUMBER OF WEEKS INVOLVED                
         MVC   HKD4,HKD3                                                        
         GOTO1 ADDAY,HKPARA,HKD4,HKD3,(R2)                                      
         CLC   HKD3,HKD2                                                        
         BHR   R5                                                               
         LA    R3,1(R3)                                                         
         B     SERV4                                                            
*                                                                               
PWPREFIX DS    CL5                                                              
         SPACE 2                                                                
HKSE     DS    CL4                 CONTAINS SS/EE ON ENTRY                      
HKD1     DS    CL6                                                              
HKD2     DS    CL6                                                              
HKD3     DS    CL6                                                              
HKD4     DS    CL6                                                              
HKMON    DS    CL7                 CONTAINS MMM/YY ON XIT                       
HKNWKS   DS    CL1                 CONTAINS NUMBER OF WEEK ON XIT               
HKWK     DS    CL6                 CONTAINS START DATE MMMDD ON XIT             
HKNDAYS  DS    XL1                 CONTAINS NUMBER OF DAYS ON XIT               
         EJECT                                                                  
HKDISP   DS    F                                                                
HKCOLDSP DS    F                                                                
HKPARA   DS    4F                                                               
HKAREA   DS    CL126                                                            
*                                                                               
         LTORG                                                                  
SVNONTDMS DS   CL150                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPREPPTBUF                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPM802 11/21/19'                                      
         END                                                                    
