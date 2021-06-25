*          DATA SET SPREPM902  AT LEVEL 072 AS OF 11/21/19                      
*PHASE SPM902T                                                                  
*INCLUDE COVAIL                                                                 
         TITLE 'SPM902 - BRAND MEDIA PLAN'                                      
SPM902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPM9**,RR=R2                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         LA    R7,2048(R9)                                                      
         LA    R7,2048(R7)                                                      
         USING SPM902+4096,R9,R7                                                
         ST    R2,RELO                                                          
         L     RA,0(R1)            RA/RC FOR WORKD                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         SPACE 1                                                                
         L     R6,MEDBUFF          R6 FOR BLOCK                                 
         USING MEDBLOCK,R6                                                      
         SPACE 1                                                                
         L     R8,BUFFBUFF                                                      
         LA    R1,MYBUFFIO                                                      
         ST    R1,BUFFIO                                                        
         SPACE 1                                                                
         LA    R2,PLANHOOK                                                      
         ST    R2,HEADHOOK                                                      
         STM   R9,RC,SPM9R9                                                     
         ST    R7,SPM9R7                                                        
         MVI   SPSUPMKT,C'Y'                                                    
         CLI   MODE,MKTFRST                                                     
         BNE   *+8                                                              
         MVI   MARKACT,C'N'                                                     
         CLI   MODE,MKTLAST                                                     
         BL    MP1                                                              
         BH    *+12                                                             
         CLI   MARKACT,C'Y'                                                     
         BNE   MP1                                                              
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         SPACE 2                                                                
MP1      CLI   MODE,RUNFRST        RUNFRST ROUTINES                             
         BNE   MP2                                                              
         MVI   ALLOWCWM,C'N'                                                    
         L     R8,=A(BUFFALOC)     R8 FOR BUFFALO CSECT/DSECT                   
         A     R8,RELO                                                          
         ST    R8,BUFFBUFF                                                      
         USING BUFFALOD,R8                                                      
         GOTO1 =V(COVAIL),DMCB,C'SETB',65000,800000,(R8)                        
         OC    4(4,R1),4(R1)       CHECK FOR GOOD ALLOCATE                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFBUFF,12(R1)     SET BUFFALO CSECT                            
         L     R8,BUFFBUFF                                                      
         L     RF,BUFFLALL         AND ITS LENGTH                               
         SR    RE,RE                                                            
         M     RE,BUFFCRMX                                                      
         LR    R8,RF                                                            
         ST    R8,LNBUFFER                                                      
         L     R8,BUFFBUFF                                                      
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',(R8)                                        
         L     R2,=A(LODAREA)                                                   
         A     R2,RELO                                                          
         GOTO1 LOADER,DMCB,=CL8'SPM8CS',(R2),(C'M',(R2))                        
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDSEED,DMCB,(RA)                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   ASPM904,MEDTABLE                                                 
         MVI   MEDDAILY,C'N'                                                    
         MVI   DAILYSW,C'N'                                                     
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
         OI    RQOPTS,RQOPTS_1DEC   FORCE 1 DECIMAL REPORTING                   
         BAS   RE,REQFILT                                                       
         MVI   RDPRD,C'Y'                                                       
         BAS   RE,CLRPOOL                                                       
         CLI   QDPTDET,C' '        ADJUST REQUESTS FROM PROFILES                
         BNE   MP2B                                                             
         CLI   PROGPROF,0          DAYPART DETAIL                               
         BE    MP2B                                                             
         MVC   QDPTDET,PROGPROF                                                 
*&&DO                                                                           
         XC    WORK,WORK                                                        
         XC    SVG0PROF,SVG0PROF   CLEAR IN CASE NOT FOUND                      
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,SVG0PROF,DATAMGR                               
*&&                                                                             
MP2B     CLI   QOPT2,C' '          RECAP OPTION                                 
         BNE   MP2D                                                             
         CLI   PROGPROF+1,0                                                     
         BE    MP2D                                                             
         MVC   QOPT2,PROGPROF+1                                                 
         SPACE 2                                                                
MP2D     CLI   QOPT3,C' '          DATA OPTION                                  
         BNE   MP2E                                                             
         CLI   PROGPROF+2,0                                                     
         BE    MP2E                                                             
         MVC   QOPT3,PROGPROF+2                                                 
         SPACE 2                                                                
MP2E     CLI   QOPT4,C' '          SUPPRESS SPILL                               
         BNE   MP2F                                                             
         CLI   PROGPROF+5,0                                                     
         BE    MP2F                                                             
         MVC   QOPT4,PROGPROF+5                                                 
         SPACE 2                                                                
MP2F     MVI   ESTSW,C'N'                                                       
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
         CLI   QOPT3,C'F'                                                       
         BNE   XIT                                                              
         CLC   QPRD(3),=C'POL'                                                  
         BNE   XIT                                                              
         MVI   FCRDGOAL,C'C'                                                    
         B     XIT                                                              
         SPACE 2                                                                
MP4      CLI   MODE,ESTFRST                                                     
         BNE   MP30                                                             
         CLI   ESTSW,C'N'                                                       
         BNE   XIT                                                              
         MVI   ESTSW,C'Y'                                                       
         L     RF,=A(LODAREA)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,QEND,(BPRD,TALFAC)                                     
         MVC   PAGE,=H'1'                                                       
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         TM    Q2USER,X'01'         UDEF=E1?                                    
         BZ    MP4AA                NO                                          
         MVC   H7(L'CEU1),CEU1      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H7+L'CEU1+2(L'EUSER1),EUSER1                                     
*                                                                               
MP4AA    TM    Q2USER,X'02'         UDEF=E2?                                    
         BZ    MP4AB                NO                                          
         MVC   H8(L'CEU2),CEU2      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H8+L'CEU2+2(L'EUSER2),EUSER2                                     
         DROP  RF                                                               
*                                                                               
MP4AB    L     RF,=A(PWPREFIX)                                                  
         MVC   0(5,RF),SPACES                                                   
         OC    EPWPCT,EPWPCT                                                    
         BNZ   *+14                                                             
         OC    ECOST2,ECOST2                                                    
         BZ    MP4NOPW                                                          
         MVC   0(3,RF),=C'IM '                                                  
         CLC   QAGY,=C'WI'                                                      
         BE    *+10                                                             
         MVC   0(3,RF),=C'AGY'                                                  
         CLI   QPWCV,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(3,RF),=C'CLT'                                                  
*                                                                               
MP4NOPW  CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         CLI   EDAILY,C'Y'         TEST FOR DAILY ESTIMATE                      
         BNE   MP4A                                                             
         DROP  RE                                                               
         CLI   MEDDAILY,C'Y'       YES-TEST DAILY ALREADY                       
         BE    MP4C                YES                                          
         MVI   MEDDAILY,C'Y'       NO-USE SPM8041 FOR MEDTABLE                  
         MVI   DAILYSW,C'Y'                                                     
         MVC   ATHISGP,=A(THISGPD)                                              
         MVC   ATHISAD,=A(THISADD)                                              
         MVC   ATHISAP,=A(THISAPD)                                              
         ICM   R4,15,ASPM9041      ALREADY LOADED?                              
         BZ    MP4LOAD             NO                                           
         GOTO1 LOADER,DMCB,=C'SPM9041 ',X'FFFFFFFF',0                           
*                                                                               
MP4LOAD  GOTO1 LOADER,DMCB,=C'SPM9041 ',0                                       
         ICM   R4,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,ASPM9041                                                      
         B     MP4B                                                             
*                                                                               
MP4A     CLI   MEDDAILY,C'Y'       WEEKLY ESTIMATE -TEST ALREADY WEEKLY         
         BNE   MP4C                YES                                          
         GOTO1 LOADER,DMCB,=C'SPM9041 ',0                                       
         ICM   R4,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,ASPM904          NO-SET MEDTABLE TO SPM904                    
         MVI   MEDDAILY,C'N'                                                    
         MVI   DAILYSW,C'N'                                                     
         MVC   ATHISGP,=A(THISGP)                                               
         MVC   ATHISAD,=A(THISAD)                                               
         MVC   ATHISAP,=A(THISAP)                                               
*                                                                               
MP4B     ST    R4,MEDTABLE                                                      
         GOTO1 MEDSEED,DMCB,(RA)                                                
*                                                                               
MP4C     DS    0H                                                               
         MVC   MEDDFORM,SPOTPROF+2                                              
         GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         CLI   RDPRD,C'Y'                                                       
         BNE   MP5                                                              
         MVI   RDPRD,C'N'                                                       
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         EJECT                                                                  
*              TRIM DOWN THE BUFFALO TABLES WHERE WE CAN                        
         SPACE 3                                                                
MP5      CLI   QDPTDET,C'B'                                                     
         BE    MP6                                                              
         CLI   QDPTDET,C'C'                                                     
         BNE   MP8                                                              
         LA    R2,LISTB            SUPPRESS SPOT/LENGTH DAY-PART DETAIL         
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP6      LA    R2,LISTA            SUPPRESS SPOTLENGTH DETAIL                   
         BAS   RE,TRIM                                                          
         SPACE 2                                                                
MP8      CLI   QOPT3,C'F'          FOR CPP GUIDES                               
         BE    MP9                                                              
         CLI   QOPT3,C'G'                                                       
         BE    MP9                                                              
         CLI   QOPT3,C'H'                                                       
         BNE   MP9B                                                             
         SPACE 2                                                                
MP9      OC    MEDMON04(4),MEDMON04   THAT ARE MORE THAN 3 MONTHS               
         BZ    MP9B                                                             
         CLI   QOPT1,C'N'                                                       
         BE    MP9B                                                             
         MVI   QOPT1,C'M'          LIMIT TO 1 REPORT                            
         SPACE 2                                                                
MP9B     CLI   QOPT1,C' '                                                       
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
* DIFFERENT TESTS FOR WEEKLY                                                    
MP15W    LA    R2,LIST2                                                         
         OC    MEDQ2WKS(4),MEDQ2WKS                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST3                                                         
         OC    MEDQ3WKS(4),MEDQ3WKS                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
         LA    R2,LIST4                                                         
         OC    MEDQ4WKS(4),MEDQ4WKS                                             
         BNZ   *+8                                                              
         BAS   RE,TRIM                                                          
*                                                                               
MP16     CLI   QOPT1,C'M'                                                       
         BE    MP20                                                             
         CLI   QOPT1,C'Y'                                                       
         BE    MP20                                                             
         CLI   QOPT1,C'N'                                                       
         BE    MP17                                                             
         CLI   QOPT2,C'M'                                                       
         BE    MP17                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    MP17                                                             
         CLI   QOPT2,C' '          THE DEFAULT IS TO GET A RECAP IF             
         BNE   MP18                MORE THAN ONE QUARTER IS REQUESTED           
         SPACE 2                                                                
MP17     CLI   MEDDAILY,C'Y'                                                    
         BE    MP17W                                                            
         OC    MEDMON04(4),MEDMON04                                             
         BNZ   MP20                                                             
         B     MP18                                                             
* DIFFERENT TEST FOR WEEKLY                                                     
MP17W    OC    MEDQ2WKS(4),MEDQ2WKS                                             
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
         CLI   MEDDAILY,C'Y'                                                    
         BE    MP20W                                                            
         OC    MEDMON04(4),MEDMON04                                             
         BNZ   MP22                                                             
         B     MP21                                                             
* DIFFERENT TEST FOR WEEKLY                                                     
MP20W    OC    MEDQ2WKS(4),MEDQ2WKS                                             
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
         SPACE 2                                                                
MP28     L     R3,BUFFCOLS         WORK OUT HOW MANY FIT IN CORE                
         M     R2,BUFFROWS                                                      
         SLL   R3,2                                                             
         A     R3,BUFFLKEY                                                      
         L     R4,LNBUFFER         IN 50K                                       
         SRDA  R4,32                                                            
         DR    R4,R3                                                            
         BCTR  R5,0                                                             
*        SRL   R5,1                                                             
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
*              LISTS FOR OPTIMIZATION ROUTINES                                  
         SPACE 3                                                                
LISTA    DC    AL1(1,4,7,10,13,16),X'FF'                                        
LISTB    DC    AL1(2,5,8,11,14,17),X'FF'                                        
LIST1    DC    AL1(1,2,3),X'FF'                                                 
LIST2    DC    AL1(4,5,6),X'FF'                                                 
LIST3    DC    AL1(7,8,9),X'FF'                                                 
LIST4    DC    AL1(10,11,12),X'FF'                                              
LIST5    DC    AL1(13,14,15),X'FF'                                              
LIST6    DC    AL1(16,17,18),X'FF'                                              
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
         DC    C'FNY',AL1(1),F'36'                                              
         DC    C'GYN',AL1(1),F'72'                                              
         DC    C'HYY',AL1(1),F'72'                                              
         DC    C'JYY',AL1(1),F'72'                                              
         DC    C'KYY',AL1(1),F'72'                                              
         DC    C'LYY',AL1(0),F'54'                                              
         DC    X'FF'                                                            
         DC    C'NY',AL1(1),F'36'                                               
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
*              PROCESS A GOAL RECORD                                            
         SPACE 3                                                                
MP30     CLI   MODE,PROCGOAL                                                    
         BNE   MP33                                                             
         L     R5,ADGOAL                                                        
         USING GOALREC,R5                                                       
         CLI   GKEYPRD,X'FF'       TEST CPP GUIDE                               
         BNE   MP30C                                                            
         LA    R1,GDELEM           YES-                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
MP30A    CLI   0(R1),0             LOOK FOR WEEK ELEMENT                        
         BNE   *+14                                                             
         LTR   RF,RF               DONE-TEST ANY WITHOUT CPP=$0.01              
         BZ    XIT                 NO-IGNORE WHOLE RECORD                       
         B     MP30C                                                            
         CLI   0(R1),X'21'                                                      
         BNE   MP30B                                                            
         USING GLEMENT,R1                                                       
         CLC   GLBUDGET,=F'100'    TEST CPP=$0.01                               
         BNE   *+12                                                             
         MVI   0(R1),X'FF'         YES-SMUDGE THE ELEMENT                       
         B     MP30B                                                            
         LA    RF,1                NO-RECORD THE FACT                           
         DROP  R1                                                               
*                                                                               
MP30B    IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     MP30A                                                            
*                                                                               
MP30C    LA    R5,KEY              POINT TO DIRECTORY === KEY ===               
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         MVI   MEDEXTPW,C'N'                                                    
         CLI   QPWCV,C'Y'                                                       
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
         GOTO1 MEDGETGL,DMCB,(RA)                                               
*                                                                               
         LA    R1,MEDPERD                                                       
         LHI   R0,1                                                             
*                                                                               
         CLI   MEDDAILY,C'Y'                                                    
         BNE   MP30D                                                            
* NEED TO CHECK FOR ACTIVITY IN EACH QUARTER!                                   
         LA    R1,MEDQRT01                                                      
         LHI   R0,4                SET FOR NUMBER OF QUARTERS                   
*                                                                               
MP30D    ICM   RF,15,4(R1)                                                      
         BZ    MP30X                                                            
         USING MEDDATA,RF                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BNZ   MP30X                                                            
         AHI   R1,L'MEDQRT01       NEXT QUARTER                                 
         BCT   R0,MP30D                                                         
         B     XIT                                                              
         DROP  RF                                                               
*                                                                               
MP30X    CLC   DPFILT,SPACES                                                    
         BE    *+14                                                             
         CLC   DPFILT,MEDDPART                                                  
         BNE   XIT                                                              
         MVI   MARKACT,C'Y'                                                     
         JIF   QOPT3,EQ,7,OR,8,OR,9,MP31,JUMP=N                                 
         CLI   QOPT3,C'J'                                                       
         BE    MP31                                                             
         CLI   QOPT3,C'K'                                                       
         BE    MP31                                                             
         CLI   QOPT3,C'L'                                                       
         BE    MP31                                                             
         MVC   MEDDPGRP,MEDDPART                                                
         SPACE 2                                                                
MP31     L     R5,ADGOAL           NOW POINT TO THE GOAL ==RECORD==             
         CLC   TODAYP,GACTDATE     WAS IT ACTIVE TODAY                          
         BNE   MP32                                                             
         L     R2,=A(ACTPOOL)                                                   
         A     R2,RELO             YES - SO LEAVE A MEMO IN POOL                
         CLC   0(4,R2),4(R2)                                                    
         BE    MP32                                                             
         L     R3,4(R2)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,4(R2)                                                         
         BCTR  R3,0                                                             
         MH    R3,=H'07'                                                        
         LA    R3,8(R2,R3)                                                      
         MVC   0(1,R3),MEDBRAND                                                 
         MVC   1(3,R3),MEDDPART                                                 
         MVC   4(1,R3),MEDSPTLN                                                 
         PACK  DUB,MKT                                                          
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   5(2,R3),DUB                                                      
         DROP  R6                                                               
         SPACE 2                                                                
MP32     GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,MYPOST                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A BUY RECORD                                             
         SPACE 3                                                                
MP33     CLI   MODE,PROCBUY                                                     
         BNE   MP40                                                             
         L     R2,=A(PSLIST)                                                    
         LR    RE,R2                                                            
         LA    RF,L'PSLIST                                                      
         XCEF                                                                   
         GOTO1 MEDPSL,DMCB,(RA),(R2)                                            
         SPACE 2                                                                
MP34     OC    0(2,R2),0(R2)       LOOP ON BRAND/SPOTLENGTH                     
         BE    XIT                                                              
         CLI   BPRD,X'FF'          DONT CHECK IF PRODUCT=POL                    
         BE    MP35                                                             
         CLC   BPRD,0(R2)                                                       
         BNE   MP38                                                             
         SPACE 2                                                                
MP35     L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         MVI   MEDEXTPW,C'N'                                                    
         CLI   QPWCV,C'Y'                                                       
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         CLI   QOPT4,C'Y'          TEST SUPPRESS SPILL OPTION                   
         BNE   *+12                                                             
         CLI   MEDSPILL,C'Y'       ON - TEST FOR SPILL                          
         BE    XIT                      YES - EXIT                              
         CLC   DPFILT,SPACES                                                    
         BE    *+14                                                             
         CLC   DPFILT,MEDDPART                                                  
         BNE   XIT                                                              
*                                                                               
         LA    R1,MEDPERD                                                       
         LHI   R0,1                                                             
*                                                                               
         CLI   MEDDAILY,C'Y'                                                    
         BNE   *+12                                                             
         LA    R1,MEDQRT01                                                      
         LHI   R0,4                                                             
*                                                                               
         ICM   RF,15,4(R1)                                                      
         BZ    MP38                                                             
         USING MEDDATA,RF                                                       
         OC    MEDBYD(16),MEDBYD                                                
         BZ    MP38                                                             
         DROP  RF                                                               
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
         BAS   RE,MYPOST                                                        
         MVI   MARKACT,C'Y'                                                     
         SPACE 2                                                                
MP38     LA    R2,2(R2)                                                         
         B     MP34                                                             
         EJECT                                                                  
*              ROUTINE TO CONTROL POSTINGS                                      
         SPACE 3                                                                
MYPOST   NTR1                                                                   
         LA    R2,LALIST                                                        
         CLI   QMGR,C' '                                                        
         BE    MY2                                                              
         LA    R2,LBLIST                                                        
         CLC   MGR1LEN,MGR2LEN                                                  
         BE    MY2                                                              
         LA    R2,LCLIST                                                        
         CLC   MGR2LEN,MGR3LEN                                                  
         BE    MY2                                                              
         LA    R2,LDLIST                                                        
         SPACE 2                                                                
MY2      CLI   0(R2),X'FF'                                                      
         BE    MY22                                                             
         XC    WORK,WORK                                                        
         CLI   0(R2),C'M'                                                       
         BNE   MY4                                                              
         PACK  DUB,MKT                                                          
         CVB   R0,DUB                                                           
         ST    R0,DUB                                                           
         MVC   WORK+1(2),DUB+2                                                  
         MVC   WORK+3(2),SPWEIGHT+2                                             
         MVC   WORK+6(12),=CL12'MARKET'                                         
         MVC   WORK+18(24),MKTNM                                                
         B     MY20                                                             
         SPACE 2                                                                
MY4      CLI   0(R2),C'A'                                                       
         BNE   MY6                                                              
         MVC   WORK+1(5),MGR1                                                   
         MVC   WORK+6(12),MGR1BK                                                
         MVC   WORK+18(24),MGR1NM                                               
         CLC   WORK+6(7),=C'MARKET '                                            
         BNE   MY20                                                             
         MVI   WORK+12,C'_'                                                     
         B     MY20                                                             
         SPACE 2                                                                
MY6      CLI   0(R2),C'B'                                                       
         BNE   MY8                                                              
         MVC   WORK+1(5),MGR2                                                   
         MVC   WORK+6(12),MGR2BK                                                
         MVC   WORK+18(24),MGR2NM                                               
         CLC   WORK+6(7),=C'MARKET '                                            
         BNE   MY20                                                             
         MVI   WORK+12,C'_'                                                     
         B     MY20                                                             
         SPACE 2                                                                
MY8      CLI   0(R2),C'C'                                                       
         BNE   MY10                                                             
         MVC   WORK+1(5),MGR3                                                   
         MVC   WORK+6(12),MGR3BK                                                
         MVC   WORK+18(24),MGR3NM                                               
         CLC   WORK+6(7),=C'MARKET '                                            
         BNE   MY20                                                             
         MVI   WORK+12,C'_'                                                     
         B     MY20                                                             
         SPACE 2                                                                
MY10     MVC   WORK+6(12),=CL12'BRAND'                                          
         MVC   WORK+18(20),PRDNM                                                
         SPACE 2                                                                
MY20     OC    WORK+6(36),SPACES                                                
         MVC   WORK(1),1(R2)                                                    
         CLI   2(R2),C' '                                                       
         BE    *+10                                                             
         MVC   WORK+1(5),=5X'FF'                                                
         SPACE 2                                                                
         NI    WORK,X'0F'                                                       
         GOTO1 MEDPOST,DMCB,(RA)                                                
         LA    R2,3(R2)                                                         
         B     MY2                                                              
         SPACE 2                                                                
MY22     CLC   QPRD,=C'ALL'        CLIENT SUMMARY WHEN QPRD=ALL                 
         BNE   XIT                                                              
         XC    WORK,WORK                                                        
         MVI   WORK,5                                                           
         MVC   WORK+1(3),PRODUCT                                                
         MVC   WORK+4(2),SPACES                                                 
         MVC   WORK+6(12),=CL12'BRAND'                                          
         L     R2,ADBLOCK                                                       
         USING DBLOCK,R2                                                        
         XC    0(256,R2),0(R2)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     NEWDCDX                                                          
*                                                                               
         L     RF,ADCLT                                                         
         CLI   CEXTRA-CLTHDR(RF),C'U' TEST US DEMOS                             
         BE    NEWDCDX                                                          
*                                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   NEWDCDX                                                          
         MVI   DBSELMED,C'C'                                                    
         DROP  R2                                                               
*                                                                               
NEWDCDX  L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
         MVC   WORK+11(7),EDEMOS+2                                              
         TM    ECNTRL,X'01'        NEW EST. HEADER                              
         BNO   MY24                                                             
         LA    RE,ENONTDMS                                                      
         ST    RE,DMCB+16                                                       
         GOTO1 DEMOCON,DMCB,(1,EDEMOS),(2,WORK+11),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,SPUSRNMS)                                            
*                                                                               
MY24     MVC   WORK+18(20),PRDNM                                                
         OC    WORK+6(36),SPACES                                                
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   QOPT3,C'2'          NOT NEEDED FOR POINTS ONLY                   
         BE    XIT                                                              
         CLI   QOPT3,C'5'                                                       
         BE    XIT                                                              
         CLI   QOPT3,C'8'                                                       
         BE    XIT                                                              
         CLI   QOPT3,C'K'                                                       
         BE    XIT                                                              
         MVC   WORK+1(5),=5X'FF'                                                
         MVC   WORK+6(12),=CL12'CLIENT'                                         
         MVC   WORK+18(20),CLTNM                                                
         GOTO1 MEDPOST,DMCB,(RA)                                                
         B     XIT                                                              
         SPACE 2                                                                
LALIST   DC    C'M4 P41',X'FF'                                                  
LBLIST   DC    C'M3 A31A4 P41',X'FF'                                            
LCLIST   DC    C'M2 B21B3 A31A4 P41',X'FF'                                      
LDLIST   DC    C'M1 C11C2 B21B3 A31A4 P41',X'FF'                                
         EJECT                                                                  
*              CONTROL OF REPORT PRINTING                                       
         SPACE 3                                                                
MP40     CLI   MODE,MKTFRST        MARKET FIRST                                 
         BNE   MP42                                                             
         MVI   MCOMSW,C'N'                                                      
         CLI   PROGPROF+6,C'Y'     TEST MARKET COMMENTS REQUIRED                
         BNE   MP42                                                             
         BAS   RE,GETMCOM          YES-GET MARKET COMMENTS                      
         SPACE 2                                                                
MP42     LA    R2,1                                                             
         MVI   SUMSW,C'Y'                                                       
         MVC   WORK(5),MGR3                                                     
         CLI   MODE,MGR3LAST                                                    
         BNE   MP46                                                             
         AP    CNT3,=P'1'                                                       
         B     SUM                                                              
         SPACE 2                                                                
MP46     LA    R2,2                                                             
         MVC   WORK(5),MGR2                                                     
         CLI   MODE,MGR2LAST                                                    
         BNE   MP48                                                             
         CP    CNT3,=P'1'          SUPPRESS IF ONLY 1 LEVEL 3 BEFORE            
         BNE   *+8                                                              
         MVI   SUMSW,C'N'                                                       
         ZAP   CNT3,=P'0'                                                       
         AP    CNT2,=P'1'                                                       
         B     SUM                                                              
         SPACE 2                                                                
MP48     LA    R2,3                                                             
         MVC   WORK(5),MGR1                                                     
         CLI   MODE,MGR1LAST                                                    
         BNE   MP50                                                             
         CP    CNT2,=P'1'          SUPPRESS IF ONLY 1 LEVEL 2 BEFORE            
         BNE   *+8                                                              
         MVI   SUMSW,C'N'                                                       
         ZAP   CNT2,=P'0'                                                       
         AP    CNT1,=P'1'                                                       
         B     SUM                                                              
         SPACE 2                                                                
MP50     LA    R2,4                                                             
         MVC   WORK(3),PRD                                                      
         MVC   WORK+3(2),=X'FFFF'                                               
         CLI   MODE,PRDLAST                                                     
         BNE   MP52                                                             
         CP    CNT1,=P'1'          SUPPRESS IF ONLY 1 LEVEL 1 BEFORE            
         BNE   *+8                                                              
         MVI   SUMSW,C'N'                                                       
         ZAP   CNT1,=P'0'                                                       
         B     SUM                                                              
         SPACE 2                                                                
MP52     LA    R2,5                                                             
         CLI   MODE,CLTLAST                                                     
         BNE   XIT                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   XIT                                                              
         SPACE 2                                                                
SUM      MVC   WORK+5(2),SPWEIGHT+2                                             
         BAS   RE,ADDPOOL                                                       
         CLI   SUMSW,C'Y'          DO WE NEED TO PRINT A SUMMARY ?              
         BNE   *+8                                                              
         BAS   RE,EDIT             YES                                          
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R2),(R8)),(X'80',1)                     
         MVI   FORCEHED,C'Y'                                                    
         L     R2,=A(ACTPOOL)                                                   
         A     R2,RELO                                                          
         XC    4(4,R2),4(R2)                                                    
         SPACE 2                                                                
XIT      XIT   1                                                                
         EJECT                                                                  
*              ROUTINES TO READ AND EDIT BUFFALO RECORDS                        
         SPACE 3                                                                
EDIT     NTR1                                                                   
         XC    LASTKEY,LASTKEY                                                  
         MVC   THISKEY,LASTKEY                                                  
         ST    R2,BUFFLEVL                                                      
         STC   R2,LASTKEY                                                       
         L     RE,BUFFIO                                                        
         LR    R3,RE                                                            
         LA    RF,1450                                                          
         XCEF                                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',((R2),(R8)),(R3),1                         
         B     EDIT4                                                            
         SPACE 2                                                                
EDIT2    L     R2,BUFFLEVL                                                      
         L     R3,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',((R2),(R8)),(R3),1                          
         SPACE 2                                                                
EDIT4    TM    DMCB+8,X'80'                                                     
         BNO   EDIT6                                                            
         SPACE 2                                                                
EDIT5    MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         SPACE 2                                                                
EDIT6    CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),0                                                          
         BE    EDIT5                                                            
         BAS   RE,COMPDISP                                                      
         CLC   THISKEY(1),LASTKEY                                               
         BNE   XIT                                                              
         CLC   THISKEY(2),LASTKEY  CHANGE OF PAGE                               
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   THISKEY(7),LASTKEY  FIRST FOR CHUNK                              
         BE    EDIT20                                                           
         BAS   RE,PAGESET                                                       
         L     R6,PAGEMARK                                                      
         MVC   0(12,R6),THISBRK                                                 
         MVC   13(6,R6),=C'TOTALS'                                              
         CLC   THISCODE,=5X'FF'                                                 
         BE    EDIT10                                                           
         MVC   13(6,R6),SPACES                                                  
         MVC   13(5,R6),THISCODE                                                
         MVI   20(R6),C'-'                                                      
         MVC   22(24,R6),THISNAME                                               
         CLC   THISBRK,=CL12'MARKET'                                            
         BE    EDIT8                                                            
         MVC   WORK(5),THISCODE    SHOW COVERAGE                                
         CLC   THISBRK(5),=C'BRAND'                                             
         BNE   *+10                                                             
         MVC   WORK+3(2),=X'FFFF'                                               
         BAS   RE,GETPOOL                                                       
         OC    WORK+5(2),WORK+5                                                 
         BZ    EDIT7                                                            
         MVC   47(9,R6),=C'COVERAGE='                                           
         EDIT  (2,WORK+5),(6,56(R6)),2,ALIGN=LEFT,WRK=DMCB                      
         SPACE 2                                                                
EDIT7    CLC   THISBRK(5),=C'BRAND'                                             
         BNE   EDIT10                                                           
         MVI   70(R6),C'('         SHOW TARGET FOR BRAND                        
         MVC   71(7,R6),THISBRK+5                                               
         MVI   78(R6),C')'                                                      
         MVC   5(7,R6),SPACES                                                   
         B     EDIT10                                                           
         SPACE 2                                                                
EDIT8    MVC   0(20,R6),SPACES                                                  
         EDIT  (2,THISCODE),(4,0(R6)),FILL=0                                    
         LH    R4,THISCODE+2                                                    
         CH    R4,=H'0'                                                         
         BE    EDIT10                                                           
         MVC   47(9,R6),=C'COVERAGE='                                           
         EDIT  (R4),(6,56(R6)),2,ALIGN=LEFT                                     
         SPACE 2                                                                
EDIT10   GOTO1 SQUASHER,DMCB,(R6),80                                            
         LA    R6,132(R6)                                                       
         ST    R6,PAGEMARK                                                      
         SPACE 2                                                                
EDIT16   MVI   DPCOUNT,0                                                        
         MVI   SLCOUNT,0                                                        
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
         L     R2,=A(ACTPOOL)      WAS THIS MKT/DPT/SL ACTIVE TODAY             
         A     R2,RELO                                                          
         L     R3,4(R2)                                                         
         LTR   R3,R3                                                            
         BZ    EDIT21D                                                          
         MVI   4(R6),C'*'          ASSUME IT IS                                 
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         MVC   WORK(1),MEDBRAND                                                 
         DROP  R1                                                               
         MVC   WORK+1(3),THISDPT+1                                              
         MVC   WORK+4(1),THISSL                                                 
         MVC   WORK+5(2),THISCODE                                               
         LA    R2,8(R2)                                                         
         SPACE 2                                                                
EDIT21B  CLC   0(7,R2),WORK                                                     
         BE    EDIT21D                                                          
         LA    R2,7(R2)                                                         
         BCT   R3,EDIT21B                                                       
         MVI   4(R6),C' '          NO MATCH - SO IT IS'NT                       
         SPACE 2                                                                
EDIT21D  MVC   5(3,R6),THISDPT+1                                                
         MVI   8(R6),C'-'                                                       
         EDIT  (1,THISSL),(3,9(R6)),ALIGN=LEFT                                  
         BAS   RE,EDIT30                                                        
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
         BNE   EDIT28                                                           
         CLI   PROGPROF+7,C'Y'                                                  
         BNE   EDIT27                                                           
         CLI   DPCOUNT,1           TEST EXACTLY ONE DPT                         
         BNE   EDIT27                                                           
         MVI   DPCOUNT,2           THEN FORCE TOTALS TO PRINT                   
*                                                                               
EDIT27   CLI   DPCOUNT,2                                                        
         BL    *+10                                                             
         MVC   5(5,R6),=C'TOTAL'                                                
         CLI   DPCOUNT,1                                                        
         BE    *+8                                                              
         BAS   RE,EDIT30                                                        
         MVI   SLCOUNT,0                                                        
         MVI   DPCOUNT,0                                                        
         BAS   RE,CHUNK                                                         
         BAS   RE,PAGESET                                                       
         SPACE 2                                                                
EDIT28   MVC   LASTKEY,THISKEY                                                  
         B     EDIT2                                                            
         EJECT                                                                  
*              ROUTINES TO HANDLE WEIGHT POOL                                   
         SPACE 3                                                                
ADDPOOL  NTR1                                                                   
         L     R2,=A(WTPOOL)                                                    
         A     R2,RELO                                                          
         L     R3,4(R2)                                                         
         CLC   0(4,R2),4(R2)                                                    
         BE    XIT                                                              
         LA    R4,1(R3)                                                         
         LA    R5,8(R2)                                                         
         LTR   R3,R3                                                            
         BZ    ADDPOOL4                                                         
         SPACE 2                                                                
ADDPOOL2 CLC   0(5,R5),WORK        CHECK IF IT'S THERE ALREADY                  
         BNE   ADDPOOL3                                                         
         CLI   MODE,MGR1LAST       YES-UPDATE ENTRY FOR MARKET GROUP            
         BH    XIT                     LAST                                     
         MVC   0(7,R5),WORK                                                     
         B     XIT                                                              
         SPACE 2                                                                
ADDPOOL3 LA    R5,7(R5)                                                         
         BCT   R3,ADDPOOL2                                                      
         SPACE 2                                                                
ADDPOOL4 MVC   0(7,R5),WORK        NO - ADD A NEW ENTRY                         
         ST    R4,4(R2)            AND UPDATE NUMBER OF ENTRIES USED            
         B     XIT                                                              
         SPACE 2                                                                
CLRPOOL  NTR1                                                                   
         L     R2,=A(WTPOOL)                                                    
         A     R2,RELO                                                          
         XC    4(4,R2),4(R2)                                                    
         B     XIT                                                              
         SPACE 2                                                                
GETPOOL  NTR1                                                                   
         L     R2,=A(WTPOOL)                                                    
         A     R2,RELO                                                          
         L     R3,4(R2)                                                         
         LA    R2,8(R2)                                                         
         SPACE 2                                                                
GETPOOL2 CLC   0(5,R2),WORK                                                     
         BE    GETPOOL4                                                         
         LA    R2,7(R2)                                                         
         BCT   R3,GETPOOL2                                                      
         XC    WORK+5(2),WORK+5                                                 
         B     XIT                                                              
         SPACE 2                                                                
GETPOOL4 MVC   WORK(7),0(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO UNWEIGHT A DETAIL LINE                                
         SPACE 3                                                                
UNWEIGHT NTR1                                                                   
         CLI   SPOTPROF+1,C'N'                                                  
         BE    XIT                                                              
         L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
*                                                                               
         CLI   EDEMOS+2,0          TEST NONT DEMO                               
         BNE   UNW1                                                             
         LLC   R3,EDEMOS+1         GET NONT DEMO INDEX                          
         BCTR  R3,0                                                             
         SLL   R3,3                X 8                                          
         LA    R3,ENONTDMS(R3)                                                  
         B     UNW1A                                                            
*                                                                               
UNW1     LA    R3,EDEMOS+1                                                      
         CLI   0(R3),X'21'         USER DEMOS                                   
         BNE   *+8                                                              
         LA    R3,EUSRNMS                                                       
*                                                                               
UNW1A    CLI   0(R3),C'E'          EXTENDED RATINGS                             
         BE    *+8                                                              
         CLI   0(R3),C'R'          ONLY UNWEIGHT RATINGS                        
         BNE   XIT                                                              
*                                                                               
         L     R3,SPWEIGHT                                                      
         CLC   THISCODE,=5X'FF'                                                 
         BE    UNW2                                                             
         MVC   WORK,THISCODE       LOOK UP WEIGHT                               
         CLC   THISBRK(5),=C'BRAND'                                             
         BNE   *+10                                                             
         MVC   WORK+3(2),=X'FFFF'                                               
         BAS   RE,GETPOOL                                                       
         MVC   DUB,WORK+5                                                       
         LH    R3,DUB                                                           
         SPACE 2                                                                
UNW2     CLC   THISBRK,=CL12'MARKET'                                            
         BNE   *+8                                                              
         LH    R3,THISCODE+2       MARKET RECORDS CARRY THE WEIGHT              
         BAS   RE,UNDO                                                          
         B     XIT                                                              
         SPACE 2                                                                
UNDO     NTR1                                                                   
         CLI   THISPAGE,5                                                       
         BE    UNDO2                                                            
         BH    UNDO4                                                            
         L     R4,ATHISGP          WEEKLY                                       
         LA    R5,18                                                            
         L     R6,MEDBUFF                                                       
         USING MEDBLOCK,R6                                                      
         CLI   MEDDAILY,C'Y'       OR DAILY                                     
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         BAS   RE,UNDO6                                                         
         L     R4,ATHISAP                                                       
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
UNDO2    LA    R4,THISMGP          MONTHLY                                      
         LA    R5,14                                                            
         BAS   RE,UNDO6                                                         
         L     R4,=A(THISMAP)                                                   
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO4    LA    R4,THISQGP                                                       
         LA    R5,5                                                             
         BAS   RE,UNDO6                                                         
         LA    R4,THISQAP                                                       
         BAS   RE,UNDO6                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO6    NTR1                                                                   
         LTR   R3,R3                                                            
         BNZ   UNDO8                                                            
         SPACE 2                                                                
UNDO7    XC    0(4,R4),0(R4)                                                    
         LA    R4,4(R4)                                                         
         BCT   R5,UNDO7                                                         
         B     XIT                                                              
         SPACE 2                                                                
UNDO8    L     R0,0(R4)                                                         
*        SRDA  R0,31                                                            
*        DR    R0,R3                                                            
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                                                             
         CVDX  DUB,0(R4)                                                        
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=PL8'10'                                                
         CVD   R3,DUB                                                           
         DP    WORK(16),DUB                                                     
         ZAP   DUB,WORK(8)                                                      
         AP    DUB,=P'5'                                                        
         ZAP   WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10'                                                
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,UNDO8                                                         
         B     XIT                                                              
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
EDIT36   CLI   1(R2),C'P'          DONT SHOW POINTS AT CLIENT LEVEL             
         BNE   EDIT37                                                           
         CLC   THISBRK(6),=C'CLIENT'                                            
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
         CLC   0(2,R2),=C'  '                                                   
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
         CLI   1(R2),C'*'                                                       
         BE    EDIT60                                                           
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
         DC    C'F',AL1(05),C'G*',X'FF'                                         
         DC    C'G',AL1(05),C'P*',X'FF'                                         
         DC    C'H',AL1(11),C'G*P*I*  ',X'FF'                                   
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
         LA    R3,THISMGP                                                       
         BE    EDMON                                                            
         LA    R3,THISQGP                                                       
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT42   MVC   14(2,R6),0(R2)      GOAL DOLLARS                                 
         LA    R3,THISGD                                                        
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         LA    R3,THISMGD                                                       
         BE    EDMON                                                            
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
         LA    R3,THISQAP                                                       
         B     EDQUT                                                            
         SPACE 2                                                                
EDIT46   MVC   14(2,R6),0(R2)      ACTUAL DOLLARS                               
         MVI   14(R6),C'P'         SHOW P$ NOT A$                               
         L     R3,ATHISAD                                                       
         CLI   THISPAGE,5                                                       
         BL    EDWEEK                                                           
         LA    R3,THISMAD                                                       
         BE    EDMON                                                            
         LA    R3,THISQAD                                                       
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
         LA    R4,THISMGP                                                       
         BE    PERMON                                                           
         LA    R3,THISQAP                                                       
         LA    R4,THISQGP                                                       
         B     PERQUT                                                           
         SPACE 2                                                                
EDIT52   MVC   14(2,R6),0(R2)      PERCENTAGE DOLLARS                           
         L     R3,ATHISAD                                                       
         LA    R4,THISGD                                                        
         CLI   THISPAGE,5                                                       
         BL    PERWEEK                                                          
         LA    R3,THISMAD                                                       
         LA    R4,THISMGD                                                       
         BE    PERMON                                                           
         LA    R3,THISQAD                                                       
         LA    R4,THISQGD                                                       
         B     PERQUT                                                           
         SPACE 2                                                                
EDIT54   L     R3,ATHISGP          GOAL COST PER POINT                          
         LA    R3,68(R3)                                                        
         LA    R4,THISGD+68                                                     
         CLI   DAILYSW,C'Y'        DAILY TABLES ARE 4 BYTES SHORTER             
         BNE   *+12                                                             
         AHI   R3,-4                                                            
         AHI   R4,-4                                                            
         CLI   THISPAGE,5                                                       
         BL    CPP                                                              
         LA    R3,THISMGP+52       THERE ARE 13 MONTHS NOW !                    
         LA    R4,THISMGD+52                                                    
         BE    CPP                                                              
         LA    R3,THISQGP+16                                                    
         LA    R4,THISQGD+16                                                    
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
         LA    R4,THISMAD                                                       
         LA    R4,48(R4)                                                        
         BE    CPP                                                              
         LA    R3,THISQAP+16                                                    
         LA    R4,THISQAD+16                                                    
         B     CPP                                                              
         SPACE 2                                                                
EDIT60   MVC   14(2,R6),0(R2)                                                   
         LA    R3,THISGD                                                        
         CLI   THISPAGE,5                                                       
         BH    CPPQUT                                                           
         BE    CPPMON                                                           
         BL    CPPWEEK                                                          
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
***      CLI   THISDEM,X'FF'                                                    
***      BNE   XIT                                                              
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
         SPACE 2                                                                
CMPD6    OC    0(4,R2),0(R2)                                                    
         BNZ   CMPD4                                                            
         LA    R3,4(R3)            DISPLACE 4 FOR INACTIVE MONTHS               
         SH    R2,=H'12'                                                        
         B     CMPD6                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              NUMBER EDITING LOOPS                                             
*                                                                               
EDWEEK   LA    R4,16(R6)                                                        
         A     R4,DISP                                                          
         LA    R5,14                                                            
         SPACE 2                                                                
EDWEEK2  CLI   1(R2),C'P'          TEST DOING POINTS                            
         JNE   EDWEEK2B                                                         
         TM    0(R3),X'C0'         TEST BIG NUMBER BITS ON                      
         JNZ   EDWEEK2B                                                         
*                                                                               
EDWEEK2A L     R1,0(R3)                                                         
         C     R1,=F'999999'       MAX WITH 1-DEC                               
         JH    EDWEEK2B                                                         
         EDIT  (R1),(8,DMCB),1                                                  
         J     EDWEEK2C                                                         
*                                                                               
EDWEEK2B BAS   RE,LOAD                                                          
         EDIT  (R1),(8,DMCB)                                                    
*                                                                               
EDWEEK2C MVC   0(6,R4),DMCB+2                                                   
         CLI   DMCB+2,C' '         WILL IT FIT ANYWAY                           
         BE    EDWEEK3                                                          
         SH    R4,=H'2'            IS THE PREVIOUS FIELD FILLED                 
         CLC   0(2,R4),SPACES                                                   
         BNE   EDWEEK2D                                                         
         MVC   0(8,R4),DMCB        NO - SO ITS OK TO MOVE FIELD IN              
         LA    R4,2(R4)                                                         
         B     EDWEEK3                                                          
         SPACE 2                                                                
EDWEEK2D MVC   132(8,R4),DMCB      YES - SO WE'LL HAVE TO PUT IT BELOW          
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
         SPACE 2                                                                
EDMON    CLI   QOPT1,C'Y'          YEAR TO DATE OPTION                          
         BE    EDYTD                                                            
         CLI   QOPT2,C'Y'                                                       
         BNE   EDMON1                                                           
         DROP  R6                                                               
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
EDQUT2   BAS   RE,LOAD4                                                         
         EDIT  (R1),(11,0(R4))                                                  
         M     R0,TALFAC           MAY NEED TO UP FOR TIME+TALENT               
         D     R0,TALBASE                                                       
         CLI   QOPT2,C'C'                                                       
         BE    *+6                                                              
         SR    R1,R1                                                            
         CLI   1(R2),C'P'                                                       
         BNE   *+6                                                              
         SR    R1,R1                                                            
         EDIT  (R1),(11,11(R4))                                                 
         LA    R3,4(R3)                                                         
         LA    R4,22(R4)                                                        
         BCT   R5,EDQUT2                                                        
         B     EDBUMP                                                           
         EJECT                                                                  
*              EDITING FOR CPP REPORTS                                          
         SPACE 2                                                                
CPPWEEK  LA    R4,16(R6)                                                        
         A     R4,DISP                                                          
         LA    R5,14                                                            
         SPACE 2                                                                
CPPWEEK2 BAS   RE,CPPLOAD                                                       
         MVC   0(6,R4),DMCB+2                                                   
         CLI   DMCB+2,C' '         WILL IT FIT ANYWAY                           
         BE    CPPWEEK6                                                         
         SH    R4,=H'2'                                                         
         CLC   0(2,R4),SPACES      IS PREVIOUS FIELD FILLED                     
         BNE   CPPWEEK4                                                         
         MVC   0(8,R4),DMCB        NO - OK TO MOVE IN                           
         LA    R4,2(R4)                                                         
         B     CPPWEEK6                                                         
         SPACE 2                                                                
CPPWEEK4 MVC   132(8,R4),DMCB      YES - PUT IT BELOW                           
         LA    R4,2(R4)                                                         
         MVC   0(6,R4),SPACES                                                   
         SPACE 2                                                                
CPPWEEK6 LA    R3,4(R3)                                                         
         A     R4,COLDISP                                                       
         BCT   R5,CPPWEEK2                                                      
         LA    R4,100(R6)          NO DO SUMMARY ON RIGHT                       
         LA    R5,4                                                             
         SPACE 2                                                                
CPPWEEK8 BAS   RE,CPPLOAD                                                       
         MVC   0(8,R4),DMCB                                                     
         LA    R3,4(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,CPPWEEK8                                                      
         B     EDBUMP                                                           
         SPACE 2                                                                
CPPMON   LA    R4,16(R6)                                                        
         A     R4,DISP                                                          
         LA    R5,13                                                            
         XC    BASECPP,BASECPP                                                  
         CLI   PROGPROF+4,0        OPTION TO INDEX BASE MONTH                   
         BE    CPPMON2                                                          
         CLI   QOPT3,C'F'                                                       
         BNE   CPPMON2                                                          
         ZIC   R1,PROGPROF+4                                                    
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R3,R1                                                            
         BAS   RE,CPPLOAD          GO AND FIGURE CPP FOR BASE MONTH             
         MVC   BASECPP,COMPCPP                                                  
         SR    R3,R1                                                            
         SPACE 2                                                                
CPPMON2  BAS   RE,CPPLOAD                                                       
         MVC   0(8,R4),DMCB                                                     
         OC    BASECPP,BASECPP     WORK OUT INDEX TO BASE MONTH                 
         BZ    CPPMON4                                                          
         OC    COMPCPP,COMPCPP                                                  
         BZ    CPPMON4                                                          
         L     R1,COMPCPP                                                       
         M     R0,=F'2000'                                                      
         D     R0,BASECPP                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(8,DMCB),1                                                  
         CLI   DMCB+2,C' '                                                      
         BE    *+10                                                             
         MVC   DMCB(8),=C'    HIGH'                                             
         MVC   132(8,R4),DMCB                                                   
         SPACE 2                                                                
CPPMON4  LA    R3,4(R3)                                                         
         LA    R4,9(R4)                                                         
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         OC    MEDMON13(4),MEDMON13                                             
         BZ    *+6                                                              
         BCTR  R4,0                                                             
         DROP  R1                                                               
         BCT   R5,CPPMON2                                                       
         BAS   RE,CPPLOAD                                                       
         MVC   124(8,R6),DMCB                                                   
         B     EDBUMP                                                           
         SPACE 2                                                                
CPPQUT   LA    R4,22(R6)                                                        
         LA    R5,5                                                             
         SPACE 2                                                                
CPPQUT2  BAS   RE,CPPLOAD                                                       
         MVC   3(8,R4),DMCB                                                     
         LA    R3,4(R3)                                                         
         LA    R4,22(R4)                                                        
         BCT   R5,CPPQUT2                                                       
         B     EDBUMP                                                           
         EJECT                                                                  
*              ROUTINE TO EDIT CPP AND CPP INDEX FOR CPP REPORTS                
         SPACE 3                                                                
CPPLOAD  NTR1                                                                   
         XC    COMPCPP,COMPCPP                                                  
         MVC   DMCB(16),SPACES     R3=A(DOLLARS)                                
         LA    R4,20               R4=DISPLACEMENT                              
         CLI   THISPAGE,5                                                       
         BH    CPPL2                                                            
         LA    R4,56                                                            
         BE    CPPL2                                                            
         LA    R4,72                                                            
         SPACE 2                                                                
CPPL2    CLI   0(R2),C'P'          ADJUST PURCHSED                              
         BNE   *+8                                                              
         AR    R3,R4                                                            
         AR    R3,R4                                                            
         L     R1,0(R3)            CASH                                         
         LPR   R1,R1                                                            
         SR    R0,R0                                                            
         TM    0(R3),X'C0'                                                      
         BNM   CPPL3                                                            
         TM    0(R3),X'80'                                                      
         BNO   CPPL3                                                            
         L     R1,0(R3)                                                         
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         M     R0,=F'100'                                                       
         SPACE 1                                                                
CPPL3    LTR   R1,R1                                                            
         BZ    XIT                                                              
         L     R5,0(R3,R4)         POINTS                                       
         LTR   R5,R5                                                            
         BZ    XIT                                                              
*        M     R0,=F'20'                                                        
*        DR    R0,R5                                                            
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                                                             
*        LR    R5,R1                                                            
*        M     R0,REQPCT           REQUESTABLE PERCENT ADJUSTMENT               
*        D     R0,=F'10000'                                                     
*        AR    R1,R5                                                            
*                                                                               
         CVD   R1,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=PL8'100'                                               
         CVD   R5,DUB                                                           
         DP    WORK(16),DUB                                                     
         ZAP   DUB,WORK(8)                                                      
         AP    DUB,=P'5'                                                        
         ZAP   WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10'                                                
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         L     R0,REQPCT                                                        
         CVD   R0,DUB                                                           
         MP    WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10000'                                             
         ZAP   DUB,WORK(8)                                                      
         CVB   R0,DUB                                                           
         AR    R1,R0                                                            
*                                                                               
         ST    R1,COMPCPP                                                       
         EDIT  (R1),(8,DMCB),2     EDIT CPP                                     
         CLI   DMCB,C' '                                                        
         BE    CPPL4                                                            
         EDIT  (R1),(10,DMCB)                                                   
         SPACE 2                                                                
CPPL4    CLI   0(R2),C'I'          INDEX ROUTINES                               
         BNE   XIT                                                              
         LR    R6,R1               SAVE GOAL CPP                                
         AR    R3,R4                                                            
         AR    R3,R4               BUMP TO PURCHASED                            
         MVC   DMCB(16),SPACES                                                  
         L     R1,0(R3)            COMPUTE PURCHASED CPP                        
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         L     R5,0(R3,R4)                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
*        M     R0,=F'20'                                                        
*        DR    R0,R5                                                            
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                                                             
*        M     R0,=F'2000'         EXPRESS PURCHASED V GOAL                     
*        DR    R0,R6                       AS A PERCENT TO 1 DEC                
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                                                             
*                                                                               
         CVD   R1,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=PL8'100'                                               
         LTR   R5,R5               PROTECT AGAINST ZERO DIVIDE                  
         BNZ   *+8                                                              
         LA    R5,1                                                             
         CVD   R5,DUB                                                           
         DP    WORK(16),DUB                                                     
         ZAP   DUB,WORK(8)                                                      
         AP    DUB,=P'5'                                                        
         ZAP   WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10'                                                
         ZAP   DUB,WORK(8)                                                      
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=PL8'10000'                                             
         LTR   R6,R6               PROTECT AGAINST ZERO DIVIDE                  
         BNZ   *+8                                                              
         LA    R6,1                                                             
         CVD   R6,DUB                                                           
         DP    WORK(16),DUB                                                     
         ZAP   DUB,WORK(8)                                                      
         AP    DUB,=P'5'                                                        
         ZAP   WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10'                                                
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         EDIT  (R1),(8,DMCB),1                                                  
         CLI   DMCB+2,C' '                                                      
         BE    XIT                                                              
         MVC   DMCB(8),=C'    HIGH'                                             
         B     XIT                                                              
         EJECT                                                                  
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
EDBUMP   L     R6,PAGEMARK                                                      
         LA    R6,132(R6)                                                       
         ST    R6,PAGEMARK                                                      
         CLC   0(132,R6),SPACES                                                 
         BNE   EDBUMP                                                           
         B     EDIT34                                                           
         SPACE 2                                                                
LOAD     CLI   1(R2),C'P'          TEST DOING POINTS                            
         BNE   LOAD1                                                            
         TM    0(R3),X'C0'                                                      
         JNZ   LOAD1                                                            
         L     R1,0(R3)            SCALE TO WHOLE NUMBER                        
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
LOAD1    L     R1,0(R3)                                                         
         LPR   R1,R1                                                            
         SR    R0,R0                                                            
         MVI   HUNDRED,C'N'                                                     
         TM    0(R3),X'C0'                                                      
         BNM   LOAD2                                                            
         TM    0(R3),X'80'                                                      
         BNO   LOAD2                                                            
         L     R1,0(R3)                                                         
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         MVI   HUNDRED,C'Y'                                                     
*                                                                               
LOAD2    CLI   1(R2),C'+'          CHILD SPOT FEATURE                           
         BNE   LOAD6                                                            
         M     R0,TALFAC                                                        
         D     R0,TALBASE                                                       
         SR    R0,R0                                                            
         B     LOAD6                                                            
         SPACE 1                                                                
HUNDRED  DC    C'N'                                                             
         SPACE 1                                                                
LOAD4    L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         MVI   HUNDRED,C'N'                                                     
         TM    0(R3),X'C0'                                                      
         BNM   LOAD6                                                            
         TM    0(R3),X'80'                                                      
         BNO   LOAD6                                                            
         SLL   R1,2                                                             
         SRL   R1,2                                                             
         MVI   HUNDRED,C'Y'                                                     
*                                                                               
LOAD6    CLI   HUNDRED,C'Y'                                                     
         BNE   *+8                                                              
         M     R0,=F'100'                                                       
         LA    RF,5                SCALE TO POINT OR DOLLARS ROUNDED            
         CLI   1(R2),C'P'                                                       
         BE    *+8                                                              
         LA    RF,50                                                            
         DR    R0,RF                                                            
         AHI   R1,1                                                             
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
PCT      MVC   WORK(6),SPACES                                                   
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
         CLC   THISBRK(6),=C'CLIENT'                                            
         BE    EDIT34                                                           
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
         CHI   RF,1                                                             
         BE    *+6                                                              
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
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
         GOTO1 REPORT                                                           
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
         LM    R9,RC,SPM9R9                                                     
         L     R7,SPM9R7                                                        
         DROP  RF                                                               
         USING SPM902,RB,R9,R7                                                  
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
         XC    COMKMKT,COMKMKT     TRY FOR EST LEVEL                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST COMMENT EXISTS                          
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
ASPM904  DS    A                                                                
ASPM9041 DC    A(0)                                                             
ATHISGP  DS    A                                                                
ATHISAD  DS    A                                                                
ATHISAP  DS    A                                                                
PAGEMARK DS    F                                                                
SLCOUNT  DS    CL1                                                              
DPCOUNT  DS    CL1                                                              
SUMSW    DC    C'Y'                                                             
DAILYSW  DC    C'N'                                                             
CNT1     DC    PL3'0'                                                           
CNT2     DC    PL3'0'                                                           
CNT3     DC    PL3'0'                                                           
RDPRD    DC    C'Y'                                                             
CENT     DC    F'0'                                                             
TALFAC   DC    F'0'                                                             
TALBASE  DC    F'0'                                                             
SVG0PROF DS    CL16                                                             
*                                                                               
FLTOT1   DC    F'0'                                                             
FLTOT2   DC    F'0'                                                             
FLTOT3   DC    F'0'                                                             
FLTOT4   DC    F'0'                                                             
SPM9R7   DS    F                                                                
SPM9R9   DS    F                                                                
SPM9RA   DS    F                                                                
SPM9RB   DS    F                                                                
SPM9RC   DS    F                                                                
COLDISP  DC    F'6'                                                             
DISP     DC    F'0'                                                             
BUFFLEVL DC    F'0'                                                             
LASTKEY  DC    CL49' '                                                          
MARKACT  DS    CL1                                                              
DPFILT   DS    CL3                                                              
REQPCT   DS    F                                                                
CSHPCT   DS    F                                                                
COMPCPP  DS    F                                                                
BASECPP  DS    F                                                                
         DS    0D                                                               
         DS    CL1                                                              
BIGNUM   DS    PL15                                                             
         ORG   BIGNUM+7                                                         
BIGDUB   DS    D                                                                
MCOMSW   DS    C                                                                
         SPACE 3                                                                
*              LTORG / CSECTS / DSECTS/ ETC                                     
         SPACE 3                                                                
RELO     DS    A                                                                
LNBUFFER DS    F                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUFFALO RECORD AREA                                              
         SPACE 3                                                                
         DS    0D                                                               
MYBUFFIO DS    1450C                                                            
         ORG   MYBUFFIO                                                         
THISKEY  DS    0CL9                                                             
THISREPT DS    CL1                                                              
THISPAGE DS    CL1                                                              
THISCODE DS    CL5                                                              
THISBRK  DS    CL12                                                             
THISNAME DS    CL24                                                             
THISDPT  DS    CL4                                                              
THISSL   DS    CL1                                                              
THISTYPE DS    CL1                                                              
         SPACE 2                                                                
THISGD   DS    CL72                                                             
THISGP   DS    CL72                                                             
THISAD   DS    CL72                                                             
THISAP   DS    CL72                                                             
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
         ORG                                                                    
         SPACE 2                                                                
PSLIST   DS    CL512                                                            
         EJECT                                                                  
*              OTHER CSECTS                                                     
         SPACE 3                                                                
*                                                                               
EDBLOCK  CSECT                                                                  
         DS    50CL132                                                          
         BUFF  LINES=0001,ROWS=1,COLUMNS=72,FLAVOR=BINARY,             X        
               KEYLIST=(49,A)                                                   
         SPACE 2                                                                
ACTPOOL  CSECT                                                                  
         DC    F'500'                                                           
         DC    F'0'                                                             
         DC    3500C'00'                                                        
         SPACE 2                                                                
LODAREA  CSECT                                                                  
         DS    CL768                                                            
         SPACE 2                                                                
WTPOOL   CSECT                                                                  
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    7000X'00'                                                        
         EJECT                                                                  
* HEADLINE HOOK ROUTINE                                                         
*                                                                               
         SPACE 1                                                                
HEDHK    NMOD1 0,**M9HK**                                                       
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
         EJECT                                                                  
*              LEFT HAND STUBS & GOAL EDITING                                   
         SPACE 3                                                                
         MVI   H10,C'-'            SELECT STUBS                                 
         MVC   H10+1(11),H10                                                    
         MVC   H11(4),=C'GRP.'                                                  
         CLC   THISBRK(12),=CL12'MARKET'                                        
         BNE   *+10                                                             
         MVC   H11(4),=C'MKT.'                                                  
         CLC   THISBRK(5),=C'BRAND'                                             
         BNE   *+10                                                             
         MVC   H11(4),=C'PRD.'                                                  
         CLI   QOPT3,C'2'                                                       
         BL    HK1                                                              
         CLI   QOPT3,C'4'                                                       
         BNE   HK1A                                                             
         SPACE 2                                                                
HK1      MVC   H12+5(5),=C'(CPP)'                                               
         L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
         LA    R3,EDEMOS+2                                                      
         TM    ECNTRL,X'01'                                                     
         BNO   *+8                                                              
         LA    R3,EDEMOS+1                                                      
         CLI   0(R3),C'E'          EXTENDED RATING                              
         BE    *+8                                                              
         CLI   0(R3),C'R'          CPP OR CPM                                   
         BE    HK1A                                                             
         MVC   H12+6(3),=C'CPM'                                                 
         SPACE 2                                                                
HK1A     MVC   H13(14),H10                                                      
         CLI   QDPTDET,C'C'                                                     
         BE    HK2                                                              
         MVC   H11+5(3),=C'DPT'                                                 
         CLI   QDPTDET,C'B'                                                     
         BE    HK2                                                              
         MVC   H11+8(4),=C'-LEN'                                                
         SPACE 2                                                                
HK2      L     R2,ADBLOCK                                                       
         USING DBLOCK,R2                                                        
         XC    0(256,R2),0(R2)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         DROP  R2                                                               
         L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
         MVC   HKAREA,SPACES                                                    
         MVC   SPUSRNMS,EUSRNMS                                                 
         LA    RE,ENONTDMS                                                      
         ST    RE,DMCB+16                                                       
         LA    R2,EDEMOS                                                        
         DROP  R2                                                               
         LA    R3,HKAREA                                                        
         LA    R4,14                                                            
         CLC   PRDBUFLN,=H'56'                                                  
         BNH   HK4                                                              
         SPACE 1                                                                
HK3      OC    0(3,R2),0(R2)       NEW EST HEADER                               
         BZ    HK6                                                              
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
HK6      CLI   MODE,PRDLAST                                                     
         BH    HK8B                                                             
         CLC   H7(20),SPACES                                                    
         BE    HK8                                                              
         MVC   MID1(8),=C'GOALS - '                                             
         GOTO1 CHOPPER,HKPARA,(112,HKAREA),(56,MID1+9),(C'P',2)                 
         B     HK8B                                                             
         SPACE 2                                                                
HK8      GOTO1 CHOPPER,HKPARA,(112,HKAREA),(39,H7+9),(C'P',3)                   
         MVC   H7(8),=C'GOALS - '                                               
         SPACE 2                                                                
HK8B     JIF   QOPT3,EQ,C'4',OR,C'5',OR,C'6',OR,C'B',HK9,JUMP=N                 
         JIF   QOPT3,EQ,C'7',OR,C'8',OR,C'9',OR,C'C',HK10,JUMP=N                
         JIF   QOPT3,EQ,C'J',OR,C'K',OR,C'L',HK10B,JUMP=N                       
         CLI   QOPT3,C'F'                                                       
         BE    HK10F                                                            
         CLI   QOPT3,C'G'                                                       
         BE    HK10G                                                            
         CLI   QOPT3,C'H'                                                       
         BE    HK10H                                                            
         B     HK11                                                             
         SPACE 2                                                                
HK9      MVC   H7+99(13),=C'(P=PURCHASED)'                                      
         B     HK11                                                             
         SPACE 2                                                                
HK10     MVC   H7+99(28),=C'(G=GOAL P=PURCHASED I=INDEX)'                       
         B     HK11                                                             
         SPACE 2                                                                
HK10B    MVC   H7+99(27),=C'(G=GOAL P=PURCHASED D=DIFF)'                        
         B     HK11                                                             
         SPACE 2                                                                
HK10F    MVC   H7+99(13),=C'(G*=GOAL CPP)'                                      
         B     HK10CPP                                                          
         SPACE 2                                                                
HK10G    MVC   H7+99(18),=C'(P*=PURCHASED CPP)'                                 
         B     HK10CPP                                                          
         SPACE 2                                                                
HK10H    MVC   H7+99(32),=C'G=GOAL P=PURCHASED I=INDEX *=CPP'                   
         SPACE 2                                                                
HK10CPP  MVC   H1+55(21),=C'COST PER POINT REPORT'                              
         MVC   H2+55(21),=21C'-'                                                
         EJECT                                                                  
HK11     LA    R4,H1+40                                                         
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
*              QUARTER ROUTINES                                                 
         SPACE 3                                                                
         CLI   THISPAGE,5                                                       
         BL    HK30                                                             
         BE    HK20                                                             
         CLI   QOPT2,C'C'          REGULAR QUARTER RECAP                        
         BE    HK12                                                             
         MVC   H11+26(7),=C' FIRST '                                            
         MVC   H11+48(7),=C'SECOND '                                            
         MVC   H11+70(7),=C' THIRD '                                            
         MVC   H11+92(7),=C'FOURTH '                                            
         MVC   H12+26(7),=C'PERIOD '                                            
         MVC   H12+48(7),=C'PERIOD '                                            
         MVC   H12+70(7),=C'PERIOD '                                            
         MVC   H12+70(7),=C'PERIOD '                                            
         MVC   H12+92(7),=C'PERIOD '                                            
         MVC   H10+115(6),=C'------'                                            
         MVC   H11+115(6),=C'ANNUAL'                                            
         MVC   H12+115(6),=C'TOTALS'                                            
         MVC   H13+115(6),=C'------'                                            
         B     HK14                                                             
         SPACE 2                                                                
HK12     MVC   H10+115(17),=17C'-' CHILD SPOT QUARTER RECAP                     
         MVC   H13+115(17),=17C'-'                                              
         MVC   H11+027(17),=C'  FIRST PERIOD   '                                
         MVC   H11+049(17),=C' SECOND PERIOD   '                                
         MVC   H11+071(17),=C'  THIRD PERIOD   '                                
         MVC   H11+093(17),=C' FOURTH PERIOD   '                                
         MVC   H11+115(17),SPACES                                               
         MVC   H11+120(6),=C'ANNUAL'                                            
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
         A     R3,DISP                                                          
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
         A     R3,DISP                                                          
         LA    R4,14                                                            
         SPACE 2                                                                
HK42     EDIT  (R5),(2,1(R3))                                                   
         GOTO1 DATCON,HKPARA,(2,0(R2)),(4,132(R3))                              
         LA    R2,12(R2)                                                        
         A     R3,COLDISP                                                       
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
PWPREFIX DS    CL5                                                              
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DEDBLOCK                                                       
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072SPREPM902 11/21/19'                                      
         END                                                                    
